{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Server (
  -- * AppM
  AppM,
  DbState (..),
  ValidatedFunction (..),

  -- * Servant
  OperationId,

  -- * REST API
  Api,
  FunctionApi,
  FunctionApi' (..),
  SingleFunctionApi,
  SingleFunctionApi' (..),
  FunctionCrud,
  FunctionCrud' (..),
  handler,

  -- * API json types
  Parameters (..),
  Parameter (..),
  Function (..),
  SimpleFunction (..),
  SimpleResponse (..),
  Reasoning (..),
  ReasoningTree (..),
  ResponseWithReason (..),
  EvaluatorError (..),
  FnLiteral (..),
  EvalBackend (..),
  FunctionImplementation (..),
  FnArguments (..),

  -- * utilities
  toDecl,
) where

import Control.Concurrent.STM
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader (ReaderT (..), asks)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Typeable
import GHC.Generics
import GHC.TypeLits
import Servant
import System.Timeout (timeout)

import Backend.Api as Api
import Backend.Simala qualified as Simala
import Data.Text.Lazy.Encoding qualified as TL

-- ----------------------------------------------------------------------------
-- Servant API
-- ----------------------------------------------------------------------------

data DbState = DbState
  { functionDatabase :: TVar (Map Text ValidatedFunction)
  }
  deriving (Eq, Generic)

data ValidatedFunction = ValidatedFunction
  { fnImpl :: !Function
  , fnEvaluator :: !(Map EvalBackend Evaluator)
  }
  deriving (Generic)

type AppM = ReaderT DbState Handler

type Api = NamedRoutes FunctionApi'
type FunctionApi = NamedRoutes FunctionApi'

-- | API that can be invoked by a custom gpt.
--
-- See https://openai.com/index/introducing-gpts/
data FunctionApi' mode = FunctionApi
  { functionRoutes :: mode :- "functions" :> FunctionCrud
  }
  deriving (Generic)

type FunctionCrud = NamedRoutes FunctionCrud'

-- | API for interacting with the 'function' resource.
data FunctionCrud' mode = FunctionCrud
  { batchEntities ::
      mode
        :- Summary "Shortened descriptions of all available functions and their parameters"
          :> OperationId "getAllFunctions"
          :> Get '[JSON] [SimpleFunction]
  , singleEntity ::
      mode
        :- Capture "name" String
          :> SingleFunctionApi
  , functionEvaluation ::
      mode
        :- NamedRoutes FunctionEvaluationApi
  }
  deriving (Generic)

data FunctionEvaluationApi mode = FunctionEvaluationApi
  { computeQualifiesFunc ::
      mode
        :- "compute_qualifies"
          :> "eval"
          :> QueryString
          :> Summary "Compute whether a person qualifies based on their properties"
          :> OperationId "runComputeQualifies"
          :> Post '[JSON] SimpleResponse
  , rodentsAndVerminFunc ::
      mode
        :- "rodents_and_vermin"
          :> "eval"
          :> QueryString
          :> Summary "Compute whether a person qualifies based on their properties."
          :> Description "A response value of `0` means that the Loss or Damage is covered, while `1` means the Loss or Damage is not covered."
          :> OperationId "runRodentsAndVermin"
          :> Post '[JSON] SimpleResponse
  }
  deriving (Generic)

type SingleFunctionApi = NamedRoutes SingleFunctionApi'
data SingleFunctionApi' mode = SingleFunctionApi
  { getFunction ::
      mode
        :- Summary "Get a detailed description of the function and its parameters"
          :> OperationId "getFunction"
          :> Get '[JSON] Function
  , postFunction ::
      mode
        :- Summary "Add a function resource that can be evaluated."
          :> ReqBody '[JSON] FunctionImplementation
          :> OperationId "createFunction"
          :> Post '[JSON] ()
  , putFunction ::
      mode
        :- Summary "Update a function resource"
          :> ReqBody '[JSON] FunctionImplementation
          :> OperationId "updateFunction"
          :> Put '[JSON] ()
  , deleteFunction ::
      mode
        :- Summary "Delete the function"
          :> OperationId "deleteFunction"
          :> Delete '[JSON] ()
  , evalFunction ::
      mode
        :- "evaluation"
          :> Summary "Evaluate a function with arguments"
          :> ReqBody '[JSON] FnArguments
          :> OperationId "evalFunction"
          :> Post '[JSON] SimpleResponse
  }
  deriving (Generic)

data SimpleFunction = SimpleFunction
  { simpleName :: Text
  , simpleDescription :: Text
  }
  deriving (Show, Read, Ord, Eq, Generic)

data Function = Function
  { name :: !Text
  , description :: !Text
  , parameters :: !Parameters
  , supportedEvalBackend :: [EvalBackend]
  }
  deriving (Show, Read, Ord, Eq, Generic)

data FunctionImplementation = FunctionImplementation
  { declaration :: !Function
  , implementation :: !(Map EvalBackend Text)
  }
  deriving (Show, Read, Ord, Eq, Generic)

newtype Parameters = Parameters
  { getParameters :: Map Text Parameter
  }
  deriving (Show, Read, Ord, Eq, Generic)

data Parameter = Parameter
  { parameterType :: !Text
  , parameterEnum :: ![Text]
  , parameterDescription :: !Text
  }
  deriving (Show, Read, Ord, Eq, Generic)

data SimpleResponse
  = SimpleResponse !ResponseWithReason
  | SimpleError !EvaluatorError
  deriving (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data FnArguments = FnArguments
  { fnEvalBackend :: Maybe EvalBackend
  , fnArguments :: Map Text (Maybe FnLiteral)
  }
  deriving (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ----------------------------------------------------------------------------
-- Servant Combinators
-- ----------------------------------------------------------------------------

data OperationId (symbol :: Symbol)
  deriving (Typeable)

instance (HasLink sub) => HasLink (OperationId s :> sub) where
  type MkLink (OperationId s :> sub) a = MkLink sub a
  toLink = simpleToLink (Proxy :: Proxy sub)

simpleToLink ::
  forall sub a combinator.
  (HasLink sub, MkLink sub a ~ MkLink (combinator :> sub) a) =>
  Proxy sub ->
  (Link -> a) ->
  Proxy (combinator :> sub) ->
  Link ->
  MkLink (combinator :> sub) a
simpleToLink _ toA _ = toLink toA (Proxy :: Proxy sub)

-- | Ignore @'OperationId'@ in server handlers.
instance (HasServer api ctx) => HasServer (OperationId desc :> api) ctx where
  type ServerT (OperationId desc :> api) m = ServerT api m

  route _ = route (Proxy :: Proxy api)
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt s

-- ----------------------------------------------------------------------------
-- Web Service Handlers
-- ----------------------------------------------------------------------------

data EvalBackend
  = GenericMathLang
  | Simala
  deriving (Show, Eq, Ord, Enum, Read, Bounded)

handler :: ServerT Api AppM
handler =
  FunctionApi
    { functionRoutes =
        FunctionCrud
          { batchEntities = getAllFunctions
          , singleEntity = \name ->
              SingleFunctionApi
                { getFunction =
                    getFunctionHandler name
                , putFunction =
                    putFunctionHandler name
                , postFunction =
                    postFunctionHandler name
                , deleteFunction =
                    deleteFunctionHandler name
                , evalFunction =
                    evalFunctionHandler name
                }
          , functionEvaluation =
              FunctionEvaluationApi
                { computeQualifiesFunc =
                    computeQualifiesHandler
                , rodentsAndVerminFunc =
                    rodentsAndVerminHandler
                }
          }
    }

evalFunctionHandler :: String -> FnArguments -> AppM SimpleResponse
evalFunctionHandler name' args = do
  functionsTVar <- asks functionDatabase
  functions <- liftIO $ readTVarIO functionsTVar
  case Map.lookup name functions of
    Nothing -> throwError err404
    Just fnImpl ->
      runEvaluatorFor args.fnEvalBackend fnImpl args
 where
  name = Text.pack name'

runEvaluatorFor :: Maybe EvalBackend -> ValidatedFunction -> FnArguments -> AppM SimpleResponse
runEvaluatorFor engine validatedFunc fnArguments = do
  eval <- evaluationEngine evalBackend validatedFunc
  evaluationResult <-
    timeoutAction $
      runExceptT
        ( runEvaluatorForFunction
            eval
            (Map.assocs fnArguments.fnArguments)
        )

  case evaluationResult of
    Left err -> pure $ SimpleError err
    Right r -> pure $ SimpleResponse r
 where
  evalBackend = Maybe.fromMaybe Simala engine

deleteFunctionHandler :: String -> AppM ()
deleteFunctionHandler name' = do
  functionsTVar <- asks functionDatabase
  exists <- liftIO $ atomically $ stateTVar functionsTVar $ \functions ->
    case Map.member name functions of
      True ->
        (True, Map.delete name functions)
      False ->
        (False, functions)

  when (not exists) $
    throwError
      err404
        { errBody =
            "Resource "
              <> BS.fromStrict (Text.encodeUtf8 name)
              <> " does not exist"
        }
 where
  name = Text.pack name'

putFunctionHandler :: String -> FunctionImplementation -> AppM ()
putFunctionHandler name' updatedFunctionImpl = do
  validatedFunction <- validateFunction updatedFunctionImpl
  functionsTVar <- asks functionDatabase
  exists <- liftIO $ atomically $ stateTVar functionsTVar $ \functions ->
    case Map.member name functions of
      True ->
        (True, Map.insert name validatedFunction functions)
      False ->
        (False, functions)

  when (not exists) $
    -- Error code has been chosen in accordance with
    -- https://stackoverflow.com/a/70371989
    throwError
      err404
        { errBody =
            "Resource "
              <> BS.fromStrict (Text.encodeUtf8 name)
              <> " does not exist"
        }
 where
  name = Text.pack name'

postFunctionHandler :: String -> FunctionImplementation -> AppM ()
postFunctionHandler name' newFunctionImpl = do
  validatedFunction <- validateFunction newFunctionImpl
  functionsTVar <- asks functionDatabase
  exists <- liftIO $ atomically $ stateTVar functionsTVar $ \functions ->
    case Map.member name functions of
      True ->
        (True, functions)
      False ->
        (False, Map.insert name validatedFunction functions)

  when exists $
    -- Error code has been chosen in accordance with
    -- https://stackoverflow.com/a/70371989
    throwError
      err409
        { errBody =
            "Resource "
              <> BS.fromStrict (Text.encodeUtf8 name)
              <> " already exists"
        }
 where
  name = Text.pack name'

validateFunction :: FunctionImplementation -> AppM ValidatedFunction
validateFunction fn = do
  evaluators <- Map.traverseWithKey validateImplementation fn.implementation
  pure
    ValidatedFunction
      { fnImpl = fn.declaration
      , fnEvaluator = evaluators
      }
 where
  validateImplementation :: EvalBackend -> Text -> AppM Evaluator
  validateImplementation GenericMathLang _program = throwError err409{errBody = "Can't add or modify gml programs."}
  validateImplementation Simala program = do
    case runExcept $ Simala.simalaEvaluator (toDecl fn.declaration) program of
      Left err -> throwError err422{errBody = "Failed to parse program: " <> TL.encodeUtf8 (TL.pack $ show err)}
      Right parsed -> pure parsed

getAllFunctions :: AppM [SimpleFunction]
getAllFunctions = do
  functions <- liftIO . readTVarIO =<< asks functionDatabase
  pure $ fmap (toSimpleFunction . fnImpl) $ Map.elems functions
 where
  toSimpleFunction s =
    SimpleFunction
      { simpleName = s.name
      , simpleDescription = s.description
      }

computeQualifiesHandler ::
  [(ByteString, Maybe ByteString)] -> AppM SimpleResponse
computeQualifiesHandler queryParameters = undefined

-- parseToFunctionCall queryParameters $ \backend params -> do
-- runEvaluatorFor
--   backend
--   ComputeQualifies
--   params

rodentsAndVerminHandler ::
  [(ByteString, Maybe ByteString)] ->
  AppM SimpleResponse
rodentsAndVerminHandler queryParameters = undefined

--  parseToFunctionCall queryParameters $ \backend params -> do
-- runEvaluatorFor
-- backend
-- RodentsAndVermin
-- params

-- parseToFunctionCall :: [(ByteString, Maybe ByteString)] -> (Maybe EvalBackend -> [(Text, Maybe FnLiteral)] -> AppM a) -> AppM a
-- parseToFunctionCall queryParameters k = do
--   backend <- getQueryParam (join (List.lookup "backend" queryParameters))
--   params <-
--     traverse
--       ( \(name, val) -> do
--           fnLiteral <- getQueryParam val
--           let
--             name' = Text.decodeUtf8 name
--           pure (name', fnLiteral)
--       )
--       queryParameters
--   let
--     finalParams = filter (("backend" /=) . fst) params
--   k
--     backend
--     finalParams

-- getQueryParam :: (FromHttpApiData a) => Maybe ByteString -> AppM (Maybe a)
-- getQueryParam Nothing = pure Nothing
-- getQueryParam (Just bs) = case parseQueryParamByteString bs of
--   Left err -> throwError err400{errBody = TL.encodeUtf8 $ TL.fromStrict err}
--   Right a -> pure $ Just a

getFunctionHandler :: String -> AppM Function
getFunctionHandler name = do
  functions <- liftIO . readTVarIO =<< asks functionDatabase
  case Map.lookup (Text.pack name) functions of
    Nothing -> throwError err404
    Just function -> pure function.fnImpl

timeoutAction :: IO b -> AppM b
timeoutAction act =
  liftIO (timeout (seconds 5) act) >>= \case
    Nothing -> throwError err500
    Just r -> pure r
 where
  seconds n = 1_000_000 * n

-- ----------------------------------------------------------------------------
-- "Database" layer
-- ----------------------------------------------------------------------------

evaluationEngine :: EvalBackend -> ValidatedFunction -> AppM Evaluator
evaluationEngine b valFn = do
  case Map.lookup b valFn.fnEvaluator of
    Nothing -> throwError err404
    Just eval -> pure eval

-------------------------------------------------------------------------------
-- Json encoders and decoders that are not derived.
-- We often need custom instances, as we want to be more lenient in what we accept
-- than what aeson does by default. Further, we try to provide a specific json schema.
--
-- ----------------------------------------------------------------------------

instance ToJSON SimpleFunction where
  toJSON (SimpleFunction n desc) =
    Aeson.object
      [ "type" .= Aeson.String "function"
      , "function"
          .= Aeson.object
            [ "name" .= Aeson.String n
            , "description" .= Aeson.String desc
            ]
      ]

instance FromJSON SimpleFunction where
  parseJSON = Aeson.withObject "Function" $ \o -> do
    "function" :: Text <- o .: "type"
    props <- o .: "function"
    simpleFn <-
      Aeson.withObject
        "function body"
        ( \p -> do
            SimpleFunction
              <$> p .: "name"
              <*> p .: "description"
        )
        props
    pure simpleFn

instance ToJSON Function where
  toJSON (Function n desc params backends) =
    Aeson.object
      [ "type" .= Aeson.String "function"
      , "function"
          .= Aeson.object
            [ "name" .= Aeson.String n
            , "description" .= Aeson.String desc
            , "parameters" .= params
            , "supportedBackends" .= backends
            ]
      ]

instance FromJSON Function where
  parseJSON = Aeson.withObject "Function" $ \o -> do
    "function" :: Text <- o .: "type"
    props <- o .: "function"
    Aeson.withObject
      "function body"
      ( \p -> do
          Function
            <$> p .: "name"
            <*> p .: "description"
            <*> p .: "parameters"
            <*> p .: "supportedBackends"
      )
      props

instance ToJSON FunctionImplementation where
  toJSON fnImpl =
    Aeson.object
      [ "declaration" .= fnImpl.declaration
      , "implementation" .= fnImpl.implementation
      ]

instance FromJSON FunctionImplementation where
  parseJSON = Aeson.withObject "Function Implementation" $ \o -> do
    FunctionImplementation
      <$> o .: "declaration"
      <*> o .: "implementation"

instance ToJSON Parameters where
  toJSON (Parameters props) =
    Aeson.object
      [ "type" .= Aeson.String "object"
      , "properties" .= props
      ]

instance FromJSON Parameters where
  parseJSON = Aeson.withObject "Parameters" $ \o -> do
    _ :: Text <- o .: "type"
    props <- o .: "properties"
    pure $ Parameters props

instance ToJSON Parameter where
  toJSON (Parameter ty enum desc) =
    Aeson.object
      [ "type" .= ty
      , "enum" .= enum
      , "description" .= desc
      ]

instance FromJSON Parameter where
  parseJSON = Aeson.withObject "Parameter" $ \p ->
    Parameter
      <$> p .: "type"
      <*> p .: "enum"
      <*> p .: "description"

instance FromHttpApiData EvalBackend where
  parseQueryParam t = case Text.toLower t of
    "gml" -> Right GenericMathLang
    "simala" -> Right Simala
    _ -> Left $ "Invalid evaluation backend: " <> t

instance ToJSON EvalBackend where
  toJSON = \case
    GenericMathLang -> Aeson.String "gml"
    Simala -> Aeson.String "simala"

instance FromJSON EvalBackend where
  parseJSON (Aeson.String s) = case Text.toLower s of
    "gml" -> pure GenericMathLang
    "simala" -> pure Simala
    o -> Aeson.prependFailure "EvalBackend" (Aeson.typeMismatch "String" $ Aeson.String o)
  parseJSON o = Aeson.prependFailure "EvalBackend" (Aeson.typeMismatch "String" o)

instance ToJSONKey EvalBackend

instance FromJSONKey EvalBackend

parseQueryParamByteString :: (FromHttpApiData a) => ByteString -> Either Text a
parseQueryParamByteString bs =
  case Text.decodeUtf8' bs of
    Left err -> Left $ Text.pack $ show err
    Right t -> parseQueryParam t

toDecl :: Function -> Api.FunctionDeclaration
toDecl fn =
  Api.FunctionDeclaration
    { Api.name = fn.name
    , Api.description = fn.description
    , Api.parameters = Map.keysSet $ fn.parameters.getParameters
    }
