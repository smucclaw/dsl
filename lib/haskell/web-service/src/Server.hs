{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Server (
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
  EvalBackends (..),
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable
import GHC.Generics
import GHC.TypeLits
import Servant
import System.Timeout (timeout)

import Backend.Api
import Backend.Explainable (genericMathLangEvaluator)
import Backend.Simala (simalaEvaluator)
import Data.Map.Strict (Map)

-- ----------------------------------------------------------------------------
-- Servant API
-- ----------------------------------------------------------------------------

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
        :- QueryParam "backend" EvalBackends :> NamedRoutes FunctionEvaluationApi
  }
  deriving (Generic)

data FunctionEvaluationApi mode = FunctionEvaluationApi
  { computeQualifiesFunc ::
      mode
        :- "compute_qualifies"
          :> DeepQuery "argument" (Map Text FnLiteral)
          :> Summary "Compute whether a person qualifies based on their properties"
          :> OperationId "runComputeQualifies"
          :> Post '[JSON] SimpleResponse
  -- ^ Run the 'compute_qualifies' function with the given parameters.
  --
  -- Due to some issues, it seems like it is impossible (or very difficult),
  -- to make custom gpts invoke REST endpoints with JSON bodies... So, for
  -- now we simply send all parameters via Query Parameters.
  -- Until the next servant release, we have to explicitly name all
  -- query parameters. See
  -- https://github.com/haskell-servant/servant/pull/1604 for the PR that we
  -- are interested in.
  , rodentsAndVerminFunc ::
      mode
        :- "rodents_and_vermin"
          :> QueryParam "Loss or Damage.caused by insects" FnLiteral
          :> QueryParam "Loss or Damage.caused by birds" FnLiteral
          :> QueryParam "Loss or Damage.caused by vermin" FnLiteral
          :> QueryParam "Loss or Damage.caused by rodents" FnLiteral
          :> QueryParam "Loss or Damage.to Contents" FnLiteral
          :> QueryParam "Loss or Damage.ensuing covered loss" FnLiteral
          :> QueryParam "any other exclusion applies" FnLiteral
          :> QueryParam "a household appliance" FnLiteral
          :> QueryParam "a swimming pool" FnLiteral
          :> QueryParam "a plumbing, heating, or air conditioning system" FnLiteral
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
          :> OperationId "getSingleFunction"
          :> Get '[JSON] Function
  }
  deriving (Generic)

data SimpleFunction = SimpleFunction
  { simpleName :: Text.Text
  , simpleDescription :: Text.Text
  }
  deriving (Show, Read, Ord, Eq, Generic)

data Function = Function
  { name :: Text.Text
  , description :: Text.Text
  , parameters :: Parameters
  }
  deriving (Show, Read, Ord, Eq, Generic)

newtype Parameters = Parameters
  { getParameters :: Map.Map String Parameter
  }
  deriving (Show, Read, Ord, Eq, Generic)

data Parameter = Parameter
  { parameterType :: String
  , parameterEnum :: [String]
  , parameterDescription :: String
  }
  deriving (Show, Read, Ord, Eq, Generic)

data SimpleResponse
  = SimpleResponse ResponseWithReason
  | SimpleError EvaluatorError
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

data EvalBackends
  = GenericMathLang
  | Simala
  deriving (Show, Eq, Ord, Enum, Bounded)

handler :: Server Api
handler =
  FunctionApi
    { functionRoutes =
        FunctionCrud
          { batchEntities = handlerFunctions
          , singleEntity = \name ->
              SingleFunctionApi
                { getFunction =
                    getFunctionHandler name
                }
          , functionEvaluation = \backend ->
              FunctionEvaluationApi
                { computeQualifiesFunc =
                    computeQualifiesHandler backend
                , rodentsAndVerminFunc =
                    rodentsAndVerminHandler backend
                }
          }
    }

handlerFunctions :: Handler [SimpleFunction]
handlerFunctions = do
  pure $ fmap toSimpleFunction $ Map.elems functionSpecs
 where
  toSimpleFunction s =
    SimpleFunction
      { simpleName = name s
      , simpleDescription = description s
      }

computeQualifiesHandler ::
  Maybe EvalBackends -> Map Text FnLiteral -> Handler SimpleResponse
computeQualifiesHandler backend queryParameters = do
  runEvaluatorFor backend ComputeQualifies
    [ Map.lookup "drinks" queryParameters
    , Map.lookup "walks" queryParameters
    , Map.lookup "eats" queryParameters
    ]

rodentsAndVerminHandler ::
  Maybe EvalBackends ->
  Maybe FnLiteral ->
  Maybe FnLiteral ->
  Maybe FnLiteral ->
  Maybe FnLiteral ->
  Maybe FnLiteral ->
  Maybe FnLiteral ->
  Maybe FnLiteral ->
  Maybe FnLiteral ->
  Maybe FnLiteral ->
  Maybe FnLiteral ->
  Handler SimpleResponse
rodentsAndVerminHandler backend a b c d e f g h i j = do
  let
    args = [a, b, c, d, e, f, g, h, i, j]
  runEvaluatorFor backend RodentsAndVermin args

runEvaluatorFor :: Maybe EvalBackends -> FunctionName -> [Maybe FnLiteral] -> Handler SimpleResponse
runEvaluatorFor engine name arguments = do
  evaluationResult <- timeoutAction $ runExceptT (runEvaluatorForFunction eval name arguments)
  case evaluationResult of
    Left err -> pure $ SimpleError err
    Right r -> pure $ SimpleResponse r
 where
  eval = evaluationEngine $ Maybe.fromMaybe Simala engine

getFunctionHandler :: String -> Handler Function
getFunctionHandler name = case Map.lookup (Text.pack name) functionSpecs of
  Nothing -> throwError err404
  Just function -> pure function

timeoutAction :: IO b -> Handler b
timeoutAction act =
  liftIO (timeout (seconds 5) act) >>= \case
    Nothing -> throwError err500
    Just r -> pure r
 where
  seconds n = 1_000_000 * n

-- ----------------------------------------------------------------------------
-- "Database" layer
-- ----------------------------------------------------------------------------

evaluationEngine :: EvalBackends -> Evaluator
evaluationEngine = \case
  GenericMathLang -> genericMathLangEvaluator
  Simala -> simalaEvaluator

functionSpecs :: Map.Map Text.Text Function
functionSpecs =
  Map.fromList
    [ (name f, f)
    | f <- [personQualifiesFunction, rodentsAndVerminFunction]
    ]

-- | Metadata about the function that the user might want to know.
-- Further, an LLM could use this info to ask specific questions to the user.
personQualifiesFunction :: Function
personQualifiesFunction =
  Function
    "compute_qualifies"
    [__i|Determines if a person qualifies for the purposes of the rule.
      The input object describes the person's properties in the primary parameters: walks, eats, drinks.
      Secondary parameters can be given which are sufficient to determine some of the primary parameters.
      A person drinks whether or not they consume an alcoholic or a non-alcoholic beverage, in part or in whole;
      those specific details don't really matter.
      The output of the function can be either a request for required information;
      a restatement of the user input requesting confirmation prior to function calling;
      or a Boolean answer with optional explanation summary.
    |]
    $ Parameters
    $ Map.fromList
      [ ("walks", Parameter "string" ["true", "false"] "Did the person walk?")
      , ("eats", Parameter "string" ["true", "false"] "Did the person eat?")
      , ("drinks", Parameter "string" ["true", "false"] "Did the person drink?")
      , ("beverage type", Parameter "string" ["alcoholic", "non-alcoholic"] "Did the person drink an alcoholic beverage?")
      , ("in whole", Parameter "string" ["true", "false"] "Did the person drink all of the beverage?")
      ]

-- | Metadata about the function that the user might want to know.
-- Further, an LLM could use this info to ask specific questions to the user.
rodentsAndVerminFunction :: Function
rodentsAndVerminFunction =
  Function
    "vermin_and_rodent"
    [__i|We do not cover any loss or damage caused by rodents, insects, vermin, or birds.
    However, this exclusion does not apply to:
    a) loss or damage to your contents caused by birds; or
    b) ensuing covered loss unless any other exclusion applies or where an animal causes water to escape from
       a household appliance, swimming pool or plumbing, heating or air conditioning system
    |]
    $ Parameters
    $ Map.fromList
      [ ("Loss or Damage.caused by insects", Parameter "string" ["true", "false"] "Was the damage caused by insects?")
      , ("Loss or Damage.caused by birds", Parameter "string" ["true", "false"] "Was the damage caused by birds?")
      , ("Loss or Damage.caused by vermin", Parameter "string" ["true", "false"] "Was the damage caused by vermin?")
      , ("Loss or Damage.caused by rodents", Parameter "string" ["true", "false"] "Was the damage caused by rodents?")
      , ("Loss or Damage.to Contents", Parameter "string" ["true", "false"] "Is the damage to your contents?")
      , ("Loss or Damage.ensuing covered loss", Parameter "string" ["true", "false"] "Is the damage ensuing covered loss")
      , ("any other exclusion applies", Parameter "string" ["true", "false"] "Are any other exclusions besides mentioned ones?")
      , ("a household appliance", Parameter "string" ["true", "false"] "Did water escape from a household appliance due to an animal?")
      , ("a swimming pool", Parameter "string" ["true", "false"] "Did water escape from a swimming pool due to an animal?")
      , ("a plumbing, heating, or air conditioning system", Parameter "string" ["true", "false"] "Did water escape from a plumbing, heating or conditioning system due to an animal?")
      ]

-- ----------------------------------------------------------------------------
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
    "function" :: Text.Text <- o .: "type"
    props <- o .: "function"
    (n, d) <-
      Aeson.withObject
        "function body"
        ( \p -> do
            (,)
              <$> p .: "name"
              <*> p .: "description"
        )
        props
    pure $ SimpleFunction n d

instance ToJSON Function where
  toJSON (Function n desc params) =
    Aeson.object
      [ "type" .= Aeson.String "function"
      , "function"
          .= Aeson.object
            [ "name" .= Aeson.String n
            , "description" .= Aeson.String desc
            , "parameters" .= params
            ]
      ]

instance FromJSON Function where
  parseJSON = Aeson.withObject "Function" $ \o -> do
    "function" :: Text.Text <- o .: "type"
    props <- o .: "function"
    (n, d, p) <-
      Aeson.withObject
        "function body"
        ( \p -> do
            (,,)
              <$> p .: "name"
              <*> p .: "description"
              <*> p .: "parameters"
        )
        props
    pure $ Function n d p

instance ToJSON Parameters where
  toJSON (Parameters props) =
    Aeson.object
      [ "type" .= Aeson.String "object"
      , "properties" .= props
      ]

instance FromJSON Parameters where
  parseJSON = Aeson.withObject "Parameters" $ \o -> do
    _ :: Text.Text <- o .: "type"
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

instance FromHttpApiData EvalBackends where
  parseQueryParam t = case Text.toLower t of
    "gml" -> Right GenericMathLang
    "simala" -> Right Simala
    _ -> Left $ "Invalid evaluation backend: " <> t
