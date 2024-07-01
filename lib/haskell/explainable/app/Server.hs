{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Server (
  -- * REST API
  Api,
  FunctionApi,
  FunctionApi'(..),
  SingleFunctionApi,
  SingleFunctionApi' (..),
  FunctionCrud,
  FunctionCrud'(..),
  handler,

  -- * API json types
  FlatValue (..),
  Arguments (..),
  Parameters (..),
  Parameter (..),
  Function (..),
  SimpleFunction (..),
  SimpleResponse (..),
) where

import Control.Arrow (first)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except
import Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.HashMap.Strict qualified as HashMap
import Data.Map qualified as Map
import Data.Scientific (toRealFloat)
import Data.String.Interpolate (__i)
import Data.Text qualified as Text
import Explainable.MathLang
import GHC.Generics
import Servant
import System.Timeout (timeout)

-- ----------------------------------------------------------------------------
-- Servant API
-- ----------------------------------------------------------------------------

type Api = NamedRoutes FunctionApi'
type FunctionApi = NamedRoutes FunctionApi'

data FunctionApi' mode = FunctionApi
  { functionRoutes :: mode :- "functions" :> FunctionCrud
  }
  deriving (Generic)

type FunctionCrud = NamedRoutes FunctionCrud'
data FunctionCrud' mode = FunctionCrud
  { getAllFunctions :: mode :- Get '[JSON] [SimpleFunction]
  , crud :: mode :- Capture "name" String :> SingleFunctionApi
  }
  deriving (Generic)


type SingleFunctionApi = NamedRoutes SingleFunctionApi'
data SingleFunctionApi' mode = SingleFunctionApi
  { getFunction :: mode :- Get '[JSON] Function
  , postFunction :: mode :- ReqBody '[JSON] Arguments :> Post '[JSON] SimpleResponse
  }
  deriving (Generic)

data FlatValue
  = Number Double
  | Boolean Bool
  deriving (Show, Read, Ord, Eq, Generic)

newtype Arguments = Arguments
  { mkArguments :: Map.Map Text.Text FlatValue
  }
  deriving (Show, Read, Ord, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

data SimpleResponse
  = SimpleResponse Double
  | Insufficient Text.Text
  deriving (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

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
  { mkParameters :: Map.Map String Parameter
  }
  deriving (Show, Read, Ord, Eq, Generic)

-- ----------------------------------------------------------------------------
-- Web Service Handlers
-- ----------------------------------------------------------------------------

handler :: Server Api
handler =
  FunctionApi
    { functionRoutes =
        FunctionCrud
          { getAllFunctions = handlerFunctions
          , crud = \name ->
              SingleFunctionApi
                { getFunction =
                    handlerParameters name
                , postFunction =
                    handlerFunction name
                }
          }
    }

-- handlerFunctions :: Handler [SimpleFunction]
handlerFunctions :: Handler [SimpleFunction]
handlerFunctions = do
  pure $ fmap (toSimpleFunction . snd) $ Map.elems functions
 where
  toSimpleFunction s =
    SimpleFunction
      { simpleName = name s
      , simpleDescription = description s
      }

handlerFunction :: String -> Arguments -> Handler SimpleResponse
handlerFunction name query = do
  case Map.lookup name functions of
    Nothing -> throwError err404
    Just (function, _) ->
      case runExcept $ fromParams query of
        Left err ->
          pure $ Insufficient err
        Right s -> do
          response <- timeoutAction $ runFunction s function
          pure $ SimpleResponse response

handlerParameters :: String -> Handler Function
handlerParameters name = case Map.lookup name functions of
  Nothing -> throwError err404
  Just (_, scenario) -> pure scenario

timeoutAction :: IO b -> Handler b
timeoutAction act =
  liftIO (timeout (seconds 5) act) >>= \case
    Nothing -> throwError err505
    Just r -> pure r
 where
  seconds n = 1_000_000 * n

-- ----------------------------------------------------------------------------
-- API specification for LLMs
-- ----------------------------------------------------------------------------

instance FromJSON FlatValue where
  parseJSON (Aeson.Number sci) = pure $ Number $ toRealFloat sci
  parseJSON (Aeson.Bool b) = pure $ Boolean b
  parseJSON o@(Aeson.String s) = case Text.toLower s of
    "true" -> pure $ Boolean True
    "false" -> pure $ Boolean False
    _ -> Aeson.parseFail $ "Unexpected value, expected Number or Bool but got: " <> show o
  parseJSON o =
    Aeson.parseFail $ "Unexpected value, expected Number or Bool but got: " <> show o

instance ToJSON FlatValue where
  toJSON (Number sci) = Aeson.Number $ fromRational $ toRational sci
  toJSON (Boolean b) = Aeson.Bool b

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
    _ :: Text.Text <- o .: "type"
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
    _ :: Text.Text <- o .: "type"
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

data Parameter = Parameter
  { parameterType :: String
  , parameterEnum :: [String]
  , parameterDescription :: String
  }
  deriving (Show, Read, Ord, Eq, Generic)

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

-- ----------------------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------------------

runFunction :: (MonadIO m) => MyState -> Expr Double -> m Double
runFunction s scenario = do
  (res, _, _, _) <- liftIO $ xplainF () s scenario
  pure res

fromParams :: Arguments -> Except Text.Text MyState
fromParams attrs = do
  let (valueMap, predMap) = Map.mapEither go (mkArguments attrs)
  pure $
    emptyState
      { symtabF = HashMap.fromList $ fmap (first Text.unpack) $ Map.toList valueMap
      , symtabP = HashMap.fromList $ fmap (first Text.unpack) $ Map.toList predMap
      }
 where
  go (Number n) = Left $ Val Nothing n
  go (Boolean b) = Right $ PredVal Nothing b

-- ----------------------------------------------------------------------------
-- Example Rules
-- ----------------------------------------------------------------------------

functions :: Map.Map String (Expr Double, Function)
functions =
  Map.fromList
    [ ("compute_qualifies", (personQualifies, personQualifiesFunction))
    ]

personQualifies :: Expr Double
personQualifies =
  "qualifies"
    @|= MathPred
      ( getvar "walks" |&& (getvar "drinks" ||| getvar "eats")
      )

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
