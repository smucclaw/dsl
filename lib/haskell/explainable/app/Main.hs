{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except
import Data.Aeson (FromJSON, ToJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.HashMap.Strict qualified as HashMap
import Data.Map qualified as Map
import Data.Scientific (toRealFloat)
import Data.String.Interpolate (__i)
import Data.Text qualified as Text
import Explainable.MathLang
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Options
import Options.Applicative
import Servant
import System.Timeout (timeout)

-- ----------------------------------------------------------------------------
-- Option Parser
-- ----------------------------------------------------------------------------

opts :: ParserInfo Options
opts =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Serve a Web Service for interacting with a MathLang evaluator"
        <> header "explainable - A web server for MathLang"
    )

-- ----------------------------------------------------------------------------
-- Servant API
-- ----------------------------------------------------------------------------

type Api = FunctionApi

type FunctionApi =
  "functions"
    :> (Functions :<|> FunctionsCrud)

type Functions = Get '[JSON] [SimpleFunction]

type FunctionsCrud =
  Capture "name" String
    :> ( ReqBody '[JSON] Attributes :> Post '[JSON] SimpleResponse
          :<|> Get '[JSON] Function
       )

data FlatValue
  = Number Double
  | Boolean Bool
  deriving (Show, Read, Ord, Eq, Generic)

instance FromJSON FlatValue where
  parseJSON (Aeson.Number sci) = pure $ Number $ toRealFloat sci
  parseJSON (Aeson.Bool b) = pure $ Boolean b
  parseJSON o =
    Aeson.parseFail $ "Unexpected value, expected Number or Bool but got: " <> show o

newtype Attributes = Attributes
  { mkAttributes :: Map.Map String FlatValue
  }
  deriving (Show, Read, Ord, Eq, Generic)
  deriving newtype (FromJSON)

data SimpleResponse
  = SimpleResponse Double
  | Insufficient String
  deriving (Show, Read, Ord, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ----------------------------------------------------------------------------
-- Main Application and wiring
-- ----------------------------------------------------------------------------

main :: IO ()
main = do
  Options{port = port} <- execParser opts
  withStdoutLogger $ \aplogger -> do
    let settings = setPort port $ setLogger aplogger defaultSettings
    runSettings settings app

app :: Application
app = serve (Proxy @Api) handler

-- ----------------------------------------------------------------------------
-- Web Service Handlers
-- ----------------------------------------------------------------------------

handler :: Server Api
handler =
  handlerFunctions
    :<|> ( \name ->
            handlerFunction name
              :<|> handlerParameters name
         )

handlerFunctions :: Handler [SimpleFunction]
handlerFunctions = do
  pure $ fmap (toSimpleFunction . snd) $ Map.elems functions
 where
  toSimpleFunction s =
    SimpleFunction
      { simpleName = name s
      , simpleDescription = description s
      }

handlerFunction :: String -> Attributes -> Handler SimpleResponse
handlerFunction name query = do
  case Map.lookup name functions of
    Nothing -> throwError err404
    Just (scenario, _) ->
      case runExcept $ fromParams query of
        Left err ->
          pure $ Insufficient err
        Right s -> do
          response <- timeoutAction $ runScenario s scenario
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

newtype Parameters = Parameters Properties
  deriving (Show, Read, Ord, Eq, Generic)

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

instance ToJSON Parameters where
  toJSON (Parameters props) =
    Aeson.object
      [ "type" .= Aeson.String "object"
      , "properties" .= props
      ]

newtype Properties = Properties
  { mkProperties :: Map.Map String Parameter
  }
  deriving (Show, Read, Ord, Eq, Generic)
  deriving newtype (ToJSON)

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

-- ----------------------------------------------------------------------------
-- Example Rules
-- ----------------------------------------------------------------------------

runScenario :: (MonadIO m) => MyState -> Expr Double -> m Double
runScenario s scenario = do
  (res, _, _, _) <- liftIO $ xplainF () s scenario
  pure res

fromParams :: Attributes -> Except String MyState
fromParams attrs = do
  let (valueMap, predMap) = Map.mapEither go (mkAttributes attrs)
  pure $
    emptyState
      { symtabF = HashMap.fromList $ Map.toList valueMap
      , symtabP = HashMap.fromList $ Map.toList predMap
      }
 where
  go (Number n) = Left $ Val Nothing n
  go (Boolean b) = Right $ PredVal Nothing b

functions :: Map.Map String (Expr Double, Function)
functions =
  Map.fromList
    [ ("compute_qualifies", (personQualifies, personQualifiesFunction))
    ]

personQualifies :: Expr Double
personQualifies =
  "qualifies"
    @|= MathPred
      ( (getvar "walks") |&& ((getvar "drinks") ||| (getvar "eats"))
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
    $ Properties
    $ Map.fromList
      [ ("walks", Parameter "string" ["true", "false", "unknown"] "Did the person walk?")
      , ("eats", Parameter "string" ["true", "false", "unknown"] "Did the person eat?")
      , ("drinks", Parameter "string" ["true", "false", "unknown"] "Did the person drink?")
      , ("beverage type", Parameter "string" ["alcoholic", "non-alcoholic", "unknown"] "Did the person drink an alcoholic beverage?")
      , ("in whole", Parameter "string" ["true", "false", "unknown"] "Did the person drink all of the beverage?")
      ]
