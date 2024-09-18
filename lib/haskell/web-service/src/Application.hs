{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Application (defaultMain) where

import Control.Concurrent.STM (newTVarIO)
import Control.Monad.Trans.Reader (ReaderT (..))
import Examples qualified
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Options
import Options.Applicative as Opts
import Schema
import Servant
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)
import Server

-- ----------------------------------------------------------------------------
-- Option Parser
-- ----------------------------------------------------------------------------

opts :: ParserInfo Options
opts =
  Opts.info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Serve a Web Service for interacting with a MathLang evaluator"
        <> header "explainable - A web server for MathLang"
    )

-- ----------------------------------------------------------------------------
-- Main Application and wiring
-- ----------------------------------------------------------------------------

defaultMain :: IO ()
defaultMain = do
  Options{port, serverName} <- execParser opts
  dbRef <- newTVarIO Examples.functionSpecs
  let
    initialState = DbState dbRef
  withStdoutLogger $ \aplogger -> do
    let
      settings = setPort port $ setLogger aplogger defaultSettings
    runSettings settings (app initialState serverName)

type ApiWithSwagger =
  SwaggerSchemaUI "swagger-ui" "swagger.json"
    :<|> Api

appWithSwagger :: DbState -> Maybe ServerName -> Servant.Server ApiWithSwagger
appWithSwagger initialDb mServerName =
  swaggerSchemaUIServer (serverOpenApi mServerName)
    :<|> hoistServer (Proxy @Api) (nt initialDb) handler
 where
  nt :: DbState -> AppM a -> Handler a
  nt s x = runReaderT x s

app :: DbState -> Maybe ServerName -> Application
app initialDb mServerName = do
  serve (Proxy @ApiWithSwagger) (appWithSwagger initialDb mServerName)
