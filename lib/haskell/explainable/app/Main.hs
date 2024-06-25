{-# LANGUAGE DataKinds #-}

module Main (main) where

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

main :: IO ()
main = do
  Options{port = port} <- execParser opts
  withStdoutLogger $ \aplogger -> do
    let settings = setPort port $ setLogger aplogger defaultSettings
    runSettings settings app

type ApiWithSwagger =
  SwaggerSchemaUI "swagger-ui" "swagger.json"
    :<|> Api

appWithSwagger :: Servant.Server ApiWithSwagger
appWithSwagger =
  swaggerSchemaUIServer serverOpenApi
    :<|> handler

app :: Application
app = serve (Proxy @ApiWithSwagger) appWithSwagger
