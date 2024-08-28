{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Schema (
  serverOpenApi,
  ServerName,
) where

--

import Backend.Api
import Control.Lens hiding ((.=))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Map (Map)
import Data.Maybe qualified as Maybe
import Data.OpenApi
import Data.Proxy
import Data.Text qualified as Text
import GHC.TypeLits
import Servant
import Servant.OpenApi
import Server hiding (description, name)

type ServerName = Text.Text

serverOpenApi :: Maybe ServerName -> OpenApi
serverOpenApi serverName =
  toOpenApi (Proxy :: Proxy Api)
    & info . title .~ "MathLang Function API"
    & info . version .~ "1.0"
    & info . description ?~ "API for invoking MathLang functions"
    & servers .~ Maybe.maybeToList ((\sName -> Server sName mempty mempty) <$> serverName)

instance (KnownSymbol desc, HasOpenApi api) => HasOpenApi (OperationId desc :> api) where
  toOpenApi _ =
    toOpenApi (Proxy :: Proxy api)
      & allOperations . operationId %~ (Just (Text.pack (symbolVal (Proxy :: Proxy desc))) <>)

-- ----------------------------------------------------------------------------
-- Document and describe the Json schema using the OpenAPI standard
-- ----------------------------------------------------------------------------

instance ToSchema SimpleFunction where
  declareNamedSchema _ = do
    textRef <- declareSchemaRef (Proxy @Text.Text)
    pure $
      NamedSchema (Just "Function") $
        mempty
          & title ?~ "Function"
          & type_ ?~ OpenApiObject
          & properties
            .~ [ ("type", textRef)
               ,
                 ( "function"
                 , Inline $
                    mempty
                      & properties
                        .~ [ ("name", textRef)
                           , ("description", textRef)
                           ]
                 )
               ]

-- This is correct, since we don't overwrite the
-- 'ToJSON SimpleResponse' instance yet.
instance ToSchema SimpleResponse

-- This is correct, since we don't overwrite the
-- 'ToJSON SimpleResponse' instance yet.
instance ToSchema EvaluatorError

-- This is correct, since we don't overwrite the
-- 'ToJSON ResponseWithReason' instance yet.
instance ToSchema ResponseWithReason

-- This is correct, since we don't overwrite the
-- 'ToJSON Reasoning' instance yet.
instance ToSchema Reasoning

instance ToSchema ReasoningTree

-- where
--   declareNamedSchema p = do
--     defSchema <- genericDeclareNamedSchema defaultSchemaOptions p
--     pure defSchema

instance ToSchema ReasonNode

-- where
--   declareNamedSchema p = do
--     defSchema <- genericDeclareNamedSchema defaultSchemaOptions p
--     pure $
--       defSchema
--         & schema . required
--           .~ [ "reasoningNodeExampleCode"
--              , "reasoningNodeExplanation"
--              ]

instance ToSchema Function where
  declareNamedSchema _ = do
    textRef <- declareSchemaRef (Proxy @Text.Text)
    parametersRef <- declareSchemaRef (Proxy @Parameters)
    pure $
      NamedSchema (Just "Function") $
        mempty
          & title ?~ "Function"
          & type_ ?~ OpenApiObject
          & properties
            .~ [ ("type", textRef)
               ,
                 ( "function"
                 , Inline $
                    mempty
                      & properties
                        .~ [ ("name", textRef)
                           , ("description", textRef)
                           ,
                             ( "parameters"
                             , Inline $
                                mempty
                                  & properties
                                    .~ [ ("type", textRef)
                                       , ("properties", parametersRef)
                                       ]
                             )
                           ]
                 )
               ]

instance ToSchema Parameters where
  declareNamedSchema _ = do
    parameterSchema <- declareSchemaRef (Proxy @Parameter)
    mapSchema <- declareNamedSchema (Proxy @(Map String Parameter))
    pure $
      mapSchema
        & name ?~ "FunctionParameters"
        & schema . properties
          .~ [ ("prop", parameterSchema)
             ]

instance ToSchema Parameter where
  declareNamedSchema _ = do
    textSchema <- declareSchemaRef (Proxy @Text.Text)
    textListSchema <- declareSchemaRef (Proxy @[Text.Text])
    pure $
      NamedSchema (Just "FunctionParameter") $
        mempty
          & type_ ?~ OpenApiObject
          & title ?~ "Parameter"
          & properties
            .~ [ ("enum", textListSchema)
               , ("description", textSchema)
               , ("type", textSchema)
               ]
          & example
            ?~ Aeson.object
              [ "enum" .= (["true", "false", "unknown"] :: Aeson.Array)
              , "description" .= Aeson.String "Can a person walk?"
              , "type" .= Aeson.String "string"
              ]

instance ToParamSchema FnLiteral where
  toParamSchema _ =
    mempty
      & title ?~ "Function argument"
      & example ?~ Aeson.String "true"
      & description ?~ "A Function argument which can be either 'true' or 'false', or a floating point number. Additionally accepts 'yes' and 'no' as synonyms for 'true' and 'false' respectively."

instance ToSchema FnLiteral where
  declareNamedSchema p = do
    pure $ NamedSchema (Just "Literal") $ toParamSchema p

instance ToParamSchema EvalBackends where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & title ?~ "Backend to use for function evaluation"
      & example ?~ Aeson.String "simala"
      & default_ ?~ Aeson.String "simala"
      & enum_ ?~ [Aeson.String "simala", Aeson.String "gml"]
      & description ?~ "Backend for evaluation of a function. Backends can greatly affect how good the explanation for results. Additionally, backends may or may not support parts of natural4."
