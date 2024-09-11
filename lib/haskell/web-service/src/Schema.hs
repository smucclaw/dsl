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
import Data.OpenApi qualified as OA3
import Data.Proxy
import Data.Text (Text)
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

-- TODO: turn QueryString into a custom type that reflects accurately the parameters we expect
instance (HasOpenApi sub) => HasOpenApi (QueryString :> sub) where
  toOpenApi _ =
    toOpenApi (Proxy :: Proxy sub)
      & addParam backendParam
      & addParam param
      & addDefaultResponse400 tname
   where
    tname = "mycoolnewparams"
    backendParam =
      mempty
        & name .~ "backend"
        & in_ .~ ParamQuery
        & schema ?~ Inline (toParamSchema $ Proxy @EvalBackends)

    param =
      mempty
        & name .~ tname
        & in_ .~ ParamQuery
        & schema ?~ Inline pschema

    pschema =
      mempty
        & title ?~ "arguments"
        & type_ ?~ OpenApiObject
        & additionalProperties
          ?~ AdditionalPropertiesSchema (Inline $ toParamSchema $ Proxy @FnLiteral)
        & properties .~ [("argument1", (Inline $ toParamSchema $ Proxy @FnLiteral))]
        & example
          ?~ Aeson.Object
            [ "drinks" .= Aeson.String "yes"
            , "eats" .= Aeson.String "yes"
            , "walks" .= Aeson.String "no"
            ]

-- | Add parameter to every operation in the spec.
addParam :: OA3.Param -> OpenApi -> OpenApi
addParam param = allOperations . OA3.parameters %~ (Inline param :)

addDefaultResponse400 :: ParamName -> OpenApi -> OpenApi
addDefaultResponse400 pname = setResponseWith (\old _new -> alter400 old) 400 (return response400)
 where
  sname = markdownCode pname
  description400 = "Invalid " <> sname
  alter400 = description %~ (<> (" or " <> sname))
  response400 = mempty & description .~ description400

-- | Format given text as inline code in Markdown.
markdownCode :: Text -> Text
markdownCode s = "`" <> s <> "`"

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
-- 'ToJSON SimpleResponse' instance yet.
instance ToSchema ParameterMismatch

-- This is correct, since we don't overwrite the
-- 'ToJSON ResponseWithReason' instance yet.
instance ToSchema ResponseWithReason

-- This is correct, since we don't overwrite the
-- 'ToJSON Reasoning' instance yet.
instance ToSchema Reasoning

-- This is correct, since we don't overwrite the
-- 'ToJSON Reasoning' instance yet.
instance ToSchema ReasoningTree

-- This is correct, since we don't overwrite the
-- 'ToJSON Reasoning' instance yet.
instance ToSchema ReasonNode

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
      & title ?~ "Argument"
      -- Even though this is strictly speaking not *only* a string, custom GPT seem
      -- to need this, otherwise they will fail to send any requests to any endpoint with
      -- this query parameter.
      & type_ ?~ OpenApiString
      & example ?~ Aeson.String "true"
      & description ?~ "A Function argument which can be either 'true' or 'false', or a floating point number. Additionally accepts 'yes' and 'no' as synonyms for 'true' and 'false' respectively."

instance ToSchema FnLiteral where
  declareNamedSchema p = do
    pure $
      NamedSchema (Just "Literal") $
        toParamSchema p

instance ToSchema EvalBackends where
  declareNamedSchema p = do
    pure $
      NamedSchema (Just "EvalBackends") $
        toParamSchema p

instance ToParamSchema EvalBackends where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & title ?~ "Evaluation Backends"
      & example ?~ Aeson.String "simala"
      & default_ ?~ Aeson.String "simala"
      & enum_ ?~ [Aeson.String "simala", Aeson.String "gml"]
      & description ?~ "Backend for evaluation of a function. Backends can greatly affect the explanation quality. Additionally, backends may or may not support parts of natural4."

instance ToParamSchema (Map Text FnLiteral) where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiObject
      & title ?~ "Function Arguments"
      & example
        ?~ Aeson.Object
          [ "drinks" .= Aeson.String "true"
          , "eats" .= Aeson.String "true"
          , "walks" .= Aeson.String "false"
          , "amount" .= Aeson.Number 2.0
          ]
      & description ?~ "Provide arguments to the function to be invoked."