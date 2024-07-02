{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Schema (
  serverOpenApi,

  -- * Tests
) where

-- runJsonTests,

import Control.Lens hiding ((.=))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Map (Map)
import Data.OpenApi
import Data.Proxy
import Data.Text qualified as Text
import Servant.OpenApi
import Server hiding (description, name)
import Test.Hspec (hspec)
import Test.QuickCheck (Arbitrary (..), oneof)
import Test.QuickCheck.Instances ()

serverOpenApi :: OpenApi
serverOpenApi =
  toOpenApi (Proxy :: Proxy Api)
    & info . title .~ "MathLang Function API"
    & info . version .~ "1.0"
    & info . description ?~ "API for invoking MathLang functions"

instance ToSchema Test where
  declareNamedSchema _ = do
    textRef <- declareSchemaRef (Proxy @Text.Text)
    pure $
      NamedSchema (Just "Test") $
        mempty
          & title ?~ "Test"
          & type_ ?~ OpenApiObject
          & properties
            .~ [ ("hello", textRef)
               ]
          & example
            ?~ Aeson.object
              [ "hello" .= ("World" :: Text.Text)
              ]
          & required .~ ["hello"]

instance ToParamSchema Test where
  toParamSchema _ = do
    mempty
      & title ?~ "Test"
      & type_ ?~ OpenApiObject
      & properties
        .~ [ ("hello", Inline $ toSchema (Proxy @Text.Text))
           ]
      & example
        ?~ Aeson.object
          [ "hello" .= ("World" :: Text.Text)
          ]

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

-- & required .~ ["name", "description"]

-- This is correct, since we don't overwrite the
-- 'ToJSON SimpleResponse' instance yet.
instance ToSchema SimpleResponse

instance ToSchema ResponseWithReason

instance ToSchema MathLangException

instance ToSchema Reasoning

instance ToSchema ReasoningTree

-- instance ToParamSchema Arguments where
--   toParamSchema _ =
--     toSchema (Proxy @(Map Text.Text FlatValue))
--       & example
--         ?~ Aeson.object
--           [ "walks" .= False
--           , "eats" .= True
--           , "drinks" .= False
--           , "drinks_amount" .= Aeson.Number 5.0
--           ]

instance ToSchema Arguments where
  declareNamedSchema _ = do
    fvRef <- declareSchemaRef (Proxy @FlatValue)
    pure $
      NamedSchema (Just "Arguments") $
        toSchema (Proxy @(Map Text.Text FlatValue))
          & type_ ?~ OpenApiObject
          & additionalProperties ?~ AdditionalPropertiesSchema fvRef
          & properties
            .~ [ ("prop", fvRef)
               ]
          & example
            ?~ Aeson.object
              [ "walks" .= False
              , "eats" .= True
              , "drinks" .= False
              , "drinks_amount" .= Aeson.Number 5.0
              ]

instance ToParamSchema FlatValue where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & title ?~ "FlatValue"
      & oneOf
        ?~ [ Inline $ toSchema (Proxy @Bool)
           , Inline $ toSchema (Proxy @Double)
           ]

instance ToSchema FlatValue where
  declareNamedSchema fv = do
    pure $
      NamedSchema (Just "FlatValue") $
        paramSchemaToSchema fv

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

-- ----------------------------------------------------------------------------
-- Arbitrary instances that allow us to verify that the JSON
-- instances and OpenAPI documentation agree on the schema.
-- ----------------------------------------------------------------------------

instance Arbitrary Reasoning where
  arbitrary = Reasoning <$> arbitrary

instance Arbitrary ReasoningTree where
  arbitrary = ReasoningTree <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ResponseWithReason where
  arbitrary = ResponseWithReason <$> arbitrary <*> arbitrary

instance Arbitrary MathLangException where
  arbitrary = MathLangException <$> arbitrary

instance Arbitrary SimpleResponse where
  arbitrary =
    oneof
      [ Server.SimpleResponse <$> arbitrary
      , Server.SimpleError <$> arbitrary
      ]

instance Arbitrary Arguments where
  arbitrary = Server.Arguments <$> arbitrary

instance Arbitrary Parameters where
  arbitrary = Server.Parameters <$> arbitrary

instance Arbitrary Parameter where
  arbitrary = Server.Parameter <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary FlatValue where
  arbitrary =
    oneof
      [ Server.Number <$> arbitrary
      , Server.Boolean <$> arbitrary
      ]

instance Arbitrary Function where
  arbitrary = Server.Function <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SimpleFunction where
  arbitrary = Server.SimpleFunction <$> arbitrary <*> arbitrary

-- runJsonTests :: IO ()
-- runJsonTests = hspec (validateEveryToJSON $ Proxy @Api)
