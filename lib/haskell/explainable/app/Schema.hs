{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Schema (
  serverOpenApi,

  -- * Tests
  runJsonTests,
) where

--
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
import Test.QuickCheck.Gen qualified as Q
import Test.QuickCheck.Instances ()

serverOpenApi :: OpenApi
serverOpenApi =
  toOpenApi (Proxy :: Proxy Api)
    & info . title .~ "MathLang Function API"
    & info . version .~ "1.0"
    & info . description ?~ "API for invoking MathLang functions"

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
-- 'ToJSON ResponseWithReason' instance yet.
instance ToSchema ResponseWithReason

-- This is correct, since we don't overwrite the
-- 'ToJSON MathLangException' instance yet.
instance ToSchema MathLangException

-- This is correct, since we don't overwrite the
-- 'ToJSON Reasoning' instance yet.
instance ToSchema Reasoning

-- This is correct, since we don't overwrite the
-- 'ToJSON ReasoningTree' instance yet.
instance ToSchema ReasoningTree where
  declareNamedSchema p = do
    defSchema <- genericDeclareNamedSchema defaultSchemaOptions p
    pure $ defSchema
      & schema.required .~
        [ "reasoningNodeExampleCode"
        , "reasoningNodeExplanation"
        ]

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

-- | The code for this instance is taken from 'Arbitrary1 containers-Data.Tree.Tree'.
-- See https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/src/Test.QuickCheck.Arbitrary.html#line-901
instance Arbitrary ReasoningTree where
  arbitrary = Q.sized $ \n -> do
    k <- Q.chooseInt (0, n)
    go k
   where
    go n = do
      -- n is the size of the trees.
      reasoningTrace <- arbitrary
      reasoningExample <- arbitrary
      pars <- arbPartition (n - 1) -- can go negative!
      forest <- mapM go pars
      return $
        ReasoningTree
          { reasoningNodeExampleCode = reasoningExample
          , reasoningNodeExplanation = reasoningTrace
          , reasoningNodeChildren = forest
          }

    arbPartition :: Int -> Q.Gen [Int]
    arbPartition k = case compare k 1 of
      LT -> pure []
      EQ -> pure [1]
      GT -> do
        first <- Q.chooseInt (1, k)
        rest <- arbPartition $ k - first
        Q.shuffle (first : rest)

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

instance Arbitrary Parameters where
  arbitrary = Server.Parameters <$> arbitrary

instance Arbitrary Parameter where
  arbitrary = Server.Parameter <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Function where
  arbitrary = Server.Function <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SimpleFunction where
  arbitrary = Server.SimpleFunction <$> arbitrary <*> arbitrary

runJsonTests :: IO ()
runJsonTests = hspec (validateEveryToJSON $ Proxy @Api)
