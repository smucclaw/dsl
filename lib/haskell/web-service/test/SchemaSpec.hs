{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SchemaSpec (spec) where

import Data.Proxy
import Servant.OpenApi
import Test.Hspec
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as Q
import Test.QuickCheck.Instances ()

import Backend.Api
import Data.Text
import Data.Text qualified as Text
import Schema ()
import Servant.API (FromHttpApiData (..))
import Server
import Test.Hspec.QuickCheck qualified as Hspec
import Test.QuickCheck.Property

spec :: Spec
spec = do
  describe "Schema" do
    describe "json" do
      validateEveryToJSON (Proxy @Api)
    describe "Param Schema" do
      describe "FnLiteral" do
        Hspec.prop "Int" $ \(n :: Integer) ->
          parseQueryParam (Text.pack $ show n) === Right (FnLitInt n)
        Hspec.prop "Bool" $ \(n :: Bool) ->
          parseQueryParam (Text.pack $ show n) === Right (FnLitBool n)

-- ----------------------------------------------------------------------------
-- Arbitrary instances that allow us to verify that the JSON
-- instances and OpenAPI documentation agree on the schema.
-- ----------------------------------------------------------------------------

instance Arbitrary FnLiteral where
  arbitrary =
    Q.frequency
      [ (1, FnLitBool <$> arbitrary)
      , (3, FnLitDouble <$> arbitrary)
      , (3, FnLitString <$> arbitrary)
      , (3, FnLitInt <$> arbitrary)
      ]

genFnLiteralTexts :: Gen Text
genFnLiteralTexts =
  Q.frequency
    [ (1, fmap (Text.pack . show) (arbitrary :: Gen Bool))
    , (2, fmap (Text.pack . show) (arbitrary :: Gen Double))
    , (2, fmap (Text.pack . show) (arbitrary :: Gen Int))
    , (2, arbitrary :: Gen Text)
    , (1, fmap (surroundWith "\"" . Text.concatMap escapeChar) (arbitrary :: Gen Text))
    ]
 where
  surroundWith :: Text -> Text -> Text
  surroundWith t = (t <>) . (<> t)

  escapeChar '\"' = Text.pack "\\\""
  escapeChar n = Text.singleton n

instance Arbitrary Reasoning where
  arbitrary = Reasoning <$> arbitrary

instance Arbitrary ReasonNode where
  arbitrary = ReasonNode <$> arbitrary <*> arbitrary

-- | The code for this instance is taken from 'Arbitrary1 containers-Data.Tree.Tree'.
-- See https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/src/Test.QuickCheck.Arbitrary.html#line-901
instance Arbitrary ReasoningTree where
  arbitrary = Q.sized $ \n -> do
    k <- Q.chooseInt (0, n)
    go k
   where
    go n = do
      -- n is the size of the trees.
      node <- arbitrary
      pars <- arbPartition (n - 1) -- can go negative!
      forest <- mapM go pars
      return $
        ReasoningTree
          { treeNode = node
          , treeChildren = forest
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

instance Arbitrary EvaluatorError where
  arbitrary = Q.oneof [InterpreterError <$> arbitrary]

instance Arbitrary SimpleResponse where
  arbitrary =
    Q.oneof
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
