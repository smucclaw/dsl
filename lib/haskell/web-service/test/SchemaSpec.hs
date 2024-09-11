{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SchemaSpec (spec) where

import Data.Proxy
import Servant.OpenApi
import Test.Hspec
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck qualified as Q
import Test.QuickCheck.Instances ()

import Backend.Api
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
      [ SimpleResponse <$> arbitrary
      , SimpleError <$> arbitrary
      ]

instance Arbitrary Parameters where
  arbitrary = Parameters <$> arbitrary

instance Arbitrary Parameter where
  arbitrary = Parameter <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Function where
  arbitrary = Server.Function <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary EvalBackend where
  arbitrary = Q.chooseEnum (minBound, maxBound)

instance Arbitrary FunctionImplementation where
  arbitrary =
    Server.FunctionImplementation <$> arbitrary <*> arbitrary

instance Arbitrary FnArguments where
  arbitrary =
    Server.FnArguments <$> arbitrary <*> arbitrary

instance Arbitrary SimpleFunction where
  arbitrary = SimpleFunction <$> arbitrary <*> arbitrary

instance Arbitrary OutcomeStyle where
  arbitrary = Q.chooseEnum (ValueOnly, BaseAttributes)

instance Arbitrary OutcomeObject where
  arbitrary =
    OutcomeObject
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary BatchRequest where
  arbitrary =
    BatchRequest
      <$> arbitrary
      <*> arbitrary

instance Arbitrary BatchResponse where
  arbitrary =
    BatchResponse
      <$> arbitrary
      <*> arbitrary

instance Arbitrary Outcomes where
  arbitrary = Q.oneof [OutcomeAttribute <$> arbitrary, OutcomePropertyObject <$> arbitrary]

instance Arbitrary InputCase where
  arbitrary = InputCase <$> arbitrary <*> arbitrary

instance Arbitrary OutputCase where
  arbitrary =
    OutputCase
      <$> arbitrary
      <*> arbitrary

instance Arbitrary OutputSummary where
  arbitrary =
    OutputSummary
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
