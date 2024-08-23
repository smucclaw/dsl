{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LibSpec (spec) where

import Test.Hspec
import Test.QuickCheck (Arbitrary (..), oneof)
import Test.QuickCheck.Gen qualified as Q
import Test.QuickCheck.Instances ()
import Data.Proxy
import Servant.OpenApi

import Schema ()
import Server

spec :: Spec
spec = do
  describe "Schema" do
    validateEveryToJSON (Proxy @Api)

-- ----------------------------------------------------------------------------
-- Arbitrary instances that allow us to verify that the JSON
-- instances and OpenAPI documentation agree on the schema.
-- ----------------------------------------------------------------------------

instance Arbitrary Reasoning where
  arbitrary = Reasoning <$> arbitrary

{- | The code for this instance is taken from 'Arbitrary1 containers-Data.Tree.Tree'.
See https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/src/Test.QuickCheck.Arbitrary.html#line-901
-}
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

instance Arbitrary EvaluatorError where
  arbitrary = EvaluatorError <$> arbitrary

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

