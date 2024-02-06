{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LS.RelationalPredicatesSpec (spec) where

import AnyAll (BoolStruct, mkAll, mkAny, mkLeaf)
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String.Interpolate (i)
import LS.RelationalPredicates (partitionExistentials)
import LS.Types
  ( BoolStructR,
    HornClause (HC, hBody, hHead),
    MTExpr (MTT),
    ParamType (TOne),
    RelationalPredicate (RPParamText),
    TypeSig (SimpleType),
    mkRpmt,
    mkRpmtLeaf,
  )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "partitionExistentials" do
    it "expand Leaf (not RPParamText)" do
      let l = mkRpmtLeaf ["sky", "is", "blue"]
          partitionedHc = mkAndPartitionHc l
      partitionedHc `shouldBe` (l, l)

    it "expand Leaf RPParamText" do
      let typeSig = Just (SimpleType TOne "Fruit")
          typedWords = "apple" :| ["orange", "banana"]
          rp = RPParamText $ (MTT <$> typedWords, typeSig) :| []
          l = mkLeaf rp
          partitionedHc = mkAndPartitionHc l
      partitionedHc `shouldBe` (l, l)

    testPartitionAnyAll "All" mkAll
    testPartitionAnyAll "Any" mkAny
  where
    mkAndPartitionHc l =
      partitionExistentials HC {hHead = mkRpmt [""], hBody = Just l} 

    testPartitionAnyAll (txt :: String) ctor = it [i|partition #{txt}|] $
      partitionedHc `shouldBe` (ctor Nothing [l1], ctor Nothing [l2])
      where
        fruitType = Just $ SimpleType TOne "Fruit"
        l1 = mkLeaf $ RPParamText $ (MTT <$> "apple" :| ["orange", "banana"], fruitType) :| []
        l2 = mkLeaf $ RPParamText $ (MTT <$> "red" :| ["orange", "yellow"], Nothing) :| []
        partitionedHc = mkAndPartitionHc $ ctor Nothing [l1, l2]