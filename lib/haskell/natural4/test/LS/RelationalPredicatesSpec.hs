{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module LS.RelationalPredicatesSpec where

import AnyAll
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty
import LS.RelationalPredicates
import LS.Types
import Test.Hspec

spec :: Spec
spec = do
  describe "partitionExistentials" $ do
    it "expand Leaf (not RPParamText)" $ do
      let
        l = mkRpmtLeaf ["sky", "is", "blue"]
        hc = HC {hHead = mkRpmt [""], hBody = Just l}
      partitionExistentials hc `shouldBe` (l,l)

    it "expand Leaf RPParamText" $ do
      let
        typeSig = Just (SimpleType TOne "Fruit")
        typedWords = "apple" :| ["orange", "banana"]
        rp = RPParamText $ (MTT <$> typedWords, typeSig) :| []
        l = mkLeaf rp
        hc = HC {hHead = mkRpmt [""], hBody = Just l}
      partitionExistentials hc `shouldBe` (l,l)

    it "partition All" $ do
      let
        fruitType = Just (SimpleType TOne "Fruit")
        l1 = mkLeaf $ RPParamText $ (MTT <$> "apple" :| ["orange", "banana"], fruitType) :| []

        colorType = Just (SimpleType TOne "Color")
        l2 = mkLeaf $ RPParamText $ (MTT <$> "red" :| ["orange", "yellow"], colorType) :| []

        hc = HC {hHead = mkRpmt [""], hBody = Just (mkAll Nothing [l1, l2])}
      partitionExistentials hc `shouldBe` (mkAll Nothing [l1, l2], mkAll Nothing [])

    it "partition Any" $ do
      let
        fruitType = Just (SimpleType TOne "Fruit")
        l1 = mkLeaf $ RPParamText $ (MTT <$> "apple" :| ["orange", "banana"], fruitType) :| []

        l2 = mkLeaf $ RPParamText $ (MTT <$> "red" :| ["orange", "yellow"], Nothing) :| []

        hc = HC {hHead = mkRpmt [""], hBody = Just (mkAny Nothing [l1, l2])}
      partitionExistentials hc `shouldBe` (mkAny Nothing [l1], mkAny Nothing [l2])
