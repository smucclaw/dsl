{-# LANGUAGE OverloadedStrings #-}
module LS.RelationalPredicatesSpec where

import Test.Hspec
import LS.Types
import AnyAll
import Data.List.NonEmpty
import qualified Data.Map as Map
import LS.RelationalPredicates

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
