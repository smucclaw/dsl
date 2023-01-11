{-# LANGUAGE OverloadedStrings #-}
module LS.RelationalPredicatesSpec where

import Test.Hspec
import LS.Types
import AnyAll
import Data.List.NonEmpty
import qualified Data.Map as Map
import AnyAll.BoolStructTree
import LS.RelationalPredicates

spec :: Spec
spec = do
  describe "partitionExistentials" $ do
    it "expand Leaf (not RPParamText)" $ do
      let
        l = mkLeaf $ RPMT ["sky", "is", "blue"]
        hc = HC {hHead = RPMT [""], hBody = Just l}
      partitionExistentials hc `shouldBe` (l,l)

    it "expand Leaf RPParamText" $ do
      let
        typeSig = Just (SimpleType TOne "Fruit")
        typedWords = "apple" :| ["orange", "banana"]
        rp = RPParamText $ (typedWords, typeSig) :| []
        l = mkLeaf rp
        hc = HC {hHead = RPMT [""], hBody = Just l}
      partitionExistentials hc `shouldBe` (l,l)

    it "partition All" $ do
      let
        fruitType = Just (SimpleType TOne "Fruit")
        l1 = mkLeaf $ RPParamText $ ("apple" :| ["orange", "banana"], fruitType) :| []

        colorType = Just (SimpleType TOne "Color")
        l2 = mkLeaf $ RPParamText $ ("red" :| ["orange", "yellow"], colorType) :| []

        hc = HC {hHead = RPMT [""], hBody = Just (mkAll Nothing [l1, l2])}
      partitionExistentials hc `shouldBe` (mkAll Nothing [l1, l2], mkAll Nothing [])

    it "partition Any" $ do
      let
        fruitType = Just (SimpleType TOne "Fruit")
        l1 = mkLeaf $ RPParamText $ ("apple" :| ["orange", "banana"], fruitType) :| []

        l2 = mkLeaf $ RPParamText $ ("red" :| ["orange", "yellow"], Nothing) :| []

        hc = HC {hHead = RPMT [""], hBody = Just (mkAny Nothing [l1, l2])}
      partitionExistentials hc `shouldBe` (mkAny Nothing [l1], mkAny Nothing [l2])

  describe "partitionExistentialsDT" $ do
    it "expand Leaf (not RPParamText)" $ do
      let
        l = mkLeafDT $ RPMT ["sky", "is", "blue"]
        hc = HC {hHead = RPMT [""], hBody = Just l}
      partitionExistentialsDT hc `shouldBe` (l,l)

    it "expand Leaf RPParamText" $ do
      let
        typeSig = Just (SimpleType TOne "Fruit")
        typedWords = "apple" :| ["orange", "banana"]
        rp = RPParamText $ (typedWords, typeSig) :| []
        l = mkLeafDT rp
        hc = HC {hHead = RPMT [""], hBody = Just l}
      partitionExistentialsDT hc `shouldBe` (l,l)

    it "partition All" $ do
      let
        fruitType = Just (SimpleType TOne "Fruit")
        l1 = mkLeafDT $ RPParamText $ ("apple" :| ["orange", "banana"], fruitType) :| []

        colorType = Just (SimpleType TOne "Color")
        l2 = mkLeafDT $ RPParamText $ ("red" :| ["orange", "yellow"], colorType) :| []

        hc = HC {hHead = RPMT [""], hBody = Just (mkAllDT Nothing [l1, l2])}
      partitionExistentialsDT hc `shouldBe` (mkAllDT Nothing [l1, l2], mkAllDT Nothing [])

    it "partition Any" $ do
      let
        fruitType = Just (SimpleType TOne "Fruit")
        l1 = mkLeafDT $ RPParamText $ ("apple" :| ["orange", "banana"], fruitType) :| []

        l2 = mkLeafDT $ RPParamText $ ("red" :| ["orange", "yellow"], Nothing) :| []

        hc = HC {hHead = RPMT [""], hBody = Just (mkAnyDT Nothing [l1, l2])}
      partitionExistentialsDT hc `shouldBe` (mkAnyDT Nothing [l1], mkAnyDT Nothing [l2])