{-# LANGUAGE OverloadedStrings #-}

module LS.XPile.PrologSpec where

import Test.Hspec
import LS.Types
import AnyAll
import LS.XPile.Prolog
import Data.List.NonEmpty

spec :: Spec
spec = do
  describe "bsp2struct" $ do
    it "Leaf" $ do
      let
        typeSig = Just (SimpleType TOne "Fruit")
        typedWords = "apple" :| ["orange", "banana"]
        rp = (MTT <$> typedWords, typeSig) :| []
        l = mkLeaf rp
      bsp2struct l `shouldBe` [vart "apple orange banana"]

    it "Not" $ do
      let
        typeSig = Just (SimpleType TOne "Fruit")
        typedWords = MTT <$> "apple" :| ["orange", "banana"]
        rp = (typedWords, typeSig) :| []
        l = mkNot $ mkLeaf rp
      bsp2struct l `shouldBe` vart "neg" : [vart "apple orange banana"]

    it "Any" $ do
      let
        fruitType = Just (SimpleType TOne "Fruit")
        fruitWords = MTT <$> "apple" :| ["orange", "banana"]
        l1 = (fruitWords, fruitType) :| []

        colorType = Just (SimpleType TOne "Color")
        l2 = (MTT <$> "red" :| ["orange", "yellow"], colorType) :| []
    
        l = mkAny Nothing [mkLeaf l1, mkLeaf l2]
      bsp2struct l `shouldBe` vart "or" : [vart "apple orange banana", vart "red orange yellow"]

    it "All" $ do
      let
        fruitType = Just (SimpleType TOne "Fruit")
        fruitWords = MTT <$> "apple" :| ["orange", "banana"]
        l1 = (fruitWords, fruitType) :| []

        colorType = Just (SimpleType TOne "Color")
        l2 = (MTT <$> "red" :| ["orange", "yellow"], colorType) :| []
    
        l = mkAll Nothing [mkLeaf l1, mkLeaf l2]
      bsp2struct l `shouldBe` [vart "apple orange banana", vart "red orange yellow"]
