{-# LANGUAGE OverloadedStrings #-}

module LS.TypesSpec (spec) where

import AnyAll
import Data.List.NonEmpty
import LS.Types
import Test.Hspec

spec :: Spec
spec = do
  describe "PrependHead" $ do
    it "PrependHead RPMT" $ do
      let
        rp = mkRpmt ["sky", "is", "blue"]
      prependHead "head" rp `shouldBe` mkRpmt ["head", "sky", "is", "blue"]
    it "PrependHead RPParamText" $ do
      let
        typeSig = Just (SimpleType TOne "Fruit")
        typedWords = MTT <$> "apple" :| ["orange", "banana"]
        rp = RPParamText $ (typedWords, typeSig) :| []
      prependHead "head" rp `shouldBe` RPParamText ((MTT <$> "head" :| ["apple", "orange", "banana"], typeSig) :| [])
    it "PrependHead RPConstraint" $ do
      let
        rp = RPConstraint (MTT <$> ["sky"]) RPis (MTT <$> ["blue"])
      prependHead "head" rp `shouldBe` RPConstraint (MTT <$> ["head", "sky"]) RPis (MTT <$> ["blue"])
    it "PrependHead RPBoolStructR" $ do
      let
        rp = RPBoolStructR (MTT <$> ["sky"]) RPis (Leaf $ mkRpmt ["sky", "is", "blue"])
      prependHead "head" rp `shouldBe` RPBoolStructR (MTT <$> ["head", "sky"]) RPis (Leaf $ mkRpmt ["sky", "is", "blue"])
    xit "PrependHead RPBoolStructR" $ do
      let
        rp = RPnary RPnot [mkRpmt ["sky", "is", "blue"]]
      prependHead "head" rp `shouldBe` RPBoolStructR (MTT <$> ["head", "sky"]) RPis (Leaf $ mkRpmt ["sky", "is", "blue"])

  describe "rp2mt" $ do
    it "rp2mt RPMT" $ do
      let
        rp = mkRpmt ["sky", "is", "blue"]
      rp2mt rp `shouldBe` (MTT <$> ["sky", "is", "blue"])
    it "rp2mt RPParamText" $ do
      let
        typeSig = Just (SimpleType TOne "Fruit")
        typedWords = "apple" :| ["orange", "banana"]
        rp = RPParamText $ (MTT <$>typedWords, typeSig) :| []
      rp2mt rp `shouldBe`  (MTT <$> ["apple", "orange", "banana"])
    it "rp2mt RPConstraint" $ do
      let
        rp = RPConstraint (MTT <$> ["sky"]) RPis (MTT <$> ["blue"])
      rp2mt rp `shouldBe` (MTT <$> ["sky","IS","blue"])
    it "rp2mt RPBoolStructR" $ do
      let
        rp = RPBoolStructR (MTT <$> ["sky"]) RPis (Leaf $ mkRpmt ["sky", "is", "blue"])
      rp2mt rp `shouldBe` (MTT <$> ["sky","IS","sky is blue"])
    it "rp2mt RPBoolStructR" $ do
      let
        rp = RPnary RPnot [mkRpmt ["sky", "is", "blue"]]
      rp2mt rp `shouldBe` (MTT <$>["NOT", "sky", "is", "blue"])

  describe "rpHead" $ do
    it "rpHead RPMT" $ do
      let
        rp = mkRpmt ["sky", "is", "blue"]
      rpHead rp `shouldBe` (MTT <$> ["sky", "is", "blue"])
    it "rpHead RPParamText" $ do
      let
        typeSig = Just (SimpleType TOne "Fruit")
        typedWords = MTT "apple" :| [MTF 10, MTT "banana"]
        rp = RPParamText $ (typedWords, typeSig) :| []
      rpHead rp `shouldBe` [MTT "apple", MTF 10.0, MTT "banana"]
    it "rpHead RPConstraint" $ do
      let
        rp = RPConstraint (MTT <$> ["sky"]) RPis (MTT <$> ["blue"])
      rpHead rp `shouldBe` (MTT <$> ["sky"])
    it "rpHead RPBoolStructR" $ do
      let
        rp = RPBoolStructR (MTT <$> ["sky"]) RPis (Leaf $ mkRpmt ["sky", "is", "blue"])
      rpHead rp `shouldBe` (MTT <$> ["sky"])
    xit "rpHead RPBoolStructR" $ do
      let
        rp = RPnary RPnot [mkRpmt ["sky", "is", "blue"]]
      rpHead rp `shouldBe` (MTT <$> ["relNot", "sky", "is", "blue"])

  describe "pt2text" $ do
    it "pt2text" $ do
      let
        fruitType = Just (SimpleType TOne "Fruit")
        fruitWords = MTT <$> "apple" :| ["orange", "banana"]

        colorType = Just (SimpleType TOne "Color")
        l2 = (MTT <$> "red" :| ["orange", "yellow"], colorType) :| [(fruitWords, fruitType)]
      pt2text l2 `shouldBe` "red orange yellow apple orange banana"
