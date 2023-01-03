{-# LANGUAGE OverloadedStrings #-}
module LS.TypesSpec (spec) where

import Test.Hspec
import LS.Types
import Data.List.NonEmpty
import AnyAll

spec :: Spec
spec = do
  describe "PrependHead" $ do
    it "PrependHead RPMT" $ do
      let
        rp = RPMT ["sky", "is", "blue"]
      prependHead "head" rp `shouldBe` RPMT ["head", "sky", "is", "blue"]
    it "PrependHead RPParamText" $ do
      let
        typeSig = Just (SimpleType TOne "Fruit")
        typedWords = "apple" :| ["orange", "banana"]
        rp = RPParamText $ (typedWords, typeSig) :| []
      prependHead "head" rp `shouldBe` RPParamText (( "head" :| ["apple", "orange", "banana"], typeSig) :| [])
    it "PrependHead RPConstraint" $ do
      let
        rp = RPConstraint ["sky"] RPis ["blue"]
      prependHead "head" rp `shouldBe` RPConstraint ["head", "sky"] RPis ["blue"]
    it "PrependHead RPBoolStructR" $ do
      let
        rp = RPBoolStructR ["sky"] RPis (Leaf $ RPMT ["sky", "is", "blue"])
      prependHead "head" rp `shouldBe` RPBoolStructR ["head", "sky"] RPis (Leaf $ RPMT ["sky", "is", "blue"])
    xit "PrependHead RPBoolStructR" $ do
      let
        rp = RPnary RPnot (RPMT ["sky", "is", "blue"])
      prependHead "head" rp `shouldBe` RPBoolStructR ["head", "sky"] RPis (Leaf $ RPMT ["sky", "is", "blue"])

  describe "rp2texts" $ do
    it "rp2texts RPMT" $ do
      let
        rp = RPMT ["sky", "is", "blue"]
      rp2texts rp `shouldBe` ["sky", "is", "blue"]
    it "rp2texts RPParamText" $ do
      let
        typeSig = Just (SimpleType TOne "Fruit")
        typedWords = "apple" :| ["orange", "banana"]
        rp = RPParamText $ (typedWords, typeSig) :| []
      rp2texts rp `shouldBe`  [ "apple orange banana"]
    it "rp2texts RPConstraint" $ do
      let
        rp = RPConstraint ["sky"] RPis ["blue"]
      rp2texts rp `shouldBe` ["sky","IS","blue"]
    it "rp2texts RPBoolStructR" $ do
      let
        rp = RPBoolStructR ["sky"] RPis (Leaf $ RPMT ["sky", "is", "blue"])
      rp2texts rp `shouldBe` ["sky","IS","sky is blue"]
    it "rp2texts RPBoolStructR" $ do
      let
        rp = RPnary RPnot (RPMT ["sky", "is", "blue"])
      rp2texts rp `shouldBe` ["NOT", "sky", "is", "blue"]

  describe "rpHead" $ do
    it "rpHead RPMT" $ do
      let
        rp = RPMT ["sky", "is", "blue"]
      rpHead rp `shouldBe` ["sky", "is", "blue"]
    it "rpHead RPParamText" $ do
      let
        typeSig = Just (SimpleType TOne "Fruit")
        typedWords = "apple" :| ["orange", "banana"]
        rp = RPParamText $ (typedWords, typeSig) :| []
      rpHead rp `shouldBe`  [ "apple orange banana"]
    it "rpHead RPConstraint" $ do
      let
        rp = RPConstraint ["sky"] RPis ["blue"]
      rpHead rp `shouldBe` ["sky"]
    it "rpHead RPBoolStructR" $ do
      let
        rp = RPBoolStructR ["sky"] RPis (Leaf $ RPMT ["sky", "is", "blue"])
      rpHead rp `shouldBe` ["sky"]
    xit "rpHead RPBoolStructR" $ do
      let
        rp = RPnary RPnot (RPMT ["sky", "is", "blue"])
      rpHead rp `shouldBe` ["relNot", "sky", "is", "blue"]

  describe "pt2text" $ do
    it "pt2text" $ do
      let
        fruitType = Just (SimpleType TOne "Fruit")
        fruitWords = "apple" :| ["orange", "banana"]

        colorType = Just (SimpleType TOne "Color")
        l2 = ("red" :| ["orange", "yellow"], colorType) :| [(fruitWords, fruitType)]
      pt2text l2 `shouldBe` "red orange yellow apple orange banana"
