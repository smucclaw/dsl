{-# LANGUAGE OverloadedStrings #-}
module LS.TypesSpec (spec) where

import Test.Hspec
import LS.Types
import Data.List.NonEmpty
import AnyAll

spec :: Spec
spec = do
  describe "RelationalPredicate" $ do
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
    it "PrependHead RPBoolStructR" $ do
      let
        rp = RPnary RPnot (RPMT ["sky", "is", "blue"])
      prependHead "head" rp `shouldBe` RPBoolStructR ["head", "sky"] RPis (Leaf $ RPMT ["sky", "is", "blue"])