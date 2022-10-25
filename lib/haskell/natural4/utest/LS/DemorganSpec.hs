{-# LANGUAGE OverloadedStrings #-}
module LS.DemorganSpec (spec) where

import Test.Hspec
import LS.Types
import LS.Interpreter
import AnyAll
import Prelude hiding (any, all)

all :: [BoolStructR] -> BoolStructR
all = All Nothing

any :: [BoolStructR] -> BoolStructR
any = Any Nothing

spec :: Spec
spec = do
  describe "nnf transformation" $ do
    let
        a = Leaf (RPMT ["foo"])
        b = Leaf (RPMT ["bar"])

    it "nnf (not (not a)) == a" $ do
      nnf (Not . Not $ a) `shouldBe` a

    it "nnf (not (not all [not not a, not not b])) == all [a, b]" $ do
      nnf (Not . Not $ all [Not . Not $ a, Not . Not $ b]) `shouldBe` all [a, b]

    it "nnf (all [not not a, not not b]) == (all [a,b])" $ do
      nnf (all [Not . Not $ a, Not . Not $ b]) `shouldBe` all [a, b]

    it "nnf (not (all a)) == (any (not b))" $ do
      nnf (Not $ all [a, b]) `shouldBe` any [Not a, Not b]

    it "nnf (not (any a)) == (all (not b))" $ do
      nnf (Not $ any [a, b]) `shouldBe` all [ Not a, Not b]

