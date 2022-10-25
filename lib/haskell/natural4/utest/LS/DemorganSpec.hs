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

    it "nnf (all [not not x, not not y]) == (all [x,y])" $ do
      nnf (all [Not . Not $ a, Not . Not $ b]) `shouldBe` all [a, b]

    it "nnf (not (all x)) == (any (not x))" $ do
      nnf (Not $ all [a, b]) `shouldBe` any [Not a, Not b]

    it "nnf (not (any x)) == (all (not x))" $ do
      nnf (Not $ any [a, b]) `shouldBe` all [ Not a, Not b]

