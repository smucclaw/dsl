{-# LANGUAGE OverloadedStrings #-}
module LS.DemorganSpec (spec) where

import Test.Hspec
import LS.Types
import LS.Interpreter
import AnyAll

spec :: Spec
spec = do
  describe "deMorgan transformation" $ do
    let
        x1 = Leaf (RPMT ["sky", "is", "blue"])
        nnx1 = Not . Not $ x1

        all_x2 = All Nothing [Leaf (RPMT ["foo"]), Leaf (RPMT ["bar"])]
        n_all_x2 = Not all_x2
        any_n_x2 = Any Nothing [ Not (Leaf (RPMT ["foo"]))
                               , Not (Leaf (RPMT ["bar"]))]

        n_any_x3 = Not $ Any Nothing [Leaf (RPMT ["foo"]), Leaf (RPMT ["bar"])]
        all_n_x3 = All Nothing [ Not (Leaf (RPMT ["foo"]))
                               , Not (Leaf (RPMT ["bar"]))]
    it "not not x == x" $ do
      deMorgan nnx1 `shouldBe` x1

    xit "not (all x) == any (not x)" $ do
      deMorgan n_all_x2 `shouldBe` any_n_x2

    xit "not (any x) == all (not x)" $ do
      deMorgan n_any_x3 `shouldBe` all_n_x3

