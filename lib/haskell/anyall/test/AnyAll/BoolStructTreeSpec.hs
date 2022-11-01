{-# LANGUAGE OverloadedStrings #-}
module AnyAll.BoolStructTreeSpec where

import Data.Text
import Data.Tree
import AnyAll.BoolStructTree
import Test.Hspec

atomNode :: Text -> Tree (Maybe Text, Formula Text)
atomNode t = Node (Nothing :: Maybe Text, FAtom t) []

allDt ::  [Tree (Maybe Text, Formula Text)] -> Tree (Maybe Text, Formula Text)
allDt = Node (Nothing :: Maybe Text, FAll :: Formula Text)

anyDt ::  [Tree (Maybe Text, Formula Text)] -> Tree (Maybe Text, Formula Text)
anyDt = Node (Nothing :: Maybe Text, FAny :: Formula Text)

spec :: Spec
spec = do
  describe "nnfdt transformation" $ do
    let
        at = atomNode "a"
        bt = atomNode "b"

    it "nnf (not (not a)) == a" $ do
      nnfDT (notDt . notDt $ at) `shouldBe` at

    it "nnf (not (all [a, b])) == (any [not a, not b])" $ do
      nnfDT (notDt $ allDt [at, bt]) `shouldBe` anyDt [notDt at, notDt bt]

    it "nnf (not (any [a, b])) == (all [not a, not b])" $ do
      nnfDT (notDt $ anyDt [at, bt]) `shouldBe` allDt [notDt at, notDt bt]

    it "nnf (not (not all [not not a, not not b])) == all [a, b]" $ do
      nnfDT (notDt . notDt $ allDt [notDt . notDt $ at, notDt . notDt $ bt]) `shouldBe` allDt [at, bt]

    it "nnf (all [not not a, not not b]) == (all [a,b])" $ do
      nnfDT (allDt [notDt . notDt $ at, notDt . notDt $ bt]) `shouldBe` allDt [at, bt]

  describe "extractLeaves" $ do
    let
        a = atomNode "a"
        b = atomNode "b"

    it "extractLeaves a == a" $ do
      extractLeavesDT a `shouldBe` ["a"]

    it "extractLeaves not a == a" $ do
      extractLeavesDT (notDt a) `shouldBe` ["a"]

    it "extractLeaves (not (any [a, b])) == [a, b]" $ do
      extractLeavesDT (notDt $ anyDt [a, b]) `shouldBe` ["a", "b"]

    it "extractLeaves  (all [a, b]) == [a, b]" $ do
      extractLeavesDT (anyDt [a, b]) `shouldBe` ["a", "b"]