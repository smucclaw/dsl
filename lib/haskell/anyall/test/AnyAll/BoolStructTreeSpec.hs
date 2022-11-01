{-# LANGUAGE OverloadedStrings #-}
module AnyAll.BoolStructTreeSpec where

import Data.Text
import Data.Tree
import AnyAll.BoolStructTree
import Test.Hspec

atomNode :: Text -> Tree (Formula (Maybe Text) Text)
atomNode t = Node (FAtom t) []

allDt ::  [Tree (Formula (Maybe Text) Text)] -> Tree (Formula (Maybe Text) Text)
allDt = Node (FAll Nothing)

anyDt ::  [Tree (Formula (Maybe Text) Text)] -> Tree (Formula (Maybe Text) Text)
anyDt = Node (FAny Nothing)

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

  describe "addJust" $ do
    let
        a = Node (FAtom "a") [] :: BoolStructDT Text Text
        b = Node (FAtom "b") [] :: BoolStructDT Text Text
        anyAB = Node (FAny "") [a, b]
        aMaybe = Node (FAtom "a") [] :: BoolStructDT (Maybe Text) Text
        bMaybe = Node (FAtom "b") [] :: BoolStructDT (Maybe Text) Text

    it "addJust a == a" $ do
      addJustDT a `shouldBe` aMaybe

    it "addJust not a == a" $ do
      addJustDT (notDt a) `shouldBe` notDt aMaybe

    it "addJust (any [a, b]) == [a, b]" $ do
      addJustDT (Node (FAny "") [a, b]) `shouldBe`  (Node (FAny (Just "")) [aMaybe, bMaybe])

    it "addJust (any [a, b]) == [a, b]" $ do
      addJustDT (Node (FAll "") [a, b]) `shouldBe` (Node (FAll (Just "")) [aMaybe, bMaybe])