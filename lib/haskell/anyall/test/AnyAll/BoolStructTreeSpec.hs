{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module AnyAll.BoolStructTreeSpec where

import Data.Text
import Data.Tree
import AnyAll.BoolStructTree
import Test.Hspec
import AnyAll.Types
import Test.Hspec.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Test.QuickCheck
import Data.List (sort)
import Control.Applicative

atomNode :: Text -> Tree (Formula (Maybe Text) Text)
atomNode t = Node (FAtom t) []

allDt ::  [Tree (Formula (Maybe Text) Text)] -> Tree (Formula (Maybe Text) Text)
allDt = Node (FAll Nothing)

anyDt ::  [Tree (Formula (Maybe Text) Text)] -> Tree (Formula (Maybe Text) Text)
anyDt = Node (FAny Nothing)


instance (Eq lbl, Eq a, Ord a, Ord lbl) => EqProp (BoolStructDT lbl a) where
    (Node (FAtom x)         _ ) =-= (Node (FAtom y)         _ ) = property (x == y)
    (Node FNot              xs) =-= (Node FNot              ys) = property (xs == ys)
    (Node (FAny x) xs) =-= (Node (FAny y) ys) = property ((x == y) && (sort xs == sort ys))
    (Node (FAll x) xs) =-= (Node (FAll y) ys) = property ((x == y) && (sort xs == sort ys))
    _ =-= _ = property False

instance (Arbitrary a, Arbitrary lbl) => Arbitrary (Formula lbl a) where
  arbitrary = oneof [FAll <$> arbitrary, FAny <$> arbitrary, pure FNot, FAtom <$> arbitrary]

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
      addJustDT (Node (FAny "") [a, b]) `shouldBe` (Node (FAny (Just "")) [aMaybe, bMaybe])

    it "addJust (any [a, b]) == [a, b]" $ do
      addJustDT (Node (FAll "") [a, b]) `shouldBe` (Node (FAll (Just "")) [aMaybe, bMaybe])

  describe "alwaysLabeled" $ do
    let
        a = Node (FAtom "a") [] :: BoolStructDT (Label Text) Text
        b = Node (FAtom "b") [] :: BoolStructDT (Label Text) Text
        aMaybe = Node (FAtom "a") [] :: BoolStructDT (Maybe (Label Text)) Text
        bMaybe = Node (FAtom "b") [] :: BoolStructDT (Maybe (Label Text)) Text
        preLabel = Pre "Label"
        prePostLabel = PrePost "Label" "Suffix"

    it "alwaysLabeled a == a" $ do
      alwaysLabeledDT aMaybe `shouldBe` a

    it "alwaysLabeled not a == a" $ do
      alwaysLabeledDT (notDt aMaybe) `shouldBe` (notDt a)

    it "alwaysLabeled (any Nothing [a, b]) == (any (pre 'any of:') [a, b])" $ do
      alwaysLabeledDT (Node (FAny Nothing) [aMaybe, bMaybe]) `shouldBe` Node (FAny (Pre "any of:")) [a, b]

    it "alwaysLabeled (all Nothing [a, b]) == (all (pre 'all of:') [a, b])" $ do
      alwaysLabeledDT (Node (FAll Nothing) [aMaybe, bMaybe]) `shouldBe` Node (FAll (Pre "all of:")) [a, b]

    it "alwaysLabeled (any (Just l) [a, b]) == (any l [a, b])" $ do
      alwaysLabeledDT (Node (FAny (Just preLabel)) [aMaybe, bMaybe]) `shouldBe` Node (FAny preLabel) [a, b]

    it "alwaysLabeled (all (Just l) [a, b]) == (all l [a, b])" $ do
       alwaysLabeledDT (Node (FAll (Just prePostLabel)) [aMaybe, bMaybe]) `shouldBe` Node (FAll prePostLabel) [a, b]

  describe "BoolStructDT Semigroup" $ do
    testBatch (semigroup (undefined ::BoolStructDT String String, 1::Int))