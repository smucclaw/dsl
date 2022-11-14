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

atomNode :: Text -> Tree (Formula (Maybe (Label Text)) Text)
atomNode t = Node (FAtom t) []

allDt ::  [Tree (Formula (Maybe a) Text)] -> Tree (Formula (Maybe a) Text)
allDt = Node (FAll Nothing)

anyDt ::  [Tree (Formula (Maybe a) Text)] -> Tree (Formula (Maybe a) Text)
anyDt = Node (FAny Nothing)

allPre :: Text -> [BoolStructDT (Maybe (Label Text)) a] -> BoolStructDT (Maybe (Label Text)) a
allPre label = Node (FAll (Just (Pre label)))

anyPre :: Text -> [BoolStructDT (Maybe (Label Text)) a] -> BoolStructDT (Maybe (Label Text)) a
anyPre label = Node (FAny (Just (Pre label)))
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

  describe "simplifyItemDT" $ do
    it "should leave Leaf " $ do
      simplifyItemDT (atomNode "foo") `shouldBe` (atomNode "foo")

  describe "siblingfyItemDT" $ do
    let
      x = 1
      foobar = [atomNode "foo", atomNode "bar"]
      fizbaz = [atomNode "fiz", atomNode "baz"]

    it "should leave Leaf " $ do
      siblingfyItemDT [atomNode "foo", atomNode "foo"] `shouldBe` [atomNode "foo", atomNode "foo"]

    it "should leave Not Leaf " $ do
      siblingfyItemDT [notDt $ atomNode "foo", notDt $ atomNode "foo"] `shouldBe` [notDt $ atomNode "foo", notDt $ atomNode "foo"]

    it "should merge alls Leaf" $ do
      siblingfyItemDT [allPre "a" foobar, allPre "a" fizbaz, atomNode "fig"] `shouldBe` [allPre "a" (foobar ++ fizbaz), atomNode "fig"]

    xit "should simplify singleton Leaf" $ do
      siblingfyItemDT [allPre "a" foobar, atomNode "fig", allPre "a" fizbaz] `shouldBe` [allPre "a" (foobar ++ fizbaz), atomNode "fig"]

    it "should merge alls Leaf" $ do
      siblingfyItemDT [anyPre "a" foobar, anyPre "a" fizbaz, atomNode "fig"] `shouldBe` [anyPre "a" (foobar ++ fizbaz), atomNode "fig"]

    xit "should simplify singleton Leaf" $ do
      siblingfyItemDT [anyPre "a" foobar, atomNode "fig", anyPre "a" fizbaz] `shouldBe` [anyPre "a" (foobar ++ fizbaz), atomNode "fig"]

    it "should merge alls Leaf" $ do
      siblingfyItemDT [anyPre "a" foobar, allPre "a" fizbaz, atomNode "fig"] `shouldBe` [anyPre "a" foobar, allPre "a" fizbaz, atomNode "fig"]