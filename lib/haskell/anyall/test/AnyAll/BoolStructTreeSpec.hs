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
import Data.Aeson (encode, decode)

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
      nnfDT (mkNotDT . mkNotDT $ at) `shouldBe` at

    it "nnf (not (all [a, b])) == (any [not a, not b])" $ do
      nnfDT (mkNotDT $ allDt [at, bt]) `shouldBe` anyDt [mkNotDT at, mkNotDT bt]

    it "nnf (not (any [a, b])) == (all [not a, not b])" $ do
      nnfDT (mkNotDT $ anyDt [at, bt]) `shouldBe` allDt [mkNotDT at, mkNotDT bt]

    it "nnf (not (not all [not not a, not not b])) == all [a, b]" $ do
      nnfDT (mkNotDT . mkNotDT $ allDt [mkNotDT . mkNotDT $ at, mkNotDT . mkNotDT $ bt]) `shouldBe` allDt [at, bt]

    it "nnf (all [not not a, not not b]) == (all [a,b])" $ do
      nnfDT (allDt [mkNotDT . mkNotDT $ at, mkNotDT . mkNotDT $ bt]) `shouldBe` allDt [at, bt]

  describe "extractLeaves" $ do
    let
        a = atomNode "a"
        b = atomNode "b"

    it "extractLeaves a == a" $ do
      extractLeavesDT a `shouldBe` ["a"]

    it "extractLeaves not a == a" $ do
      extractLeavesDT (mkNotDT a) `shouldBe` ["a"]

    it "extractLeaves (not (any [a, b])) == [a, b]" $ do
      extractLeavesDT (mkNotDT $ anyDt [a, b]) `shouldBe` ["a", "b"]

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
      addJustDT (mkNotDT a) `shouldBe` mkNotDT aMaybe

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
      alwaysLabeledDT (mkNotDT aMaybe) `shouldBe` (mkNotDT a)

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

  describe "simplifyItem" $ do
    it "should leave atomNode " $ do
      simplifyBoolStructDT (atomNode "foo") `shouldBe` atomNode "foo"

    it "should elevate nested All children" $ do
      simplifyBoolStructDT ( allDt
                      [ allDt [atomNode "foo1", atomNode "foo2"],
                        allDt [atomNode "bar", allDt [atomNode "bat"]],
                        allPre "something"
                          [ atomNode "baz",
                            allPre "something" [atomNode "bbb"]
                          ],
                        anyPre "different something" [atomNode "qux"]
                      ] )
        `shouldBe` allDt
                    [ atomNode "foo1",
                      atomNode "foo2",
                      atomNode "bar",
                      atomNode "bat",
                      allPre "something" [atomNode "baz", atomNode "bbb"],
                      atomNode "qux"
                    ]

    it "should simplify singleton atomNode" $ do
      simplifyBoolStructDT ( allDt [atomNode "foo"] ) `shouldBe`  atomNode "foo"

    it "should simplify singleton atomNode" $ do
      simplifyBoolStructDT ( allDt [atomNode "foo"] ) `shouldBe`  atomNode "foo"

    it "should simplify not-nots" $ do
      simplifyBoolStructDT ( mkNotDT $ mkNotDT $ atomNode "not" ) `shouldBe`  atomNode "not"

    it "collapse all" $ do
      simplifyBoolStructDT ( allPre "a" [allPre "a" [atomNode "foo", atomNode "bar"], atomNode "baz"] ) `shouldBe`  allPre "a" [atomNode "foo", atomNode "bar", atomNode "baz"]

    it "does not collapse all" $ do
      simplifyBoolStructDT (allPre "b" [allPre "a" [atomNode "foo", atomNode "bar"], atomNode "baz"]) `shouldBe` allPre "b" [allPre "a" [atomNode "foo", atomNode "bar"], atomNode "baz"]

    it "collapse any" $ do
      simplifyBoolStructDT ( anyPre "a" [anyPre "a" [atomNode "foo", atomNode "bar"], atomNode "baz"] )
      `shouldBe`
                       anyPre "a" [atomNode "foo", atomNode "bar", atomNode "baz"]

    it "does not collapse any" $ do
      simplifyBoolStructDT (anyPre "b" [anyPre "a" [atomNode "foo", atomNode "bar"], atomNode "baz"]) `shouldBe` anyPre "b"  [anyPre "a" [atomNode "foo", atomNode "bar"], atomNode "baz"]

    it "does not collapse any all" $ do
      simplifyBoolStructDT (anyPre "b" [allPre "a" [atomNode "foo", atomNode "bar"], atomNode "baz"]) `shouldBe` anyPre "b"  [allPre "a" [atomNode "foo", atomNode "bar"], atomNode "baz"]

    it "does not collapse all any" $ do
      simplifyBoolStructDT (allPre "b" [anyPre "a" [atomNode "foo", atomNode "bar"], atomNode "baz"]) `shouldBe` allPre "b"  [anyPre "a" [atomNode "foo", atomNode "bar"], atomNode "baz"]

  describe "siblingfyBoolStructDT" $ do
    let
      x = 1
      foobar = [atomNode "foo", atomNode "bar"]
      fizbaz = [atomNode "fiz", atomNode "baz"]

    it "should leave atomNode " $ do
      siblingfyBoolStructDT [atomNode "foo", atomNode "foo"] `shouldBe` [atomNode "foo", atomNode "foo"]

    it "should leave Not atomNode " $ do
      siblingfyBoolStructDT [mkNotDT $ atomNode "foo", mkNotDT $ atomNode "foo"] `shouldBe` [mkNotDT $ atomNode "foo", mkNotDT $ atomNode "foo"]

    it "should merge alls atomNode" $ do
      siblingfyBoolStructDT [allPre "a" foobar, allPre "a" fizbaz, atomNode "fig"] `shouldBe` [allPre "a" (foobar ++ fizbaz), atomNode "fig"]

    xit "should simplify singleton atomNode" $ do
      siblingfyBoolStructDT [allPre "a" foobar, atomNode "fig", allPre "a" fizbaz] `shouldBe` [allPre "a" (foobar ++ fizbaz), atomNode "fig"]

    it "should merge alls atomNode" $ do
      siblingfyBoolStructDT [anyPre "a" foobar, anyPre "a" fizbaz, atomNode "fig"] `shouldBe` [anyPre "a" (foobar ++ fizbaz), atomNode "fig"]

    xit "should simplify singleton atomNode" $ do
      siblingfyBoolStructDT [anyPre "a" foobar, atomNode "fig", anyPre "a" fizbaz] `shouldBe` [anyPre "a" (foobar ++ fizbaz), atomNode "fig"]

    it "should merge alls atomNode" $ do
      siblingfyBoolStructDT [anyPre "a" foobar, allPre "a" fizbaz, atomNode "fig"] `shouldBe` [anyPre "a" foobar, allPre "a" fizbaz, atomNode "fig"]

  describe "JSON marshalling" $ do
    let
      foobar = [atomNode "foo", atomNode "bar"]
      fizbaz = [atomNode "fiz", atomNode "baz"]

    it "encode Leaf" $ do
      encode (atomNode "foo") `shouldBe` "{\"contents\":\"foo\",\"tag\":\"Leaf\"}"

    it "encode Not" $ do
      encode (mkNotDT (atomNode "foo")) `shouldBe` "{\"contents\":{\"contents\":\"foo\",\"tag\":\"Leaf\"},\"tag\":\"Not\"}"

    it "encode Any (Label Pre)" $ do
      encode (anyPre "any" foobar) `shouldBe` "{\"contents\":[{\"contents\":\"any\",\"tag\":\"Pre\"},[{\"contents\":\"foo\",\"tag\":\"Leaf\"},{\"contents\":\"bar\",\"tag\":\"Leaf\"}]],\"tag\":\"Any\"}"

    it "encode Any (Label PrePost)" $ do
      encode (Node (FAny (Just (PrePost "PreText" "PostText"))) foobar) `shouldBe` "{\"contents\":[{\"contents\":[\"PreText\",\"PostText\"],\"tag\":\"PrePost\"},[{\"contents\":\"foo\",\"tag\":\"Leaf\"},{\"contents\":\"bar\",\"tag\":\"Leaf\"}]],\"tag\":\"Any\"}"

    it "encode Any (Label Nothing)" $ do
      encode (Node (FAny Nothing) foobar) `shouldBe` "{\"contents\":[null,[{\"contents\":\"foo\",\"tag\":\"Leaf\"},{\"contents\":\"bar\",\"tag\":\"Leaf\"}]],\"tag\":\"Any\"}"

    it "encode All (Label Pre)" $ do
      encode (allPre "all" foobar) `shouldBe` "{\"contents\":[{\"contents\":\"all\",\"tag\":\"Pre\"},[{\"contents\":\"foo\",\"tag\":\"Leaf\"},{\"contents\":\"bar\",\"tag\":\"Leaf\"}]],\"tag\":\"All\"}"

    it "encode All (Label PrePost)" $ do
      encode (Node (FAll (Just (PrePost "PreText" "PostText"))) foobar) `shouldBe` "{\"contents\":[{\"contents\":[\"PreText\",\"PostText\"],\"tag\":\"PrePost\"},[{\"contents\":\"foo\",\"tag\":\"Leaf\"},{\"contents\":\"bar\",\"tag\":\"Leaf\"}]],\"tag\":\"All\"}"

    it "encode All (Label Nothing)" $ do
      encode (Node (FAll Nothing) foobar) `shouldBe` "{\"contents\":[null,[{\"contents\":\"foo\",\"tag\":\"Leaf\"},{\"contents\":\"bar\",\"tag\":\"Leaf\"}]],\"tag\":\"All\"}"

  describe "JSON unmarshalling" $ do
    let
      foobar = [atomNode "foo", atomNode "bar"]
      fizbaz = [atomNode "fiz", atomNode "baz"]

    it "decode Leaf" $ do
      decode "{\"contents\":\"foo\",\"tag\":\"Leaf\"}" `shouldBe` Just (atomNode "foo")

    it "decode Not" $ do
      decode "{\"contents\":{\"contents\":\"foo\",\"tag\":\"Leaf\"},\"tag\":\"Not\"}" `shouldBe` Just (mkNotDT (atomNode "foo"))

    it "decode Any (Label Pre)" $ do
      decode "{\"contents\":[{\"contents\":\"any\",\"tag\":\"Pre\"},[{\"contents\":\"foo\",\"tag\":\"Leaf\"},{\"contents\":\"bar\",\"tag\":\"Leaf\"}]],\"tag\":\"Any\"}"
      `shouldBe`
      Just (anyPre "any" foobar)

    it "decode Any (Label PrePost)" $ do
      decode "{\"contents\":[{\"contents\":[\"PreText\",\"PostText\"],\"tag\":\"PrePost\"},[{\"contents\":\"foo\",\"tag\":\"Leaf\"},{\"contents\":\"bar\",\"tag\":\"Leaf\"}]],\"tag\":\"Any\"}"
      `shouldBe`
      Just (Node (FAny (Just (PrePost "PreText" "PostText"))) foobar)

    it "decode Any (Label Nothing)" $ do
      decode "{\"contents\":[null,[{\"contents\":\"foo\",\"tag\":\"Leaf\"},{\"contents\":\"bar\",\"tag\":\"Leaf\"}]],\"tag\":\"Any\"}"
      `shouldBe`
      Just (Node (FAny Nothing) foobar)

    it "decode Any Nothing - Missing" $ do
      decode "{\"contents\":[[{\"contents\":\"foo\",\"tag\":\"Leaf\"},{\"contents\":\"bar\",\"tag\":\"Leaf\"}]],\"tag\":\"Any\"}"
      `shouldBe`
      (Nothing::Maybe (BoolStructDT (Maybe (Label Text)) Text))

    it "decode All (Label Pre)" $ do
      decode "{\"contents\":[{\"contents\":\"all\",\"tag\":\"Pre\"},[{\"contents\":\"foo\",\"tag\":\"Leaf\"},{\"contents\":\"bar\",\"tag\":\"Leaf\"}]],\"tag\":\"All\"}"
      `shouldBe`
      Just (allPre "all" foobar)

    it "decode All (Label PrePost)" $ do
      decode "{\"contents\":[{\"contents\":[\"PreText\",\"PostText\"],\"tag\":\"PrePost\"},[{\"contents\":\"foo\",\"tag\":\"Leaf\"},{\"contents\":\"bar\",\"tag\":\"Leaf\"}]],\"tag\":\"All\"}"
      `shouldBe`
      Just (Node (FAll (Just (PrePost "PreText" "PostText"))) foobar)

    it "decode All (Label Nothing)" $ do
      decode "{\"contents\":[null,[{\"contents\":\"foo\",\"tag\":\"Leaf\"},{\"contents\":\"bar\",\"tag\":\"Leaf\"}]],\"tag\":\"All\"}"
      `shouldBe`
      Just (Node (FAll Nothing) foobar)
  
    it "decode All Nothing - Missing" $ do
      decode "{\"contents\":[[{\"contents\":\"foo\",\"tag\":\"Leaf\"},{\"contents\":\"bar\",\"tag\":\"Leaf\"}]],\"tag\":\"All\"}"
      `shouldBe`
      (Nothing::Maybe (BoolStructDT (Maybe (Label Text)) Text))

