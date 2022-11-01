{-# LANGUAGE OverloadedStrings #-}
module AnyAll.BoolStructSpec where

import Test.Hspec
import Data.Text (Text)
import AnyAll.BoolStruct
import Prelude hiding (any, all)
import AnyAll.Types

all :: [BoolStruct (Maybe l) a] -> BoolStruct (Maybe l) a
all = All Nothing

allPre :: Text -> [BoolStruct (Maybe (Label Text)) a] -> BoolStruct (Maybe (Label Text)) a
allPre label = All (Just (Pre label))

anyPre :: Text -> [BoolStruct (Maybe (Label Text)) a] -> BoolStruct (Maybe (Label Text)) a
anyPre label = All (Just (Pre label))

any :: [BoolStruct (Maybe l) a ] -> BoolStruct (Maybe l) a
any = Any Nothing

atom :: Text -> BoolStruct (Maybe Text) Text
atom = Leaf

type WireBoolStruct = BoolStruct (Maybe (Label Text)) Text

leaf :: Text -> WireBoolStruct
leaf = Leaf

spec :: Spec
spec = do
  describe "simplifyItem" $ do
    it "should leave Leaf " $ do
      simplifyItem (leaf "foo") `shouldBe` leaf "foo"

    it "should elevate nested All children" $ do
      simplifyItem ( all
                      [ all [leaf "foo1", leaf "foo2"],
                        all [leaf "bar", all [leaf "bat"]],
                        allPre "something"
                          [ leaf "baz",
                            allPre "something" [leaf "bbb"]
                          ],
                        anyPre "something" [leaf "qux"]
                      ] )
        `shouldBe` all
                    [ leaf "foo1",
                      leaf "foo2",
                      leaf "bar",
                      leaf "bat",
                      allPre "something" [leaf "baz", leaf "bbb"],
                      leaf "qux"
                    ]

    it "should simplify singleton Leaf" $ do
      simplifyItem ( all [leaf "foo"] )
        `shouldBe`   leaf "foo"
    it "should simplify not-nots" $ do
      simplifyItem ( Not $ Not $ leaf "not" )
        `shouldBe`   leaf "not"
    
  describe "nnf transformation" $ do
    let
        a = atom "a"
        b = atom "b"

    it "nnf (not (not a)) == a" $ do
      nnf (Not . Not $ a) `shouldBe` a

    it "nnf (not (not all [not not a, not not b])) == all [a, b]" $ do
      nnf (Not . Not $ all [Not . Not $ a, Not . Not $ b]) `shouldBe` all [a, b]

    it "nnf (all [not not a, not not b]) == (all [a,b])" $ do
      nnf (all [Not . Not $ a, Not . Not $ b]) `shouldBe` all [a, b]

    it "nnf (not (all [a, b])) == (any [not a, not b])" $ do
      nnf (Not $ all [a, b]) `shouldBe` any [Not a, Not b]

    it "nnf (not (any a)) == (all (not b))" $ do
      nnf (Not $ any [a, b]) `shouldBe` all [ Not a, Not b]

  describe "extractLeaves" $ do
    let
        a = atom "a"
        b = atom "b"

    it "extractLeaves a == a" $ do
      extractLeaves a `shouldBe` ["a"]

    it "extractLeaves not a == a" $ do
      extractLeaves (Not a) `shouldBe` ["a"]

    it "extractLeaves (not (any [a, b])) == [a, b]" $ do
      extractLeaves (Not $ any [a, b]) `shouldBe` ["a", "b"]

    it "extractLeaves  (all [a, b]) == [a, b]" $ do
      extractLeaves (all [a, b]) `shouldBe` ["a", "b"]