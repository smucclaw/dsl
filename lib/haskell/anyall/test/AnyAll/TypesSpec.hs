{-# LANGUAGE OverloadedStrings #-}
module AnyAll.TypesSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import Data.Aeson (decode, encode)
import qualified Data.Map as Map
import AnyAll.Types
import Prelude hiding (any, all)

type MarkingMap = Map.Map Text (Default Bool)

markingMap :: Either (Maybe Bool) (Maybe Bool) -> Map.Map Text (Default Bool)
markingMap payload = Map.singleton "key" (Default payload)

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

  describe "labelFirst" $ do
    it "extracts the only from Pre" $ do
      labelFirst (Pre "a") `shouldBe` "a"
    it "extracts first from PrePost" $ do
      labelFirst (PrePost "c" "b") `shouldBe` "c"

  describe "labelSecond" $ do
    it "extracts the only from Pre" $ do
      maybeSecond (Pre "a") `shouldBe` Nothing
    it "extracts first from PrePost" $ do
      maybeSecond (PrePost "c" "b") `shouldBe` Just "b"

  describe "deserialize Marking" $ do
    it "empty marking" $ do
      let q = decode "{\"getMarking\":{}}" :: Maybe TextMarking
      q `shouldBe` Just (Marking {getMarking = Map.empty})

    it "Left empty" $ do
      let q = decode "{\"key\":{\"getDefault\":{\"Left\":null}}}"
          ma = markingMap $ Left Nothing
      q `shouldBe` Just (Marking {getMarking = ma})

    it "Left true" $ do
      let q = decode "{\"key\":{\"getDefault\":{\"Left\":true}}}"
          ma = markingMap $ Left $ Just True
      q `shouldBe` Just (Marking {getMarking = ma})

    it "Left false" $ do
      let q = decode "{\"key\":{\"getDefault\":{\"Left\":false}}}"
          ma = markingMap $ Left $ Just False
      q `shouldBe` Just (Marking {getMarking = ma})

    it "Right empty" $ do
      let q = decode "{\"key\":{\"getDefault\":{\"Right\":null}}}"
          ma = markingMap $ Right Nothing
      q `shouldBe` Just (Marking {getMarking = ma})

    it "Right true" $ do
      let q = decode "{\"key\":{\"getDefault\":{\"Right\":true}}}"
          ma = markingMap $ Right $ Just True
      q `shouldBe` Just (Marking {getMarking = ma})

    it "Right false" $ do
      let q = decode "{\"key\":{\"getDefault\":{\"Right\":false}}}"
          ma = markingMap $ Right $ Just False
      q `shouldBe` Just (Marking {getMarking = ma})

  describe "serialize Marking" $ do
    it "empty marking" $ do
      let q = encode (Marking {getMarking = Map.empty} :: Marking Text)
      q `shouldBe` "{\"getMarking\":{}}"

    it "marking Left empty" $ do
      let ma = markingMap $ Left Nothing
          q = encode Marking {getMarking = ma}
      q `shouldBe` "{\"getMarking\":{\"key\":{\"getDefault\":{\"Left\":null}}}}"

    it "marking Left true" $ do
      let ma = markingMap $ Left $ Just True
          q = encode Marking {getMarking = ma}
      q `shouldBe` "{\"getMarking\":{\"key\":{\"getDefault\":{\"Left\":true}}}}"

    it "marking Left false" $ do
      let ma = markingMap $ Left $ Just False
          q = encode Marking {getMarking = ma}
      q `shouldBe` "{\"getMarking\":{\"key\":{\"getDefault\":{\"Left\":false}}}}"

    it "marking Right empty" $ do
      let ma = markingMap $ Right Nothing
          q = encode Marking {getMarking = ma}
      q `shouldBe` "{\"getMarking\":{\"key\":{\"getDefault\":{\"Right\":null}}}}"

    it "marking Right true" $ do
      let ma = markingMap $ Right $ Just True
          q = encode Marking {getMarking = ma}
      q `shouldBe` "{\"getMarking\":{\"key\":{\"getDefault\":{\"Right\":true}}}}"

    it "marking Right false" $ do
      let ma = markingMap $ Right $ Just False
          q = encode Marking {getMarking = ma}
      q `shouldBe` "{\"getMarking\":{\"key\":{\"getDefault\":{\"Right\":false}}}}"

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

    it "nnf (not (all a)) == (any (not b))" $ do
      nnf (Not $ all [a, b]) `shouldBe` any [Not a, Not b]

    it "nnf (not (any a)) == (all (not b))" $ do
      nnf (Not $ any [a, b]) `shouldBe` all [ Not a, Not b]
