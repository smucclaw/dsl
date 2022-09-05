{-# LANGUAGE OverloadedStrings #-}
module AnyAll.TypesSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import Data.Aeson (decode, encode)
import qualified Data.Map as Map
import AnyAll.Types

type MarkingMap = Map.Map Text (Default Bool)

markingMap :: Either (Maybe Bool) (Maybe Bool) -> Map.Map Text (Default Bool)
markingMap payload = Map.singleton "key" (Default payload)

spec :: Spec
spec = do
  describe "simplifyItem" $ do
    it "should leave Leaf " $ do
      simplifyItem (Leaf "foo")
        `shouldBe` (Leaf "foo")
    it "should elevate nested All children" $ do
      simplifyItem ( All Nothing [All Nothing [ Leaf "foo1"
                                              , Leaf "foo2"]
                                 ,All Nothing [ Leaf "bar"
                                              , All Nothing [Leaf "bat"] ]
                                 ,All (Just (Pre "something")) [Leaf "baz"
                                                               ,All (Just (Pre "something")) [Leaf "bbb"]]
                                 ,Any (Just (Pre "something")) [Leaf "qux"]
                                 ] )
        `shouldBe` All Nothing [Leaf "bat"
                               ,Leaf "bar"
                               ,Leaf "foo2"
                               ,Leaf "foo1"
                               ,All (Just (Pre "something")) [Leaf "bbb"
                                                             ,Leaf "baz"]
                               ,Leaf "qux"]
    it "should simplify singleton Leaf" $ do
      simplifyItem ( All Nothing [Leaf "foo"] )
        `shouldBe`   Leaf "foo"
    it "should simplify not-nots" $ do
      simplifyItem ( Not $ Not $ Leaf "not" )
        `shouldBe`   Leaf "not"

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
