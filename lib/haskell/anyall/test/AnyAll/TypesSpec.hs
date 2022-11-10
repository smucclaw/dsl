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
      let q = decode "{\"key\":{\"Left\":null}}"
          ma = markingMap $ Left Nothing
      q `shouldBe` Just (Marking {getMarking = ma})

    it "Left true" $ do
      let q = decode "{\"key\":{\"Left\":true}}"
          ma = markingMap $ Left $ Just True
      q `shouldBe` Just (Marking {getMarking = ma})

    it "Left false" $ do
      let q = decode "{\"key\":{\"Left\":false}}"
          ma = markingMap $ Left $ Just False
      q `shouldBe` Just (Marking {getMarking = ma})

    it "Right empty" $ do
      let q = decode "{\"key\":{\"Right\":null}}"
          ma = markingMap $ Right Nothing
      q `shouldBe` Just (Marking {getMarking = ma})

    it "Right true" $ do
      let q = decode "{\"key\":{\"Right\":true}}"
          ma = markingMap $ Right $ Just True
      q `shouldBe` Just (Marking {getMarking = ma})

    it "Right false" $ do
      let q = decode "{\"key\":{\"Right\":false}}"
          ma = markingMap $ Right $ Just False
      q `shouldBe` Just (Marking {getMarking = ma})

  describe "serialize Marking" $ do
    it "empty marking" $ do
      let q = encode (Marking {getMarking = Map.empty} :: Marking Text)
      q `shouldBe` "{\"getMarking\":{}}"

    it "marking Left empty" $ do
      let ma = markingMap $ Left Nothing
          q = encode Marking {getMarking = ma}
      q `shouldBe` "{\"getMarking\":{\"key\":{\"Left\":null}}}"

    it "marking Left true" $ do
      let ma = markingMap $ Left $ Just True
          q = encode Marking {getMarking = ma}
      q `shouldBe` "{\"getMarking\":{\"key\":{\"Left\":true}}}"

    it "marking Left false" $ do
      let ma = markingMap $ Left $ Just False
          q = encode Marking {getMarking = ma}
      q `shouldBe` "{\"getMarking\":{\"key\":{\"Left\":false}}}"

    it "marking Right empty" $ do
      let ma = markingMap $ Right Nothing
          q = encode Marking {getMarking = ma}
      q `shouldBe` "{\"getMarking\":{\"key\":{\"Right\":null}}}"

    it "marking Right true" $ do
      let ma = markingMap $ Right $ Just True
          q = encode Marking {getMarking = ma}
      q `shouldBe` "{\"getMarking\":{\"key\":{\"Right\":true}}}"

    it "marking Right false" $ do
      let ma = markingMap $ Right $ Just False
          q = encode Marking {getMarking = ma}
      q `shouldBe` "{\"getMarking\":{\"key\":{\"Right\":false}}}"
