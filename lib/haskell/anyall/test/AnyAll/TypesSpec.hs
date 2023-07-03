{-# LANGUAGE OverloadedStrings #-}

module AnyAll.TypesSpec (spec) where

import Test.Hspec
import Data.Aeson (decode, encode)
import qualified Data.HashMap.Strict as Map
import AnyAll.Types
import Test.QuickCheck
import qualified Data.Text as T
import Test.QuickCheck.Instances.Text

type MarkingMap = Map.HashMap T.Text (Default Bool)

markingMap :: Either (Maybe Bool) (Maybe Bool) -> MarkingMap
  -- Map.Map T.Text (Default Bool)
markingMap payload = Map.singleton "key" (Default payload)

instance Arbitrary (ShouldView) where
  arbitrary = oneof [pure View, pure Hide, pure Ask]

instance (Arbitrary a) => Arbitrary (AndOr a) where
  arbitrary = oneof [pure And, pure Or, Simply <$> arbitrary, pure Neg]

instance (Arbitrary a) => Arbitrary (Label a) where
  arbitrary = oneof [Pre <$> arbitrary, PrePost <$> arbitrary <*> arbitrary]

instance (Arbitrary a) => Arbitrary (Default a) where
  arbitrary = Default <$> arbitrary

instance (Arbitrary a) => Arbitrary (Q a) where
  arbitrary = Q <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

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
      let q = encode (Marking {getMarking = Map.empty} :: Marking T.Text)
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

  describe "Q functions" $ do
    it "ask2hide" $ property $
      \q -> ask2hide q{ shouldView = Ask } `shouldBe` (q{ shouldView = Hide } :: (Q T.Text))
    it "ask2view" $ property $
      \q -> ask2view q{ shouldView = Ask } `shouldBe` (q{ shouldView = View } :: (Q T.Text))
