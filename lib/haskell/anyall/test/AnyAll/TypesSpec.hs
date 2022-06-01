{-# LANGUAGE OverloadedStrings #-}
module AnyAll.TypesSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import AnyAll.Types

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