{-# LANGUAGE OverloadedStrings #-}
module AnyAll.SVGLadderSpec (spec) where

import Test.Hspec
import AnyAll.SVGLadder
import Data.Text (Text)
import AnyAll.Types (Label (Pre, PrePost))

spec :: Spec
spec = do
  describe "topText" $ do
    it "extracts the only from Pre" $ do
      topText (Just $ Pre "a") `shouldBe` Just "a"
    it "extracts first from PrePost" $ do
      topText (Just $ PrePost "c" "b") `shouldBe` Just "c"
    it "does Nothing" $ do
      topText (Nothing:: Maybe (Label Text)) `shouldBe` Nothing

  describe "bottomText" $ do
    it "extracts second from PrePost" $ do
      bottomText (Just $ PrePost "c" "b") `shouldBe` Just "b"
    it "extracts Nothing from Pre" $ do
      bottomText (Just $ Pre "a") `shouldBe` Nothing
    it "does Nothing" $ do
      bottomText (Nothing:: Maybe (Label Text)) `shouldBe` Nothing