{-# LANGUAGE OverloadedStrings #-}
module AnyAll.SVGLadderSpec (spec) where

import Test.Hspec
import AnyAll.SVGLadder
import Data.Text (Text)
import AnyAll.Types (Label (Pre, PrePost))
import Graphics.Svg

spec :: Spec
spec = do
  let one = 1

  describe "with SVGLadder, drawing primitives" $ do
    let dc = defaultAAVConfig

    it "should be able to create a real basic SVG rectangle" $ do
      (show $ makeSvg' dc ( defaultBBox (cscale dc)
                          , rect_ [ X_ <<-* 0, Y_ <<-* 0
                                  , Width_ <<-* 60 , Height_ <<-* 30
                                  , Fill_ <<- "#bbbbbb", Stroke_ <<- "none" ]))
        `shouldBe`
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n    \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\"><svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" version=\"1.11.1\"><rect height=\"30\" fill=\"#bbbbbb\" y=\"0\" transform=\"translate(23.0000 23.0000)\" width=\"60\" stroke=\"none\" x=\"0\"/></svg>"

    it "should be able to test a BoxedSVG" $ do
      show (defaultBBox (cscale dc)
           , rect_ [ X_ <<-* 0, Y_ <<-* 0
                   , Width_ <<-* 60 , Height_ <<-* 30
                   , Fill_ <<- "#bbbbbb", Stroke_ <<- "none" ] :: Element)
        `shouldBe`
        show (defaultBBox (cscale dc)
             , rect_ [ X_ <<-* 0, Y_ <<-* 0
                     , Width_ <<-* 60 , Height_ <<-* 30
                     , Fill_ <<- "#bbbbbb", Stroke_ <<- "none" ] :: Element)
  
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
