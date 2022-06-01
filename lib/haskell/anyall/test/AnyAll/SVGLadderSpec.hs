{-# LANGUAGE OverloadedStrings #-}

module AnyAll.SVGLadderSpec (spec) where

import AnyAll.SVGLadder hiding (tl)
import AnyAll.Types (Label (Pre, PrePost))
import Data.Text (Text)
import qualified Data.Text.Lazy.IO as TIO
import Graphics.Svg
import Test.Hspec

data SVGRect = Rect { tl :: (Integer, Integer), br :: (Integer, Integer), fill :: Text, stroke :: Text}

svgRect :: SVGRect -> Element
svgRect Rect { tl = (x, y), br = (w, h), fill = f, stroke = s} =
  rect_
    [ X_ <<-* x,
      Y_ <<-* y,
      Width_ <<-* w,
      Height_ <<-* h,
      Fill_ <<- f,
      Stroke_ <<- s
    ]

spec :: Spec
spec = do
  describe "with SVGLadder, drawing primitives" $ do
    basicSvg <- runIO $ TIO.readFile "out/basic.svg"
    let dc = defaultAAVConfig
        rectangle = svgRect $ Rect (0,0) (60, 30) "#bbbbbb" "none"
        basicSvg' = makeSvg' dc (defaultBBox (cscale dc), rectangle)
    it "should be able to create a real basic SVG rectangle" $ do
      renderText basicSvg' `shouldBe` basicSvg

    it "should be able to test a BoxedSVG" $ do
      show
        ( defaultBBox (cscale dc),
          rect_
            [ X_ <<-* 0,
              Y_ <<-* 0,
              Width_ <<-* 60,
              Height_ <<-* 30,
              Fill_ <<- "#bbbbbb",
              Stroke_ <<- "none"
            ] ::
            Element
        )
        `shouldBe` show
          ( defaultBBox (cscale dc),
            rect_
              [ X_ <<-* 0,
                Y_ <<-* 0,
                Width_ <<-* 60,
                Height_ <<-* 30,
                Fill_ <<- "#bbbbbb",
                Stroke_ <<- "none"
              ] ::
              Element
          )

  describe "topText" $ do
    it "extracts the only from Pre" $ do
      topText (Just $ Pre "a") `shouldBe` Just "a"
    it "extracts first from PrePost" $ do
      topText (Just $ PrePost "c" "b") `shouldBe` Just "c"
    it "does Nothing" $ do
      topText (Nothing :: Maybe (Label Text)) `shouldBe` Nothing

  describe "bottomText" $ do
    it "extracts second from PrePost" $ do
      bottomText (Just $ PrePost "c" "b") `shouldBe` Just "b"
    it "extracts Nothing from Pre" $ do
      bottomText (Just $ Pre "a") `shouldBe` Nothing
    it "does Nothing" $ do
      bottomText (Nothing :: Maybe (Label Text)) `shouldBe` Nothing
