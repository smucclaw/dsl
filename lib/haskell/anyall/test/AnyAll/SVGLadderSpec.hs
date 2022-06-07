{-# LANGUAGE OverloadedStrings #-}

module AnyAll.SVGLadderSpec (spec) where

import AnyAll.SVGLadder hiding (tl)
import AnyAll.Types (Label (Pre, PrePost))
import Data.Text (Text, splitOn, pack)
import qualified Data.Text.Lazy.IO as TIO
import Graphics.Svg
import Test.Hspec
import qualified Data.Text.Lazy as TL (toStrict)
import qualified Data.Set as Set
import qualified Text.XML.Light as XML
import Text.XML.Light.Output (showTopElement)
import Text.XML.Light (Attr(attrKey))


data SVGRect = Rect {tl :: (Integer, Integer), br :: (Integer, Integer), fill :: Text, stroke :: Text}

svgRect :: SVGRect -> Element
svgRect Rect {tl = (x, y), br = (w, h), fill = f, stroke = s} =
  rect_
    [ X_ <<-* x,
      Y_ <<-* y,
      Width_ <<-* w,
      Height_ <<-* h,
      Fill_ <<- f,
      Stroke_ <<- s
    ]

cleanXMLAttr :: XML.Attr -> (Text, Text)
cleanXMLAttr at = ( pack $ XML.qName $ XML.attrKey at, pack $ XML.attrVal at)

parseSVG :: Text -> Set.Set (Text, Text)
parseSVG s =
        case  XML.parseXMLDoc s of
          Nothing  -> Set.empty
          Just doc -> Set.fromList $ cleanXMLAttr <$> XML.elAttribs doc

spec :: Spec
spec = do
  let
    dc = defaultAAVConfig
    templatedBoundingBox = defaultBBox (cscale dc)
  describe "with SVGLadder, drawing primitives" $ do
    basicSvg <- runIO $ TIO.readFile "out/basic.svg"
    let
      rectangle = svgRect $ Rect (0, 0) (60, 30) "black" "none"
      basicSvg' = makeSvg' dc (defaultBBox (cscale dc), rectangle)
    it "should be able to create a real basic SVG rectangle" $ do
      renderText basicSvg' `shouldBe` basicSvg

    it "should be able to test a BoxedSVG" $ do
      show
        ( defaultBBox (cscale dc),
          svgRect $ Rect (0, 0) (60, 30) "black" "none"
        )
        `shouldBe` show
          ( defaultBBox (cscale dc),
            svgRect $ Rect (0, 0) (60, 30) "black" "none"
          )

  describe "test aligment" $ do
    let dc = defaultAAVConfig
        firstBox = templatedBoundingBox {bbw = 60, bbh = 10}
        firstRect = svgRect $ Rect (0, 0) (60, 10) "black" "none"
        secondBox = templatedBoundingBox {bbw = 20, bbh = 30}
        secondRect = svgRect $ Rect (0, 0) (20, 30) "black" "none"
    it "should be able to create a real basic SVG rectangle" $ do
      let
        alignBoxes = hAlign HLeft [(firstBox, firstRect), (secondBox, secondRect)]
        svgs = TL.toStrict . renderText . snd <$> alignBoxes
        svgsAttrs = parseSVG <$> svgs

        firstExpected = Set.fromList  [("fill","black"),("height","10"),("stroke","none"),("width","60"),("y","0"),("x","0")]
        secondExpected = Set.fromList  [("fill","black"),("height","30"),("stroke","none"),("width","20"),("y","0"),("x","0")]
      svgsAttrs `shouldBe` [firstExpected, secondExpected]

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
