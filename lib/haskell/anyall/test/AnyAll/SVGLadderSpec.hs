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

extractBoxesAndSVGs:: [BoxedSVG] -> ([Set.Set (Text, Text)],[BBox])
extractBoxesAndSVGs alignBoxes = (svgsAttrs, boundingBoxes)
  where
  svgs = TL.toStrict . renderText . snd <$> alignBoxes
  svgsAttrs = parseSVG <$> svgs
  boundingBoxes = fst <$> alignBoxes

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
    let
      firstBox = templatedBoundingBox {bbw = 60, bbh = 10}
      firstRect = svgRect $ Rect (0, 0) (60, 10) "black" "none"
      secondBox = templatedBoundingBox {bbw = 20, bbh = 30}
      secondRect = svgRect $ Rect (0, 0) (20, 30) "black" "none"
      firstSVGAttrs  = [("fill","black"),("height","10"),("stroke","none"),("width","60"),("y","0"),("x","0")]
      secondSVGAttrs = [("fill","black"),("height","30"),("stroke","none"),("width","20"),("y","0"),("x","0")]

    it "expands bounding box on Left alignment" $ do
      let
        alignBoxes = hAlign HLeft [(firstBox, firstRect), (secondBox, secondRect)]
        (svgsAttrs, boundingBoxes) = extractBoxesAndSVGs alignBoxes

        firstExpected  = Set.fromList  firstSVGAttrs
        secondExpected = Set.fromList  secondSVGAttrs
      svgsAttrs `shouldBe` [firstExpected, secondExpected]
      boundingBoxes `shouldBe` [firstBox, secondBox{bbw=60, bbrm=40}]

    it "expands bounding box and shift rectangle on Central alignment" $ do
      let
        alignBoxes = hAlign HCenter [(firstBox, firstRect), (secondBox, secondRect)]
        (svgsAttrs, boundingBoxes) = extractBoxesAndSVGs alignBoxes

        firstExpected  = Set.fromList $ ("transform","translate(0.0000 0.0000)") : firstSVGAttrs
        secondExpected = Set.fromList $ ("transform","translate(20.0000 0.0000)") : secondSVGAttrs
      svgsAttrs `shouldBe` [firstExpected, secondExpected]
      boundingBoxes `shouldBe` [firstBox, secondBox{bbw=60, bblm=20, bbrm=20}]

    it "expands bounding box and shift rectangle on Right alignment" $ do
      let
        alignBoxes = hAlign HRight [(firstBox, firstRect), (secondBox, secondRect)]
        (svgsAttrs, boundingBoxes) = extractBoxesAndSVGs alignBoxes

        firstExpected  = Set.fromList $ ("transform","translate(0.0000 0.0000)") : firstSVGAttrs
        secondExpected = Set.fromList $ ("transform","translate(40.0000 0.0000)") : secondSVGAttrs
      svgsAttrs `shouldBe` [firstExpected, secondExpected]
      boundingBoxes `shouldBe` [firstBox, secondBox{bbw=60, bblm=40}]

    it "expands bounding box on Top alignment" $ do
      let
        alignBoxes = vAlign VTop [(firstBox, firstRect), (secondBox, secondRect)]
        (svgsAttrs, boundingBoxes) = extractBoxesAndSVGs alignBoxes

        firstExpected  = Set.fromList  firstSVGAttrs
        secondExpected = Set.fromList  secondSVGAttrs
      svgsAttrs `shouldBe` [firstExpected, secondExpected]
      boundingBoxes `shouldBe` [firstBox{bbh=30, bbbm=20}, secondBox]

    it "expands bounding box and shift rectangle on Middle alignment" $ do
      let
        alignBoxes = vAlign VMiddle [(firstBox, firstRect), (secondBox, secondRect)]
        (svgsAttrs, boundingBoxes) = extractBoxesAndSVGs alignBoxes

        firstExpected  = Set.fromList $ ("transform","translate(0.0000 10.0000)") : firstSVGAttrs
        secondExpected = Set.fromList $ ("transform","translate(0.0000 0.0000)") : secondSVGAttrs
      svgsAttrs `shouldBe` [firstExpected, secondExpected]
      boundingBoxes `shouldBe` [firstBox{bbh=30, bbbm=10, bbtm = 10.0}, secondBox]

    it "expands bounding box and shift rectangle on Bottom alignment" $ do
      let
        alignBoxes = vAlign VBottom [(firstBox, firstRect), (secondBox, secondRect)]
        (svgsAttrs, boundingBoxes) = extractBoxesAndSVGs alignBoxes

        firstExpected  = Set.fromList $ ("transform","translate(0.0000 20.0000)") : firstSVGAttrs
        secondExpected = Set.fromList $ ("transform","translate(0.0000 0.0000)") : secondSVGAttrs
      svgsAttrs `shouldBe` [firstExpected, secondExpected]
      boundingBoxes `shouldBe` [firstBox{bbh=30, bbtm = 20.0}, secondBox]

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
