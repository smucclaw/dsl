{-# LANGUAGE OverloadedStrings #-}

module AnyAll.SVGLadderSpec (spec) where

import AnyAll.SVGLadder hiding (tl)
import AnyAll.Types (Label (Pre, PrePost))
import Data.Text (Text, splitOn, pack, replace)
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
cleanXMLAttr at = ( pack $ XML.qName $ XML.attrKey at, replace ".0000" "" (pack $ XML.attrVal at))

parseRectSVG :: Text -> Set.Set (Text, Text)
parseRectSVG s =
        case  XML.parseXMLDoc s of
          Nothing  -> Set.empty
          Just doc -> Set.fromList $ cleanXMLAttr <$> XML.elAttribs doc

parseSVGContent :: XML.Content -> Set.Set (Text, Text)
parseSVGContent (XML.Elem e) = Set.fromList $ ("svgName", pack $ XML.qName $ XML.elName e) : (cleanXMLAttr <$> XML.elAttribs e)
parseSVGContent (XML.Text _) = Set.empty
parseSVGContent (XML.CRef _) = Set.empty

parseRectsSVG :: Text -> [Set.Set (Text, Text)]
parseRectsSVG s = [] -- XML.parseXML s

extractBoxesAndSVGs:: [BoxedSVG] -> ([Set.Set (Text, Text)],[BBox])
extractBoxesAndSVGs alignBoxes = (svgsAttrs, boundingBoxes)
  where
  svgs = TL.toStrict . renderText . snd <$> alignBoxes
  svgsAttrs = parseRectSVG <$> svgs
  boundingBoxes = fst <$> alignBoxes

extractBoxAndSVG:: BoxedSVG -> (BBox, [Set.Set (Text, Text)])
extractBoxAndSVG alignBoxes = (boundingBoxes, svgsAttrs)
  where
  svgs = TL.toStrict . renderText . snd $ alignBoxes
  svgsAttrs = parseSVGContent <$> XML.parseXML svgs
  boundingBoxes = fst alignBoxes

spec :: Spec
spec = do
  let
    dc = defaultAAVConfig { cdebug = True }
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

        firstExpected  = Set.fromList $ ("transform","translate(0 0)") : firstSVGAttrs
        secondExpected = Set.fromList $ ("transform","translate(20 0)") : secondSVGAttrs
      svgsAttrs `shouldBe` [firstExpected, secondExpected]
      boundingBoxes `shouldBe` [firstBox, secondBox{bbw=60, bblm=20, bbrm=20}]

    it "expands bounding box and shift rectangle on Right alignment" $ do
      let
        alignBoxes = hAlign HRight [(firstBox, firstRect), (secondBox, secondRect)]
        (svgsAttrs, boundingBoxes) = extractBoxesAndSVGs alignBoxes

        firstExpected  = Set.fromList $ ("transform","translate(0 0)") : firstSVGAttrs
        secondExpected = Set.fromList $ ("transform","translate(40 0)") : secondSVGAttrs
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

        firstExpected  = Set.fromList $ ("transform","translate(0 10)") : firstSVGAttrs
        secondExpected = Set.fromList $ ("transform","translate(0 0)") : secondSVGAttrs
      svgsAttrs `shouldBe` [firstExpected, secondExpected]
      boundingBoxes `shouldBe` [firstBox{bbh=30, bbbm=10, bbtm = 10.0}, secondBox]

    it "expands bounding box and shift rectangle on Bottom alignment" $ do
      let
        alignBoxes = vAlign VBottom [(firstBox, firstRect), (secondBox, secondRect)]
        (svgsAttrs, boundingBoxes) = extractBoxesAndSVGs alignBoxes

        firstExpected  = Set.fromList $ ("transform","translate(0 20)") : firstSVGAttrs
        secondExpected = Set.fromList $ ("transform","translate(0 0)") : secondSVGAttrs
      svgsAttrs `shouldBe` [firstExpected, secondExpected]
      boundingBoxes `shouldBe` [firstBox{bbh=30, bbtm = 20.0}, secondBox]

  describe "test hlayout" $ do
    let
      firstBox = templatedBoundingBox {bbw = 60, bbh = 10}
      firstRect = svgRect $ Rect (0, 0) (60, 10) "black" "none"
      secondBox = templatedBoundingBox {bbw = 20, bbh = 30}
      secondRect = svgRect $ Rect (0, 0) (20, 30) "black" "none"
      firstSVGAttrs  = [("svgName","rect"), ("fill","black"),("height","10"),("stroke","none"),("width","60"),("y","0"),("x","0")]
      secondSVGAttrs = [("svgName","rect"), ("fill","#f5f5f5"),("height","35.0"),("stroke","none"),("transform","translate(65 0)"),("width","20.0"),("y","0"),("x","0")]
      thirdSVGAttrs  = [("svgName","rect"), ("fill","#f8eeee"),("height","30.0"),("stroke","none"),("transform","translate(65 0)"),("width","20.0"),("x","0"),("y","0.0")]
      forthSVGAttrs  = [("svgName","rect"), ("fill","black"),("height","30"),("stroke","none"),("transform","translate(65 0)"),("width","20"),("x","0"),("y","0")]
      pathSVGAttrs  =  [("svgName","path"), ("d","M 60,5 c 5,0 0,10 5 10"),("fill","none"),("stroke","red")]
    it "expands bounding box on Left alignment" $ do
      let
        alignBox = hlayout dc (firstBox, firstRect) (secondBox, secondRect)
        (resultBox, resultSVG) = extractBoxAndSVG alignBox
      --  xx = TL.toStrict . renderText . snd $ alignBox
      -- _ <- print xx
      resultBox `shouldBe` firstBox{bbw = 85.0, bbh = 30.0, pl = PVoffset 5.0, pr = PVoffset 15.0}
      resultSVG `shouldBe` Set.fromList <$> [firstSVGAttrs, secondSVGAttrs, thirdSVGAttrs, forthSVGAttrs, pathSVGAttrs]

  describe "test vlayout" $ do
    let
      firstBox = templatedBoundingBox {bbw = 60, bbh = 10}
      firstRect = svgRect $ Rect (0, 0) (60, 10) "black" "none"
      secondBox = templatedBoundingBox {bbw = 20, bbh = 30}
      secondRect = svgRect $ Rect (0, 0) (20, 30) "black" "none"
      myScale     = getScale (cscale dc)
      lrVgap      = slrv myScale
      elems = [(firstBox, firstRect), (secondBox, secondRect)]
      childheights = lrVgap * fromIntegral (length elems - 1) + sum (bbh . fst <$> elems)
      mybbox = (defaultBBox (cscale dc)) { bbh = childheights, bbw = maximum ( bbw . fst <$> elems ) }
      firstSVGAttrs  = [("svgName","rect"), ("fill","black"),("height","10"),("stroke","none"),("width","60"),("y","0"),("x","0")]
      secondSVGAttrs = [("svgName","rect"), ("fill","black"),("height","30"),("stroke","none"),("transform","translate(0 15)"),("width","20"),("x","0"),("y","0")]
      forthSVGAttrs  = [("d","M -6,27.5000 C 0,27.5000 -6,30 0 30"),("fill","none"),("stroke","green"),("svgName","path")]
      pathSVGAttrs  =  [("d","M 66,27.5000 C 60,27.5000 66,30 60 30"),("fill","none"),("stroke","green"),("svgName","path")]
    it "expands bounding box on Left alignment" $ do
      let
        alignBox = vlayout dc mybbox (firstBox, firstRect) (secondBox, secondRect)
        (resultBox, resultSVG) = extractBoxAndSVG alignBox
      --  xx = TL.toStrict . renderText . snd $ alignBox
      -- _ <- print xx
      resultBox `shouldBe` firstBox{bbw = 60.0, bbh = 45.0}
      resultSVG `shouldBe` Set.fromList <$> [firstSVGAttrs, secondSVGAttrs, forthSVGAttrs, pathSVGAttrs]

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
