{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module AnyAll.SVGLadderSpec (spec) where

import AnyAll.SVGLadder hiding (tl)
import AnyAll.Types
import Data.Text (Text, splitOn, pack, replace)
import qualified Data.Text.Lazy.IO as TIO
import Graphics.Svg
import Test.Hspec
import qualified Data.Text.Lazy as TL (toStrict)
import qualified Data.Set as Set
import qualified Text.XML.Light as XML
import Text.XML.Light.Output (showTopElement)
import Text.XML.Light (Attr(attrKey))
import qualified Data.ByteString.Lazy as B
import Data.Aeson (eitherDecode)
import AnyAll (hardnormal)
import Data.Tree
import Data.Sequence.Internal.Sorting (Queue(Q))

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

compositeAndTree :: Tree (Q Text)
compositeAndTree =
  Node
    { rootLabel =
        AnyAll.Types.Q
          { shouldView = View,
            andOr = And,
            prePost = Just (Pre "all of"),
            mark = Default {getDefault = Left Nothing}
          },
      subForest =
        [ Node
            { rootLabel =
                AnyAll.Types.Q
                  { shouldView = View,
                    andOr = Simply "walk",
                    prePost = Nothing,
                    mark = Default {getDefault = Right (Just True)}
                  },
              subForest = []
            },
          Node
            { rootLabel =
                AnyAll.Types.Q
                  { shouldView = View,
                    andOr = And,
                    prePost = Just (Pre "all"),
                    mark = Default {getDefault = Left Nothing}
                  },
              subForest =
                [ Node
                    { rootLabel =
                        AnyAll.Types.Q
                          { shouldView = Ask,
                            andOr = Simply "eat",
                            prePost = Nothing,
                            mark = Default {getDefault = Left (Just False)}
                          },
                      subForest = []
                    },
                  Node
                    { rootLabel =
                        AnyAll.Types.Q
                          { shouldView = Ask,
                            andOr = Simply "drink",
                            prePost = Nothing,
                            mark = Default {getDefault = Left (Just True)}
                          },
                      subForest = []
                    }
                ]
            }
        ]
    }


simpleAndTree :: Tree (Q Text)
simpleAndTree =
  Node
    { rootLabel =
        AnyAll.Types.Q
          { shouldView = View,
            andOr = And,
            prePost = Just (Pre "all"),
            mark = Default {getDefault = Right (Just True)}
          },
      subForest =
        [ Node
            { rootLabel =
                AnyAll.Types.Q
                  { shouldView = Ask,
                    andOr = Simply "eat",
                    prePost = Nothing,
                    mark = Default {getDefault = Right (Just True)}
                  },
              subForest = []
            },
          Node
            { rootLabel =
                AnyAll.Types.Q
                  { shouldView = Ask,
                    andOr = Simply "drink",
                    prePost = Nothing,
                    mark = Default {getDefault = Right (Just True)}
                  },
              subForest = []
            }
        ]
    }

simpleOrTree :: Tree (Q Text)
simpleOrTree =
  Node
    { rootLabel =
        AnyAll.Types.Q
          { shouldView = View,
            andOr = Or,
            prePost = Just (Pre "any"),
            mark = Default {getDefault = Left Nothing}
          },
      subForest =
        [ Node
            { rootLabel =
                AnyAll.Types.Q
                  { shouldView = Ask,
                    andOr = Simply "eat",
                    prePost = Nothing,
                    mark = Default {getDefault = Left (Just False)}
                  },
              subForest = []
            },
          Node
            { rootLabel =
                AnyAll.Types.Q
                  { shouldView = Ask,
                    andOr = Simply "drink",
                    prePost = Nothing,
                    mark = Default {getDefault = Left (Just True)}
                  },
              subForest = []
            }
        ]
    }

spec :: Spec
spec = do
  let
    dc = defaultAAVConfig { cdebug = True}
    c = dc{cscale=Full, cdebug = False}
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

  describe "test rowLayouter" $ do
    let
      firstBox = templatedBoundingBox {bbw = 60, bbh = 10}
      firstRect = svgRect $ Rect (0, 0) (60, 10) "black" "none"
      secondBox = templatedBoundingBox {bbw = 20, bbh = 30}
      secondRect = svgRect $ Rect (0, 0) (20, 30) "black" "none"
      elems = [(firstBox, firstRect), (secondBox, secondRect)]
      alignedBox1:alignedBox2:_ = vAlign VMiddle elems
      alignBox = rowLayouter c alignedBox1 alignedBox2
      firstSVGAttrs  = [("svgName","rect"), ("fill","black"),("height","10"),("stroke","none"),("transform","translate(0 10)"),("width","60"),("y","0"),("x","0")]
      forthSVGAttrs  = [("svgName","rect"), ("fill","black"),("height","30"),("stroke","none"),("transform","translate(0 0)translate(70 0)"),("width","20"),("x","0"),("y","0")]
      pathSVGAttrs  =  [("svgName","path"), ("class","h_connector"), ("d","M 60,15 c 10,0 0,0 10 0"),("fill","none"),("stroke","green")]
      (resultBox, resultSVG) = extractBoxAndSVG alignBox
    it "bounding box is correct" $ do
      resultBox `shouldBe` firstBox{bbw = 90.0, bbh = 30.0, pl = PVoffset 15.0, pr = PVoffset 15.0}
    it "svg is correct" $ do
      resultSVG `shouldBe` Set.fromList <$> [firstSVGAttrs, forthSVGAttrs, pathSVGAttrs]
    it "print debug" $ do
      let
        svgXml = TL.toStrict . renderText . move (23,23) $ snd alignBox
      _ <- print resultBox
      pendingWith "it's not a real test but just a debug code"

  describe "test columnLayouter" $ do
    let
      firstBox = templatedBoundingBox {bbw = 60, bbh = 10}
      firstRect = svgRect $ Rect (0, 0) (60, 10) "black" "none"
      secondBox = templatedBoundingBox {bbw = 20, bbh = 30}
      secondRect = svgRect $ Rect (0, 0) (20, 30) "black" "none"
      myScale     = getScale (cscale c)
      lrVgap      = slrv myScale
      elems = [(firstBox, firstRect), (secondBox, secondRect)]
      startBox = (defaultBBox (cscale c), mempty::SVGElement)
      alignedBox1:alignedBox2:_ = hAlign HCenter elems
      childheights = lrVgap * fromIntegral (length elems - 1) + sum (bbh . fst <$> elems)
      mybbox = (defaultBBox (cscale c)) { bbh = childheights, bbw = maximum ( bbw . fst <$> elems ) }
      -- Have to use vlayout 2 times to feed start box
      tempBox = columnLayouter c mybbox startBox alignedBox1
      alignBox = columnLayouter c mybbox tempBox alignedBox2

      (resultBox, resultSVG) = extractBoxAndSVG alignBox
      firstSVGBox  = [("svgName","rect"), ("fill","black"),("height","10"),("stroke","none"),("transform","translate(0 0)translate(0 10)"),("width","60"),("y","0"),("x","0")]
      inConnector1 = [("d","M -22,32 C 0,32 -22,15 0 15"),("fill","none"),("stroke","green"),("svgName","path"), ("class","v_connector_in")]
      outConnector1  = [("d","M 82,32 C 60,32 82,15 60 15"),("fill","none"),("stroke","green"),("svgName","path"), ("class","v_connector_out")]

      secondSVGBox = [("svgName","rect"), ("fill","black"),("height","30"),("stroke","none"),("transform","translate(20 0)translate(0 30)"),("width","20"),("x","0"),("y","0")]
      inConnector2  = [("d","M -22,32 C 0,32 -22,45 20 45"),("fill","none"),("stroke","green"),("svgName","path"), ("class","v_connector_in")]
      outConnector2  =  [("d","M 82,32 C 60,32 82,45 40 45"),("fill","none"),("stroke","green"),("svgName","path"),("class","v_connector_out")]
    it "gets correct vbox" $ do
      resultBox `shouldBe` firstBox{bbw = 60.0, bbh = 60.0, pl = PTop, pr = PTop}
    it "gets correct svg" $ do
      resultSVG `shouldBe` Set.fromList <$> [firstSVGBox, inConnector1, outConnector1, secondSVGBox, inConnector2, outConnector2]
    xit "print debug" $ do
      let
        svgXml = TL.toStrict . renderText . move (23,23) $ snd alignBox
      _ <- print svgXml
      pendingWith "it's not a real test but just a debug code"

  describe "test combineAnd" $ do
    mycontents <- runIO $ B.readFile "out/example-and-short.json"
    myFixture <- runIO $ B.readFile "out/example-and-short.svg"
    let
      c = dc{cscale=Full, cdebug = False} 
      myinput = eitherDecode mycontents :: Either String (StdinSchema Text)
      (Right myright) = myinput
      questionTree = hardnormal (marking myright) (andOrTree myright)
      --(bbox, svg) = q2svg' c qq
      (bbox2, svg2) = drawItemFull c False simpleAndTree
      svgs = renderBS svg2
      (Node (AnyAll.Types.Q  sv ao               pp m) childqs) = simpleAndTree
      rawChildren = drawItemFull c False <$> childqs
      hrawChildren = hAlign HCenter rawChildren
    -- _ <- runIO $ print svgs
    -- _ <- runIO $ print rawChildren
    it "expands bounding box on Left alignment" $ do
      svgs `shouldBe` myFixture
    it "print debug" $ do
      let
        svgXml = TL.toStrict . renderText . move (23,23) $ svg2
      _ <- print bbox2
      pendingWith "it's not a real test but just a debug code"

  describe "test combineOr" $ do
    mycontents <- runIO $ B.readFile "out/example-or-short.json"
    myFixture <- runIO $ B.readFile "out/example-or-short.svg"
    let
      c = dc{cscale=Full, cdebug = False} 
      myinput = eitherDecode mycontents :: Either String (StdinSchema Text)
      (Right myright) = myinput
      questionTree = hardnormal (marking myright) (andOrTree myright)
      --(bbox, svg) = q2svg' c qq
      (bbox2, svg2) = drawItemFull c False simpleOrTree
      svgs = renderBS svg2
      (Node (AnyAll.Types.Q  sv ao               pp m) childqs) = simpleOrTree
      rawChildren = drawItemFull c False <$> childqs
      hrawChildren = hAlign HCenter rawChildren
    -- _ <- runIO $ print hrawChildren
    -- _ <- runIO $ print questionTree
    it "expands bounding box on Left alignment" $ do
      svgs `shouldBe` myFixture

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
