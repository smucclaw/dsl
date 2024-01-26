{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module AnyAll.SVGLadderSpec (spec) where

import AnyAll (hardnormal)
import AnyAll.BoolStruct (StdinSchema (andOrTree, marking))
import AnyAll.SVGLadder
  ( AAVConfig (cdebug, cscale),
    BBox (dimensions),
    BoxDimensions (boxHeight, boxWidth),
    BoxedSVG,
    DrawConfig (DrawConfig),
    HAlignment (HCenter, HLeft, HRight),
    PortStyleV (PMiddle, PTop, PVoffset),
    Question,
    QuestionTree,
    SVGElement,
    Scale (Full, Small, Tiny),
    VAlignment (VBottom, VMiddle, VTop),
    aavscaleHorizontalLayout,
    bboxHeight,
    bboxWidth,
    bottomMargin,
    boxMargins,
    boxPorts,
    columnLayouter,
    combineAnd,
    combineAndS,
    defaultAAVConfig,
    defaultBBox,
    defaultBBox',
    drawItemFull,
    drawLeafR,
    gapVertical,
    getColorsBox,
    getColorsText,
    getScale,
    hAlign,
    leftMargin,
    leftPort,
    makeSvg',
    move,
    rightMargin,
    rightPort,
    rowLayouter,
    rowLayouterS,
    textBoxLengthFull,
    textBoxLengthTiny,
    topMargin,
    vAlign,
    (<<-*),
  )
import AnyAll.Types
  ( AndOr (And, Or, Simply),
    Default (Default),
    Label (Pre),
    Q (Q, andOr, mark, prePost, shouldView),
    ShouldView (Ask, View),
  )
import Control.Monad.RWS (execRWS)
import Control.Monad.Reader (runReader)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as B
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Sequence.Internal.Sorting (Queue (Q))
import Data.Set qualified as Set
import Data.Text (Text, pack, replace, splitOn)
import Data.Text.Lazy qualified as TL (toStrict)
import Data.Text.Lazy.IO qualified as TIO
import Data.Tree (Tree (Node, rootLabel, subForest))
import Graphics.Svg
  ( AttrTag (Fill_, Height_, Stroke_, Width_, X_, Y_),
    Element,
    rect_,
    renderBS,
    renderText,
    (<<-),
  )
import Lens.Micro.Platform ((%~), (&), (.~), (^.), _1)
import Test.Hspec
  ( Spec,
    describe,
    it,
    pendingWith,
    runIO,
    shouldBe,
    shouldSatisfy,
  )
import Test.Hspec.Golden (Golden (..))
import Text.XML.Light (Attr (attrKey))
import Text.XML.Light qualified as XML
import Text.XML.Light.Output (showTopElement)

data SVGRect = Rect {tl :: (Integer, Integer), br :: (Integer, Integer), fill :: Text, stroke :: Text}

goldenBytestring :: String -> B.ByteString -> Golden B.ByteString
goldenBytestring name actualOutput =
    Golden {
        output = actualOutput,
        encodePretty = unpack,
        writeToFile = B.writeFile,
        readFromFile = B.readFile,
        goldenFile = "test/golden/" <> name <> ".svg",
        actualFile = Just ("test/golden/.actual-" <> name <> ".svg"),
        failFirstTime = False
    }

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

compositeAndTree :: QuestionTree
compositeAndTree =
  Node
    { rootLabel =
        AnyAll.Types.Q
          { shouldView = View,
            andOr = And,
            prePost = Just (Pre "all of"),
            mark = Default ( Left Nothing )
          },
      subForest =
        [ Node
            { rootLabel =
                AnyAll.Types.Q
                  { shouldView = View,
                    andOr = Simply "walk",
                    prePost = Nothing,
                    mark = Default ( Right (Just True) )
                  },
              subForest = []
            },
          Node
            { rootLabel =
                AnyAll.Types.Q
                  { shouldView = View,
                    andOr = And,
                    prePost = Just (Pre "all"),
                    mark = Default ( Left Nothing )
                  },
              subForest =
                [ Node
                    { rootLabel =
                        AnyAll.Types.Q
                          { shouldView = Ask,
                            andOr = Simply "eat",
                            prePost = Nothing,
                            mark = Default ( Left (Just False) )
                          },
                      subForest = []
                    },
                  Node
                    { rootLabel =
                        AnyAll.Types.Q
                          { shouldView = Ask,
                            andOr = Simply "drink",
                            prePost = Nothing,
                            mark = Default ( Left (Just True) )
                          },
                      subForest = []
                    }
                ]
            }
        ]
    }


simpleAndTree :: QuestionTree
simpleAndTree =
  Node
    { rootLabel =
        AnyAll.Types.Q
          { shouldView = View,
            andOr = And,
            prePost = Just (Pre "all"),
            mark = Default ( Right (Just True) )
          },
      subForest =
        [ Node
            { rootLabel =
                AnyAll.Types.Q
                  { shouldView = Ask,
                    andOr = Simply "eat",
                    prePost = Nothing,
                    mark = Default ( Right (Just True) )
                  },
              subForest = []
            },
          Node
            { rootLabel =
                AnyAll.Types.Q
                  { shouldView = Ask,
                    andOr = Simply "drink",
                    prePost = Nothing,
                    mark = Default ( Right (Just True) )
                  },
              subForest = []
            }
        ]
    }

simpleOrTree :: QuestionTree
simpleOrTree =
  Node
    { rootLabel =
        AnyAll.Types.Q
          { shouldView = View,
            andOr = Or,
            prePost = Just (Pre "any"),
            mark = Default ( Left Nothing )
          },
      subForest =
        [ Node
            { rootLabel =
                AnyAll.Types.Q
                  { shouldView = Ask,
                    andOr = Simply "eat",
                    prePost = Nothing,
                    mark = Default ( Left (Just False) )
                  },
              subForest = []
            },
          Node
            { rootLabel =
                AnyAll.Types.Q
                  { shouldView = Ask,
                    andOr = Simply "drink",
                    prePost = Nothing,
                    mark = Default ( Left (Just True) )
                  },
              subForest = []
            }
        ]
    }

makeSingleNodeTree :: Text -> Question
makeSingleNodeTree t =
  AnyAll.Types.Q
    { shouldView = View,
      andOr = Simply t,
      prePost = Nothing,
      mark = Default ( Left Nothing )
    }

spec :: Spec
spec = do
  let
    dc = defaultAAVConfig { cdebug = True}
    c = dc{cscale=Full, cdebug = False}
    templatedBoundingBox = defaultBBox (cscale dc)
  describe "with SVGLadder, drawing primitives" do
    basicSvg <- runIO $ TIO.readFile "test/fixtures/basic.svg"
    let
      rectangle = svgRect $ Rect (0, 0) (60, 30) "black" "none"
      basicSvg' = makeSvg' dc (defaultBBox (cscale dc), rectangle)
    it "should be able to create a real basic SVG rectangle" do
      renderText basicSvg' `shouldBe` basicSvg
    it "should be able to test a BoxedSVG" do
      show
        ( defaultBBox (cscale dc),
          svgRect $ Rect (0, 0) (60, 30) "black" "none"
        )
        `shouldBe` show
          ( defaultBBox (cscale dc),
            svgRect $ Rect (0, 0) (60, 30) "black" "none"
          )

  describe "test aligment" do
    let
      firstBox = templatedBoundingBox & bboxWidth .~ 60 & bboxHeight .~  10
      firstRect = svgRect $ Rect (0, 0) (60, 10) "black" "none"
      secondBox = templatedBoundingBox & bboxWidth .~ 20 & bboxHeight .~ 30
      secondRect = svgRect $ Rect (0, 0) (20, 30) "black" "none"
      firstSVGAttrs  = [("fill","black"),("height","10"),("stroke","none"),("width","60"),("y","0"),("x","0")]
      secondSVGAttrs = [("fill","black"),("height","30"),("stroke","none"),("width","20"),("y","0"),("x","0")]

    it "expands bounding box on Left alignment" do
      let
        alignBoxes = hAlign HLeft [(firstBox, firstRect), (secondBox, secondRect)]
        (svgsAttrs, boundingBoxes) = extractBoxesAndSVGs alignBoxes

        firstExpected  = Set.fromList  firstSVGAttrs
        secondExpected = Set.fromList  secondSVGAttrs
      svgsAttrs `shouldBe` [firstExpected, secondExpected]
      boundingBoxes `shouldBe` [firstBox, secondBox & bboxWidth .~ 60 & boxMargins.rightMargin .~ 40]

    it "expands bounding box and shift rectangle on Central alignment" do
      let
        alignBoxes = hAlign HCenter [(firstBox, firstRect), (secondBox, secondRect)]
        (svgsAttrs, boundingBoxes) = extractBoxesAndSVGs alignBoxes

        firstExpected  = Set.fromList $ ("transform","translate(0 0)") : firstSVGAttrs
        secondExpected = Set.fromList $ ("transform","translate(20 0)") : secondSVGAttrs
      svgsAttrs `shouldBe` [firstExpected, secondExpected]
      boundingBoxes `shouldBe` [firstBox, secondBox & bboxWidth .~ 60 & boxMargins.leftMargin .~ 20 & boxMargins.rightMargin .~ 20]

    it "expands bounding box and shift rectangle on Right alignment" do
      let
        alignBoxes = hAlign HRight [(firstBox, firstRect), (secondBox, secondRect)]
        (svgsAttrs, boundingBoxes) = extractBoxesAndSVGs alignBoxes

        firstExpected  = Set.fromList $ ("transform","translate(0 0)") : firstSVGAttrs
        secondExpected = Set.fromList $ ("transform","translate(40 0)") : secondSVGAttrs
      svgsAttrs `shouldBe` [firstExpected, secondExpected]
      boundingBoxes `shouldBe` [firstBox, secondBox & bboxWidth .~ 60 & boxMargins.leftMargin .~ 40]

    it "expands bounding box on Top alignment" do
      let
        alignBoxes = vAlign VTop [(firstBox, firstRect), (secondBox, secondRect)]
        (svgsAttrs, boundingBoxes) = extractBoxesAndSVGs alignBoxes

        firstExpected  = Set.fromList  firstSVGAttrs
        secondExpected = Set.fromList  secondSVGAttrs
      svgsAttrs `shouldBe` [firstExpected, secondExpected]
      boundingBoxes `shouldBe` [firstBox & bboxHeight .~ 30 & boxMargins.bottomMargin .~ 20, secondBox]

    it "expands bounding box and shift rectangle on Middle alignment" do
      let
        alignBoxes = vAlign VMiddle [(firstBox, firstRect), (secondBox, secondRect)]
        (svgsAttrs, boundingBoxes) = extractBoxesAndSVGs alignBoxes

        firstExpected  = Set.fromList $ ("transform","translate(0 10)") : firstSVGAttrs
        secondExpected = Set.fromList $ ("transform","translate(0 0)") : secondSVGAttrs
      svgsAttrs `shouldBe` [firstExpected, secondExpected]
      boundingBoxes `shouldBe` [firstBox & bboxHeight .~ 30 & boxMargins.bottomMargin .~ 10 & boxMargins.topMargin .~ 10, secondBox]

    it "expands bounding box and shift rectangle on Bottom alignment" do
      let
        alignBoxes = vAlign VBottom [(firstBox, firstRect), (secondBox, secondRect)]
        (svgsAttrs, boundingBoxes) = extractBoxesAndSVGs alignBoxes

        firstExpected  = Set.fromList $ ("transform","translate(0 20)") : firstSVGAttrs
        secondExpected = Set.fromList $ ("transform","translate(0 0)") : secondSVGAttrs
      svgsAttrs `shouldBe` [firstExpected, secondExpected]
      boundingBoxes `shouldBe` [firstBox & bboxHeight .~ 30 & boxMargins.topMargin .~ 20, secondBox]

  describe "test rowLayouter" do
    let
      firstBox = templatedBoundingBox & bboxWidth .~ 60 & bboxHeight .~ 10 & boxMargins.leftMargin .~ 17
      firstRect = svgRect $ Rect (0, 0) (60, 10) "black" "none"
      secondBox = templatedBoundingBox & bboxWidth .~ 20 & bboxHeight .~ 30 & boxMargins.rightMargin .~ 13
      secondRect = svgRect $ Rect (0, 0) (20, 30) "black" "none"
      elems = [(firstBox, firstRect), (secondBox, secondRect)]
      alignedBox1:alignedBox2:_ = vAlign VMiddle elems
      alignBox = rowLayouter (cscale c) alignedBox1 alignedBox2
      firstSVGAttrs  = [("svgName","rect"), ("fill","black"),("height","10"),("stroke","none"),("transform","translate(0 10)"),("width","60"),("y","0"),("x","0")]
      forthSVGAttrs  = [("svgName","rect"), ("fill","black"),("height","30"),("stroke","none"),("transform","translate(0 0)translate(70 0)"),("width","20"),("x","0"),("y","0")]
      pathSVGAttrs  =  [("svgName","path"), ("class","h_connector"), ("d","M 60,15 c 5,0 5,0 10 0"),("fill","none"),("stroke","darkgrey")]
      (resultBox, resultSVG) = extractBoxAndSVG alignBox
    it "bounding box is correct" do
      resultBox `shouldBe` (firstBox 
                              & bboxWidth .~ 90
                              & bboxHeight .~ 30
                              & boxMargins.leftMargin .~ 17
                              & boxMargins.rightMargin .~ 13
                              & boxPorts.rightPort .~ PVoffset 15
                              & boxPorts.leftPort .~ PVoffset 15)
    it "svg is correct" do
      resultSVG `shouldBe` Set.fromList <$> [firstSVGAttrs, forthSVGAttrs, pathSVGAttrs]
    it "print debug" do
      let
        svgXml = TL.toStrict . renderText . move (23,23) $ snd alignBox
      _ <- print resultBox
      pendingWith "it's not a real test but just a debug code"

  describe "test combineAnd margins" do
    let
      firstBox = templatedBoundingBox & bboxWidth .~ 60 & bboxHeight .~ 10 & boxMargins.leftMargin .~ 17
      firstRect = svgRect $ Rect (0, 0) (60, 10) "black" "none"
      secondBox = templatedBoundingBox & bboxWidth .~ 20 & bboxHeight .~ 30 & boxMargins.rightMargin .~ 13
      secondRect = svgRect $ Rect (0, 0) (20, 30) "black" "none"
      elems = [(firstBox, firstRect), (secondBox, secondRect)]
      alignedBox1:alignedBox2:_ = vAlign VMiddle elems
      alignBox = combineAnd (cscale c) elems
      firstSVGAttrs  = [("svgName","rect"), ("fill","black"),("height","10"),("stroke","none"),("transform","translate(22 0)"),("width","60"),("y","0"),("x","0")]
      forthSVGAttrs  = [("svgName","rect"), ("fill","black"),("height","30"),("stroke","none"),("transform","translate(70 0)translate(22 0)"),("width","20"),("x","0"),("y","0")]
      pathSVGAttrs  =  [("svgName","path"), ("class","h_connector"), ("d","M 60,5 c 5,0 5,10 10 10"),("fill","none"),("stroke","darkgrey"),("transform","translate(22 0)")]
      (resultBox, resultSVG) = extractBoxAndSVG alignBox
    it "bounding box is correct" do
      resultBox `shouldBe` (firstBox & bboxWidth .~ 134 & bboxHeight .~ 30
                              & boxMargins.leftMargin .~ 22 + 17
                              & boxMargins.rightMargin .~ 22 + 13
                              & boxPorts.rightPort .~ PVoffset 15
                              & boxPorts.leftPort .~ PVoffset 5)
    it "svg is correct" do
      resultSVG `shouldBe` Set.fromList <$> [firstSVGAttrs, forthSVGAttrs, pathSVGAttrs]
    it "print debug" do
      let
        svgXml = TL.toStrict . renderText . move (23,23) $ snd alignBox
      _ <- print resultBox
      pendingWith "it's not a real test but just a debug code"

  describe "test RWS combineAnd margins" do
    let
      mark = Default ( Right (Just True) )
      contextR = DrawConfig Full True mark (defaultBBox Full) (getScale Full) textBoxLengthFull
      firstBox = templatedBoundingBox & bboxWidth .~ 60 & bboxHeight .~ 10 & boxMargins.leftMargin .~ 17
      firstRect = svgRect $ Rect (0, 0) (60, 10) "black" "none"
      secondBox = templatedBoundingBox & bboxWidth .~ 20 & bboxHeight .~ 30 & boxMargins.rightMargin .~ 13
      secondRect = svgRect $ Rect (0, 0) (20, 30) "black" "none"
      elems = [(firstBox, firstRect), (secondBox, secondRect)]
      alignedBox1:alignedBox2:_ = vAlign VMiddle elems
      alignBox = fst (execRWS (combineAndS elems) contextR (defaultBBox', mempty::SVGElement))
      firstSVGAttrs  = [("svgName","rect"), ("fill","black"),("height","10"),("stroke","none"),("transform","translate(22 0)"),("width","60"),("y","0"),("x","0")]
      forthSVGAttrs  = [("svgName","rect"), ("fill","black"),("height","30"),("stroke","none"),("transform","translate(70 0)translate(22 0)"),("width","20"),("x","0"),("y","0")]
      pathSVGAttrs  =  [("svgName","path"), ("class","h_connector"), ("d","M 60,5 c 5,0 5,10 10 10"),("fill","none"),("stroke","darkgrey"),("transform","translate(22 0)")]
      (resultBox, resultSVG) = extractBoxAndSVG alignBox
    it "bounding box is correct" do
      resultBox `shouldBe` (firstBox & bboxWidth .~ 134 & bboxHeight .~ 30
                              & boxMargins.leftMargin .~ 22 + 17
                              & boxMargins.rightMargin .~ 22 + 13
                              & boxPorts.rightPort .~ PVoffset 15
                              & boxPorts.leftPort .~ PVoffset 5)
    it "svg is correct" do
      resultSVG `shouldBe` Set.fromList <$> [firstSVGAttrs, forthSVGAttrs, pathSVGAttrs]
    it "print debug" do
      let
        svgXml = TL.toStrict . renderText . move (23,23) $ snd alignBox
      _ <- print resultBox
      pendingWith "it's not a real test but just a debug code"

  describe "test RWS rowLayouter" do
    let
      firstBox = templatedBoundingBox & bboxWidth .~ 60 & bboxHeight .~ 10 & boxMargins.leftMargin .~ 17
      firstRect = svgRect $ Rect (0, 0) (60, 10) "black" "none"
      secondBox = templatedBoundingBox & bboxWidth .~ 20 & bboxHeight .~ 30 & boxMargins.rightMargin .~ 13
      secondRect = svgRect $ Rect (0, 0) (20, 30) "black" "none"
      elems = [(firstBox, firstRect), (secondBox, secondRect)]
      alignedBox1:alignedBox2:_ = vAlign VMiddle elems
      contextR = DrawConfig Full True (Default ( Right (Just True) )) (defaultBBox Full) (getScale Full) textBoxLengthFull
      alignBox = fst (execRWS (rowLayouterS alignedBox2) contextR alignedBox1)
      firstSVGAttrs  = [("svgName","rect"), ("fill","black"),("height","10"),("stroke","none"),("transform","translate(0 10)"),("width","60"),("y","0"),("x","0")]
      forthSVGAttrs  = [("svgName","rect"), ("fill","black"),("height","30"),("stroke","none"),("transform","translate(0 0)translate(70 0)"),("width","20"),("x","0"),("y","0")]
      pathSVGAttrs  =  [("svgName","path"), ("class","h_connector"), ("d","M 60,15 c 5,0 5,0 10 0"),("fill","none"),("stroke","darkgrey")]
      (resultBox, resultSVG) = extractBoxAndSVG alignBox
    it "bounding box is correct" do
      resultBox `shouldBe` (firstBox
                              & bboxWidth .~ 90
                              & bboxHeight .~ 30
                              & boxMargins.leftMargin .~ 17
                              & boxMargins.rightMargin .~ 13
                              & boxPorts.rightPort .~ PVoffset 15
                              & boxPorts.leftPort .~ PVoffset 15)
    it "svg is correct" do
      resultSVG `shouldBe` Set.fromList <$> [firstSVGAttrs, forthSVGAttrs, pathSVGAttrs]
    it "print debug" do
      let
        svgXml = TL.toStrict . renderText . move (23,23) $ snd alignBox
      _ <- print resultBox
      pendingWith "it's not a real test but just a debug code"

  describe "test columnLayouter" do
    let
      firstBox = templatedBoundingBox & bboxWidth .~ 60 & bboxHeight .~ 10 & boxMargins.leftMargin .~ 17 & boxMargins.rightMargin .~ 13
      firstRect = svgRect $ Rect (0, 0) (60, 10) "black" "none"
      secondBox = templatedBoundingBox & bboxWidth .~ 20 & bboxHeight .~ 30 & boxMargins.leftMargin .~ 7 & boxMargins.rightMargin .~ 5
      secondRect = svgRect $ Rect (0, 0) (20, 30) "black" "none"
      myScale     = getScale (cscale c)
      lrVgap      = myScale ^. aavscaleHorizontalLayout.gapVertical
      elems = [(firstBox, firstRect), (secondBox, secondRect)]
      startBox = (defaultBBox (cscale c), mempty::SVGElement)
      alignedBox1:alignedBox2:_ = hAlign HCenter elems
      childheights = lrVgap * fromIntegral (length elems - 1) + sum (boxHeight . dimensions . fst <$> elems)
      mybbox = defaultBBox (cscale c) & bboxWidth .~ maximum ( boxWidth . dimensions . fst <$> elems ) & bboxHeight .~ childheights
      -- Have to use vlayout 2 times to feed start box
      tempBox = columnLayouter (cscale c) mybbox startBox alignedBox1
      alignBox = columnLayouter (cscale c) mybbox tempBox alignedBox2

      (resultBox, resultSVG) = extractBoxAndSVG alignBox
      firstSVGBox  = [("svgName","rect"), ("fill","black"),("height","10"),("stroke","none"),("transform","translate(0 0)translate(0 10)"),("width","60"),("y","0"),("x","0")]
      inConnector1 = [("d","M -22,32 C 0,32 -22,15 17 15"),("fill","none"),("stroke","darkgrey"),("svgName","path"), ("class","v_connector_in")]
      outConnector1  = [("d","M 82,32 C 60,32 82,15 47 15"),("fill","none"),("stroke","darkgrey"),("svgName","path"), ("class","v_connector_out")]

      secondSVGBox = [("svgName","rect"), ("fill","black"),("height","30"),("stroke","none"),("transform","translate(20 0)translate(0 30)"),("width","20"),("x","0"),("y","0")]
      inConnector2  = [("d","M -22,32 C 0,32 -22,45 27 45"),("fill","none"),("stroke","darkgrey"),("svgName","path"), ("class","v_connector_in")]
      outConnector2  =  [("d","M 82,32 C 60,32 82,45 35 45"),("fill","none"),("stroke","darkgrey"),("svgName","path"),("class","v_connector_out")]
    it "gets correct vbox" do
      resultBox `shouldBe` (firstBox & bboxWidth .~ 60 & bboxHeight .~ 60
              & boxMargins.leftMargin .~ 0
              & boxMargins.rightMargin .~ 0
              & boxPorts.leftPort .~ PTop
              & boxPorts.rightPort .~ PTop)
    it "gets correct svg" do
      resultSVG `shouldBe` Set.fromList <$> [firstSVGBox, inConnector1, outConnector1, secondSVGBox, inConnector2, outConnector2]
    it "print debug" do
      let
        svgXml = TL.toStrict . renderText . move (23,23) $ snd alignBox
      _ <- print svgXml
      pendingWith "it's not a real test but just a debug code"

  describe "test hAlign" do
    let
      leftMargin' = 7
      rightMargin' = 5
      aligmentPadOneSide = 20
      columnWidth = 60
      firstBox = templatedBoundingBox & bboxWidth .~ columnWidth & bboxHeight .~ 10 & boxMargins.leftMargin .~ 17 & boxMargins.rightMargin .~ 13
      firstRect = svgRect $ Rect (0, 0) (60, 10) "black" "none"
      secondBox = templatedBoundingBox & bboxWidth .~ columnWidth - aligmentPadOneSide * 2 & bboxHeight .~ 30 & boxMargins.leftMargin .~ leftMargin' & boxMargins.rightMargin .~ rightMargin'
      secondRect = svgRect $ Rect (0, 0) (20, 30) "black" "none"

      _:(alignedBox2,_):_ = hAlign HCenter [(firstBox, firstRect), (secondBox, secondRect)]
    it "aligns smaller box" do
      alignedBox2 `shouldBe` (secondBox & bboxWidth .~ columnWidth
                                & boxMargins.leftMargin %~ (+ aligmentPadOneSide)
                                & boxMargins.rightMargin %~ (+ aligmentPadOneSide)
                                & boxPorts.leftPort .~ PMiddle
                                & boxPorts.rightPort .~ PMiddle)

  describe "golden test combineAnd" do
    mycontents <- runIO $ B.readFile "test/fixtures/example-and-short.json"
    let
      myinput = eitherDecode mycontents :: Either String (StdinSchema Text)
      (Right myright) = myinput
      questionTree = hardnormal (marking myright) (andOrTree myright)
      (bbox2, svg2) = drawItemFull Full False simpleAndTree
      svgs = renderBS svg2
    it "expands bounding box on Left alignment" do
      goldenBytestring "example-and-short" svgs

  describe "test combineOr" do
    mycontents <- runIO $ B.readFile "test/fixtures/example-or-short.json"
    let
      myinput = eitherDecode mycontents :: Either String (StdinSchema Text)
      (Right myright) = myinput
      questionTree = hardnormal (marking myright) (andOrTree myright)
      --(bbox, svg) = q2svg' c qq
      (bbox2, svg2) = drawItemFull Full False simpleOrTree
      svgs = renderBS svg2
    it "expands bounding box on Left alignment" do
      goldenBytestring "example-or-short" svgs

  describe "drawLeaf" do
    let
      shortTextNode = makeSingleNodeTree "swim"
      longTextNode = makeSingleNodeTree "discombobulate"
      mark = Default ( Right (Just True) )
    it "makes elements of different sizes for Full scale" do
      let
        shortLeaf = fst (execRWS (drawLeafR "swim") (DrawConfig Full True mark (defaultBBox Full) (getScale Full) textBoxLengthFull) (defaultBBox', mempty::SVGElement))
        longLeaf = fst (execRWS (drawLeafR "discombobulate") (DrawConfig Full True mark (defaultBBox Full) (getScale Full) textBoxLengthFull) (defaultBBox', mempty::SVGElement))
        shortBoxLength = shortLeaf ^. _1 . bboxWidth
        longBoxLength = longLeaf ^. _1 . bboxWidth
      (longBoxLength - shortBoxLength) `shouldSatisfy` (> 0)
    it "makes elements of the same size for Tiny scale" do
      let
        shortLeaf = fst (execRWS (drawLeafR "swim") (DrawConfig Tiny True mark (defaultBBox Tiny) (getScale Tiny) textBoxLengthTiny) (defaultBBox', mempty::SVGElement))
        longLeaf = fst (execRWS (drawLeafR "discombobulate") (DrawConfig Tiny True mark (defaultBBox Tiny) (getScale Tiny) textBoxLengthTiny) (defaultBBox', mempty::SVGElement))
        shortBoxLength = shortLeaf ^. _1 . bboxWidth
        longBoxLength = longLeaf ^. _1 . bboxWidth
      (longBoxLength - shortBoxLength) `shouldSatisfy` (== 0)

  describe "getColors Box" do
    it "box colors for (Tiny     True)" do
      let
        (boxStroke, boxFill) = getColorsBox True
      (boxStroke, boxFill) `shouldBe` ("none",   "none")
    it "box colors for (Tiny     False)" do
      let
        (boxStroke, boxFill) = getColorsBox False
      (boxStroke, boxFill) `shouldBe` ("none",   "darkgrey")
    it "box colors for (Small     True)" do
      let
        (boxStroke, boxFill) = getColorsBox True
      (boxStroke, boxFill) `shouldBe` ("none",   "none")
    it "box colors for (Small     False)" do
      let
        (boxStroke, boxFill) = getColorsBox False
      (boxStroke, boxFill) `shouldBe` ("none",   "darkgrey")
    it "box colors for (Full     True)" do
      let
        (boxStroke, boxFill) = getColorsBox True
      (boxStroke, boxFill) `shouldBe` ("none",   "none")
    it "box colors for (Full     False)" do
      let
        (boxStroke, boxFill) = getColorsBox False
      (boxStroke, boxFill) `shouldBe` ("none",   "darkgrey")

  describe "getColors Text" do
    it "Text colors for (Tiny     True)" do
      let
        textFill = getColorsText Tiny True
      textFill `shouldBe` "black"
    it "Text colors for (Tiny     False)" do
      let
        textFill = getColorsText Tiny False
      textFill `shouldBe` "lightgrey"
    it "Text colors for (Small     True)" do
      let
        textFill = getColorsText Small True
      textFill `shouldBe` "black"
    it "Text colors for (Small     False)" do
      let
        textFill = getColorsText Small False
      textFill `shouldBe` "white"
    it "Text colors for (Full     True)" do
      let
        textFill = getColorsText Full True
      textFill `shouldBe` "black"
    it "Text colors for (Full     False)" do
      let
       textFill = getColorsText Full False
      textFill `shouldBe` "white"
