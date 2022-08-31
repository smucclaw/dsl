{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

-- usage:
-- cat out/example-or.json | stack run -- --only svg > out/example4.svg
-- A visualization inspired by Ladder Logic and by Layman Allen (1978).

module AnyAll.SVGLadder (module AnyAll.SVGLadder) where

import Data.List (foldl')

import AnyAll.Types hiding ((<>))

import Data.String
import Graphics.Svg
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Tree
import Debug.Trace
import           Data.Text.Lazy                   (toStrict, Text)
import           Data.Text.Lazy.Builder           (toLazyText)
import           Data.Text.Lazy.Builder.Int
import Text.Printf (formatInteger)
import Control.Monad.Reader
import Lens.Micro.Platform

type Length  = Integer
type SVGElement = Element

intToText :: Integral a => a -> T.Text
intToText = toStrict . toLazyText . decimal

mAInt :: Integral a => a -> a -> T.Text
mAInt x y = T.concat ["M " , intToText x, ",",  intToText y, " "]

cRInt :: Integral a =>  a -> a -> a -> a -> a -> a -> T.Text
cRInt dc1x dc1y dc2x dc2y dx dy = T.concat
  [ "c ", intToText dc1x, ",", intToText dc1y, " ", intToText dc2x
  , ",", intToText dc2y, " ", intToText dx, " ", intToText dy]

cAInt :: Integral a =>  a -> a -> a -> a -> a -> a -> T.Text
cAInt c1x c1y c2x c2y x y = T.concat
  [ "C ", intToText c1x, ",", intToText c1y, " ", intToText c2x, ","
  , intToText c2y, " ", intToText x, " ", intToText y]

translateCommand :: Integral a =>  a -> a -> T.Text
translateCommand x y = T.concat ["translate(", intToText x, " ", intToText y, ")"]

move :: Integral a => (a, a) -> SVGElement -> SVGElement
move (x, y) geoms =
  with geoms [Transform_ <<- translateCommand x y]

data Margins = Margins
  { _leftMargin :: Length,
    _rightMargin :: Length,
    _topMargin :: Length,
    _bottomMargin :: Length
  }
  deriving (Eq, Show)

data BBox = BBox
  { bbw                    :: Length
  , bbh                    :: Length
  , margins                :: Margins
  , ports                  :: Ports
  }
  deriving (Eq, Show)

data BoxDimensions = BoxDimensions
  { boxWidth :: Length
  , boxHeight :: Length
  }
  deriving (Eq, Show)

data PortStyleV = PTop  | PMiddle | PBottom | PVoffset Length deriving (Eq, Show)
data PortStyleH = PLeft | PCenter | PRight  | PHoffset Length deriving (Eq, Show)

data Ports = Ports
  { _leftPort   :: PortStyleV,
    _rightPort  :: PortStyleV,
    _topPort    :: PortStyleH,
    _bottomPort :: PortStyleH
  }
  deriving (Eq, Show)

boxPorts :: Lens' BBox Ports
boxPorts = lens ports (\x y -> x { ports = y })

boxMargins :: Lens' BBox Margins
boxMargins = lens margins (\x y -> x { margins = y })

makeLenses ''Ports

makeLenses ''Margins

bTopMargin :: Lens' BBox Length
bTopMargin = boxMargins . topMargin

type BoxedSVG = (BBox, SVGElement)

type Question = Q T.Text
type QuestionTree = Tree Question

defaultBBox :: Scale -> BBox
defaultBBox Tiny  = defaultBBox' {ports = defaultPorts { _leftPort = PMiddle, _rightPort = PMiddle } }
defaultBBox Small = defaultBBox Tiny
defaultBBox Full  = defaultBBox'

defaultBBox' :: BBox
defaultBBox' = BBox
  { bbw = 0
  , bbh = 0
  , margins = defaultMargins
  , ports = defaultPorts
  }

defaultPorts :: Ports
defaultPorts = Ports
  { _leftPort = PTop
  , _rightPort = PTop
  , _topPort = PCenter
  , _bottomPort = PCenter
  }

defaultMargins :: Margins
defaultMargins = Margins
  { _leftMargin = 0
  , _rightMargin = 0
  , _topMargin = 0
  , _bottomMargin = 0
  }


portL, portT, portR, portB :: BBox -> AAVScale -> Length
portL bb = portLR (bb ^. boxPorts.leftPort) bb
portR bb = portLR (bb ^. boxPorts.rightPort) bb
portT bb = portTB (bb ^. boxPorts.topPort) bb
portB bb = portTB (bb ^. boxPorts.bottomPort) bb

portLR :: PortStyleV -> BBox -> AAVScale -> Length
portLR PTop    bb s = bb ^. bTopMargin +                                    sbh s `div` 2
portLR PMiddle bb s = bb ^. bTopMargin + (bbh bb - bb ^. bTopMargin - bb ^. boxMargins.bottomMargin)  `div` 2
portLR PBottom bb s =           (bbh bb           - bb ^. boxMargins.bottomMargin)     - sbh s  `div` 2
portLR (PVoffset x) bb s = bb ^. bTopMargin + x

portTB :: PortStyleH -> BBox -> AAVScale -> Length
portTB PLeft   bb s = bb ^. boxMargins.leftMargin +                                    stbv s
portTB PCenter bb s = bb ^. boxMargins.leftMargin + (bbw bb - bb ^. boxMargins.leftMargin - bb ^. boxMargins.rightMargin)  `div` 2
portTB PRight  bb s =           (bbw bb           - bb ^. boxMargins.rightMargin)     - stbv s
portTB (PHoffset x) bb s = bb ^. boxMargins.leftMargin + x

-- | how compact should the output be?
data Scale = Tiny  -- @ ---o---
           | Small -- @ --- [1.1] ---
           | Full  -- @ --- the first item ---
           deriving (Show, Eq)

-- | how is a particular widget to be laid out?
data Direction = LR -- ^ left-to-right
               | TB -- ^ top-to-bottom
               deriving (Show, Eq)

-- AAV = AnyAll Visualization
data AAVConfig = AAVConfig
  { cscale       :: Scale
  , cdirection   :: Direction
  , cgetMark     :: Marking T.Text
  , cdebug       :: Bool
  }
  deriving (Show, Eq)

defaultAAVConfig :: AAVConfig
defaultAAVConfig = AAVConfig
  { cscale = Tiny
  , cdirection = LR
  , cgetMark = Marking Map.empty
  , cdebug = False
  }

data AAVScale = AAVScale
  { sbw :: Length  -- ^ box width
  , sbh :: Length -- ^ box height
  , slm :: Length  -- ^ left margin
  , stm :: Length  -- ^ top margin
  , srm :: Length  -- ^ right margin
  , sbm :: Length  -- ^ bottom margin
  , slrv :: Length  -- ^ LR: vertical   gap between elements
  , slrh :: Length  -- ^ LR: horizontal gap between elements
  , stbv :: Length  -- ^ TB: vertical   gap between elements
  , stbh :: Length  -- ^ TB: horizontal gap between elements
  } deriving (Show, Eq)

getScale :: Scale -> AAVScale -- sbw sbh slm stm srm sbm slrv slrh stbv stbh
getScale Full      = AAVScale    120  44  22  20  22  20  10   10    10   10
getScale Small     = AAVScale     44  30  11  14  11  14   7    7     7    7
getScale Tiny      = AAVScale      8   8   6  10   6  10   5    5     5    5

--                              (boxStroke, boxFill,     textFill
getColorsBox :: Scale -> Bool ->   (T.Text,   T.Text)
getColorsBox    _        True    = ("none",   "none")
getColorsBox    _        False   = ("none",   "lightgrey")

getColorsText :: Scale -> Bool ->    T.Text
getColorsText    Tiny     False   = "lightgrey"
getColorsText    _        True    = "black"
getColorsText    _        False   = "white"

getBoxColorsR :: DrawConfigM (T.Text, T.Text)
getBoxColorsR = do
  m <- asks markingR
  sc <- asks myScale
  return $ getColorsBox sc (confidence m)


getTextColorsR :: DrawConfigM T.Text
getTextColorsR = do
  m <- asks markingR
  sc <- asks myScale
  return $ getColorsText sc (confidence m)

type ItemStyle = Maybe Bool

(<<-*) :: Show a => AttrTag -> a -> Attribute
(<<-*) tag a = bindAttr tag (T.pack (show a))

infix 4 <<-*

makeSvg' :: AAVConfig -> BoxedSVG -> SVGElement
makeSvg' c = makeSvg

makeSvg :: BoxedSVG -> SVGElement
makeSvg (box, geom) =
     doctype
  <> with (svg11_ (move (23,23) geom)) [Version_ <<- "1.1", Width_ <<-* 23 + bbw box + _leftMargin (margins box) + _rightMargin (margins box), Height_ <<-* 23 + bbh box + _topMargin (margins box) + _bottomMargin (margins box)]

data LineHeight = NoLine | HalfLine | FullLine
  deriving (Eq, Show)

q2svg :: AAVConfig -> QTree T.Text -> SVGElement
q2svg c qt = snd $ q2svg' c qt

q2svg' :: AAVConfig -> QTree T.Text -> BoxedSVG
q2svg' c qt@(Node q childqs) = drawItem (cscale c) False qt

drawItem :: Scale -> Bool -> QTree T.Text -> BoxedSVG
drawItem sc negContext qt
  | sc == Tiny = drawItemTiny sc negContext qt
  | otherwise        = drawItemFull sc negContext qt

data HAlignment = HLeft | HCenter | HRight   deriving (Eq, Show)
data VAlignment = VTop  | VMiddle | VBottom  deriving (Eq, Show)

drawItemTiny :: Scale -> Bool -> QTree T.Text -> BoxedSVG
drawItemTiny sc negContext (Node qt@(Q _sv ao@(Simply txt) pp m) childqs) =  runReader (drawLeafR txt) $ DrawConfig sc negContext m
drawItemTiny sc negContext qt@(Node (Q _sv ao@(Neg)         pp m) childqs) = drawItemTiny sc (not negContext) (head childqs)
drawItemTiny sc negContext qt                                              = drawItemFull sc      negContext   qt      -- [TODO]

alignV :: VAlignment -> Length -> BoxedSVG -> BoxedSVG
alignV alignment maxHeight (box, el) = (adjustMargins box {bbh = maxHeight}, moveElement el)
  where
    boxHeight = bbh box
    alignmentPad = maxHeight - boxHeight
    adjustMargins = adjustBoxMargins alignment alignmentPad
    moveElement = alignVCalcElement alignment alignmentPad

adjustBoxMargins :: VAlignment -> Length -> BBox -> BBox
adjustBoxMargins alignment alignmentPad bx =
  bx
    & boxMargins.topMargin    %~ (+ topPadding)
    & boxMargins.bottomMargin %~ (+ bottomPadding)
  where
    (topPadding, bottomPadding) = columnAlignMargins alignment alignmentPad

columnAlignMargins :: VAlignment -> Length -> (Length, Length)
columnAlignMargins VMiddle alignmentPad = (alignmentPad  `div` 2, alignmentPad  `div` 2)
columnAlignMargins VTop    alignmentPad = (0, alignmentPad)
columnAlignMargins VBottom alignmentPad = (alignmentPad, 0)

alignVCalcElement :: VAlignment -> Length -> (SVGElement -> SVGElement)
alignVCalcElement VMiddle alignmentPad = move (0, alignmentPad `div` 2)
alignVCalcElement VTop    alignmentPad = id
alignVCalcElement VBottom alignmentPad = move (0, alignmentPad)

alignH :: HAlignment -> Length -> BoxedSVG -> BoxedSVG
alignH alignment maxWidth (bb, x) = (adjustMargins bb {bbw = maxWidth}, moveElement x)
  where
    boxWidth = bbw bb
    alignmentPad = maxWidth - boxWidth
    adjustMargins = adjustSideMargins alignment alignmentPad
    moveElement = alignHCalcMove alignment alignmentPad

adjustSideMargins :: HAlignment -> Length -> BBox -> BBox
adjustSideMargins alignment alignmentPad box =
  box
    & boxMargins.leftMargin  %~ (+ newLeftMargin)
    & boxMargins.rightMargin %~ (+ newRightMargin)
  where
    (newLeftMargin, newRightMargin) = rowAlignMargins alignment alignmentPad

rowAlignMargins :: HAlignment -> Length -> (Length, Length)
rowAlignMargins HCenter alignmentPad = (alignmentPad  `div` 2, alignmentPad  `div` 2)
rowAlignMargins HLeft alignmentPad = (0, alignmentPad)
rowAlignMargins HRight alignmentPad = (alignmentPad, 0)

alignHCalcMove :: HAlignment -> Length -> (SVGElement -> SVGElement)
alignHCalcMove HCenter alignmentPad = move ( alignmentPad `div` 2, 0)
alignHCalcMove HLeft alignmentPad = id
alignHCalcMove HRight alignmentPad = move (alignmentPad, 0)

-- | see page 2 of the "box model" documentation.
-- | if we used the diagrams package all of this would be calculated automatically for us.
vAlign :: VAlignment -> [BoxedSVG] -> [BoxedSVG]
vAlign alignment elems = alignV alignment mx <$> elems
  where mx = maximum $ bbh . fst <$> elems

hAlign :: HAlignment -> [BoxedSVG] -> [BoxedSVG]
hAlign alignment elems = alignH alignment mx <$> elems
  where mx = maximum $ bbw . fst <$> elems

rowLayouter :: Scale -> BoxedSVG -> BoxedSVG -> BoxedSVG
rowLayouter sc (bbold, old) (bbnew, new) =
  ( templateBox
      & boxPorts.leftPort  .~ PVoffset (portL bbold myScale)
      & boxPorts.rightPort .~ PVoffset (portR bbnew myScale)
      & boxMargins.leftMargin .~ (bbold ^. boxMargins.leftMargin)
      & boxMargins.rightMargin .~ (bbnew ^. boxMargins.rightMargin)
  ,
    old
      <> move (newBoxStart, 0) new
      <> connectingCurve
  )
  where
    templateBox = (defaultBBox sc)
      { bbh = max (bbh bbold) (bbh bbnew)
      , bbw = bbw bbold + lrHgap + bbw bbnew
      }
    myScale = getScale sc
    lrHgap = slrh myScale
    newBoxStart = bbw bbold + lrHgap
    connectingCurve =
      if bbw bbold /= 0
        then svgConnector $ rowConnectorData sc bbold bbnew
        else mempty

data Dot = Dot {x::Length, y::Length}
data Curve = Curve {start::Dot, startGuide::Dot, endGuide::Dot, end::Dot}

rowConnectorData :: Scale -> BBox -> BBox -> Curve
rowConnectorData sc bbold bbnew =
  Curve
    { start = Dot {x = bbw bbold - rightMargin', y = startPortY},
      startGuide = Dot {x = endPortX `div` 2, y = 0},
      endGuide = Dot {x = endPortX `div` 2, y = endPortY},
      end = Dot {x = endPortX, y = endPortY}
    }
  where
    myScale = getScale sc
    gap = slrh myScale
    rightMargin' = bbold ^. boxMargins.rightMargin
    startPortY = portR bbold myScale
    endPortY = portL bbnew myScale - startPortY
    endPortX = rightMargin' + gap + bbnew ^. boxMargins.leftMargin

svgConnector :: Curve -> SVGElement
svgConnector
  Curve
    { start = Dot {x = sx, y = sy},
      startGuide = Dot {x = sgx, y = sgy},
      endGuide = Dot {x = egx, y = egy},
      end = Dot {x = ex, y = ey}
    } =
    path_ [D_ <<- curveMoveCommand <> curveBezierCommand, Stroke_ <<- "green", Fill_ <<- "none", Class_ <<- "h_connector"]
    where
      curveMoveCommand = mAInt sx sy
      curveBezierCommand = cRInt sgx 0 egx  egy  ex ey

columnLayouter :: Scale -> BBox -> BoxedSVG -> BoxedSVG -> BoxedSVG
columnLayouter sc parentbbox (bbold, old) (bbnew, new) = (bbox, svg)
  where
    parentPortIn = portL parentbbox myScale + lrVgap
    parentPortOut = portR parentbbox myScale + lrVgap
    myScale = getScale sc
    lrVgap = slrv myScale
    inboundConnector = inboundCurve sc parentbbox bbold bbnew
    outboundConnector = outboundCurve sc parentbbox bbold bbnew
    bbox =
      (defaultBBox sc)
        { bbh = bbh bbold + bbh bbnew + lrVgap,
          bbw = max (bbw bbold) (bbw bbnew)
        }
    svg =
      old
        <> move (0, bbh bbold + lrVgap) new
        <> inboundConnector
        <> outboundConnector

inboundCurve :: Scale -> BBox -> BBox -> BBox -> SVGElement
inboundCurve sc parentbbox bbold bbnew =
  path_ ((D_ <<- startPosition <> bezierCurve) : (Class_ <<- "v_connector_in") : pathcolors)
  where
    parentPortIn = portL parentbbox myScale + lrVgap
    pathcolors = [Stroke_ <<- "green", Fill_ <<- "none"]
    myScale = getScale sc
    lrVgap = slrv myScale
    leftMargin' = slm myScale
    startPosition = mAInt (- leftMargin') parentPortIn
    bezierCurve =
      cAInt
        0
        parentPortIn
        (-leftMargin')
        (bbh bbold + lrVgap + portL bbnew myScale)
        (bbnew ^. boxMargins.leftMargin)
        (bbh bbold + lrVgap + portL bbnew myScale)

outboundCurve :: Scale -> BBox -> BBox -> BBox -> SVGElement
outboundCurve sc parentbbox bbold bbnew =
  path_ ((D_ <<- startPosition <> bezierCurve) : (Class_ <<- "v_connector_out") : pathcolors)
  where
    parentPortOut = portR parentbbox myScale + lrVgap
    pathcolors = [Stroke_ <<- "green", Fill_ <<- "none"]
    myScale = getScale sc
    lrVgap = slrv myScale
    rightMargin' = srm myScale
    startPosition = mAInt (bbw parentbbox + rightMargin') parentPortOut
    bezierCurve =
      cAInt
        (bbw parentbbox)
        parentPortOut
        (bbw parentbbox + rightMargin')
        (bbh bbold + lrVgap + portR bbnew myScale)
        (bbw parentbbox - (bbnew ^. boxMargins.rightMargin))
        (bbh bbold + lrVgap + portR bbnew myScale)

combineOr :: Scale -> [BoxedSVG] -> BoxedSVG
combineOr sc elems =
  ( childbbox
      { bbw = bbw childbbox + leftMargin + rightMargin,
        bbh = bbh childbbox - interElementGap
      },
    move (leftMargin, - interElementGap) children
  )
  where
    myScale = getScale sc
    interElementGap = slrv myScale
    leftMargin = slm myScale
    rightMargin = srm myScale
    childheights = interElementGap * fromIntegral (length elems - 1) + sum (bbh . fst <$> elems)
    mybbox = (defaultBBox sc) {bbh = childheights, bbw = maximum (bbw . fst <$> elems)}
    addElementToColumn = columnLayouter sc mybbox
    (childbbox, children) = foldl' addElementToColumn (defaultBBox sc, mempty) $ hAlign HCenter elems


combineAnd :: Scale -> [BoxedSVG] -> BoxedSVG
combineAnd sc elems =
  ( combinedBox
      & boxPorts.leftPort  .~ PVoffset (portL childbbox myScale)
      & boxPorts.rightPort .~ PVoffset (portR childbbox myScale)
      & boxMargins.leftMargin %~ (+ leftMargin')
      & boxMargins.rightMargin %~ (+ rightMargin')
  ,
    move (leftMargin', 0) children
  )
  where
    myScale = getScale sc
    leftMargin' = slm myScale
    rightMargin' = srm myScale
    addElementToRow = rowLayouter sc
    (childbbox, children) = foldl1 addElementToRow $ vAlign VTop elems
    combinedBox = childbbox { bbw = bbw childbbox + leftMargin' + rightMargin'}

drawPreLabelTop :: Scale -> T.Text -> BoxedSVG -> BoxedSVG
drawPreLabelTop sc label (childBox, childSVG) =
    (labeledBox
      & boxMargins.topMargin %~ (+ labelHeight)
    ,
    move (0, labelHeight) childSVG <> svgLabel)
  where
    labeledBox = childBox { bbh = bbh childBox + labelHeight }
    labelHeight = stm (getScale sc)
    lbox = labelBox sc "hanging" label
    (_,svgLabel) = alignH HCenter (bbw labeledBox) lbox

drawPrePostLabelTopBottom :: Scale -> T.Text -> T.Text -> BoxedSVG -> BoxedSVG
drawPrePostLabelTopBottom sc preTxt postTxt (childBox, childSVG) =
    (labeledBox
      & boxMargins.topMargin %~ (+ labelHeight)
      & boxMargins.bottomMargin %~ (+ labelHeight)
    ,
    move (0, labelHeight) childSVG <> svgPreLabel <>  move (0, bbh childBox + 2 * labelHeight) svgPostLabel)
  where
    labeledBox = childBox { bbh = bbh childBox + 2 * labelHeight }
    labelHeight = stm (getScale sc)
    prelbox = labelBox sc "hanging" preTxt
    (_,svgPreLabel) = alignH HCenter (bbw labeledBox) prelbox
    postlbox = labelBox sc "ideographic" postTxt
    (_,svgPostLabel) = alignH HCenter (bbw labeledBox) postlbox

labelBox :: Scale -> T.Text -> T.Text -> BoxedSVG
labelBox sc baseline mytext =
  (,)
  (defaultBBox sc) { bbw = boxWidth, bbh = boxHeight }
  boxContent
  where
    boxHeight        = sbh (getScale sc)
    defBoxWidth      = sbw (getScale sc)
    boxWidth         = defBoxWidth - 15 + (3 * fromIntegral (T.length mytext))
    boxContent = text_ [ X_  <<-* (boxWidth `div` 2), Text_anchor_ <<- "middle", Dominant_baseline_ <<- baseline] (toElement mytext)

decorateWithLabel :: Scale -> Maybe (Label T.Text) -> BoxedSVG -> BoxedSVG
decorateWithLabel sc pp childBox =
  case (sc, pp) of
    (Tiny, _ ) -> childBox
    (_, Nothing) -> childBox
    (_, Just (Pre txt)) -> drawPreLabelTop sc txt childBox
    (_, Just (PrePost preTxt postTxt)) -> drawPrePostLabelTopBottom sc preTxt postTxt childBox

drawItemFull :: Scale -> Bool -> QuestionTree -> BoxedSVG
drawItemFull sc negContext (Node (Q sv ao pp m) childqs) =
  case ao of
    Or -> decorateWithLabel sc pp (combineOr sc rawChildren)
    And -> decorateWithLabel sc pp  (combineAnd sc rawChildren)
    Simply txt -> runReader (drawLeafR txt) contextR
    Neg -> drawItemFull sc (not negContext) (head childqs)
  where
    contextR = DrawConfig sc negContext m
    rawChildren = drawItemFull sc negContext <$> childqs

deriveBoxCap :: Bool -> Default Bool -> (LineHeight, LineHeight, LineHeight)
deriveBoxCap negContext m =
  case extractSoft m of
    (Just True) -> (HalfLine, notLine HalfLine, topLine (not negContext))
    (Just False) -> (FullLine, notLine NoLine, topLine negContext)
    Nothing -> (NoLine, notLine NoLine, NoLine)
  where
    notLine = if negContext then const FullLine else id
    topLine = \nc -> if nc then FullLine else NoLine

extractSoft :: Default Bool -> Maybe Bool
extractSoft (Default (Right b)) = b
extractSoft (Default (Left b)) = b

deriveConfidence :: Default a -> Bool
deriveConfidence (Default (Right _)) = True
deriveConfidence (Default (Left _)) = False

confidence :: Default Bool -> Bool
confidence (Default (Right _)) = True
confidence (Default (Left _)) = False

drawBoxCapR :: T.Text -> DrawConfigM SVGElement
drawBoxCapR caption = do
  negContext <- asks negContext
  m <- asks markingR
  drawBoxCap negContext m <$> deriveBoxSize caption

drawBoxContentR :: T.Text -> DrawConfigM SVGElement
drawBoxContentR caption = do
  sc <- asks myScale
  textFill <- getTextColorsR
  drawBoxContent sc caption textFill <$> deriveBoxSize caption

drawBoxCap :: Bool -> Default Bool -> BoxDimensions -> SVGElement
drawBoxCap negContext m BoxDimensions{boxWidth=bw, boxHeight=bh} =
  leftLineSVG <> rightLineSVG <> topLineSVG
  where
    (leftline, rightline, topline) = deriveBoxCap negContext m
    leftLineSVG = drawVerticalLine 0 bh leftline "leftline"
    rightLineSVG = drawVerticalLine bw bh rightline "rightline"
    topLineSVG = drawHorizontalLine 0 bw topline "topline"

drawVerticalLine :: Length -> Length -> LineHeight -> T.Text -> SVGElement
drawVerticalLine xPosition length lineType linePosition =
  case lineType of
    FullLine -> renderVerticalLine xPosition length (T.append linePosition ".full")
    HalfLine -> renderVerticalLine xPosition (length `div` 2) (T.append linePosition ".half")
    NoLine -> mempty

renderVerticalLine :: Length -> Length -> T.Text -> SVGElement
renderVerticalLine xPosition length lineClass =
  line_
    [ X1_ <<-* xPosition, Y1_ <<-* 0,
      X2_ <<-* xPosition, Y2_ <<-* length,
      Stroke_ <<- "black",
      Class_ <<- lineClass
    ]

drawHorizontalLine :: Length -> Length -> LineHeight -> T.Text -> SVGElement
drawHorizontalLine yPosition length lineType linePosition =
  case lineType of
    FullLine -> renderHorizontalLine yPosition length (T.append linePosition ".full")
    HalfLine -> renderHorizontalLine yPosition (length `div` 2) (T.append linePosition ".half")
    NoLine -> mempty

renderHorizontalLine :: Length -> Length -> T.Text -> SVGElement
renderHorizontalLine yPosition length lineClass =
  line_
    [ X1_ <<-* 0,      Y1_ <<-* yPosition,
      X2_ <<-* length, Y2_ <<-* yPosition,
      Stroke_ <<- "black",
      Class_ <<- lineClass
    ]

drawBoxContent :: Scale -> T.Text -> T.Text -> BoxDimensions -> SVGElement
drawBoxContent Tiny _ textFill BoxDimensions{boxWidth=boxWidth, boxHeight=boxHeight} =
  circle_ [Cx_  <<-* (boxWidth `div` 2) ,Cy_      <<-* (boxHeight `div` 2) , R_ <<-* (boxWidth `div` 3), Fill_ <<- textFill ]
drawBoxContent _ mytext textFill BoxDimensions{boxWidth=boxWidth, boxHeight=boxHeight} =
  text_ [ X_  <<-* (boxWidth `div` 2) , Y_      <<-* (boxHeight `div` 2) , Text_anchor_ <<- "middle" , Dominant_baseline_ <<- "central" , Fill_ <<- textFill ] (toElement mytext)

data DrawConfig = DrawConfig{
    myScale :: Scale,
    negContext :: Bool,
    markingR :: Default Bool
  }

type DrawConfigM = Reader DrawConfig

deriveBoxSize :: T.Text -> DrawConfigM BoxDimensions
deriveBoxSize caption = do
  sc <- asks myScale
  let
      boxHeight = sbh (getScale sc)
      defBoxWidth = sbw (getScale sc)
      boxWidth = if sc == Tiny then defBoxWidth else defBoxWidth - 15 + (3 * fromIntegral (T.length caption))
  return BoxDimensions{boxWidth=boxWidth, boxHeight=boxHeight}

drawLeafR :: T.Text -> DrawConfigM BoxedSVG
drawLeafR caption = do
  sc <- asks myScale
  (boxStroke, boxFill) <- getBoxColorsR
  BoxDimensions{boxWidth=boxWidth, boxHeight=boxHeight} <- deriveBoxSize caption
  boxContent <- drawBoxContentR caption
  boxCap <- drawBoxCapR caption
  return $
    (,)
      (defaultBBox sc) {bbw = boxWidth, bbh = boxHeight}
      ( rect_ [X_ <<-* 0, Y_ <<-* 0, Width_ <<-* boxWidth, Height_ <<-* boxHeight, Stroke_ <<- boxStroke, Fill_ <<- boxFill, Class_ <<- "textbox"]
          <> boxContent
          <> boxCap
      )

box :: AAVConfig -> Double -> Double -> Double -> Double -> SVGElement
box c x y w h =
  rect_ [ X_ <<-* x, Y_ <<-* y, Width_ <<-* w, Height_ <<-* h
        , Fill_ <<- "none", Stroke_ <<- "black" ]

debugBox :: T.Text -> Length -> Length -> SVGElement
debugBox m w h =
  rect_ [ X_ <<-* 0, Y_ <<-* 0, Width_ <<-* w, Height_ <<-* h
        , Fill_ <<- "none", Stroke_ <<- "black", Class_ <<- m]
