{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

-- usage:
-- cat out/example-or.json | stack run -- --only svg > out/example4.svg
-- A visualization inspired by Ladder Logic and by Layman Allen (1978).

module AnyAll.SVGLadder (module AnyAll.SVGLadder) where

import Data.List (foldl', sortBy)
import Data.Function  (on)

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
import Lens.Micro.Platform
import Control.Monad.RWS

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
  { dimensions :: BoxDimensions
  , margins    :: Margins
  , ports      :: Ports
  , connect    :: Connect
  }
  deriving (Eq, Show)

-- similar to a Default, but looking at the overall value of a connection
type Connect = Maybe Bool

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

boxDims :: Lens' BBox BoxDimensions
boxDims = lens dimensions (\x y -> x { dimensions = y })

boxConnect :: Lens' BBox Connect
boxConnect = lens connect (\x y -> x { connect = y })

dimWidth :: Lens' BoxDimensions Length
dimWidth = lens boxWidth (\x y -> x { boxWidth = y })

dimHeight :: Lens' BoxDimensions Length
dimHeight = lens boxHeight (\x y -> x { boxHeight = y })

makeLenses ''Ports

makeLenses ''Margins

bTopMargin :: Lens' BBox Length
bTopMargin = boxMargins . topMargin

bboxWidth :: Lens' BBox Length
bboxWidth = boxDims . dimWidth

bboxHeight :: Lens' BBox Length
bboxHeight = boxDims . dimHeight

type BoxedSVG = (BBox, SVGElement)

type Question = Q T.Text
type QuestionTree = Tree Question

defaultBBox :: Scale -> BBox
defaultBBox Tiny  = defaultBBox' {ports = defaultPorts { _leftPort = PMiddle, _rightPort = PMiddle } }
defaultBBox Small = defaultBBox Tiny
defaultBBox Full  = defaultBBox'

defaultBBox' :: BBox
defaultBBox' = BBox
  { dimensions = defaultDimensions
  , margins    = defaultMargins
  , ports      = defaultPorts
  , connect    = defaultConnect
  }

defaultDimensions = BoxDimensions
  { boxWidth = 0
  , boxHeight = 0
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

-- | Does a bounding box connect?
-- This is similar to the idea of a marking, but after Not-processing;
-- so we only really care if the overall value, of the Marking * Leaf, is true;
-- if the leaf is a Not, then we look for Default Left False or Default Right False;
-- if the leaf is not a Not, then we look for either Default Left True or Default Right True.
-- if the contents of the bounding box are an Any or an All we evaluate over all of the children.
-- This is used for display purposes; a bounding box has an overall connect value as well.
defaultConnect :: Connect
defaultConnect = Nothing


-- default stroke colour for the connecting lines between boxes
defaultStroke_ :: T.Text
defaultStroke_ = "darkgrey"

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

data GapDimensions = GapDimensions
  { _gapVertical :: Length
  , _gapHorizontal :: Length
  }
  deriving (Eq, Show)

makeLenses ''GapDimensions

data AAVScale = AAVScale
  { scaleDims :: BoxDimensions
  , scaleMargins :: Margins
  , horizontalLayout :: GapDimensions
  , verticalLayout :: GapDimensions
  } deriving (Show, Eq)

aavscaleDims :: Lens' AAVScale BoxDimensions
aavscaleDims = lens scaleDims (\x y -> x { scaleDims = y })

aavscaleMargins :: Lens' AAVScale Margins
aavscaleMargins = lens scaleMargins (\x y -> x { scaleMargins = y })

aavscaleHorizontalLayout :: Lens' AAVScale GapDimensions
aavscaleHorizontalLayout = lens horizontalLayout (\x y -> x { horizontalLayout = y })

aavscaleVerticalLayout :: Lens' AAVScale GapDimensions
aavscaleVerticalLayout = lens verticalLayout (\x y -> x { verticalLayout = y })

getScale :: Scale -> AAVScale --                 sbw sbh slm srm stm sbm slrv slrh stbv stbh
getScale Full      = AAVScale    (BoxDimensions 120  44)  (Margins 22  22  20  20)  (GapDimensions 10   10)    (GapDimensions 10   10)
getScale Small     = AAVScale    (BoxDimensions  44  30)  (Margins 11  11  14  14)  (GapDimensions 7    7 )    (GapDimensions 10   10)
getScale Tiny      = AAVScale    (BoxDimensions   8   8)  (Margins 6   6  10  10)   (GapDimensions 5    5 )    (GapDimensions 10   10)

portL, portT, portR, portB :: BBox -> AAVScale -> Length
portL bb = portLR (bb ^. boxPorts.leftPort) bb
portR bb = portLR (bb ^. boxPorts.rightPort) bb
portT bb = portTB (bb ^. boxPorts.topPort) bb
portB bb = portTB (bb ^. boxPorts.bottomPort) bb

portLR :: PortStyleV -> BBox -> AAVScale -> Length
portLR PTop    bb s = bb ^. bTopMargin +                                   s ^. aavscaleDims.dimHeight `div` 2
portLR PMiddle bb s = bb ^. bTopMargin + (bb ^. bboxHeight - bb ^. bTopMargin - bb ^. boxMargins.bottomMargin)  `div` 2
portLR PBottom bb s =           (bb ^. bboxHeight           - bb ^. boxMargins.bottomMargin)     -  s ^. aavscaleDims.dimHeight  `div` 2
portLR (PVoffset x) bb s = bb ^. bTopMargin + x

portTB :: PortStyleH -> BBox -> AAVScale -> Length
portTB PLeft   bb s = bb ^. boxMargins.leftMargin +                                   s ^. aavscaleVerticalLayout.gapVertical
portTB PCenter bb s = bb ^. boxMargins.leftMargin + (bb ^. bboxWidth - bb ^. boxMargins.leftMargin - bb ^. boxMargins.rightMargin)  `div` 2
portTB PRight  bb s =           (bb ^. bboxWidth           - bb ^. boxMargins.rightMargin)     - s ^. aavscaleVerticalLayout.gapVertical
portTB (PHoffset x) bb s = bb ^. boxMargins.leftMargin + x

--                              (boxStroke, boxFill,     textFill
getColorsBox :: Bool ->   (T.Text,   T.Text)
getColorsBox True    = ("none",   "none")
getColorsBox False   = ("none",   "darkgrey")

getColorsText :: Scale -> Bool ->    T.Text
getColorsText    Tiny     False   = "lightgrey"
getColorsText    _        True    = "black"
getColorsText    _        False   = "white"

getBoxColorsR :: SVGCanvas (T.Text, T.Text)
getBoxColorsR = do
  m <- asks markingR
  return $ getColorsBox (confidence m)


getTextColorsR :: SVGCanvas T.Text
getTextColorsR = do
  m <- asks markingR
  sc <- asks contextScale
  return $ getColorsText sc (confidence m)

type ItemStyle = Maybe Bool

(<<-*) :: Show a => AttrTag -> a -> Attribute
(<<-*) tag a = bindAttr tag (T.pack (show a))

infix 4 <<-*

makeSvg' :: AAVConfig -> BoxedSVG -> SVGElement
makeSvg' c = makeSvg

mysteryWidth :: BBox -> Length
mysteryWidth box = box ^. bboxWidth + box ^. boxMargins.leftMargin + box ^. boxMargins.rightMargin

mysteryHeight :: BBox -> Length
mysteryHeight box = box ^. bboxHeight + box ^. boxMargins.topMargin + box ^. boxMargins.bottomMargin

makeSvg :: BoxedSVG -> SVGElement
makeSvg (box, geom) =
     doctype
  <> with (svg11_ (move (23,23) geom)) [Version_ <<- "1.1", Width_ <<-* 23 + mysteryWidth box, Height_ <<-* 23 + mysteryHeight box]

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
drawItemTiny sc negContext (Node (Q _sv (Simply txt) pp m) childqs) = fst (execRWS (drawLeafR txt) (DrawConfig sc negContext m (defaultBBox sc) (getScale sc) (if sc == Tiny then textBoxLengthTiny else textBoxLengthFull)) (defaultBBox', mempty::SVGElement))
drawItemTiny sc negContext (Node (Q _sv Neg         pp m) childqs)  = drawItemTiny sc (not negContext) (head childqs)
drawItemTiny sc negContext qt                                       = drawItemFull sc      negContext   qt      -- [TODO]

alignV :: VAlignment -> Length -> BoxedSVG -> BoxedSVG
alignV alignment maxHeight (box, el) = (adjustMargins (box & bboxHeight .~ maxHeight), moveElement el)
  where
    boxHeight = box ^. bboxHeight
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
alignH alignment maxWidth (bb, x) = (adjustMargins (bb & boxDims.dimWidth .~ maxWidth), moveElement x)
  where
    boxWidth = bb ^. bboxWidth
    alignmentPad = maxWidth - boxWidth
    adjustMargins = adjustSideMargins alignment alignmentPad
    moveElement = alignHCalcMove alignment alignmentPad

adjustSideMargins :: HAlignment -> Length -> BBox -> BBox
adjustSideMargins alignment alignmentPad box =
  box
    & boxMargins.leftMargin  %~ (+ leftPadding)
    & boxMargins.rightMargin %~ (+ rightPadding)
  where
    (leftPadding, rightPadding) = rowAlignMargins alignment alignmentPad

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
  where mx = maximum $ boxHeight . dimensions . fst <$> elems

hAlign :: HAlignment -> [BoxedSVG] -> [BoxedSVG]
hAlign alignment elems = alignH alignment mx <$> elems
  where mx = maximum $ boxWidth . dimensions . fst <$> elems

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
        & boxDims.dimHeight .~ max (bbold ^. bboxHeight) (bbnew ^. bboxHeight)
        & boxDims.dimWidth .~ bbold ^. bboxWidth + lrHgap + bbnew ^. bboxWidth
    myScale = getScale sc
    lrHgap = myScale ^. aavscaleHorizontalLayout.gapHorizontal
    newBoxStart = bbold ^. bboxWidth + lrHgap
    connectingCurve =
      if bbold ^. bboxWidth /= 0
        then svgConnector $ rowConnectorData sc bbold bbnew
        else mempty

data Dot = Dot {xPos::Length, yPos::Length}
data Curve = Curve {start::Dot, startGuide::Dot, endGuide::Dot, end::Dot}

rowConnectorData :: Scale -> BBox -> BBox -> Curve
rowConnectorData sc bbold bbnew =
  Curve
    { start = Dot {xPos = bbold ^. bboxWidth - rightMargin', yPos = startPortY},
      startGuide = Dot {xPos = endPortX `div` 2, yPos = 0},
      endGuide = Dot {xPos = endPortX `div` 2, yPos = endPortY},
      end = Dot {xPos = endPortX, yPos = endPortY}
    }
  where
    myScale = getScale sc
    gap = myScale ^. aavscaleHorizontalLayout.gapHorizontal
    rightMargin' = bbold ^. boxMargins.rightMargin
    startPortY = portR bbold myScale
    endPortY = portL bbnew myScale - startPortY
    endPortX = rightMargin' + gap + bbnew ^. boxMargins.leftMargin

svgConnector :: Curve -> SVGElement
svgConnector
  Curve
    { start = Dot {xPos = sx, yPos = sy},
      startGuide = Dot {xPos = sgx, yPos = sgy},
      endGuide = Dot {xPos = egx, yPos = egy},
      end = Dot {xPos = ex, yPos = ey}
    } =
    path_ [D_ <<- curveMoveCommand <> curveBezierCommand
          , Stroke_ <<- defaultStroke_
          , Fill_   <<- "none"
          , Class_  <<- "h_connector"]
    where
      curveMoveCommand = mAInt sx sy
      curveBezierCommand = cRInt sgx 0 egx  egy  ex ey

columnLayouter :: Scale -> BBox -> BoxedSVG -> BoxedSVG -> BoxedSVG
columnLayouter sc parentbbox (bbold, old) (bbnew, new) = (bbox, svg)
  where
    parentPortIn = portL parentbbox myScale + lrVgap
    parentPortOut = portR parentbbox myScale + lrVgap
    myScale = getScale sc
    lrVgap = myScale ^. aavscaleHorizontalLayout.gapVertical
    inboundConnector = inboundCurve sc parentbbox bbold bbnew
    outboundConnector = outboundCurve sc parentbbox bbold bbnew
    bbox =
      (defaultBBox sc)
        & boxDims.dimHeight .~ bbold ^. bboxHeight + bbnew ^. bboxHeight + lrVgap
        & boxDims.dimWidth .~ max (bbold ^. bboxWidth) (bbnew ^. bboxWidth)
        & boxConnect .~ (connect bbold `mergeConnects` connect bbnew)
    svg =
      old
        <> move (0, bbold ^. bboxHeight + lrVgap) new
        <> inboundConnector
        <> outboundConnector

inboundCurve :: Scale -> BBox -> BBox -> BBox -> SVGElement
inboundCurve sc parentbbox bbold bbnew =
  path_ ((D_ <<- startPosition <> bezierCurve) : (Class_ <<- "v_connector_in") : pathcolors)
  where
    parentPortIn = portL parentbbox myScale + lrVgap
    pathcolors = (Fill_ <<- "none") : curveColour sc bbnew
    myScale = getScale sc
    lrVgap = myScale ^. aavscaleHorizontalLayout.gapVertical
    leftMargin' = myScale ^. aavscaleMargins.leftMargin
    startPosition = mAInt (- leftMargin') parentPortIn
    bezierCurve =
      cAInt
        0
        parentPortIn
        (-leftMargin')
        (bbold ^. bboxHeight + lrVgap + portL bbnew myScale)
        (bbnew ^. boxMargins.leftMargin)
        (bbold ^. bboxHeight + lrVgap + portL bbnew myScale)

-- | the stroke colour and width for a curve from a child to its parent; it depends on whether the child is connected or not
curveColour :: Scale -> BBox -> [Attribute]
curveColour sc bb
  | connect bb == Just True = [ Stroke_ <<- "black",        Stroke_width_ <<- "2px" ]
  | otherwise               = [ Stroke_ <<- defaultStroke_, Stroke_width_ <<- "1px" ]

outboundCurve :: Scale -> BBox -> BBox -> BBox -> SVGElement
outboundCurve sc parentbbox bbold bbnew =
  path_ ((D_ <<- startPosition <> bezierCurve) : (Class_ <<- "v_connector_out") : pathcolors)
  where
    parentPortOut = portR parentbbox myScale + lrVgap
    pathcolors = (Fill_ <<- "none") : curveColour sc bbnew
    myScale = getScale sc
    lrVgap = myScale ^. aavscaleHorizontalLayout.gapVertical
    rightMargin' = myScale ^. aavscaleMargins.rightMargin
    startPosition = mAInt (parentbbox ^. bboxWidth + rightMargin') parentPortOut
    bezierCurve =
      cAInt
        (parentbbox ^. bboxWidth)
        parentPortOut
        (parentbbox ^. bboxWidth + rightMargin')
        (bbold ^. bboxHeight + lrVgap + portR bbnew myScale)
        (parentbbox ^. bboxWidth - (bbnew ^. boxMargins.rightMargin))
        (bbold ^. bboxHeight + lrVgap + portR bbnew myScale)

-- | disjunctive combination of child nodes. (non-monadic)
-- If any of the child nodes provides a connection, this parent node is deemed also connected.
-- We sort the child bboxes according to connectivity: connects at top, disconnects at bottom, unknowns in between.
-- See the original bbox spec for details.
combineOr :: Scale -> [BoxedSVG] -> BoxedSVG
combineOr sc elems =
  ( childbbox
    & boxDims.dimWidth .~  childbbox ^. bboxWidth + leftMargin' + rightMargin'
    & boxDims.dimHeight .~ childbbox ^. bboxHeight - interElementGap
  ,
    move (leftMargin', - interElementGap) children
  )
  where
    myScale = getScale sc
    interElementGap = myScale ^. aavscaleHorizontalLayout.gapVertical
    leftMargin' = myScale ^. aavscaleMargins.leftMargin
    rightMargin' = myScale ^. aavscaleMargins.rightMargin
    childheights = interElementGap * fromIntegral (length elems - 1) + sum (boxHeight . dimensions . fst <$> elems)
    mybbox = (defaultBBox sc)
      & boxDims.dimWidth .~  maximum (boxWidth . dimensions . fst <$> elems)
      & boxDims.dimHeight .~ childheights
    reorderedChildren = reorderByConnectivity elems
    addElementToColumn :: BoxedSVG -> BoxedSVG -> BoxedSVG
    addElementToColumn = columnLayouter sc mybbox
    (childbbox, children) = foldl' addElementToColumn (defaultBBox sc, mempty) $ hAlign HCenter reorderedChildren

-- | re-order a list of elements. The elements could be in a disjunctive or conjunctive context -- we would do the same in both cases.
-- But if re-ordering an and-list would be too confusing, then `combineAnd` doesn't need to call this; only `combineOr` needs to call this.
-- And that would be fine.
-- We want the non-connecting elements to go to the top and the known-non-connecting elements go to the bottom.
reorderByConnectivity :: [BoxedSVG] -> [BoxedSVG]
reorderByConnectivity = sortBy (orderConnects `on` connect . fst)

-- | -- In the simple case, where elements have to be true, if we have a bunch of @[Unknown, False, True, False]@
-- we would reorder that to be @[True, Unknown, False, False]@
-- In the opposite case, where elements have to be false to connect (e.g. @Not Leaf@) if we have a bunch of @[Unknown, False, True, False]@
-- we would reorder that to be @[False, False, Unknown, True]@

orderConnects :: Connect -> Connect -> Ordering
Just True  `orderConnects` Just False = LT
Just False `orderConnects` Just True  = GT

Just True  `orderConnects` Nothing    = LT
Nothing    `orderConnects` Just True  = GT

Nothing    `orderConnects` Just False = LT
Just False `orderConnects` Nothing    = GT

_          `orderConnects` _          = EQ

-- | if we are combining two bounding boxes, which connection wins?
mergeConnects :: Connect -> Connect -> Connect
mergeConnects (Just True) _  = Just True
mergeConnects _ (Just True)  = Just True
mergeConnects (Just False) _ = Just False
mergeConnects _ (Just False) = Just False
mergeConnects _ _ = Nothing


combineAnd :: Scale -> [BoxedSVG] -> BoxedSVG
combineAnd sc elems =
  ( combinedBox
      & boxPorts.leftPort  .~ PVoffset (portL childbbox myScale)
      & boxPorts.rightPort .~ PVoffset (portR childbbox myScale)
      & boxMargins.leftMargin %~ (+ leftPad)
      & boxMargins.rightMargin %~ (+ rightPad)
  ,
    move (leftPad, 0) children
  )
  where
    myScale = getScale sc
    leftPad = myScale ^. aavscaleMargins.leftMargin
    rightPad = myScale ^. aavscaleMargins.rightMargin
    addElementToRow = rowLayouter sc
    (childbbox, children) = foldl1 addElementToRow $ vAlign VTop elems
    combinedBox = childbbox & bboxWidth %~ (+ (leftPad + rightPad))

combineAndS ::  [BoxedSVG] -> SVGCanvas ()
combineAndS elems = do
  myScale <- asks aav
  put firstE
  mapM_ rowLayouterS restE
  (childbbox, children) <- get
  let
    leftPad = myScale ^. aavscaleMargins.leftMargin
    rightPad = myScale ^. aavscaleMargins.rightMargin
    combinedBox = childbbox & bboxWidth %~ (+ (leftPad + rightPad))
  put ( combinedBox
      & boxPorts.leftPort  .~ PVoffset (portL childbbox myScale)
      & boxPorts.rightPort .~ PVoffset (portR childbbox myScale)
      & boxMargins.leftMargin %~ (+ leftPad)
      & boxMargins.rightMargin %~ (+ rightPad)
    ,
      move (leftPad, 0) children
    )
  where
    (firstE:restE) = vAlign VTop elems

rowLayouterS :: BoxedSVG -> SVGCanvas ()
rowLayouterS (bbnew, new) = do
  sc <- asks contextScale
  myScale <- asks aav
  (bbold, old) <- get
  let
    templateBox = (defaultBBox sc)
        & boxDims.dimHeight .~ max (bbold ^. bboxHeight) (bbnew ^. bboxHeight)
        & boxDims.dimWidth .~ bbold ^. bboxWidth + lrHgap + bbnew ^. bboxWidth
    lrHgap = myScale ^. aavscaleHorizontalLayout.gapHorizontal
    newBoxStart = bbold ^. bboxWidth + lrHgap
    connectingCurve =
      if bbold ^. bboxWidth /= 0
        then svgConnector $ rowConnectorData sc bbold bbnew
        else mempty
  put ( templateBox
      & boxPorts.leftPort  .~ PVoffset (portL bbold myScale)
      & boxPorts.rightPort .~ PVoffset (portR bbnew myScale)
      & boxMargins.leftMargin .~ (bbold ^. boxMargins.leftMargin)
      & boxMargins.rightMargin .~ (bbnew ^. boxMargins.rightMargin)
      ,
      old
        <> move (newBoxStart, 0) new
        <> connectingCurve
    )

drawPreLabelTop :: Scale -> T.Text -> BoxedSVG -> BoxedSVG
drawPreLabelTop sc label (childBox, childSVG) =
    (childBox
      & boxDims.dimHeight  %~ (+ labelHeight)
      & boxMargins.topMargin %~ (+ labelHeight)
    ,
    move (0, labelHeight) childSVG <> svgLabel)
  where
    labelHeight = getScale sc ^. aavscaleMargins.topMargin
    lbox = labelBox sc "hanging" label
    (_,svgLabel) = alignH HCenter (childBox ^. bboxWidth) lbox

drawPrePostLabelTopBottom :: Scale -> T.Text -> T.Text -> BoxedSVG -> BoxedSVG
drawPrePostLabelTopBottom sc preTxt postTxt (childBox, childSVG) =
    (childBox
      & boxDims.dimHeight %~ (+ 2 * labelHeight)
      & boxMargins.topMargin %~ (+ labelHeight)
      & boxMargins.bottomMargin %~ (+ labelHeight)
    ,
    move (0, labelHeight) childSVG <> svgPreLabel <>  move (0, childBox ^. bboxHeight + 2 * labelHeight) svgPostLabel)
  where
    labelHeight = getScale sc ^. aavscaleMargins.topMargin
    prelbox = labelBox sc "hanging" preTxt
    (_,svgPreLabel) = alignH HCenter (childBox ^. bboxWidth) prelbox
    postlbox = labelBox sc "ideographic" postTxt
    (_,svgPostLabel) = alignH HCenter (childBox ^. bboxWidth) postlbox

decorateWithLabel :: Scale -> Maybe (Label T.Text) -> BoxedSVG -> BoxedSVG
decorateWithLabel Tiny _ childBox = childBox
decorateWithLabel _ Nothing childBox = childBox
decorateWithLabel sc (Just (Pre txt)) childBox = drawPreLabelTop sc txt childBox
decorateWithLabel sc (Just (PrePost preTxt postTxt)) childBox = drawPrePostLabelTopBottom sc preTxt postTxt childBox

decorateWithLabelR :: Maybe (Label T.Text) -> BoxedSVG -> SVGCanvas BoxedSVG
decorateWithLabelR Nothing childBox = return childBox
decorateWithLabelR (Just (Pre txt)) childBox = asks contextScale >>= \sc -> return $ drawPreLabelTop sc txt childBox
decorateWithLabelR (Just (PrePost preTxt postTxt)) childBox = asks contextScale >>= \sc -> return $ drawPrePostLabelTopBottom sc preTxt postTxt childBox

decorateWithLabelGuardR :: Maybe (Label T.Text) -> BoxedSVG -> SVGCanvas BoxedSVG
decorateWithLabelGuardR ml childBox = asks contextScale >>= \sc -> if sc == Tiny then pure childBox else decorateWithLabelR ml childBox

drawItemFull :: Scale -> Bool -> QuestionTree -> BoxedSVG
drawItemFull sc negContext (Node (Q sv ao pp m) childqs) =
  case ao of
    Or -> decorateWithLabel sc pp (combineOr sc rawChildren)
    And -> decorateWithLabel sc pp  (combineAnd sc rawChildren)
    Simply txt -> fst (execRWS (drawLeafR txt) contextR (defaultBBox', mempty::SVGElement))
    Neg -> drawItemFull sc (not negContext) (head childqs)
  where
    contextR = DrawConfig sc negContext m (defaultBBox sc) (getScale sc) (if sc == Tiny then textBoxLengthTiny else textBoxLengthFull)
    rawChildren = drawItemFull sc negContext <$> childqs

-- | the low-level boolean logic of when to connect.
-- In a negContext, draw the topline if the inner value is a False.
-- Not in a negContext, draw the topline if the inner value is a True.
-- we repeat this logic in the drawLeafR function.
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

drawBoxCapR :: T.Text -> SVGCanvas SVGElement
drawBoxCapR caption = do
  negContext <- asks negContext
  m <- asks markingR
  drawBoxCap negContext m <$> deriveBoxSize caption

drawBoxContentR :: T.Text -> SVGCanvas SVGElement
drawBoxContentR caption = do
  sc <- asks contextScale
  textFill <- getTextColorsR
  drawBoxContent sc caption textFill <$> deriveBoxSize caption

drawBoxCap :: Bool -> Default Bool -> BoxDimensions -> SVGElement
drawBoxCap negContext m BoxDimensions{boxWidth=bw, boxHeight=bh} =
  leftLineSVG <> rightLineSVG <> topLineSVG
  where
    (leftline, rightline, topline) = deriveBoxCap negContext m
    leftLineSVG = drawVerticalLine 0 bh leftline "leftline" "black"
    rightLineSVG = drawVerticalLine bw bh rightline "rightline" "black"
                   -- in Full and Small mode, draw a white line just to the left of the full-height right black line
                   -- for increased visibility
                   -- [TODO] don't draw the line when Tiny. Need to move this into SVGCanvas
                   <> if rightline == FullLine
                      then drawVerticalLine (bw-2) bh rightline "rightline" "white"
                      else mempty
    topLineSVG = drawHorizontalLine 0 bw topline "topline"

drawVerticalLine :: Length -> Length -> LineHeight -> T.Text -> T.Text -> SVGElement
drawVerticalLine xPosition length lineType linePosition strokeColor =
  case lineType of
    FullLine -> renderVerticalLine xPosition length (T.append linePosition ".full") strokeColor
    HalfLine -> renderVerticalLine xPosition (length `div` 2) (T.append linePosition ".half") strokeColor
    NoLine -> mempty

renderVerticalLine :: Length -> Length -> T.Text -> T.Text -> SVGElement
renderVerticalLine xPosition length lineClass strokeColor =
  line_
    [ X1_ <<-* xPosition, Y1_ <<-* 0,
      X2_ <<-* xPosition, Y2_ <<-* length,
      Stroke_ <<- strokeColor,
      Class_ <<- lineClass
    , Stroke_width_ <<- "2px"
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
    , Stroke_width_ <<- "2px"
    ]

drawBoxContent :: Scale -> T.Text -> T.Text -> BoxDimensions -> SVGElement
drawBoxContent Tiny _ textFill BoxDimensions{boxWidth=boxWidth, boxHeight=boxHeight} =
  circle_ [Cx_  <<-* (boxWidth `div` 2) ,Cy_      <<-* (boxHeight `div` 2) , R_ <<-* (boxWidth `div` 3), Fill_ <<- textFill ]
drawBoxContent _ mytext textFill BoxDimensions{boxWidth=boxWidth, boxHeight=boxHeight} =
  text_ [ X_  <<-* (boxWidth `div` 2) , Y_      <<-* (boxHeight `div` 2) , Text_anchor_ <<- "middle" , Dominant_baseline_ <<- "central" , Fill_ <<- textFill ] (toElement mytext)

data DrawConfig = DrawConfig{
    contextScale :: Scale,
    negContext :: Bool,
    markingR :: Default Bool,
    defaultBox :: BBox,
    aav :: AAVScale,
    textBoxLengthFn :: Length -> Length -> Length
  }

type SVGCanvas = RWS DrawConfig T.Text BoxedSVG

textBoxLengthFull :: Length -> Length -> Length
textBoxLengthFull defBoxWidth captionLength = defBoxWidth + (6 * captionLength)

textBoxLengthTiny :: Length -> Length -> Length
textBoxLengthTiny defBoxWidth _ = defBoxWidth

deriveBoxSize :: T.Text -> SVGCanvas BoxDimensions
deriveBoxSize caption = do
  textBoxLength <- asks textBoxLengthFn
  BoxDimensions{boxWidth=defBoxWidth, boxHeight=boxHeight} <- asks (scaleDims.aav)
  let
      boxWidth = textBoxLength defBoxWidth (fromIntegral (T.length caption))
  return BoxDimensions{boxWidth=boxWidth, boxHeight=boxHeight}

labelBox :: Scale -> T.Text -> T.Text -> BoxedSVG
labelBox sc baseline caption =
  ( (defaultBBox sc)
      & boxDims.dimWidth .~ boxWidth
      & boxDims.dimHeight .~ boxHeight,
    boxContent
  )
  where
    boxHeight = getScale sc ^. aavscaleDims.dimHeight
    defBoxWidth =  getScale sc ^. aavscaleDims.dimWidth
    boxWidth = textBoxLengthFull defBoxWidth (fromIntegral (T.length caption))
    boxContent = text_ [X_ <<-* (boxWidth `div` 2), Text_anchor_ <<- "middle", Dominant_baseline_ <<- baseline] (toElement caption)

labelBoxR :: T.Text -> T.Text -> SVGCanvas BoxedSVG
labelBoxR baseline mytext = do
  dbox <- asks defaultBox
  dims@BoxDimensions{boxWidth=boxWidth} <- deriveBoxSize mytext
  return (
    dbox & boxDims .~ dims
    ,
    text_ [ X_  <<-* (boxWidth `div` 2), Text_anchor_ <<- "middle", Dominant_baseline_ <<- baseline] (toElement mytext))

drawLeafR :: T.Text -> SVGCanvas ()
drawLeafR caption = do
  dbox <- asks defaultBox
  (boxStroke, boxFill) <- getBoxColorsR
  dims@BoxDimensions{boxWidth=boxWidth, boxHeight=boxHeight} <- deriveBoxSize caption
  boxContent <- drawBoxContentR caption
  boxCap <- drawBoxCapR caption -- line goes across if it connects
  negContext <- asks negContext
  m          <- asks markingR
  let connection =
        case (m, negContext) of
          (Default (Left Nothing),  _ )  -> Nothing
          (Default (Right Nothing), _ )  -> Nothing
          (Default (Left l),        r )  -> (/=) <$> l <*> pure r
          (Default (Right l),       r )  -> (/=) <$> l <*> pure r

  put
    (dbox & boxDims .~ dims & boxConnect .~ connection
    ,
      rect_ [X_ <<-* 0, Y_ <<-* 0, Width_ <<-* boxWidth, Height_ <<-* boxHeight, Stroke_ <<- boxStroke, Fill_ <<- boxFill, Class_ <<- "textbox"]
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
