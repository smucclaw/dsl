{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

-- usage:
-- (base) ┌─[mengwong@solo-8] - [~/src/smucclaw/dsl/lib/haskell/anyall] - [2022-05-18 12:38:04]
-- └─[255] <git:(ladder 88053e0✱✈) > cat out/example-or.json | stack run -- --only svg | perl -ple 's/\.00+//g' > out/example4.svg

-- | A visualization inspired by Ladder Logic and by Layman Allen (1978).

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

translateInt :: Integral a =>  a -> a -> T.Text
translateInt x y = T.concat ["translate(", intToText x, " ", intToText y, ")"]

moveInt :: Integral a => (a, a) -> SVGElement -> SVGElement
moveInt (x, y) geoms =
  with geoms [Transform_ <<- translateInt x y]

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

showLabels :: Scale -> Bool
showLabels Full = True
showLabels Small = False
showLabels Tiny = False

type ItemStyle = Maybe Bool

(<<-*) :: Show a => AttrTag -> a -> Attribute
(<<-*) tag a = bindAttr tag (T.pack (show a))

infix 4 <<-*

makeSvg' :: AAVConfig -> BoxedSVG -> SVGElement
makeSvg' c = makeSvg

makeSvg :: BoxedSVG -> SVGElement
makeSvg (box, geom) =
     doctype
  <> with (svg11_ (moveInt (23,23) geom)) [Version_ <<- "1.1", Width_ <<-* 23 + bbw box + _leftMargin (margins box) + _rightMargin (margins box), Height_ <<-* 23 + bbh box + _topMargin (margins box) + _bottomMargin (margins box)]

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

-- | item drawing proceeds in the following stages:
-- - construct all children -- just the boxes, no port connectors yet. if the children are themselves complex, we trust in the bounding boxes returned.
-- - for each child, position horizontally, centered or left/right aligned appropriately.
-- - position children vertically. usually this means spreading them out, with a gap between them. we do this by adding a topmargin to each bounding box
-- - flatten all the children into a single element. attach input and output horizontal lines to ports.
-- - return adjusted bounding box to caller.

data HAlignment = HLeft | HCenter | HRight   deriving (Eq, Show)
data VAlignment = VTop  | VMiddle | VBottom  deriving (Eq, Show)

-- | see page 1 of "box model" documentation
(>>>), (<<<), (\|/), (/|\) :: BoxedSVG -> Length -> BoxedSVG
(>>>) (bb,e) n = (bb { bbw = bbw bb + n} & boxMargins.leftMargin   %~ (+ n), moveInt (n, 0) e)
(<<<) (bb,e) n = (bb { bbw = bbw bb + n} & boxMargins.rightMargin  %~ (+ n),                e)
(\|/) (bb,e) n = (bb { bbh = bbh bb + n} & boxMargins.topMargin    %~ (+ n), moveInt (0, n) e)
(/|\) (bb,e) n = (bb { bbh = bbh bb + n} & boxMargins.bottomMargin %~ (+ n),                e)
infix 4 >>>, <<<, \|/, /|\

topText :: Maybe (Label a) -> Maybe a
topText = (=<<) maybeFirst

bottomText :: Maybe (Label a) -> Maybe a
bottomText = (=<<) maybeSecond

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
alignVCalcElement VMiddle alignmentPad = moveInt (0, alignmentPad `div` 2)
alignVCalcElement VTop    alignmentPad = id
alignVCalcElement VBottom alignmentPad = moveInt (0, alignmentPad)

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
alignHCalcMove HCenter alignmentPad = moveInt ( alignmentPad `div` 2, 0)
alignHCalcMove HLeft alignmentPad = id
alignHCalcMove HRight alignmentPad = moveInt (alignmentPad, 0)

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
      <> moveInt (newBoxStart, 0) new
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

-- bezier curves: "M"            is the position of                       the first  point.
-- the first  argument after "c" is the position of the control point for the first  point, relative to the first point.
-- the second argument after "c" is the position of the control point for the second point, relative to the first point.
-- the third  argument after "c" is the position of                       the second point, relative to the first point.
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
        <> moveInt (0, bbh bbold + lrVgap) new
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

txtToBBE ::  AAVConfig -> T.Text -> BoxedSVG
txtToBBE c x = ( (defaultBBox (cscale c)) { bbh = boxHeight, bbw = boxWidth } {- [TODO] resizeHBox -}
              , text_ [ X_ <<-* 0, Y_ <<-* boxHeight `div` 2, Text_anchor_ <<- "middle", Dominant_baseline_ <<- "central", Fill_ <<- textFill ] (toElement x) )
  where
    myScale     = getScale (cscale c)
    textFill = getColorsText (cscale c) True
    boxWidth    = sbw myScale
    boxHeight   = sbh myScale

combineOr :: Scale -> [BoxedSVG] -> BoxedSVG
combineOr sc elems =
  ( childbbox
      { bbw = bbw childbbox + leftMargin + rightMargin,
        bbh = bbh childbbox - interElementGap
      },
    moveInt (leftMargin, - interElementGap) children
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
    moveInt (leftMargin', 0) children
  )
  where
    myScale = getScale sc
    leftMargin' = slm myScale
    rightMargin' = srm myScale
    addElementToRow = rowLayouter sc
    (childbbox, children) = foldl1 addElementToRow $ vAlign VTop elems
    combinedBox = childbbox { bbw = bbw childbbox + leftMargin' + rightMargin'}

drawLabel ::  AAVConfig -> Maybe (Label T.Text) -> SVGElement
drawLabel _ Nothing = mempty
drawLabel c (Just l) =
  case l of
    Pre t -> boxContent t
    PrePost pre post -> boxContent (T.append pre post)
  where
    boxHeight        = sbh (getScale (cscale c))
    defBoxWidth      = sbw (getScale (cscale c))
    boxWidth         = \txt -> defBoxWidth - 15 + (3 * fromIntegral (T.length txt))
    boxContent = \txt -> drawBoxContent (cscale c) txt "black" BoxDimensions {boxWidth=(boxWidth txt), boxHeight=boxHeight}

drawPreLabelTop :: Scale -> T.Text -> BoxedSVG -> BoxedSVG
drawPreLabelTop sc label (childBox, childSVG) =
    (labeledBox
      & boxMargins.topMargin %~ (+ labelHeight)
    ,
    moveInt (0, labelHeight) childSVG <> svgLabel)
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
    moveInt (0, labelHeight) childSVG <> svgPreLabel <>  moveInt (0, bbh childBox + 2 * labelHeight) svgPostLabel)
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

type Boolean = Bool
itemBox :: AAVConfig
        -> Double          -- ^ x top left
        -> Double          -- ^ y top left
        -> AndOr T.Text   -- ^ Item, recast as an AndOr Text for display
        -> Default Bool    -- ^ mark for the box
        -> [QTree T.Text] -- ^ children
        -> Bool            -- ^ did we get here because we were contained by a Neg?
        -> BoxedSVG
itemBox c x y Neg m cs amNot = itemBox c x y (andOr $ rootLabel $ head cs) m [] False
itemBox c x y (Simply t)  m cs amNot
  | cscale c  == Tiny  = (,) (defaultBBox (cscale c)) { bbw = 10, bbh = 10 } $ g_ [] ( rect_ [ X_ <<-* x, Y_ <<-* y, Width_ <<-* 10, Height_ <<-* 10, Stroke_ <<- "red", Fill_ <<- "green" ] )
-- [TODO] small
  | cscale c  `elem` [Full,Small]  = (,) ((defaultBBox (cscale c)) { bbw = fromIntegral $ T.length t * 3, bbh = 25 }) $ g_ [] (
      rect_ [ X_ <<-* x      , Y_ <<-* y, Width_ <<-* 10, Height_ <<-* 10, Stroke_ <<- "red", Fill_ <<- "green" ]
        <> mempty ) -- some text
itemBox c x y andor m cs amNot = (,) ((defaultBBox (cscale c)) { bbw = fromIntegral $ 25, bbh = 25 }) $ g_ [] (
  rect_ [ X_ <<-* x      , Y_ <<-* y, Width_ <<-* 10, Height_ <<-* 10, Stroke_ <<- bs cs, Fill_ <<- bf cs ]
    <> mempty) -- some text

  -- [TODO]: for And and Or, recurse into children, and move them around, and update current bounding box.
  where cs = colorScheme c m amNot

data ColorScheme = ColorScheme
  { -- | box stroke
    bs :: T.Text,
    -- | box fill
    bf :: T.Text,
    -- | text fill
    tf :: T.Text,
    -- | left  "negation" line -- the marking is False
    ll :: T.Text,
    -- | right "negation" line -- we are drawing a Not element
    rl :: T.Text,
    -- | top "truth" line -- drawn if the value is true, or if the marking is false and the item is a Not
    tl :: T.Text
  }
-- | the color scheme depends on the marking
colorScheme :: AAVConfig
            -> Default Bool
            -> Boolean   -- ^ iff we got here via a Not, this value is True
            -> ColorScheme
colorScheme c m amNot = case m of
                          Default (Right (Just b@True )) -> ColorScheme "none" "none" "black" "none"  notLine (topLine b) -- user says true, or computed to true
                          Default (Right (Just b@False)) -> ColorScheme "none" "none" "black" "black" notLine (topLine b) -- user says false, or computed to false
                          Default (Right (Nothing     )) -> ColorScheme "none" "none" "black" "black" notLine "none"      -- user says explicitly they don't know
                          Default (Left  (Just b@True )) -> ColorScheme "none" "grey" "white" "none"  notLine (topLine b) -- no user input, default is true
                          Default (Left  (Just b@False)) -> ColorScheme "none" "grey" "white" "black" notLine (topLine b) -- no user input, default is false
                          Default (Left  Nothing      )  -> ColorScheme "none" "grey" "white" "black" notLine "none"      -- no user input, no default
  where
    notLine   = if amNot                   then "black" else "none"
    topLine b = if          amNot && not b then "black"
                else if not amNot &&     b then "black" else "none"


box :: AAVConfig -> Double -> Double -> Double -> Double -> SVGElement
box c x y w h =
  rect_ [ X_ <<-* x, Y_ <<-* y, Width_ <<-* w, Height_ <<-* h
        , Fill_ <<- "none", Stroke_ <<- "black" ]

debugBox :: T.Text -> Length -> Length -> SVGElement
debugBox m w h =
  rect_ [ X_ <<-* 0, Y_ <<-* 0, Width_ <<-* w, Height_ <<-* h
        , Fill_ <<- "none", Stroke_ <<- "black", Class_ <<- m]

line :: (Length , Length) -> (Length, Length) -> SVGElement
line (x1, y1) (x2, y2) =
  line_ [ X1_ <<-* x1, X2_ <<-* x2, Y1_ <<-* y1, Y2_ <<-* y2
        , Stroke_ <<- "grey" ]

item :: ToElement a => AAVConfig -> Double -> Double -> a -> SVGElement
item c x y desc =
  let w = 20
  in
    g_ [] (  box c x y w w
          <> text_ [ X_ <<-* (x + w + 5), Y_ <<-* (y + w - 5) ] (toElement desc)  )

move :: (Double, Double) -> SVGElement -> SVGElement
move (x, y) geoms =
  with geoms [Transform_ <<- translate x y]

type OldBBox = (Length, Length)

renderChain :: AAVConfig -> [(OldBBox, SVGElement)] -> SVGElement
renderChain c [] = mempty
renderChain c [(_,g)] = g
renderChain c (((w,h),g):hgs) =
  g_ [] (  g
        <> line (10, 20) (10, h)
        <> moveInt (0, h) (renderChain c hgs)  )

renderLeaf :: (ToElement a) => AAVConfig -> a -> (OldBBox, SVGElement)
renderLeaf c desc =
  let height = 25
      geom = item c 0 0 desc
  in ((25,height), geom)

renderNot :: (ToElement a) => AAVConfig -> [ItemMaybeLabel a] -> (OldBBox, SVGElement)
renderNot c children =
  let
      ((w,h), g) = renderItem c $ head children
      height = h

      geom :: SVGElement
      geom = g_ [] ( line (-5, 5) (-10, 15)  --  /
                     <> line (10,0) (10,25)  --  |
                     <> move (00, 0) g )
  in ((w,height), geom)


renderSuffix :: (ToElement a) => AAVConfig -> Length -> Length -> a -> (OldBBox, SVGElement)
renderSuffix c x y desc =
  let h = 20 -- h/w of imaginary box
      geom :: SVGElement
      geom = g_ [] ( text_ [ X_ <<-* x, Y_ <<-* (y + h - 5) ] (toElement desc) )
  in ((25,h), geom)

renderAll :: (ToElement a) => AAVConfig -> Maybe (Label T.Text) -> [ItemMaybeLabel a] -> (OldBBox, SVGElement)
renderAll c Nothing childnodes = renderAll c allof childnodes
renderAll c (Just (Pre prefix)) childnodes =
  let
      hg = map (renderItem c) childnodes
      (hs, gs) = unzip hg

      width = 25
      height = sum (snd <$> hs) + 30

      geom :: SVGElement
      geom = g_ [] (  item c 0 0 prefix
                   -- elbow connector
                   <> line (10, 20) (10, 25)
                   <> line (10, 25) (40, 25)
                   <> line (40, 25) (40, 30)
                   -- children translated by (30, 30)
                   <> move (30, 30) (renderChain c hg)  )
  in ((width,height), geom)
renderAll c (Just (PrePost prefix suffix)) childnodes =
  let hg = map (renderItem c) childnodes
      (hs, gs) = unzip hg

      ((fw, fh), fg) = renderSuffix c 0 0 suffix

      width = 25
      height = sum (snd <$> hs) + fh + 30

      geom :: SVGElement
      geom = g_ [] (  item c 0 0 prefix
                   <> line (10, 20) (10, 25)
                   <> line (10, 25) (40, 25)
                   <> line (40, 25) (40, 30)
                   <> moveInt (30, 30) (renderChain c hg)
                   <> moveInt (40, 30 + sum (snd <$> hs)) fg  )
  in ((width,height), geom)

renderAny :: (ToElement a) => AAVConfig -> Maybe (Label T.Text) -> [ItemMaybeLabel a] -> (OldBBox, SVGElement)
renderAny c Nothing childnodes = renderAny c (Just (Pre "any of:")) childnodes
renderAny c (Just (Pre prefix)) childnodes =
  let hg = map (renderItem c) childnodes
      (hs, gs) = unzip hg

      width = 25
      height = sum (snd <$> hs) + 25

      geom :: SVGElement
      geom = g_ [] (  item c 0 0 prefix
                   <> line (10, 20) (10, sum (init (snd <$> hs)) + 25 + 10)
                   <> moveInt (30, 25) (go 0 hg)  )
                 where go y [] = mempty
                       go y (((w,h),g):hgs) =
                         g_ [] (  g
                               <> line (-20, 10) (0, 10)
                               <> moveInt (0, h) (go (y+h) hgs)  )
  in ((width,height), geom)
renderAny c (Just (PrePost prefix suffix)) childnodes =
  let hg = map (renderItem c) childnodes
      (hs, gs) = unzip hg

      ((fw,fh), fg) = renderSuffix c 0 0 suffix

      width = 25
      height = sum (snd <$> hs) + fh + 25

      geom :: SVGElement
      geom = g_ [] (  item c 0 0 prefix
                   <> line (10, 20) (10, sum (snd <$> init hs) + 25 + 10)
                   <> moveInt (30, 25) (go 0 hg)
                   <> moveInt (40, 25 + sum (snd <$> hs)) fg)
                 where go y [] = mempty
                       go y (((w,h),g):hgs) =
                         g_ [] (  g
                               <> line (-20, 10) (0, 10)
                               <> moveInt (0, h) (go (y+h) hgs)  )
  in ((width, height), geom)


renderItem :: (ToElement a) => AAVConfig -> ItemMaybeLabel a -> (OldBBox, SVGElement)
renderItem c (Leaf label)     = renderLeaf c label
renderItem c (Not       args) = renderNot c      [args]
renderItem c (All label args) = renderAll c label args
renderItem c (Any label args) = renderAny c label args

toy :: (OldBBox, SVGElement)
toy = renderItem defaultAAVConfig $
  All (Just $ PrePost "You need all of" ("to survive." :: T.Text))
      [ Leaf ("Item 1;" :: T.Text)
      , Leaf "Item 2;"
      , Any (Just $ Pre "Item 3 which may be satisfied by any of:" )
            [ Leaf "3.a;"
            , Leaf "3.b; or"
            , Leaf "3.c;" ]
      , Leaf "Item 4; and"
      , All ( Just $ Pre "Item 5 which requires all of:" )
            [ Leaf "5.a;"
            , Leaf "5.b; and"
            , Leaf "5.c." ]
      ]

