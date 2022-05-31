{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

-- usage:
-- (base) ┌─[mengwong@solo-8] - [~/src/smucclaw/dsl/lib/haskell/anyall] - [2022-05-18 12:38:04]
-- └─[255] <git:(ladder 88053e0✱✈) > cat out/example-or.json | stack run -- --only svg | perl -ple 's/\.00+//g' > out/example4.svg

-- | A visualization inspired by Ladder Logic and by Layman Allen (1978).

module AnyAll.SVGLadder where

import Data.List (foldl')

import AnyAll.Types hiding ((<>))

import Data.String
import Graphics.Svg
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Tree
import Debug.Trace

type Length  = Double
type SVGElement = Element

data BBox = BBox
  { bbw :: Length
  , bbh :: Length
  , bblm, bbtm, bbrm, bbbm :: Length -- left, top, right, bottom margins
  ,  pl,         pr        :: PortStyleV -- left and right  ports
  ,  pt,         pb        :: PortStyleH --  top and bottom ports
  }
  deriving (Eq, Show)

type BoxedSVG = (BBox, SVGElement)

data PortStyleV = PTop  | PMiddle | PBottom | PVoffset Length deriving (Eq, Show)
data PortStyleH = PLeft | PCenter | PRight  | PHoffset Length deriving (Eq, Show)

-- | default bounding box
defaultBBox Tiny  = defaultBBox' { pl = PMiddle, pr = PMiddle }
defaultBBox Small = defaultBBox Tiny
defaultBBox Full  = defaultBBox'
defaultBBox' = BBox
  { bbw = 0
  , bbh = 0
  , bblm = 0
  , bbtm = 0
  , bbrm = 0
  , bbbm = 0
  , pl = PTop,    pr = PTop    -- [TODO] default to PTop later
  , pt = PCenter, pb = PCenter
  }

portL, portT, portR, portB :: BBox -> AAVScale -> Length
portL bb = portLR (pl bb) bb
portR bb = portLR (pr bb) bb
portT bb = portTB (pt bb) bb
portB bb = portTB (pb bb) bb

portLR :: PortStyleV -> BBox -> AAVScale -> Length
portLR PTop    bb s = bbtm bb +                                    sbh s / 2 -- [TODO] clip to max size of element
portLR PMiddle bb s = bbtm bb + (bbh bb - bbtm bb - bbbm bb) / 2
portLR PBottom bb s =           (bbh bb           - bbbm bb)     - sbh s / 2
portLR (PVoffset x) bb s = bbtm bb + x

portTB :: PortStyleH -> BBox -> AAVScale -> Length
portTB PLeft   bb s = bblm bb +                                    stbv s -- [TODO] clip to max size of element
portTB PCenter bb s = bblm bb + (bbw bb - bblm bb - bbrm bb) / 2
portTB PRight  bb s =           (bbw bb           - bbrm bb)     - stbv s
portTB (PHoffset x) bb s = bblm bb + x

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
  }
  deriving (Show, Eq)

defaultAAVConfig :: AAVConfig
defaultAAVConfig = AAVConfig
  { cscale = Tiny
  , cdirection = LR
  , cgetMark = Marking Map.empty
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

getColors True = ("none", "none", "black")
getColors False = ("none", "lightgrey", "white")

showLabels Full = True
showLabels Small = False
showLabels Tiny = False

type ItemStyle = Maybe Bool

(<<-*) :: Show a => AttrTag -> a -> Attribute
(<<-*) tag a = bindAttr tag (T.pack (show a))

-- string to element
tpsa a = T.pack $ show a

infix 4 <<-*

makeSvg' :: AAVConfig -> BoxedSVG -> SVGElement
makeSvg' c = makeSvg

makeSvg :: BoxedSVG -> SVGElement
makeSvg (_bbx, geom) =
     doctype
  <> with (svg11_ (move (23,23) geom)) [Version_ <<- "1.1" ]

data LineHeight = NoLine | HalfLine | FullLine
  deriving (Eq, Show)

q2svg :: AAVConfig -> QTree T.Text -> SVGElement
q2svg c qt = snd $ q2svg' c qt

q2svg' :: AAVConfig -> QTree T.Text -> BoxedSVG
q2svg' c qt@(Node q childqs) = drawItem c False qt 

drawItem :: AAVConfig -> Bool -> QTree T.Text -> BoxedSVG
drawItem c negContext qt
  | cscale c == Tiny = drawItemTiny c negContext qt
  | otherwise        = drawItemFull c negContext qt

-- | item drawing proceeds in the following stages:
-- - construct all children -- just the boxes, no port connectors yet. if the children are themselves complex, we trust in the bounding boxes returned.
-- - for each child, position horizontally, centered or left/right aligned appropriately.
-- - position children vertically. usually this means spreading them out, with a gap between them. we do this by adding a topmargin to each bounding box
-- - flatten all the children into a single element. attach input and output horizontal lines to ports.
-- - return adjusted bounding box to caller.

data HAlignment = HLeft | HCenter | HRight   deriving (Eq, Show)
data VAlignment = VTop  | VMiddle | VBottom  deriving (Eq, Show)

-- | see page 1 of "box model" documentation
(>>>), (<<<), (\|/), (/|\) :: BoxedSVG -> Double -> BoxedSVG
(>>>) (bb,e) n = (bb { bbw = bbw bb + n, bblm = bblm bb + n }, move (  n,   0) e)
(<<<) (bb,e) n = (bb { bbw = bbw bb + n, bbrm = bbrm bb + n }, id              e)
(\|/) (bb,e) n = (bb { bbh = bbh bb + n, bbtm = bbtm bb + n }, move (  0,   n) e)
(/|\) (bb,e) n = (bb { bbh = bbh bb + n, bbbm = bbbm bb + n }, id              e)
infix 4 >>>, <<<, \|/, /|\

topText :: Maybe (Label a) -> Maybe a
topText = (=<<) maybeFirst

bottomText :: Maybe (Label a) -> Maybe a
bottomText = (=<<) maybeSecond

drawItemTiny :: AAVConfig -> Bool -> QTree T.Text -> BoxedSVG
drawItemTiny c negContext qt@(Node (Q _sv ao@(Simply _txt) pp m) childqs) = drawLeaf     c      negContext qt
drawItemTiny c negContext qt@(Node (Q _sv ao@(Neg)         pp m) childqs) = drawItemTiny c (not negContext) (head childqs)
drawItemTiny c negContext qt                                              = drawItemFull c      negContext   qt      -- [TODO]

alignV :: VAlignment -> Length -> BoxedSVG -> BoxedSVG
alignV VMiddle  mx (bb,x) = (bb { bbh = mx, bbtm = (mx - bbh bb) / 2, bbbm = (mx - bbh bb) / 2 }, move (0, (mx - bbh bb) / 2) x)
alignV VTop     mx (bb,x) = (bb { bbh = mx, bbtm = 0,                 bbbm = (mx - bbh bb) / 1 }, x)
alignV VBottom  mx (bb,x) = (bb { bbh = mx, bbtm = (mx - bbh bb) / 1, bbbm = 0                 }, move (0, (mx - bbh bb) / 1) x)

alignH :: HAlignment -> Length -> BoxedSVG -> BoxedSVG
alignH HCenter mx (bb,x) = (bb { bbw = mx, bblm = (mx - bbw bb) / 2, bbrm = (mx - bbw bb) / 2 }, move ((mx - bbw bb) / 2, 0) x)
alignH HLeft   mx (bb,x) = (bb { bbw = mx, bblm = 0,                 bbrm = (mx - bbw bb) / 1 }, x)
alignH HRight  mx (bb,x) = (bb { bbw = mx, bblm = (mx - bbw bb) / 1, bbrm = 0                 }, move ((mx - bbw bb) / 1, 0) x)

-- | see page 2 of the "box model" documentation.
-- | if we used the diagrams package all of this would be calculated automatically for us.
vAlign :: VAlignment -> [BoxedSVG] -> [BoxedSVG]
vAlign alignment elems = alignV alignment mx <$> elems
  where mx = maximum $ bbh . fst <$> elems

hAlign :: HAlignment -> [BoxedSVG] -> [BoxedSVG]
hAlign alignment elems = alignH alignment mx <$> elems
  where mx = maximum $ bbw . fst <$> elems

hlayout :: AAVConfig -> BoxedSVG -> BoxedSVG -> BoxedSVG
hlayout c (bbold,old) (bbnew,new) =
  ((defaultBBox (cscale c)) { bbh = max (bbh bbold) (bbh bbnew)
                            , bbw = bbw bbold + lrHgap + bbw bbnew
                            ,  pl = PVoffset (portL bbold myScale)
                            ,  pr = PVoffset (portR bbnew myScale)
                            }
  , old
    <> move (bbw bbold + lrHgap, 0) (rect_ [ X_ <<-* 0, Y_ <<-* 0, Width_ <<-* bbw bbnew , Height_ <<-* bbh bbnew + 5, Fill_ <<- "#f5f5f5", Stroke_ <<- "none" ] ) -- grayish tint
    <> move (bbw bbold + lrHgap + bblm bbnew, 0) (rect_ [ X_ <<-* 0, Y_ <<-* bbtm bbnew, Width_ <<-* (bbw bbnew - bblm bbnew - bbrm bbnew) , Height_ <<-* bbh bbnew - bbtm bbnew - bbbm bbnew, Fill_ <<- "#f8eeee", Stroke_ <<- "none" ] ) -- reddish tint
    <> move (bbw bbold + lrHgap + bblm bbnew, 0) new
    <> if (bbw bbold /= 0)
        then path_ [ D_ <<- (mA  (bbw bbold - bbrm bbold)  (portR bbold myScale) <>
                            (cR (bbrm bbold + lrHgap)              0
                              (   bbrm bbold                      ) (portL bbnew myScale - portR bbold myScale)
                              (   bbrm bbold + lrHgap + bblm bbnew) (portL bbnew myScale - portR bbold myScale)
                            ))
                  , Stroke_ <<- "red", Fill_ <<- "none" ]
        else mempty
  )
  where
    myScale     = getScale (cscale c)
    lrHgap      = slrh myScale

-- bezier curves: "M"            is the position of                       the first  point.
-- the first  argument after "c" is the position of the control point for the first  point, relative to the first point.
-- the second argument after "c" is the position of the control point for the second point, relative to the first point.
-- the third  argument after "c" is the position of                       the second point, relative to the first point.
vlayout :: AAVConfig -> BBox -> BoxedSVG -> BoxedSVG -> BoxedSVG
vlayout c parentbbox (bbold,old) (bbnew,new) =
  let parentPortIn  = portL parentbbox myScale + lrVgap
      parentPortOut = portR parentbbox myScale + lrVgap
      pathcolors    = [ Stroke_ <<- "green", Fill_ <<- "none" ]
      parent2child  = path_ ( [ D_ <<- (mA (-leftMargin)     (parentPortIn) <>
                                        (cA 0                 parentPortIn
                                            (-leftMargin)     (bbh bbold + lrVgap + portL bbnew myScale)
                                            (bblm bbnew)      (bbh bbold + lrVgap + portL bbnew myScale)
                                        )) ] ++ pathcolors ) <>
                      path_ ( [ D_ <<- (mA  (bbw parentbbox + rightMargin)  (parentPortOut) <>
                                        (cA (bbw parentbbox)                 parentPortOut
                                            (bbw parentbbox + rightMargin)   (bbh bbold + lrVgap + portR bbnew myScale)
                                            (bbw parentbbox - bbrm bbnew)    (bbh bbold + lrVgap + portR bbnew myScale)
                                        )) ] ++ pathcolors )
  in ((defaultBBox (cscale c)) { bbh = bbh bbold + bbh bbnew + lrVgap
                                , bbw = max (bbw bbold) (bbw bbnew)
                                }
      , old
        <> move (0, bbh bbold + lrVgap) new
        <> parent2child)
  where
    myScale     = getScale (cscale c)
    lrHgap      = slrh myScale
    lrVgap      = slrv myScale
    leftMargin  = slm myScale
    rightMargin = srm myScale

txtToBBE ::  AAVConfig -> T.Text -> BoxedSVG
txtToBBE c x = ( (defaultBBox (cscale c)) { bbh = boxHeight, bbw = boxWidth } {- [TODO] resizeHBox -}
              , text_ [ X_ <<-* 0, Y_ <<-* boxHeight / 2, Text_anchor_ <<- "middle", Dominant_baseline_ <<- "central", Fill_ <<- textFill ] (toElement x) )
  where
    (boxStroke, boxFill, textFill) = getColors True
    myScale     = getScale (cscale c)
    boxWidth    = sbw myScale
    boxHeight   = sbh myScale

combineOr :: AAVConfig -> Maybe BoxedSVG -> Maybe BoxedSVG -> [BoxedSVG] -> BoxedSVG
combineOr c mpre mpost elems =
  let childheights = lrVgap * (fromIntegral $ length elems - 1) +      (sum $ bbh . fst <$> elems)
      mybbox = (defaultBBox (cscale c)) { bbh = childheights, bbw = maximum ( bbw . fst <$> elems ) }
      layout = case cdirection c of
        LR -> vlayout c mybbox
        TB -> error "hlayout not yet implemented for Or"
      (childbbox, children) = foldl' layout (defaultBBox (cscale c), mempty) elems
  in (childbbox { bbw = bbw childbbox + leftMargin + rightMargin {- only for LR -} }, move (leftMargin, -lrVgap) children)
  where
    myScale     = getScale (cscale c)
    lrVgap      = slrv myScale
    leftMargin  = slm myScale
    rightMargin = srm myScale

combineAnd :: AAVConfig -> Maybe BoxedSVG -> Maybe BoxedSVG -> [BoxedSVG] -> BoxedSVG
combineAnd c mpre mpost elems =
  let layout = case cdirection c of
        LR -> hlayout c
        TB -> error "vlayout not yet implemented for And"
      (childbbox, children) = foldl1 layout elems
  in (childbbox { bbw = bbw childbbox + leftMargin + rightMargin
                , bblm = leftMargin, bbrm = rightMargin
                , pl = PVoffset (portL childbbox myScale)
                , pr = PVoffset (portR childbbox myScale)
                }
      , move (leftMargin, 0) (rect_ [ X_ <<-* 0, Y_ <<-* 0, Width_ <<-* bbw childbbox , Height_ <<-* bbh childbbox, Fill_ <<- "#e5e5f5", Stroke_ <<- "none"] <> -- blueish tint
                              rect_ [ X_ <<-* bblm childbbox, Y_ <<-* bbtm childbbox, Width_ <<-* (bbw childbbox - bblm childbbox - bbrm childbbox)
                                    , Height_ <<-* bbh childbbox - bbtm childbbox - bbbm childbbox, Fill_ <<- "#eaf5ea", Stroke_ <<- "none"] <> -- greenish tint
                              children ))
  where
    myScale     = getScale (cscale c)
    leftMargin  = slm myScale
    rightMargin = srm myScale

drawItemFull :: AAVConfig -> Bool -> QTree T.Text -> BoxedSVG
drawItemFull c negContext qt@(Node (Q  sv ao               pp m) childqs) =
  -- in a LR layout, each of the ORs gets a row below.
  -- we max up the bounding boxes and return that as our own bounding box.
  
  case ao of
       Or -> let rawChildren = drawItemFull c negContext <$> childqs
                 drawnChildren = case showLabels (cscale c) of
                   False -> combineOr c Nothing  Nothing  $ hAlign HCenter $ rawChildren
                   True  -> combineOr c topTextE botTextE $ hAlign HCenter $ rawChildren
             in (,) (defaultBBox (cscale c)) { bbw = leftMargin + rightMargin + (bbw.fst $ drawnChildren)
                                             , bbh =                            (bbh.fst $ drawnChildren) } 
                (snd drawnChildren)

       And -> let rawChildren = drawItemFull c negContext <$> childqs
                  drawnChildren = case showLabels (cscale c) of
                   False -> combineAnd c Nothing  Nothing  $ vAlign VTop $ rawChildren
                   True ->  combineAnd c topTextE botTextE $ vAlign VTop $ rawChildren
              in (,) (defaultBBox (cscale c)) { bbw = leftMargin + rightMargin + (bbw.fst $ drawnChildren) -- the toptext will move this a bit later
                                              , bbh = bbh.fst $ drawnChildren }
                 (snd drawnChildren)
       Simply _txt -> drawLeaf     c      negContext   qt
       Neg         -> drawItemFull c (not negContext) (head childqs)
     
    where
      myScale     = getScale (cscale c)
      boxWidth    = sbw myScale
      boxHeight   = sbh myScale
      leftMargin  = slm myScale
      rightMargin = srm myScale
      lrVgap      = slrv myScale
      lrHgap      = slrh myScale
      
      (boxStroke, boxFill, textFill) = getColors True

      topTextE = txtToBBE c <$> topText pp
      botTextE = txtToBBE c <$> bottomText pp

drawLeaf :: AAVConfig
         -> Bool -- ^ are we in a Neg context? i.e. parent was Negging to us
         -> QTree T.Text -- ^ the tree to draw
         -> BoxedSVG
drawLeaf c negContext qt@(Node q childqs) =
  let (boxStroke, boxFill, textFill) = getColors confidence
      notLine = if negContext then const FullLine else id
      (leftline, rightline, topline, confidence) = case mark q of
        Default (Right (Just True))  -> (HalfLine,  notLine HalfLine, not negContext, True)
        Default (Right (Just False)) -> (FullLine,  notLine NoLine,       negContext, True)
        Default (Right Nothing     ) -> (  NoLine,  notLine NoLine,            False, True)
        Default (Left  (Just True))  -> (HalfLine,  notLine HalfLine, not negContext, False)
        Default (Left  (Just False)) -> (FullLine,  notLine NoLine,       negContext, False)
        Default (Left  Nothing     ) -> (  NoLine,  notLine NoLine,            False, False)
      boxContents = if cscale c == Tiny
                    then (circle_ [Cx_  <<-* (boxWidth  / 2) ,Cy_      <<-* (boxHeight / 2) , R_ <<-* (boxWidth / 3), Fill_ <<- textFill ] )
                    else   (text_ [ X_  <<-* (boxWidth  / 2) , Y_      <<-* (boxHeight / 2) , Text_anchor_ <<- "middle" , Dominant_baseline_ <<- "central" , Fill_ <<- textFill ] (toElement mytext))
  in
  (,) (defaultBBox (cscale c)) { bbw = boxWidth, bbh = boxHeight } $
     rect_ [ X_      <<-* 0 , Y_      <<-* 0 , Width_  <<-* boxWidth , Height_ <<-* boxHeight , Stroke_ <<-  boxStroke , Fill_   <<-  boxFill ]
  <> boxContents
  <> (if leftline  == HalfLine then line_ [ X1_ <<-* 0        , Y1_ <<-* 0, X2_ <<-* 0         , Y2_ <<-* boxHeight / 2 , Stroke_ <<- "black" ] else mempty)
  <> (if rightline == HalfLine then line_ [ X1_ <<-* boxWidth , Y1_ <<-* 0, X2_ <<-* boxWidth  , Y2_ <<-* boxHeight / 2 , Stroke_ <<- "black" ] else mempty)
  <> (if leftline  == FullLine then line_ [ X1_ <<-* 0        , Y1_ <<-* 0, X2_ <<-* 0         , Y2_ <<-* boxHeight     , Stroke_ <<- "black" ] else mempty)
  <> (if rightline == FullLine then line_ [ X1_ <<-* boxWidth , Y1_ <<-* 0, X2_ <<-* boxWidth  , Y2_ <<-* boxHeight     , Stroke_ <<- "black" ] else mempty)
  <> (if topline               then line_ [ X1_ <<-* 0        , Y1_ <<-* 0, X2_ <<-* boxWidth  , Y2_ <<-* 0             , Stroke_ <<- "black" ] else mempty)
  where
    boxHeight        = sbh (getScale (cscale c))
    defBoxWidth      = sbw (getScale (cscale c))
    boxWidth         = defBoxWidth - 15 + (3 * fromIntegral (length (show mytext)))
    mytext = case andOr q of
      (Simply txt) -> txt
      (Neg)        -> "neg..."
      (And)        -> "and..."
      (Or)         -> "or..."

  -- itemBox c 0 0 ao mark children False






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
  { bs -- | box stroke
  , bf -- | box fill
  , tf -- | text fill
  , ll -- | left  "negation" line -- the marking is False
  , rl -- | right "negation" line -- we are drawing a Not element
  , tl -- | top "truth" line -- drawn if the value is true, or if the marking is false and the item is a Not
    :: T.Text
  }

-- | the color scheme depends on the marking
colorScheme :: AAVConfig
            -> Default Bool
            -> Boolean   -- | iff we got here via a Not, this value is True
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

line :: (Double , Double) -> (Double, Double) -> SVGElement
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
        <> move (0, h) (renderChain c hgs)  )

renderLeaf :: (ToElement a) => AAVConfig -> a -> (OldBBox, SVGElement)
renderLeaf c desc =
  let height = 25
      geom = item c 0 0 desc
  in ((25,height), geom)

renderNot :: (ToElement a) => AAVConfig -> [Item a] -> (OldBBox, SVGElement)
renderNot c children =
  let
      ((w,h), g) = renderItem c $ head children
      height = h

      geom :: SVGElement
      geom = g_ [] ( line (-5, 5) (-10, 15)  --  /
                     <> line (10,0) (10,25)  --  |
                     <> move (00, 0) g )
  in ((w,height), geom)


renderSuffix :: (ToElement a) => AAVConfig -> Double -> Double -> a -> (OldBBox, SVGElement)
renderSuffix c x y desc =
  let h = 20 -- h/w of imaginary box
      geom :: SVGElement
      geom = g_ [] ( text_ [ X_ <<-* x, Y_ <<-* (y + h - 5) ] (toElement desc) )
  in ((25,h), geom)

renderAll :: (ToElement a) => AAVConfig -> Maybe (Label T.Text) -> [Item a] -> (OldBBox, SVGElement)
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
                   <> move (30, 30) (renderChain c hg)
                   <> move (40, 30 + sum (snd <$> hs)) fg  )
  in ((width,height), geom)

renderAny :: (ToElement a) => AAVConfig -> Maybe (Label T.Text) -> [Item a] -> (OldBBox, SVGElement)
renderAny c Nothing childnodes = renderAny c (Just (Pre "any of:")) childnodes
renderAny c (Just (Pre prefix)) childnodes =
  let hg = map (renderItem c) childnodes
      (hs, gs) = unzip hg

      width = 25
      height = sum (snd <$> hs) + 25

      geom :: SVGElement
      geom = g_ [] (  item c 0 0 prefix
                   <> line (10, 20) (10, sum (init (snd <$> hs)) + 25 + 10)
                   <> move (30, 25) (go 0 hg)  )
                 where go y [] = mempty
                       go y (((w,h),g):hgs) =
                         g_ [] (  g
                               <> line (-20, 10) (0, 10)
                               <> move (0, h) (go (y+h) hgs)  )
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
                   <> move (30, 25) (go 0 hg)
                   <> move (40, 25 + sum (snd <$> hs)) fg)
                 where go y [] = mempty
                       go y (((w,h),g):hgs) =
                         g_ [] (  g
                               <> line (-20, 10) (0, 10)
                               <> move (0, h) (go (y+h) hgs)  )
  in ((width, height), geom)


renderItem :: (ToElement a) => AAVConfig -> Item a -> (OldBBox, SVGElement)
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
