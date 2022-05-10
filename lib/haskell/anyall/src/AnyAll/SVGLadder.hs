{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

-- | A visualization inspired by Ladder Logic and by Layman Allen (1978).

module AnyAll.SVGLadder where

import AnyAll.Types hiding ((<>))

import Data.String
import Graphics.Svg
import qualified Data.Text as T
import qualified Data.Text.Lazy       as TL
import qualified Data.Map as Map
import Data.Tree

type Height = Double
type Width  = Double
type BBox = (Width, Height)

-- | how compact should the output be?
data Scale = Tiny  -- @ ---o---
           | Small -- @ --- [1.1] ---
           | Full  -- @ --- the first item ---
           deriving (Show, Eq)

-- | how is a particular widget to be laid out?
data Direction = LR -- ^ left-to-right
               | TB -- ^ top-to-bottom
               deriving (Show, Eq)

data AAVConfig = AAVConfig
  { cscale       :: Scale
  , cdirection   :: Direction
  , cgetMark     :: Marking TL.Text
  }
  deriving (Show, Eq)

defaultAAVConfig :: AAVConfig
defaultAAVConfig = AAVConfig
  { cscale = Tiny
  , cdirection = LR
  , cgetMark = Marking Map.empty
  }

type ItemStyle = Maybe Bool

(<<-*) :: Show a => AttrTag -> a -> Attribute
(<<-*) tag a = bindAttr tag (T.pack (show a))

makeSvg' :: AAVConfig -> (BBox, Element) -> Element
makeSvg' c = makeSvg

makeSvg :: (BBox, Element) -> Element
makeSvg ((width, height), geom) =
     doctype
  <> with (svg11_ geom) [Version_ <<- "1.1" ]


data LineHeight = NoLine | HalfLine | FullLine
  deriving (Eq, Show)

q2svg :: AAVConfig -> QTree TL.Text -> Element
q2svg c qt = snd $ q2svg' c qt

-- need to add a param if the parent was a Not
q2svg' :: AAVConfig -> QTree TL.Text -> (BBox, Element)
q2svg' c qt@(Node q childqs) =
  drawItem c qt 

drawItem, drawItemTiny, drawItemFull :: AAVConfig
         -> QTree TL.Text
         -> (BBox, Element)
drawItem c qt
  | cscale c == Tiny = drawItemTiny c qt
  | otherwise        = drawItemFull c qt

drawItemTiny c qt@(Node (Q _sv ao@(Simply _txt) pp m) childqs) = drawLeaf c qt             6  10  6 10   8  8 False
drawItemTiny c qt@(Node (Q _sv ao@(Neg)         pp m) childqs) = drawLeaf c (head childqs) 6  10  6 10   8  8 True
drawItemTiny c qt                                              = drawItemFull c qt
drawItemFull c qt@(Node (Q _sv ao@(Simply _txt) pp m) childqs) = drawLeaf c qt             22 20 22 20 120 44 False
drawItemFull c qt@(Node (Q _sv ao@(Neg        ) pp m) childqs) = drawLeaf c (head childqs) 22 20 22 20 120 44 True
drawItemFull c qt@(Node (Q _sv ao@And           pp m) childqs) = drawLeaf c (head childqs) 22 20 22 20 120 44 False -- [TODO]
drawItemFull c qt@(Node (Q _sv ao@Or            pp m) childqs) = drawLeaf c (head childqs) 22 20 22 20 120 44 False -- [TODO]

drawLeaf :: AAVConfig -> QTree TL.Text
         -> Int -- ^    topMargin
         -> Int -- ^  rightMargin
         -> Int -- ^ bottomMargin
         -> Int -- ^   leftMargin
         -> Int -- ^    boxWidth
         -> Int -- ^    boxHeight
         -> Bool       -- ^ are we in a Neg context? i.e. parent was Negging to us
         -> (BBox, Element)
drawLeaf c qt@(Node q childqs)
  topMargin rightMargin bottomMargin leftMargin
  boxWidth boxHeight
  negContext=
  let (boxStroke, boxFill, textFill) = case confidence of
        True  -> ("none", "none", "black")
        False -> ("none", "lightgrey", "white")
      mytext = case andOr q of
        (Simply txt) -> fromString (TL.unpack txt)
        (Neg)        -> "neg..."
        (And)        -> "and..."
        (Or)         -> "or..."
      notLine x = if negContext then FullLine else x
      (leftline, rightline, topline, confidence) = case mark q of
        Default (Right (Just True))  -> (HalfLine,  notLine HalfLine, not negContext, True)
        Default (Right (Just False)) -> (FullLine,  notLine NoLine,       negContext, True)
        Default (Right Nothing     ) -> (  NoLine,  notLine NoLine,            False, True)
        Default (Left  (Just True))  -> (HalfLine,  notLine HalfLine, not negContext, False)
        Default (Left  (Just False)) -> (FullLine,  notLine NoLine,       negContext, False)
        Default (Left  Nothing     ) -> (  NoLine,  notLine NoLine,            False, False)
      boxContents = if cscale c == Tiny
                    then          (circle_ [Cx_  <<-* (boxWidth  `div` 2 + leftMargin) ,Cy_      <<-* (boxHeight `div` 2 + topMargin) , R_ <<-* (boxWidth `div` 3), Fill_ <<- textFill ] )
                    else            (text_ [ X_  <<-* (boxWidth  `div` 2 + leftMargin) , Y_      <<-* (boxHeight `div` 2 + topMargin) , Text_anchor_ <<- "middle" , Dominant_baseline_ <<- "central" , Fill_ <<- textFill ] mytext)
  in
  (,) (fromIntegral $ leftMargin + boxWidth + rightMargin, fromIntegral $ topMargin + boxHeight + bottomMargin) $
     rect_ [ X_      <<-* leftMargin , Y_      <<-* topMargin , Width_  <<-* boxWidth , Height_ <<-* boxHeight , Stroke_ <<-  boxStroke , Fill_   <<-  boxFill ]
  <> boxContents
  <>                                line_ [ X1_ <<-* 0                       , Y1_ <<-* (topMargin + boxHeight `div` 2) , X2_     <<-* leftMargin ,                            Y2_ <<-* (topMargin + boxHeight `div` 2) , Stroke_    <<- "black" ] -- LR: line in on the left
  <>                                line_ [ X1_ <<-* (leftMargin + boxWidth) , Y1_ <<-* (topMargin + boxHeight `div` 2) , X2_     <<-* (leftMargin + boxWidth + rightMargin) , Y2_ <<-* (topMargin + boxHeight `div` 2) , Stroke_    <<- "black" ] -- LR: line out on the right
  <> (if leftline  == HalfLine then line_ [ X1_ <<-* leftMargin              , Y1_ <<-* topMargin ,                       X2_ <<-* leftMargin                                , Y2_ <<-* (topMargin + boxHeight `div` 2) , Stroke_ <<- "black" ] else mempty)
  <> (if rightline == HalfLine then line_ [ X1_ <<-* (leftMargin + boxWidth) , Y1_ <<-* topMargin ,                       X2_ <<-* (leftMargin + boxWidth)                   , Y2_ <<-* (topMargin + boxHeight `div` 2) , Stroke_ <<- "black" ] else mempty)
  <> (if leftline  == FullLine then line_ [ X1_ <<-* leftMargin              , Y1_ <<-* topMargin ,                       X2_ <<-* leftMargin                                , Y2_ <<-* (topMargin + boxHeight)         , Stroke_ <<- "black" ] else mempty)
  <> (if rightline == FullLine then line_ [ X1_ <<-* (leftMargin + boxWidth) , Y1_ <<-* topMargin ,                       X2_ <<-* (leftMargin + boxWidth)                   , Y2_ <<-* (topMargin + boxHeight)         , Stroke_ <<- "black" ] else mempty)
  <> (if topline               then line_ [ X1_ <<-* leftMargin              , Y1_ <<-* topMargin ,                       X2_ <<-* (leftMargin + boxWidth)                   , Y2_ <<-* topMargin                       , Stroke_ <<- "black" ] else mempty)




  -- itemBox c 0 0 ao mark children False






type Boolean = Bool

itemBox :: AAVConfig
        -> Double          -- ^ x top left
        -> Double          -- ^ y top left
        -> AndOr TL.Text   -- ^ Item, recast as an AndOr Text for display
        -> Default Bool    -- ^ mark for the box
        -> [QTree TL.Text] -- ^ children
        -> Bool            -- ^ did we get here because we were contained by a Neg?
        -> (BBox, Element)
itemBox c x y Neg m cs amNot = itemBox c x y (andOr $ rootLabel $ head cs) m [] False
itemBox c x y (Simply t)  m cs amNot
  | cscale c  == Tiny  = (,) (10,10) $ g_ [] ( rect_ [ X_ <<-* x, Y_ <<-* y, Width_ <<-* 10, Height_ <<-* 10, Stroke_ <<- "red", Fill_ <<- "green" ] )
-- [TODO] small
  | cscale c  `elem` [Full,Small]  = (,) (fromIntegral $ TL.length t * 3, 25) $ g_ [] (
      rect_ [ X_ <<-* x      , Y_ <<-* y, Width_ <<-* 10, Height_ <<-* 10, Stroke_ <<- "red", Fill_ <<- "green" ]
        <> mempty ) -- some text
itemBox c x y andor m cs amNot = (,) (fromIntegral $ 25, 25) $ g_ [] (
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
  

box :: AAVConfig -> Double -> Double -> Double -> Double -> Element
box c x y w h =
  rect_ [ X_ <<-* x, Y_ <<-* y, Width_ <<-* w, Height_ <<-* h
        , Fill_ <<- "none", Stroke_ <<- "black" ]

line :: (Double , Double) -> (Double, Double) -> Element
line (x1, y1) (x2, y2) =
  line_ [ X1_ <<-* x1, X2_ <<-* x2, Y1_ <<-* y1, Y2_ <<-* y2
        , Stroke_ <<- "grey" ]

item :: ToElement a => AAVConfig -> Double -> Double -> a -> Element
item c x y desc =
  let w = 20
  in
    g_ [] (  box c x y w w
          <> text_ [ X_ <<-* (x + w + 5), Y_ <<-* (y + w - 5) ] (toElement desc)  )

move :: (Double, Double) -> Element -> Element
move (x, y) geoms =
  with geoms [Transform_ <<- translate x y]

renderChain :: AAVConfig -> [(BBox, Element)] -> Element
renderChain c [] = mempty
renderChain c [(_,g)] = g
renderChain c (((w,h),g):hgs) =
  g_ [] (  g
        <> line (10, 20) (10, h)
        <> move (0, h) (renderChain c hgs)  )

renderLeaf :: (ToElement a) => AAVConfig -> a -> (BBox, Element)
renderLeaf c desc =
  let height = 25
      geom = item c 0 0 desc
  in ((25,height), geom)

renderNot :: (ToElement a) => AAVConfig -> [Item a] -> (BBox, Element)
renderNot c children =
  let
      ((w,h), g) = renderItem c $ head children
      height = h

      geom :: Element
      geom = g_ [] ( line (-5, 5) (-10, 15)  --  /
                     <> line (10,0) (10,25)  --  |
                     <> move (00, 0) g )
  in ((w,height), geom)


renderSuffix :: (ToElement a) => AAVConfig -> Double -> Double -> a -> (BBox, Element)
renderSuffix c x y desc =
  let h = 20 -- h/w of imaginary box
      geom :: Element
      geom = g_ [] ( text_ [ X_ <<-* x, Y_ <<-* (y + h - 5) ] (toElement desc) )
  in ((25,h), geom)

renderAll :: (ToElement a) => AAVConfig -> Maybe (Label TL.Text) -> [Item a] -> (BBox, Element)
renderAll c Nothing childnodes = renderAll c allof childnodes
renderAll c (Just (Pre prefix)) childnodes =
  let
      hg = map (renderItem c) childnodes
      (hs, gs) = unzip hg

      width = 25
      height = sum (snd <$> hs) + 30

      geom :: Element
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

      geom :: Element
      geom = g_ [] (  item c 0 0 prefix
                   <> line (10, 20) (10, 25)
                   <> line (10, 25) (40, 25)
                   <> line (40, 25) (40, 30)
                   <> move (30, 30) (renderChain c hg)
                   <> move (40, 30 + sum (snd <$> hs)) fg  )
  in ((width,height), geom)

renderAny :: (ToElement a) => AAVConfig -> Maybe (Label TL.Text) -> [Item a] -> (BBox, Element)
renderAny c Nothing childnodes = renderAny c (Just (Pre "any of:")) childnodes
renderAny c (Just (Pre prefix)) childnodes =
  let hg = map (renderItem c) childnodes
      (hs, gs) = unzip hg

      width = 25
      height = sum (snd <$> hs) + 25

      geom :: Element
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

      geom :: Element
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


renderItem :: (ToElement a) => AAVConfig -> Item a -> (BBox, Element)
renderItem c (Leaf label)     = renderLeaf c label
renderItem c (Not       args) = renderNot c      [args]
renderItem c (All label args) = renderAll c label args
renderItem c (Any label args) = renderAny c label args

toy :: (BBox, Element)
toy = renderItem defaultAAVConfig $
  All (Just $ PrePost "You need all of" ("to survive." :: TL.Text))
      [ Leaf ("Item 1;" :: TL.Text)
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
