{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

-- | A visualization inspired by Ladder Logic and by Layman Allen (1978).

module AnyAll.SVGLadder where

import AnyAll.Types hiding ((<>))

import Data.String
import Graphics.Svg
import qualified Data.Text as T
import qualified Data.Text.Lazy       as TL

type Height = Double
type Width  = Double
type BBox = (Width, Height)

(<<-*) :: Show a => AttrTag -> a -> Attribute
(<<-*) tag a = bindAttr tag (T.pack (show a))

makeSvg :: (BBox, Element) -> Element
makeSvg ((width, height), geom) =
     doctype
  <> with (svg11_ geom) [Version_ <<- "1.1", Height_ <<-* height]

box :: Double -> Double -> Double -> Double -> Element
box x y w h =
  rect_ [ X_ <<-* x, Y_ <<-* y, Width_ <<-* w, Height_ <<-* h
        , Fill_ <<- "none", Stroke_ <<- "black" ]

line :: (Double , Double) -> (Double, Double) -> Element
line (x1, y1) (x2, y2) =
  line_ [ X1_ <<-* x1, X2_ <<-* x2, Y1_ <<-* y1, Y2_ <<-* y2
        , Stroke_ <<- "grey" ]

item :: ToElement a => Double -> Double -> a -> Element
item x y desc =
  let w = 20
  in
    g_ [] (  box x y w w
          <> text_ [ X_ <<-* (x + w + 5), Y_ <<-* (y + w - 5) ] (toElement desc)  )

move :: (Double, Double) -> Element -> Element
move (x, y) geoms =
  with geoms [Transform_ <<- translate x y]

renderChain :: [(BBox, Element)] -> Element
renderChain [] = mempty
renderChain [(_,g)] = g
renderChain (((w,h),g):hgs) =
  g_ [] (  g
        <> line (10, 20) (10, h)
        <> move (0, h) (renderChain hgs)  )

renderLeaf :: (ToElement a) => a -> (BBox, Element)
renderLeaf desc =
  let height = 25
      geom = item 0 0 desc
  in ((25,height), geom)

renderNot :: (ToElement a) => [Item a] -> (BBox, Element)
renderNot children =
  let
      ((w,h), g) = renderItem $ head children
      height = h

      geom :: Element
      geom = g_ [] ( line (-5, 5) (-10, 15)  --  /
                     <> line (10,0) (10,25)  --  |
                     <> move (00, 0) g )
  in ((w,height), geom)


renderSuffix :: (ToElement a) => Double -> Double -> a -> (BBox, Element)
renderSuffix x y desc =
  let h = 20 -- h/w of imaginary box
      geom :: Element
      geom = g_ [] ( text_ [ X_ <<-* x, Y_ <<-* (y + h - 5) ] (toElement desc) )
  in ((25,h), geom)

renderAll :: (ToElement a) => Maybe (Label TL.Text) -> [Item a] -> (BBox, Element)
renderAll Nothing childnodes = renderAll allof childnodes
renderAll (Just (Pre prefix)) childnodes =
  let
      hg = map renderItem childnodes
      (hs, gs) = unzip hg

      width = 25
      height = sum (snd <$> hs) + 30

      geom :: Element
      geom = g_ [] (  item 0 0 prefix
                   -- elbow connector
                   <> line (10, 20) (10, 25)
                   <> line (10, 25) (40, 25)
                   <> line (40, 25) (40, 30)
                   -- children translated by (30, 30)
                   <> move (30, 30) (renderChain hg)  )
  in ((width,height), geom)
renderAll (Just (PrePost prefix suffix)) childnodes =
  let hg = map renderItem childnodes
      (hs, gs) = unzip hg

      ((fw, fh), fg) = renderSuffix 0 0 suffix

      width = 25
      height = sum (snd <$> hs) + fh + 30

      geom :: Element
      geom = g_ [] (  item 0 0 prefix
                   <> line (10, 20) (10, 25)
                   <> line (10, 25) (40, 25)
                   <> line (40, 25) (40, 30)
                   <> move (30, 30) (renderChain hg)
                   <> move (40, 30 + sum (snd <$> hs)) fg  )
  in ((width,height), geom)

renderAny :: (ToElement a) => Maybe (Label TL.Text) -> [Item a] -> (BBox, Element)
renderAny Nothing childnodes = renderAny (Just (Pre "any of:")) childnodes
renderAny (Just (Pre prefix)) childnodes =
  let hg = map renderItem childnodes
      (hs, gs) = unzip hg

      width = 25
      height = sum (snd <$> hs) + 25

      geom :: Element
      geom = g_ [] (  item 0 0 prefix
                   <> line (10, 20) (10, sum (init (snd <$> hs)) + 25 + 10)
                   <> move (30, 25) (go 0 hg)  )
                 where go y [] = mempty
                       go y (((w,h),g):hgs) =
                         g_ [] (  g
                               <> line (-20, 10) (0, 10)
                               <> move (0, h) (go (y+h) hgs)  )
  in ((width,height), geom)
renderAny (Just (PrePost prefix suffix)) childnodes =
  let hg = map renderItem childnodes
      (hs, gs) = unzip hg

      ((fw,fh), fg) = renderSuffix 0 0 suffix

      width = 25
      height = sum (snd <$> hs) + fh + 25

      geom :: Element
      geom = g_ [] (  item 0 0 prefix
                   <> line (10, 20) (10, sum (snd <$> init hs) + 25 + 10)
                   <> move (30, 25) (go 0 hg)
                   <> move (40, 25 + sum (snd <$> hs)) fg)
                 where go y [] = mempty
                       go y (((w,h),g):hgs) =
                         g_ [] (  g
                               <> line (-20, 10) (0, 10)
                               <> move (0, h) (go (y+h) hgs)  )
  in ((width, height), geom)


renderItem :: (ToElement a) => Item a -> (BBox, Element)
renderItem (Leaf label) = renderLeaf label
renderItem (Not       args) = renderNot      [args]
renderItem (All label args) = renderAll label args
renderItem (Any label args) = renderAny label args

toy :: (BBox, Element)
toy = renderItem $
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
