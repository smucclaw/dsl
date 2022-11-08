{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-| transpiler to SVG visualization of the AnyAll and/or trees.

Largely a wrapper. Most of the functionality is in the anyall lib.

-}

module LS.XPile.SVG where

import LS
import AnyAll as AA
import qualified Data.Map as Map
import qualified Data.Text as T
-- import Debug.Trace (trace)

-- | extract the tree-structured rules from Interpreter
-- for each rule, print as svg according to options we were given

asAAsvg :: AAVConfig -> Interpreted -> [Rule] -> Map.Map RuleName (SVGElement, SVGElement, BoolStructT, QTree T.Text)
asAAsvg aavc l4i _rs =
  Map.fromList [ ( T.unwords <$> names
                 , (svgtiny, svgfull, bs, qtree) )
               | (names, bs) <- qaHornsT l4i
               , isInteresting bs
               , let qtree   = hardnormal (cgetMark aavc) --  $ trace ("asAAsvg aaT = " ++ show aaT)
                               bs
                     svgtiny = makeSvg $ q2svg' aavc { cscale = Tiny } qtree
                     svgfull = makeSvg $ q2svg' aavc { cscale = Full } qtree
               ]
  where
    -- | don't show SVG diagrams if they only have a single element
    isInteresting :: BoolStruct lbl a -> Bool
    isInteresting (AA.Leaf _) = False
    isInteresting (AA.Not (AA.Leaf _)) = False
    isInteresting _ = True
