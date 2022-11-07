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
asAAsvg aavc l4i rs =
  let _rs = stitchRules l4i rs -- connect up the rules internally, expand HENCE and LEST rulealias links, expand defined terms
      rs2 = groupedByAOTree l4i rs
  in Map.fromList [ (rn ++ if length totext > 1
                           then [T.pack (show rn_n)]
                           else []
                    , (svgtiny, svgfull, aaT, qtree))
                  | (_,rulegroup) <- rs2
                  , not $ null rulegroup
                  , let r = Prelude.head rulegroup
                        rn      = ruleLabelName r
                        ebsr = expandBSR l4i 1 <$>
                               -- trace ("asAAsvg getBSR = " ++ show (getBSR r)) $
                               getBSR r
                        totext = filter isInteresting $ fmap rp2text <$> -- trace ("asAAsvg expandBSR = " ++ show ebsr)
                                 ebsr
                  , not (isRuleAlias l4i rn)
                  , (rn_n, aaT) <- zip [1::Int ..] --  $ trace ("asAAsvg aaT <- totext = " ++ show totext)
                                   totext
                  , let qtree   = hardnormal (cgetMark aavc) --  $ trace ("asAAsvg aaT = " ++ show aaT)
                                  aaT
                        svgtiny = makeSvg $ q2svg' aavc { cscale = Tiny } qtree
                        svgfull = makeSvg $ q2svg' aavc { cscale = Full } qtree
                  ]
  where
    -- | don't show SVG diagrams if they only have a single element
    isInteresting :: BoolStruct lbl a -> Bool
    isInteresting (AA.Leaf _) = False
    isInteresting (AA.Not (AA.Leaf _)) = False
    isInteresting _ = True
