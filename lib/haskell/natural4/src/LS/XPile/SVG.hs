{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LS.XPile.SVG where

import LS
import AnyAll as AA
import qualified Data.Map as Map
import qualified Data.Text as T

-- extract the tree-structured rules from Interpreter
-- for each rule, print as svg according to options we were given
asAAsvg :: AAVConfig -> Interpreted -> [Rule] -> Map.Map RuleName (SVGElement, SVGElement, ItemMaybeLabel T.Text, QTree T.Text)
asAAsvg aavc l4i rs =
  let rs' = stitchRules l4i rs -- connect up the rules internally, expand HENCE and LEST rulealias links, expand defined terms
  in Map.fromList [ (rn, (svgtiny, svgfull, aaT, qtree))
                  | r <- rs'
                  , let rn      = ruleLabelName r
                        aaT     = getAndOrTree l4i r
                        qtree   = hardnormal (cgetMark aavc) aaT
                        svgtiny = makeSvg $ q2svg' aavc { cscale = Tiny } qtree
                        svgfull = makeSvg $ q2svg' aavc { cscale = Full } qtree
                  ]
