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
asAAsvg :: AAVConfig -> Interpreted -> [Rule] -> Map.Map RuleName (SVGElement, ItemMaybeLabel T.Text)
asAAsvg aavc l4i rs =
  let rs' = stitchRules l4i rs -- connect up the rules internally, expand HENCE and LEST rulealias links, expand defined terms
  in Map.fromList [ (rn, (svge, aaT))
                  | r <- rs'
                  , let rn = ruleLabelName r
                        aaT = getAndOrTree rs r
                        svge = makeSvg $ q2svg' aavc (hardnormal (cgetMark aavc) aaT)
                  ]
