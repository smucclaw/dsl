{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LS.XPile.SVG where

import LS
import AnyAll as AA
import qualified Data.Map as Map

-- extract the tree-structured rules from Interpreter
-- for each rule, print as svg according to options we were given
asAAsvg :: AAVConfig -> Interpreted -> [Rule] -> Map.Map RuleName SVGElement
asAAsvg aavc l4i rs =
  let rs' = stitchRules l4i rs -- connect up the rules internally, expand HENCE and LEST rulealias links, expand defined terms
  in Map.fromList [ (rn, svge)
                  | r <- rs'
                  , let rn = ruleLabelName r
                        aaMT = getAndOrTree rs r
                        aaT = mt2text <$> aaMT
                        svge = makeSvg $ q2svg' aavc (hardnormal (cgetMark aavc) aaT)
                  ]
