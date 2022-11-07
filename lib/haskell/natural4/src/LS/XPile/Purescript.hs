{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-| transpiler to SVG visualization of the AnyAll and/or trees.

Largely a wrapper. Most of the functionality is in the anyall lib.

-}

module LS.XPile.Purescript where

import LS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Prettyprinter
import Text.Pretty.Simple (pShowNoColor)

-- import Debug.Trace (trace)

-- | extract the tree-structured rules from Interpreter
-- currently: construct a Data.Map of rulenames to exposed decision root expanded BSR
-- in future: also ship out a Marking which represents the TYPICALLY values
-- far future: construct a JSON with everything in it, and get the Purescript to read the JSON, so we are more interoperable with non-FP languages

data Tuple a b = Tuple a b
  deriving (Show, Eq, Ord)

toTuple :: (a,b) -> Tuple a b
toTuple (x,y) = Tuple x y

asPurescript :: Interpreted -> String
asPurescript l4i =
  let rs1 = exposedRoots l4i -- connect up the rules internally, expand HENCE and LEST rulealias links, expand defined terms
      rs2 = groupedByAOTree l4i rs1
  in show (vsep
           [ "toplevel :: Map.Map (List String) (Item String)"
           , "toplevel = Map.fromFoldable "]) ++
     TL.unpack ( pShowNoColor
                 [ toTuple (rn ++ [ T.pack (show rn_n) | length totext > 1 ]
                   , aaT)
                 | (_mbst,rulegroup) <- rs2
                 , not $ null rulegroup
                 , let r = Prelude.head rulegroup
                       rn      = ruleLabelName r
                       ebsr = expandBSR l4i 1 <$> getBSR r
                       totext = fmap rp2text <$> -- trace ("asAAsvg expandBSR = " ++ show ebsr)
                                ebsr
                 , (rn_n, aaT) <- zip [1::Int ..] --  $ trace ("asAAsvg aaT <- totext = " ++ show totext)
                                  totext
                 ]
     )
