{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-| transpiler to SVG visualization of the AnyAll and/or trees.

Largely a wrapper. Most of the functionality is in the anyall lib.

-}

module LS.XPile.Purescript where

import LS
import AnyAll.BoolStruct (alwaysLabeled)
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
     show (vsep
           [ "toplevelDecisions :: Map.Map (String) (Item String)"
           , "toplevelDecisions = Map.fromFoldable " <>
             (pretty $ TL.unpack (
                 pShowNoColor
                   [ toTuple ( T.intercalate " / " (T.unwords <$> names)
                             , alwaysLabeled bs)
                   | (names,bs) <- qaHornsT l4i
                   ]
                 )
             )
           , "toplevelDefaultMarking :: Marking"
           , "toplevelDefaultMarking = Marking (Map.fromFoldable " <>
             "[]" <>
             ")"
           ]
          )
