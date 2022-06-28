{-# LANGUAGE OverloadedStrings #-}

module LS.XPile.AA where

import LS
import AnyAll
import qualified Data.Text     as T
import qualified Data.Text       (Text)
import qualified Data.Map      as Map
import qualified Data.Map        (Map)
import Data.Maybe

-- | a module to export the conditional logic embedded in rules.

-- for example, a Regulative rule has WHO and WHEN preconditions.
-- this module organizes those preconditions into an AnyAll boolstruct
-- and obtains an SVG visualization from the AnyAll library.

-- a Hornlike rule has the `clauses` attribute which contains a BoolStructR,
-- which can be intimidatingly complex. This module organizes the preconditions
-- in the BoolStructR -- the hBody parts, basically -- and sends them for rendering.

-- along the way, we (optionally) expand the term graph -- if a condition tree2native-- includes the term "degustates" and we know that "degustates" links to its own
-- definition, then we (if follow_links is turned on) expand the "degustates",
-- making the render larger.

toAAsvg :: Interpreted -> [Rule] -> Maybe (Marking T.Text) -> Rule -> SVGElement
toAAsvg l4i rs mm r =
  let aatree = getAndOrTree r
      mkg = fromMaybe (Marking Map.empty) mm
  in makeSvg $
     q2svg' defaultAAVConfig $
     hardnormal mkg aatree

