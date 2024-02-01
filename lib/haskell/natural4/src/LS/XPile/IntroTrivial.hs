{-| a trivial transpiler, part of the Introductory series
-}

module LS.XPile.IntroTrivial where

import Data.Bifunctor (first)
import Data.Graph.Inductive (prettify)
import Data.HashMap.Strict qualified as Map
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Text qualified as Text
import LS.Interpreter
  ( qaHornsT,
  )
import LS.Rule
  ( Interpreted (..),
    Rule (clauses, given),
  )
import LS.Types (unCT)
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    emptyDoc,
    hsep,
    viaShow,
    vsep,
    (<+>),
  )
import Text.Pretty.Simple (pShowNoColor)



toTrivial :: Interpreted -> String
toTrivial l4i = do
  let rules = origrules l4i
  "output from the IntroTrivial transpiler"

