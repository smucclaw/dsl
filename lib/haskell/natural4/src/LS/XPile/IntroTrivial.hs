{-| a trivial transpiler, part of the Introductory series
-}

module LS.XPile.IntroTrivial where

import LS.Interpreter
       ( qaHornsT )
import LS.PrettyPrinter
       ( tildes, (</>), vvsep, myrender )
import LS.Rule
       ( Interpreted(..),
         Rule(clauses, given) )
import LS.Types ( unCT )
import LS.PrettyPrinter

import Prettyprinter
       ( vsep, viaShow, hsep, emptyDoc, (<+>), Pretty(pretty), Doc )
import Text.Pretty.Simple ( pShowNoColor )
import Data.HashMap.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.List (nub)
import Data.Bifunctor (first)
import Data.Graph.Inductive (prettify)
import Data.Text qualified as Text



toTrivial :: Interpreted -> String
toTrivial l4i = do
  let rules = origrules l4i
  "output from the IntroTrivial transpiler"
  
