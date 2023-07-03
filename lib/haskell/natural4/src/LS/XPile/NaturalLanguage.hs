-- | example of a simple transpiler

module LS.XPile.NaturalLanguage where

import AnyAll as AA
import Data.Map qualified as Map
import Data.Text qualified as T
import LS
import LS.Rule
-- import Debug.Trace (trace)

toNatLang :: Interpreted -> String
toNatLang l4i =
  unlines [ show names
          | (names, _bs) <- qaHornsT l4i
          ]

