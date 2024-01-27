-- | example of a simple transpiler

module LS.XPile.NaturalLanguage (toNatLang) where

import LS.Interpreter (qaHornsT)
import LS.Rule (Interpreted)

-- import Debug.Trace (trace)

toNatLang :: Interpreted -> String
toNatLang l4i =
  unlines
    [ show names
      | (names, _bs) <- qaHornsT l4i
    ]
