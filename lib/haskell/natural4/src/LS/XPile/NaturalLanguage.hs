-- | example of a simple transpiler

module LS.XPile.NaturalLanguage (toNatLang) where

import LS.Interpreter (Interpreted, qaHornsT)

-- import Debug.Trace (trace)

toNatLang :: Interpreted -> String
toNatLang l4i =
  unlines
    [ show names
      | (names, _bs) <- qaHornsT l4i
    ]
