-- | example of a simple transpiler

module LS.XPile.NaturalLanguage where

import AnyAll as AA
import Data.Map qualified as Map
import Data.Text qualified as T
import LS.Interpreter ( qaHornsT, QAHorn(..) )
import LS.Rule ( Interpreted )
-- import Debug.Trace (trace)

toNatLang :: Interpreted -> String
toNatLang l4i =
  unlines [ show names
          | QAHorn names _qahead _bs <- qaHornsT l4i
          ]

