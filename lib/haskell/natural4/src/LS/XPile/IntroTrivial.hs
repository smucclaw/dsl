{-| a trivial transpiler, part of the Introductory series
-}

module LS.XPile.IntroTrivial where

import LS.Interpreter (Interpreted (..))

toTrivial :: Interpreted -> String
toTrivial l4i = do
  let rules = origrules l4i
  "output from the IntroTrivial transpiler"

