{-# LANGUAGE GHC2021 #-}

{-| transpiler to SVG visualization of the AnyAll and/or trees.

Largely a wrapper. Most of the functionality is in the anyall lib.

-}

module LS.XPile.GFTrees where

import AnyAll qualified as AA
import Data.Map qualified as Map
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Text qualified as T
import LS.NLP.NL4Transformations (BoolStructGText)
import LS.NLP.NLG (NLGEnv, ruleQnTrees)
import LS.Rule (Rule (DefNameAlias))
import Prettyprinter (Pretty (pretty))
import Text.Pretty.Simple (pShowNoColor)


trees :: NLGEnv -> [Rule] -> [BoolStructGText]
trees env rl = concat $ concatMap (ruleQnTrees env alias) rl
  where
    alias = listToMaybe [(you,org) | DefNameAlias you org _ _ <- rl]

printTrees :: NLGEnv -> [Rule] -> String
printTrees env rl =
  show (
    pretty $ pShowNoColor $ trees env rl
  )