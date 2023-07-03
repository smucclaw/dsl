{-# LANGUAGE DerivingStrategies #-}

{-| transpiler to SVG visualization of the AnyAll and/or trees.

Largely a wrapper. Most of the functionality is in the anyall lib.

-}

module LS.XPile.GFTrees where

import AnyAll qualified as AA
import Data.Map qualified as Map
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Text qualified as T
import LS
import LS.NLP.NL4Transformations
import LS.NLP.NLG
import Prettyprinter
import Text.Pretty.Simple (pShowNoColor)


trees :: NLGEnv -> [Rule] -> [BoolStructGText]
trees env rl = concatMap (ruleQnTrees env alias) rl
  where
    alias = listToMaybe [(you,org) | DefNameAlias you org _ _ <- rl]

printTrees :: NLGEnv -> [Rule] -> String
printTrees env rl =
  show (
    pretty $ pShowNoColor $ trees env rl
  )
