{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-| transpiler to SVG visualization of the AnyAll and/or trees.

Largely a wrapper. Most of the functionality is in the anyall lib.

-}

module LS.XPile.GFTrees where

import LS
import LS.NLP.NLG
import LS.NLP.NL4Transformations

import qualified Data.Text as T
import qualified AnyAll as AA
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, catMaybes, mapMaybe)
import Text.Pretty.Simple (pShowNoColor)
import Prettyprinter


trees :: NLGEnv -> [Rule] -> [BoolStructGText]
trees env rl = concatMap (ruleQnTrees env alias) rl
  where
    alias = listToMaybe [(you,org) | DefNameAlias you org _ _ <- rl]

printTrees :: NLGEnv -> [Rule] -> String
printTrees env rl =
  show (
    pretty $ pShowNoColor $ trees env rl
  )
