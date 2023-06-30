{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}

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
import LS.XPile.Logging (XPileLog)
import Control.Monad (mapM, join)


gftrees :: NLGEnv -> [Rule] -> XPileLog [BoolStructGText]
gftrees env rl = join <$> sequence (ruleQnTrees env alias <$> rl)
  where
    alias = listToMaybe [(you,org) | DefNameAlias you org _ _ <- rl]

