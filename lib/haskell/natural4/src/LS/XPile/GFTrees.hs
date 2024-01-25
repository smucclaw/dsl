{-| transpiler to SVG visualization of the AnyAll and/or trees.

Largely a wrapper. Most of the functionality is in the anyall lib.

-}

module LS.XPile.GFTrees where

import AnyAll qualified as AA
import Control.Monad (join)
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Text qualified as T
import LS
import LS.NLP.NL4Transformations
import LS.NLP.NLG
import LS.XPile.Logging (XPileLog)
import Prettyprinter
import Text.Pretty.Simple (pShowNoColor)


gftrees :: NLGEnv -> [Rule] -> XPileLog [BoolStructGText]
gftrees env rl = join <$> ruleQnTrees env alias `traverse` rl
  where
    alias = listToMaybe [(you,org) | DefNameAlias you org _ _ <- rl]

