{-| transpiler to SVG visualization of the AnyAll and/or trees.

Largely a wrapper. Most of the functionality is in the anyall lib.

-}

module LS.XPile.GFTrees (gftrees) where

import Control.Monad (join)
import Data.Maybe (listToMaybe)
import LS.NLP.NL4Transformations (BoolStructGText)
import LS.NLP.NLG (NLGEnv, ruleQnTrees)
import LS.Rule (Rule (DefNameAlias))
import LS.XPile.Logging (XPileLog)

gftrees :: NLGEnv -> [Rule] -> XPileLog [BoolStructGText]
gftrees env rl = join <$> ruleQnTrees env alias `traverse` rl
  where
    alias = listToMaybe [(you,org) | DefNameAlias you org _ _ <- rl]
