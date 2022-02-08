{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LS.XPile.VueJSON where

import LS
import AnyAll.Types

import Options.Generic
import Data.Maybe (maybeToList)

-- https://en.wikipedia.org/wiki/Ground_expression
groundrules :: Opts Unwrapped -> [Rule] -> [MultiTerm]
groundrules opts rs = concatMap (rulegrounds opts) rs

rulegrounds :: Opts Unwrapped -> Rule -> [MultiTerm]
rulegrounds opts r@Regulative{..} = concat . concat $
  [ maybeToList (aaLeavesFilter (ignoreTypicalRP opts r) <$> who)
  ]

rulegrounds opts r = [ ]

ignoreTypicalRP :: Opts Unwrapped -> Rule -> (RelationalPredicate -> Bool)
ignoreTypicalRP opts r = if not $ extd opts
                         then hasDefaultValue r
                         else const False

-- is the "head-like" key of a relationalpredicate found in the list of defaults associated with the rule?
hasDefaultValue :: Rule -> RelationalPredicate -> Bool
hasDefaultValue r rp = rpHead rp `elem` (rpHead <$> defaults r)

