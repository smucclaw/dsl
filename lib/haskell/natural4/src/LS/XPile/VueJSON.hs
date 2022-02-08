{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LS.XPile.VueJSON where

import LS
import AnyAll.Types

import Options.Generic
import Data.Maybe (maybeToList)

-- https://en.wikipedia.org/wiki/Ground_expression
groundrules :: Opts Unwrapped -> [Rule] -> [MultiTerm]
groundrules opts rs = concatMap (rulegrounds opts globalrules) rs
  where
    globalrules :: [Rule]
    globalrules = [ r
                  | r@DefTypically{..} <- rs ]

rulegrounds :: Opts Unwrapped -> [Rule] -> Rule -> [MultiTerm]
rulegrounds opts globalrules r@Regulative{..} = concat . concat $
  [ maybeToList (aaLeavesFilter (ignoreTypicalRP opts globalrules r) <$> who)
  ]

rulegrounds opts globalrules r = [ ]

ignoreTypicalRP :: Opts Unwrapped -> [Rule] -> Rule -> (RelationalPredicate -> Bool)
ignoreTypicalRP opts globalrules r =
  if not $ extd opts
  then (\rp -> hasDefaultValue r rp || defaultInGlobals globalrules rp)
  else const True

-- is the "head-like" key of a relationalpredicate found in the list of defaults associated with the rule?
hasDefaultValue :: Rule -> RelationalPredicate -> Bool
hasDefaultValue r rp = rpHead rp `elem` (rpHead <$> defaults r)

defaultInGlobals :: [Rule] -> RelationalPredicate -> Bool
defaultInGlobals rs rp = any (\r -> hasDefaultValue r rp) rs
