{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LS.XPile.VueJSON where

import LS
import AnyAll.Types

import Options.Generic
import Data.Maybe (maybeToList, catMaybes)

-- https://en.wikipedia.org/wiki/Ground_expression
groundrules :: RunConfig -> [Rule] -> [MultiTerm]
groundrules rc rs = concatMap (rulegrounds rc globalrules) rs
  where
    globalrules :: [Rule]
    globalrules = [ r
                  | r@DefTypically{..} <- rs ]

rulegrounds :: RunConfig -> [Rule] -> Rule -> [MultiTerm]
rulegrounds rc globalrules r@Regulative{..} =
  let whoGrounds  = (bsp2text subj :) <$> bsr2grounds who
      condGrounds =                       bsr2grounds cond
  in concat [whoGrounds, condGrounds]
  where bsr2grounds = concat . maybeToList . fmap (aaLeavesFilter (ignoreTypicalRP rc globalrules r))

rulegrounds rc globalrules r = [ ]

ignoreTypicalRP :: RunConfig -> [Rule] -> Rule -> (RelationalPredicate -> Bool)
ignoreTypicalRP rc globalrules r =
  if not $ extendedGrounds rc
  then (\rp -> not (hasDefaultValue r rp || defaultInGlobals globalrules rp))
  else const True

-- is the "head-like" key of a relationalpredicate found in the list of defaults associated with the rule?
hasDefaultValue :: Rule -> RelationalPredicate -> Bool
hasDefaultValue r rp = rpHead rp `elem` (rpHead <$> defaults r)

defaultInGlobals :: [Rule] -> RelationalPredicate -> Bool
defaultInGlobals rs rp = any (`hasDefaultValue` rp) rs
