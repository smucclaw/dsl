{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LS.XPile.VueJSON where

import LS
import AnyAll.Types

import Options.Generic
import Data.Maybe (maybeToList, catMaybes)
import Data.List (nub)
import qualified Data.Text.Lazy as Text

-- https://en.wikipedia.org/wiki/Ground_expression
groundrules :: RunConfig -> [Rule] -> [MultiTerm]
groundrules rc rs = nub $ concatMap (rulegrounds rc globalrules) rs
  where
    globalrules :: [Rule]
    globalrules = [ r
                  | r@DefTypically{..} <- rs ]

checklist :: RunConfig -> [Rule] -> [MultiTerm]
checklist rc rs = groundToChecklist <$> groundrules rc rs

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


-- meng's crude natural language conversion
-- this is to be read as an "external requirement interface"
-- the implementation is totally up to the NLG team who can make use of more sophisticated code
-- to achieve the same goals.
-- As a starting point, we begin with hard-coded conversion functions.

groundToChecklist :: MultiTerm -> [Text.Text]
groundToChecklist (subj : "is" : "not" : y) = groundToChecklist $ subj : "is" : y
groundToChecklist (subj : "is not" : y)     = groundToChecklist $ subj : "is" : y
groundToChecklist (subj : "is"     : y)     = groundToChecklist $ "Is" : "the" : subj : quaero y -- ++ ["or not?"]
groundToChecklist (subj : "has"    : y)     = groundToChecklist $ "Does" : "the" : subj : "have" : quaero y -- ++ ["or not?"]
groundToChecklist ("the" : something1 : something2 : "occurs" : blahblah) = groundToChecklist $ "Did the" : something1 : something2 : "occur" : quaero blahblah
groundToChecklist mts
  | length mts == 1 = groundToChecklist $ concatMap Text.words mts
  | otherwise = pure $ Text.unwords mts

quaero :: [Text.Text] -> [Text.Text]
quaero xs = init xs ++ [last xs <> "?"]
