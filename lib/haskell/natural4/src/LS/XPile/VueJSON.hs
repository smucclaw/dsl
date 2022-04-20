{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LS.XPile.VueJSON where

import LS
import AnyAll.Types
import LS.NLP.NLG

import Options.Generic
import Data.Maybe (maybeToList, catMaybes)
import Data.List (nub)
import qualified Data.Text.Lazy as Text

import PGF ( linearize, languages )
import LS.NLP.UDExt (gf)

-- https://en.wikipedia.org/wiki/Ground_expression
groundrules :: RunConfig -> [Rule] -> Grounds
groundrules rc rs = nub $ concatMap (rulegrounds rc globalrules) rs
  where
    globalrules :: [Rule]
    globalrules = [ r
                  | r@DefTypically{..} <- rs ]

checklist :: RunConfig -> [Rule] -> IO Grounds
checklist rc rs = groundToChecklist `mapM` groundrules rc rs

rulegrounds :: RunConfig -> [Rule] -> Rule -> Grounds
rulegrounds rc globalrules r@Regulative{..} =
  let whoGrounds  = (bsp2text subj :) <$> bsr2grounds rc globalrules r who
      condGrounds =                       bsr2grounds rc globalrules r cond
  in concat [whoGrounds, condGrounds]

rulegrounds rc globalrules r@Hornlike{..} =
  let givenGrounds  = pt2grounds rc globalrules r <$> maybeToList given
      uponGrounds   = pt2grounds rc globalrules r <$> maybeToList upon
      clauseGrounds = [ rp2grounds  rc globalrules r (hHead clause) ++
                        bsr2grounds rc globalrules r (hBody clause)
                      | clause <- clauses ]

  in concat $ concat [givenGrounds, uponGrounds, clauseGrounds]

rulegrounds rc globalrules r = [ ]

-- [TODO]: other forms of Rule need their ground terms expressed.
-- [TODO]: also, we should return the terms as a plain BoolStruct (Item Text.Text) so we don't lose the structure. but for now we work out just the plain dumping, then we put back the logic so Grounds becomes Item Text.


-- [TODO] in future this will become
-- type Grounds = AA.Item Text.Text
type Grounds = [MultiTerm]

bsr2grounds :: RunConfig -> [Rule] -> Rule -> Maybe BoolStructR -> Grounds
bsr2grounds rc globalrules r = concat . maybeToList . fmap (aaLeavesFilter (ignoreTypicalRP rc globalrules r))

pt2grounds :: RunConfig -> [Rule] -> Rule -> ParamText -> Grounds
pt2grounds _rc _globalrules _r _pt = [["pt2grounds","unimplemented"]]

rp2grounds :: RunConfig -> [Rule] -> Rule ->  RelationalPredicate -> Grounds
rp2grounds  rc  globalrules  r (RPParamText pt) = pt2grounds rc globalrules r pt
rp2grounds _rc _globalrules _r (RPMT mt) = [mt]
rp2grounds _rc _globalrules _r (RPConstraint mt1 _rprel mt2) = [mt1, mt2]
rp2grounds  rc  globalrules  r (RPBoolStructR mt _rprel bsr) = mt : bsr2grounds rc globalrules r (Just bsr)

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

groundToChecklist :: MultiTerm -> IO [Text.Text]
-- groundToChecklist (subj : "is" : "not" : y) = groundToChecklist $ subj : "is" : y
-- groundToChecklist (subj : "is not" : y)     = groundToChecklist $ subj : "is" : y
-- groundToChecklist (subj : "is"     : y)     = groundToChecklist $ "Is" : "the" : subj : quaero y -- ++ ["or not?"]
-- groundToChecklist (subj : "has"    : y)     = groundToChecklist $ "Does" : "the" : subj : "have" : quaero y -- ++ ["or not?"]
-- groundToChecklist ("the" : something1 : something2 : "occurs" : blahblah) = groundToChecklist $ "Did the" : something1 : something2 : "occur" : quaero blahblah
-- groundToChecklist [mt]
--   | mts@(_:_:_) <- Text.words mt = groundToChecklist mts -- Only loop if there are multiple words to prevent infinite loops
-- groundToChecklist mts = pure $ Text.unwords mts
groundToChecklist mt = do
  env <- myNLGEnv
  guds <- parseUD env $ Text.unwords mt
  let trees = udsToTreeGroups guds
  let gqs = getGQSFromTrees trees
  gr <- nlgExtPGF
  return $ quaero $ map Text.pack [linearize gr (head $ languages gr) $ gf gqs]

quaero :: [Text.Text] -> [Text.Text]
quaero xs = init xs ++ [last xs <> "?"]

-- quaero :: Text.Text -> Text.Text
-- quaero xs = xs <> "?"