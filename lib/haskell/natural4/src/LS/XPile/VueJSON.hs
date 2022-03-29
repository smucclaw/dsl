{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LS.XPile.VueJSON where

import LS
import AnyAll.Types
import qualified AnyAll.Types as AA

import Options.Generic
import Data.Maybe (maybeToList, catMaybes)
import Data.List (nub)
import qualified Data.Text.Lazy as Text

-- https://en.wikipedia.org/wiki/Ground_expression
groundrules :: RunConfig -> [Rule] -> OldGrounds
groundrules rc rs = nub $ concatMap (rulegrounds rc globalrules) rs
  where
    globalrules :: [Rule]
    globalrules = [ r
                  | r@DefTypically{..} <- rs ]

checklist :: RunConfig -> [Rule] -> OldGrounds
checklist rc rs = groundToChecklist <$> groundrules rc rs

rulegrounds' :: RunConfig -> [Rule] -> Rule -> NewGrounds
rulegrounds' rc globalrules r@Regulative{..} =
  let whoGrounds  = (bsp2text subj `shallowPrependBSR`) <$> (bsr2grounds' rc globalrules r <$> who)
      condGrounds =                                          bsr2grounds' rc globalrules r <$> cond
  in whoGrounds <> condGrounds
rulegrounds' rc globalrules r = Just $ Leaf "unimplemented"

-- | extract the grounds from a given rule.
rulegrounds :: RunConfig -> [Rule] -> Rule -> OldGrounds
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


-- [TODO] let's switch the list to a proper and/or tree soon
type NewGrounds = Maybe (Item Text.Text)
type OldGrounds = [MultiTerm]

-- | extract the grounds from a given BoolStructR
bsr2grounds :: RunConfig -> [Rule] -> Rule -> Maybe BoolStructR -> OldGrounds
bsr2grounds rc globalrules r = concat . maybeToList . fmap (aaLeavesFilter (ignoreTypicalRP rc globalrules r))

bsr2grounds' :: RunConfig -> [Rule] -> Rule -> BoolStructR -> NewGrounds
bsr2grounds' rc globalrules r = toText (ignoreTypicalRP rc globalrules r) rp2text

toText :: (Item Text.Text -> Bool)
       -> (Item Text.Text -> Text.Text)
       -> Item a
       -> Maybe (Item Text.Text)
toText f g (AA.All pre xs) = Just $ AA.All pre (filter f (catMaybes $ toText f g <$> xs))
toText f g (AA.Any pre xs) = Just $ AA.Any pre (filter f (catMaybes $ toText f g <$> xs))
toText f g (AA.Leaf    x ) = if f x then Just $ AA.Leaf (g x) else Nothing
toText f g (AA.Not     x ) = toText f g x

-- | extract the grounds from a given ParamText
pt2grounds :: RunConfig -> [Rule] -> Rule -> ParamText -> OldGrounds
pt2grounds _rc _globalrules _r _pt = [["pt2grounds","unimplemented"]]

-- | extract the grounds from a given RelationalPredicate
rp2grounds :: RunConfig -> [Rule] -> Rule ->  RelationalPredicate -> OldGrounds
rp2grounds  rc  globalrules  r (RPParamText pt) = pt2grounds rc globalrules r pt
rp2grounds _rc _globalrules _r (RPMT mt) = [mt]
rp2grounds _rc _globalrules _r (RPConstraint mt1 _rprel mt2) = [mt1, mt2]
rp2grounds  rc  globalrules  r (RPBoolStructR mt _rprel bsr) = mt : bsr2grounds rc globalrules r (Just bsr)



-- | if a given RelationalPredicate carries a @TYPICALLY@ annotation, we may want to exclude it from extraction,
--   to keep the focus on those terms which are of greater interest.
ignoreTypicalRP :: RunConfig -> [Rule] -> Rule -> (RelationalPredicate -> Bool)
ignoreTypicalRP rc globalrules r =
  if not $ extendedGrounds rc
  then (\rp -> not (hasDefaultValue r rp || defaultInGlobals globalrules rp))
  else const True

-- | ignore ParamTexts that carry a @TYPICALLY@ annotation
ignoreTypicalPT :: RunConfig -> [Rule] -> Rule -> (ParamText -> Bool)
ignoreTypicalPT rc globalrules r =
  const False
  -- [FIXME]
  




  
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
groundToChecklist [mt]
  | mts@(_:_:_) <- Text.words mt = groundToChecklist mts -- Only loop if there are multiple words to prevent infinite loops
groundToChecklist mts = pure $ Text.unwords mts


quaero :: [Text.Text] -> [Text.Text]
quaero xs = init xs ++ [last xs <> "?"]
