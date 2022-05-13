{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LS.XPile.VueJSON where

import LS
import AnyAll.Types
import LS.NLP.NLG

import Options.Generic
import Data.Maybe (maybeToList, catMaybes)
import Data.List (nub, groupBy)
import qualified Data.Text.Lazy as Text
import Control.Monad (when)

import PGF ( linearize, languages )
import LS.NLP.UDExt (gf)

-- https://en.wikipedia.org/wiki/Ground_expression
groundrules :: RunConfig -> [Rule] -> Grounds
groundrules rc rs = nub $ concatMap (rulegrounds rc globalrules) rs
  where
    globalrules :: [Rule]
    globalrules = [ r
                  | r@DefTypically{..} <- rs ]

checklist :: NLGEnv -> RunConfig -> [Rule] -> IO Grounds
checklist env _cfg rs = nlgQuestion env `mapM` rs -- Use this as soon as tests pass
checklist env rc rs = groundsToChecklist env $ groundrules rc rs

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


-- this is to be read as an "external requirement interface"

groundsToChecklist :: NLGEnv -> Grounds -> IO Grounds
groundsToChecklist env mts = sequence [
  case mtGroup of
    [multiterm] -> groundToChecklist env multiterm
    _ -> return $ pickOneOf mtGroup
  | mtGroup <- groupBy groupSingletons mts
  ]
groundToChecklist :: NLGEnv -> MultiTerm -> IO [Text.Text]
groundToChecklist env mt = do
  let txt = Text.unwords mt
  uds <- parseUD env txt
  let qs = gf $ getQSFromTrees $ udsToTreeGroups uds
  -- Debug output: print the AST of the question generated in getQSFromTrees
  when (verbose env) $ putStrLn ("The generated QS from the UDApp tree:\n" ++ showExpr qs)
  gr <- nlgExtPGF
  let lin = linearize gr (head $ languages gr) qs
  let result = case words lin of
        "is":"there":"parseUD:":"fail":_ -> Text.pack "Is it true that " <> txt
        _ -> Text.pack lin
  return $ quaero [result]

pickOneOf :: [MultiTerm] -> MultiTerm
pickOneOf mts = Text.pack "Does any of the following hold?" :
  map (\[x] -> Text.pack "* " <> x) mts

groupSingletons :: MultiTerm -> MultiTerm -> Bool
groupSingletons [mt1] [mt2] -- both multiterms are singletons and contain only 1 word
                | [_t1] <- Text.words mt1
                , [_t2] <- Text.words mt2 = True
groupSingletons _ _ = False -- a) one/both mts not singleton, or b) are singletons but contain >1 word

quaero :: [Text.Text] -> [Text.Text]
quaero [x] = [Text.unwords $ quaero $ Text.words x]
quaero (x:xs) = Text.toTitle x : init xs ++ [last xs <> "?"]
quaero xs = xs
