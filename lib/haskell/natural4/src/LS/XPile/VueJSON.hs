{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-| transpiler to Purescript and JSON types intended for consumption by Vue. -}

-- [TODO] refactor and rename this module so that we distinguish Purescript from JSON.

module LS.XPile.VueJSON where

import LS
import LS.NLP.NLG
import LS.NLP.TreeTransformations (showExpr, udsToTreeGroups, getQSFromTrees)
import AnyAll.Types
import AnyAll.BoolStruct

import Data.Maybe (maybeToList)
import Data.List (nub, groupBy)
import qualified Data.Text as Text
import Control.Monad (when)

import PGF ( linearize, languages )
import LS.NLP.UDExt (gf)
-- import Data.Graph.Inductive.Internal.Thread (threadList)
import qualified Data.Map as Map
import qualified Data.Text as T


-- https://en.wikipedia.org/wiki/Ground_expression
groundrules :: RunConfig -> [Rule] -> Grounds
groundrules rc rs = nub $ concatMap (rulegrounds rc globalrules) rs
  where
    globalrules :: [Rule]
    globalrules = [ r
                  | r@DefTypically{} <- rs ]

checklist :: NLGEnv -> RunConfig -> [Rule] -> IO Grounds
checklist env _ rs = do
   qs <- nlgQuestion env `mapM` rs
   let nonEmptyQs = [ MTT <$> q | q@(_:_) <- qs ]
   pure $ sequence nonEmptyQs
-- original:
-- checklist env rc rs = groundsToChecklist env $ groundrules rc rs

-- toHTML :: NLGEnv -> RunConfig -> [Rule] -> IO (Text, Text)
-- toHTML env _ rs = do
--   htm <- nlg env `mapM` rs

--   <div class="deontic rule">
--   <div class="quantifier">Every person who</div>
--   <ul class="who">
--     <li> walks, and
--         <ul>
--            <li> eats, or </li>
--            <li> drinks </li>
--         </ul>
--     </li>
--    </ul>
--    <div class="deontic">must</div>
--    <div class="action">sing</div>
-- </div>

  -- return $ Text.breakOn "\n" $ Text.unwords htm

rulegrounds :: RunConfig -> [Rule] -> Rule -> Grounds
rulegrounds rc globalrules r@Regulative{..} =
  let whoGrounds  = (MTT (bsp2text subj) :) <$> bsr2grounds rc globalrules r who
      condGrounds =                             bsr2grounds rc globalrules r cond
  in concat [whoGrounds, condGrounds]

rulegrounds rc globalrules r@Hornlike{..} =
  let givenGrounds  = pt2grounds rc globalrules r <$> maybeToList given
      uponGrounds   = pt2grounds rc globalrules r <$> maybeToList upon
      clauseGrounds = [ rp2grounds  rc globalrules r (hHead clause) ++
                        bsr2grounds rc globalrules r (hBody clause)
                      | clause <- clauses ]

  in concat $ concat [givenGrounds, uponGrounds, clauseGrounds]

rulegrounds _rc _globalrules _r = [ ]

-- [TODO]: other forms of Rule need their ground terms expressed.
-- [TODO]: also, we should return the terms as a plain BoolStruct (Item Text.Text) so we don't lose the structure. but for now we work out just the plain dumping, then we put back the logic so Grounds becomes Item Text.


-- [TODO] in future this will become
-- type Grounds = AA.Item Text.Text
type Grounds = [MultiTerm]

bsr2grounds :: RunConfig -> [Rule] -> Rule -> Maybe BoolStructR -> Grounds
bsr2grounds rc globalrules r = concat . maybeToList . fmap (aaLeavesFilter (ignoreTypicalRP rc globalrules r))

pt2grounds :: RunConfig -> [Rule] -> Rule -> ParamText -> Grounds
pt2grounds _rc _globalrules _r _pt = [MTT <$> ["pt2grounds","unimplemented"]]

rp2grounds :: RunConfig -> [Rule] -> Rule ->  RelationalPredicate -> Grounds
rp2grounds  rc  globalrules  r (RPParamText pt) = pt2grounds rc globalrules r pt
rp2grounds _rc _globalrules _r (RPMT mt) = [mt]
rp2grounds _rc _globalrules _r (RPConstraint mt1 _rprel mt2) = [mt1, mt2]
rp2grounds  rc  globalrules  r (RPBoolStructR mt _rprel bsr) = mt : bsr2grounds rc globalrules r (Just bsr)
rp2grounds  rc  globalrules  r (RPnary     _rprel rp) = rp2grounds rc  globalrules  r rp

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
groundToChecklist :: NLGEnv -> MultiTerm -> IO MultiTerm
groundToChecklist env mt = do
  let txt = mt2text mt
  uds <- parseUD env txt
  let qs = gf $ getQSFromTrees $ udsToTreeGroups uds
  -- Debug output: print the AST of the question generated in getQSFromTrees
  when (verbose env) $ putStrLn ("The generated QS from the UDApp tree:\n" ++ showExpr qs)
  gr <- nlgExtPGF
  let lin = linearize gr (head $ languages gr) qs
  let result = case words lin of
        "is":"there":"parseUD:":"fail":_ -> Text.pack "Is it true that " <> txt
        _ -> Text.pack lin
  return $ MTT <$> quaero [result]

pickOneOf :: [MultiTerm] -> MultiTerm
pickOneOf mts = MTT "Does any of the following hold?" :
  [ MTT $ "* " <> mt2text mt | mt <- mts ]

groupSingletons :: MultiTerm -> MultiTerm -> Bool
groupSingletons [mt1] [mt2] -- both multiterms are singletons and contain only 1 word
                | [_t1] <- Text.words (mtexpr2text mt1)
                , [_t2] <- Text.words (mtexpr2text mt2) = True
groupSingletons _ _ = False -- a) one/both mts not singleton, or b) are singletons but contain >1 word

quaero :: [Text.Text] -> [Text.Text]
quaero [x] = [Text.unwords $ quaero $ Text.words x]
quaero (x:xs) = Text.toTitle x : init xs ++ [last xs <> "?"]
quaero xs = xs

toVueRules :: [Rule] -> BoolStructR
-- [TODO] is there something in RelationalPredicates or elsewhere that knows how to extract the Item from an HC2. there is a lot going on so we need to sort out the semantics first.
toVueRules [Hornlike {clauses=[HC {hBody=Just t}]}] = t
toVueRules _ = error "toVueRules cannot handle a list of more than one rule"

-- define custom types here for things we care about in purescript

itemRPToItemJSON :: BoolStructR -> BoolStructLT
itemRPToItemJSON (Leaf b) = mkLeaf (rp2text b)
itemRPToItemJSON (All Nothing items) = mkAll (Pre "all of the following") (map itemRPToItemJSON items)
itemRPToItemJSON (All (Just pre@(Pre _)) items) = AnyAll.BoolStruct.All pre (map itemRPToItemJSON items)
itemRPToItemJSON (All (Just pp@(PrePost _ _)) items) = AnyAll.BoolStruct.All pp (map itemRPToItemJSON items)
itemRPToItemJSON (Any Nothing items) = mkAny (AnyAll.Types.Pre "any of the following") (map itemRPToItemJSON items)
itemRPToItemJSON (Any (Just pre@(Pre _)) items) = mkAny pre (map itemRPToItemJSON items)
itemRPToItemJSON (Any (Just pp@(PrePost _ _)) items) = mkAny pp (map itemRPToItemJSON items)
itemRPToItemJSON (Not item) = mkNot (itemRPToItemJSON item)

type RuleJSON = Map.Map String BoolStructLT

rulesToRuleJSON :: [Rule] -> RuleJSON
rulesToRuleJSON rs = mconcat $ fmap ruleToRuleJSON rs

ruleToRuleJSON :: Rule -> RuleJSON
ruleToRuleJSON Hornlike {clauses=[HC {hHead=RPMT mt,hBody=Just itemRP}]}
  = Map.fromList [(T.unpack $ mt2text mt, itemRPToItemJSON itemRP)]
ruleToRuleJSON r@Regulative {who=whoRP, cond=condRP}
  =  maybe Map.empty (\bsr -> Map.singleton (T.unpack (mt2text $ ruleName r) <> " (relative to subj)") (((bsp2text (subj r) <> " ") <>) <$> itemRPToItemJSON bsr)) whoRP
  <> maybe Map.empty (Map.singleton (T.unpack (mt2text $ ruleName r) <> " (absolute condition)") . itemRPToItemJSON) condRP
ruleToRuleJSON DefNameAlias{} = Map.empty
ruleToRuleJSON x = Map.fromList [(T.unpack $ mt2text $ ruleName x, mkLeaf "unimplemented")]
