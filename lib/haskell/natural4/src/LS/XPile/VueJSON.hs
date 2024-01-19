{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-| transpiler to Purescript and JSON types intended for consumption by Vue. -}

-- [TODO] refactor and rename this module so that we distinguish Purescript from JSON.

module LS.XPile.VueJSON where

import AnyAll.BoolStruct
  ( BoolStruct (All, Any, Leaf, Not),
    BoolStructLT,
    mkAll,
    mkAny,
    mkLeaf,
    mkNot,
  )
import AnyAll.Types (Label (Pre, PrePost))
import Data.List (groupBy, nub)
-- import Data.Graph.Inductive.Internal.Thread (threadList)
import Data.HashMap.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Text qualified as T
import Data.Traversable (for)
import LS.NLP.NLG (NLGEnv, nlgQuestion)
import LS.RelationalPredicates (aaLeavesFilter)
import LS.Rule
  ( Rule
      ( Constitutive,
        DefNameAlias,
        DefTypically,
        Hornlike,
        NotARule,
        RegBreach,
        RegFulfilled,
        Regulative,
        RuleAlias,
        RuleGroup,
        Scenario,
        TypeDecl,
        action,
        clauses,
        cond,
        defaults,
        deontic,
        given,
        giveth,
        having,
        hence,
        keyword,
        lest,
        lsource,
        name,
        rkeyword,
        rlabel,
        srcref,
        subj,
        super,
        symtab,
        temporal,
        upon,
        who,
        wwhere
      ),
    ruleLabelName,
    ruleName,
  )
import LS.Types
  ( BoolStructR,
    HornClause (HC, hBody, hHead),
    MTExpr (MTT),
    MultiTerm,
    ParamText,
    RelationalPredicate (..),
    RuleName,
    RunConfig (extendedGrounds),
    bsp2text,
    mt2text,
    mtexpr2text,
    rp2text,
    rpHead,
  )
import LS.XPile.Logging
  ( XPileLog,
    XPileLogE,
    mutter,
    xpError,
    xpReturn,
  )

-- https://en.wikipedia.org/wiki/Ground_expression
groundrules :: RunConfig -> [Rule] -> Grounds
groundrules rc rs = nub $ foldMap (rulegrounds rc globalrules) rs
  where
    globalrules :: [Rule]
    globalrules = [ r
                  | r@DefTypically{} <- rs ]

checklist :: NLGEnv -> RunConfig -> [Rule] -> XPileLog Grounds
checklist env _ rs = do
  qs <- nlgQuestion env `traverse` rs
  --  let nonEmptyQs = [ MTT <$> q | q@(_:_) <- qs ]
  --  pure $ sequence nonEmptyQs
  pure $ for qs \case
    [] -> mempty
    q -> MTT <$> q

multiChecklist :: [NLGEnv] -> RunConfig -> [Rule] -> XPileLog Grounds
multiChecklist env rc rs = do
  qs <- (\e -> checklist e rc rs) `traverse` env
  pure $ mconcat qs

-- original:
-- checklist env rc rs = groundsToChecklist env $ groundrules rc rs

-- toHTML :: NLGEnv -> RunConfig -> [Rule] -> IO (T, T)
-- toHTML env _ rs = do
--   htm <- nlg env `traverse` rs

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

  -- return $ T.breakOn "\n" $ T.unwords htm

rulegrounds :: RunConfig -> [Rule] -> Rule -> Grounds
rulegrounds rc globalrules r@Regulative{..} =
  let whoGrounds  = (MTT (bsp2text subj) :) <$> bsr2grounds rc globalrules r who
      condGrounds =                             bsr2grounds rc globalrules r cond
  in mconcat [whoGrounds, condGrounds]

rulegrounds rc globalrules r@Hornlike{..} =
  let givenGrounds  = pt2grounds rc globalrules r <$> maybeToList given
      uponGrounds   = pt2grounds rc globalrules r <$> maybeToList upon
      clauseGrounds = [ rp2grounds  rc globalrules r (hHead clause) ++
                        bsr2grounds rc globalrules r (hBody clause)
                      | clause <- clauses ]

  in mconcat $ mconcat [givenGrounds, uponGrounds, clauseGrounds]

rulegrounds _rc _globalrules _r = [ ]

-- [TODO]: other forms of Rule need their ground terms expressed.
-- [TODO]: also, we should return the terms as a plain BoolStruct (Item T.T) so we don't lose the structure. but for now we work out just the plain dumping, then we put back the logic so Grounds becomes Item T.


-- [TODO] in future this will become
-- type Grounds = AA.Item T.T
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
rp2grounds  rc  globalrules  r (RPnary     _rprel rps) = foldMap (rp2grounds rc  globalrules  r) rps

ignoreTypicalRP :: RunConfig -> [Rule] -> Rule -> (RelationalPredicate -> Bool)
ignoreTypicalRP rc globalrules r =
  if not $ extendedGrounds rc
  then \rp -> not (hasDefaultValue r rp || defaultInGlobals globalrules rp)
  else const True

-- is the "head-like" key of a relationalpredicate found in the list of defaults associated with the rule?
hasDefaultValue :: Rule -> RelationalPredicate -> Bool
hasDefaultValue r rp = rpHead rp `elem` (rpHead <$> defaults r)

defaultInGlobals :: [Rule] -> RelationalPredicate -> Bool
defaultInGlobals rs rp = any (`hasDefaultValue` rp) rs


-- this is to be read as an "external requirement interface"

groundsToChecklist :: NLGEnv -> Grounds -> XPileLog Grounds
groundsToChecklist env mts = sequence [
  case mtGroup of
    [multiterm] -> groundToChecklist env multiterm
    _ -> return $ pickOneOf mtGroup
  | mtGroup <- groupBy groupSingletons mts
  ]
groundToChecklist :: NLGEnv -> MultiTerm -> XPileLog MultiTerm
groundToChecklist env mt = do
{-  let txt = T.unwords mt
  uds <- parseUD env txt
  let qs = gf $ getQSFromTrees $ udsToTreeGroups uds
  -- Debug output: print the AST of the question generated in getQSFromTrees
  when (verbose env) $ putStrLn ("The generated QS from the UDApp tree:\n" ++ showExpr qs)
  let lin = linearize (gfGrammar env) (gfLang gr) qs
  let result = case words lin of
        "is":"there":"parseUD:":"fail":_ -> T.pack "Is it true that " <> txt
        _ -> T.pack lin -}
  let result = T.pack "NLG is under construction"
  return $ MTT <$> quaero [result]

pickOneOf :: [MultiTerm] -> MultiTerm
pickOneOf mts = MTT "Does any of the following hold?" :
  [ MTT $ "* " <> mt2text mt | mt <- mts ]

groupSingletons :: MultiTerm -> MultiTerm -> Bool
groupSingletons [mt1] [mt2] -- both multiterms are singletons and contain only 1 word
                | [_t1] <- T.words (mtexpr2text mt1)
                , [_t2] <- T.words (mtexpr2text mt2) = True
groupSingletons _ _ = False -- a) one/both mts not singleton, or b) are singletons but contain >1 word

quaero :: [T.Text] -> [T.Text]
quaero [x] = [T.unwords $ quaero $ T.words x]
quaero (x:xs) = T.toTitle x : init xs ++ [last xs <> "?"]
quaero xs = xs

toVueRules :: [Rule] -> [(RuleName, XPileLogE BoolStructR)]
-- [TODO] is there something in RelationalPredicates or elsewhere that knows how to extract the Item from an HC2. there is a lot going on so we need to sort out the semantics first.
-- clearly this is not ready for primetime, we need to get this transpiler at least as functional as the Purescript outputter that it is meant to replace.
toVueRules rs = [ (ruleLabelName r, toVueRule r) | r <- rs ]

toVueRule :: Rule -> XPileLogE BoolStructR
toVueRule r@(Hornlike {clauses=[HC {hBody=Just t}]}) = do
  mutter "branch 1: handling Hornlike rule"
  xpReturn t
toVueRule r@(Regulative {who=Just whoRP, cond=Just condRP}) = do
  mutter "branch 2: this goes to stderr and is a more principled alternative to Debug.Trace"
  xpReturn $ All Nothing [whoRP, condRP]
  -- xpError ["handling branch2, whoRP && condRP"]
toVueRule r@(Regulative {who=Just whoRP}) = do
  mutter "branch 3: this goes to stderr and is a more principled alternative to Debug.Trace"
  xpReturn whoRP
  -- xpError ["handling branch 3, whoRP"]
toVueRule r@(Regulative {cond=Just condRP}) = do
  mutter "branch 4: this goes to stderr and is a more principled alternative to Debug.Trace"
  xpReturn condRP
  -- xpError ["handling branch 4, condRP"]
toVueRule _ = do
  mutter "branch 5: this goes to stderr and is a more principled alternative to Debug.Trace"
  xpError ["toVueRules not handling any other type of rule"]


-- define custom types here for things we care about in purescript

itemRPToItemJSON :: BoolStructR -> BoolStructLT
itemRPToItemJSON (Leaf b)                            = mkLeaf (rp2text b)
itemRPToItemJSON (All Nothing items)                 = mkAll (Pre "all of the following") (map itemRPToItemJSON items)
itemRPToItemJSON (All (Just pre@(Pre _)) items)      = AnyAll.BoolStruct.All pre (map itemRPToItemJSON items)
itemRPToItemJSON (All (Just pp@(PrePost _ _)) items) = AnyAll.BoolStruct.All pp (map itemRPToItemJSON items)
itemRPToItemJSON (Any Nothing items)                 = mkAny (AnyAll.Types.Pre "any of the following") (map itemRPToItemJSON items)
itemRPToItemJSON (Any (Just pre@(Pre _)) items)      = mkAny pre (map itemRPToItemJSON items)
itemRPToItemJSON (Any (Just pp@(PrePost _ _)) items) = mkAny pp (map itemRPToItemJSON items)
itemRPToItemJSON (Not item)                          = mkNot (itemRPToItemJSON item)

type RuleJSON = Map.HashMap String BoolStructLT

rulesToRuleJSON :: [Rule] -> RuleJSON
rulesToRuleJSON rs = mconcat $ fmap ruleToRuleJSON rs

ruleToRuleJSON :: Rule -> RuleJSON
ruleToRuleJSON Hornlike {clauses=[HC {hHead=RPMT mt,hBody=Just itemRP}]}
  = Map.fromList [(T.unpack $ mt2text mt, itemRPToItemJSON itemRP)]
ruleToRuleJSON r@Regulative {who=whoRP, cond=condRP}
  =  maybe Map.empty (\bsr -> Map.singleton (T.unpack (mt2text $ ruleName r) <> " (relative to subj)") (((bsp2text (subj r) <> " ") <>) <$> itemRPToItemJSON bsr)) whoRP
  <> maybe Map.empty (Map.singleton (T.unpack (mt2text $ ruleName r) <> " (absolute condition)") . itemRPToItemJSON) condRP
ruleToRuleJSON Constitutive{}  = Map.empty
ruleToRuleJSON TypeDecl{}      = Map.empty
ruleToRuleJSON Scenario{}      = Map.empty
ruleToRuleJSON DefNameAlias{}  = Map.empty
ruleToRuleJSON DefTypically{}  = Map.empty
ruleToRuleJSON RuleAlias {}    = Map.empty
ruleToRuleJSON RuleGroup {}    = Map.empty
ruleToRuleJSON RegFulfilled {} = Map.empty
ruleToRuleJSON RegBreach {}    = Map.empty
ruleToRuleJSON NotARule {}     = Map.empty
ruleToRuleJSON x               = [(T.unpack $ mt2text $ ruleName x, mkLeaf "unimplemented")]
