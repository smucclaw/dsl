{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-| transpiler to JSON types intended for consumption by Vue. -}

module LS.XPile.JSON where

import Data.List (groupBy, nub)
import Data.HashMap.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Aeson
import Data.Aeson.Types
import LS.Rule
  ( Interpreted(classtable, scopetable),
    Rule(..),
    hasGiven,
    hasClauses,
    ruleLabelName,
    Rule(clauses, given),
    ruleLabelName,
    ruleName,
  )
import LS.Interpreter
    (
      ruleLocals,
    )
import LS.Types
  ( unCT,
    BoolStructR,
    ClassHierarchyMap,
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
import LS.PrettyPrinter
    ( myrender, vvsep, (</>), tildes, (<//>), snake_case, srchs, orgexample )
import Prettyprinter
    ( vsep, viaShow, hsep, emptyDoc, (<+>), Pretty(pretty), Doc, indent, line )


justClassTypes :: Interpreted -> [Rule] -> String
justClassTypes l4i rs = Text.unpack $ myrender $ vvsep [ "***" <+> pretty lhs </> srchs rhs | (lhs, rhs) <- Map.toList (unCT $ classtable l4i) ]



-- classtable
-- type ClassHierarchyMap = Map.HashMap EntityType TypedClass


-- vvsep [ "***" <+> pretty lhs </> srchs rhs | (lhs, rhs) <- Map.toList (unCT $ classtable l4i) ]

-- type RuleJSON = Map.Map String BoolStructLT

-- rulesToRuleJSON :: [Rule] -> RuleJSON
-- rulesToRuleJSON rs = mconcat $ fmap ruleToRuleJSON rs

-- ruleToRuleJSON :: Rule -> RuleJSON
-- ruleToRuleJSON Hornlike {clauses=[HC {hHead=RPMT mt,hBody=Just itemRP}]}
--   = Map.fromList [(T.unpack $ mt2text mt, itemRPToItemJSON itemRP)]
-- ruleToRuleJSON r@Regulative {who=whoRP, cond=condRP}
--   =  maybe Map.empty (\bsr -> Map.singleton (T.unpack (mt2text $ ruleName r) <> " (relative to subj)") (((bsp2text (subj r) <> " ") <>) <$> itemRPToItemJSON bsr)) whoRP
--   <> maybe Map.empty (Map.singleton (T.unpack (mt2text $ ruleName r) <> " (absolute condition)") . itemRPToItemJSON) condRP
-- ruleToRuleJSON Constitutive{}  = Map.empty
-- ruleToRuleJSON TypeDecl{}      = Map.empty
-- ruleToRuleJSON Scenario{}      = Map.empty
-- ruleToRuleJSON DefNameAlias{}  = Map.empty
-- ruleToRuleJSON DefTypically{}  = Map.empty
-- ruleToRuleJSON RuleAlias {}    = Map.empty
-- ruleToRuleJSON RuleGroup {}    = Map.empty
-- ruleToRuleJSON RegFulfilled {} = Map.empty
-- ruleToRuleJSON RegBreach {}    = Map.empty
-- ruleToRuleJSON NotARule {}     = Map.empty
-- ruleToRuleJSON x               = Map.fromList [(T.unpack $ mt2text $ ruleName x, mkLeaf "unimplemented")]
