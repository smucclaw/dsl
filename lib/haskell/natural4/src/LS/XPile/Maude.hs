{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{-
  Work-in-progress transpiler to Maude.
  Note that since we do all the parsing and transpilation within Maude itself,
  all we do here is convert the list of rules to a textual, string
  representation that Maude can parse.
-}
module LS.XPile.Maude where

import AnyAll (BoolStruct (Leaf))
import Data.Coerce (coerce)
import Data.Foldable (Foldable (foldMap'))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified as T
import Flow ((|>))
import LS.Rule
  ( Rule (..),
  )
import LS.Types
  ( Deontic (DMay, DMust, DShant),
    MTExpr (MTT),
    RegKeywords (RParty),
    TComparison (TBefore),
    TemporalConstraint (TemporalConstraint),
  )
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    hsep,
    line,
  )

{-
  Based on experiments being run here:
  https://docs.google.com/spreadsheets/d/1leBCZhgDsn-Abg2H_OINGGv-8Gpf9mzuX1RR56v0Sss/edit#gid=929226277
-}
testRule :: String
testRule = rules2maudeStr [Regulative {..}]
  where
    rlabel = Just ("§", 1, "START")
    rkeyword = RParty
    subj = Leaf ((MTT "actor" :| [], Nothing) :| [])
    deontic = DMust
    action = Leaf ((MTT "action" :| [], Nothing) :| [])
    temporal = Just (TemporalConstraint TBefore (Just 5) "day")
    hence = Just (RuleAlias [MTT "rule0", MTT "and", MTT "rule1"])
    lest = Nothing

    -- The remaining fields aren't used and hence don't matter.
    given = Nothing
    having = Nothing
    who = Nothing
    cond = Nothing
    lsource = Nothing
    srcref = Nothing
    upon = Nothing
    wwhere = []
    defaults = []
    symtab = []

-- This function is still a work in progress.
rule2doc :: Rule -> Doc ann
rule2doc
  Regulative
    { rlabel = Just (_, _, ruleName),
      rkeyword = RParty,
      subj = Leaf ((MTT actorName :| [], _) :| []),
      deontic,
      action = Leaf ((MTT actionName :| [], _) :| []),
      temporal = Just (TemporalConstraint TBefore (Just n) (T.toLower -> "day")),
      hence,
      lest
    } =
    [ ["RULE", pretty2Qid ruleName],
      ["PARTY", pretty2Qid actorName],
      [deontic2str deontic, "DO", pretty2Qid actionName],
      ["WITHIN", pretty n, "DAY"],
      [henceLest2maudeStr hence],
      [henceLest2maudeStr lest]
    ]
    |> map hsep
    |> foldMap' coerce2CatWithNewLine
    |> coerce2Doc
    where
      deontic2str DMust = "MUST"
      deontic2str DMay = "MAY"
      deontic2str DShant = "SHANT"
      coerce2CatWithNewLine :: Doc ann -> CatWithNewLine ann
      coerce2CatWithNewLine = coerce
      coerce2Doc :: CatWithNewLine ann -> Doc ann
      coerce2Doc = coerce

rule2doc _ = "Not supported."

rules2doc :: Foldable t => t Rule -> Doc ann
rules2doc rules = rules
 |> foldMap' toCatWithCommaAndNewLines
 |> coerce2Doc
 where
  toCatWithCommaAndNewLines rule = rule
    |> rule2doc |> coerce2CatWithCommaAndNewLines
  coerce2CatWithCommaAndNewLines :: Doc ann -> CatWithCommaAndNewLines ann
  coerce2CatWithCommaAndNewLines = coerce
  coerce2Doc :: CatWithCommaAndNewLines ann -> Doc ann
  coerce2Doc = coerce

pretty2Qid :: T.Text -> Doc ann
pretty2Qid x = x |> T.strip |> pretty |> ("'" <>)

rules2maudeStr :: Foldable t => t Rule -> String
rules2maudeStr rules = rules |> rules2doc |> show

henceLest2maudeStr :: Maybe Rule -> Doc ann
henceLest2maudeStr hence = hence |> maybe "NOTHING" f
  where
    f (RuleAlias hence') = hence' <&> quotOrUpper |> hsep
    f _ = ""
    quotOrUpper (MTT (T.toLower -> "and")) = "AND"
    quotOrUpper (MTT x) = x |> pretty2Qid
    quotOrUpper _ = ""

-- foldMapWithNewLines ::
--   (Foldable t, Semigroup (CatWithNewLine ann)) =>
--   (a -> Doc ann) ->
--   t a ->
--   Doc ann
-- foldMapWithNewLines f docs = docs |> foldMap' f' |> coerce
--   where
--     f' :: a -> CatWithNewLine ann
--     f' x = x |> f |> coerce

-- Boring utilities below.
newtype CatWithNewLine ann = CatWithNewLine (Doc ann)

newtype CatWithCommaAndNewLines ann = CatWithCommaAndNewLines (Doc ann)

instance Semigroup (CatWithNewLine ann) where
  x <> y = [x', line, y'] |> mconcat |> coerce @(Doc ann)
    where
      x' = coerce x
      y' = coerce y

instance Monoid (CatWithNewLine ann) where
  mempty = CatWithNewLine ""

instance Semigroup (CatWithCommaAndNewLines ann) where
  x <> y = [x', ",", line, line, y'] |> mconcat |> coerce @(Doc ann)
    where
      x' = coerce x
      y' = coerce y

instance Monoid (CatWithCommaAndNewLines ann) where
  mempty = CatWithCommaAndNewLines ""