{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Data.Coerce (Coercible, coerce)
import Data.Foldable (Foldable (foldMap'))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified as T
-- import Debug.Trace
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
    (<+>),
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

rule2doc :: forall ann. Rule -> Doc ann
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
      [deontic2str deontic, pretty2Qid actionName],
      ["WITHIN", pretty n, "DAY"],
      [henceLest2maudeStr Hence hence],
      [henceLest2maudeStr Lest lest]
    ]
      |> foldMapToDocViaMonoid @(CatWithNewLine ann) hsep
    where
      deontic2str DMust = "MUST"
      deontic2str DMay = "MAY"
      deontic2str DShant = "SHANT"

rule2doc _ = errMsg

rules2doc :: forall ann t. Foldable t => t Rule -> Doc ann
rules2doc rules =
  rules
    |> foldMapToDocViaMonoid @(CatWithCommaAndNewLines ann) rule2doc

pretty2Qid :: T.Text -> Doc ann
pretty2Qid x = x |> T.strip |> pretty |> ("'" <>)

rules2maudeStr :: Foldable t => t Rule -> String
rules2maudeStr rules = rules |> rules2doc |> show

data HenceOrLest = Hence | Lest

henceLest2maudeStr :: HenceOrLest -> Maybe Rule -> Doc ann
henceLest2maudeStr henceOrLest hence =
  hence |> maybe "" f
  where
    f (RuleAlias hence') =
      hence'
        |> fmap quotOrUpper
        |> hsep
        |> parenthesizeIf (length hence' > 1)
        |> (henceOrLest' <+>)
    f _ = errMsg
    quotOrUpper (MTT (T.toLower -> "and")) = "AND"
    quotOrUpper (MTT x) = x |> pretty2Qid
    quotOrUpper _ = errMsg
    parenthesizeIf True x = mconcat ["(", x, ")"]
    parenthesizeIf False x = x
    henceOrLest'
      | henceOrLest == Hence = "HENCE"
      | henceOrLest == Lest = "LEST"

errMsg :: a
errMsg = error "Not supported."

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

catViaDocAnn ::
  forall ann a.
  Coercible a (Doc ann) =>
  Doc ann ->
  a ->
  a ->
  a
catViaDocAnn sep x y = [x', sep', y'] |> mconcat |> coerce @(Doc ann)
  where
    sep'
      | "" `elem` show <$> [x', y'] = ""
      | otherwise = sep
    x' = coerce x
    y' = coerce y

foldMapToDocViaMonoid ::
  forall m ann a t.
  (Coercible (Doc ann) m, Foldable t, Monoid m) =>
  (a -> Doc ann) ->
  t a ->
  Doc ann
foldMapToDocViaMonoid f xs = xs |> foldMap' f' |> coerce @m
  where
    f' x = x |> f |> coerce

instance Semigroup (CatWithNewLine ann) where
  (<>) = catViaDocAnn @ann line

instance Monoid (CatWithNewLine ann) where
  mempty = CatWithNewLine ""

instance Semigroup (CatWithCommaAndNewLines ann) where
  (<>) = catViaDocAnn @ann sep
    where
      sep = [",", line, line] |> mconcat

instance Monoid (CatWithCommaAndNewLines ann) where
  mempty = CatWithCommaAndNewLines ""