{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-
  Work-in-progress transpiler to Maude.
  Note that since we do all the parsing and transpilation within Maude itself,
  all we do here is convert the list of rules to a textual, string
  representation that Maude can parse.
-}
module LS.XPile.Maude where

import Control.Lens (bimap)
import Control.Monad (join)
import Data.Coerce ( coerce )
import Data.Foldable ( Foldable(foldMap') )
import Data.List.NonEmpty ( NonEmpty((:|)) )
import Data.Text qualified as T
import GHC.TypeLits ( Nat )

-- import Data.Text qualified as T

import LS.Types
    ( Deontic(DShant, DMust, DMay),
      TemporalConstraint(TemporalConstraint),
      RegKeywords(RParty), MTExpr (MTT), TComparison (TBefore) )
import LS.Rule
    ( Rule(..) )
import Flow ( (|>) )
import Prettyprinter
    ( hsep, line, Doc, Pretty(pretty) )
import AnyAll (BoolStruct(Leaf))
import Data.Functor ((<&>))

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
      [hencelest2str hence],
      [hencelest2str lest]
    ]
    |> foldMapWithNewLines @1 hsep
    where
      deontic2str DMust = "MUST"
      deontic2str DMay = "MAY"
      deontic2str DShant = "SHANT"

rule2doc _ = "Not supported."

rules2doc :: Foldable t => t Rule -> Doc ann
rules2doc rules = rules |> foldMapWithNewLines @2 rule2doc

pretty2Qid :: T.Text -> Doc ann
pretty2Qid x = x |> T.strip |> pretty |> ("'" <>)

rules2maudeStr :: Foldable t => t Rule -> String
rules2maudeStr rules = rules |> rules2doc |> show

hencelest2str :: Maybe Rule -> Doc ann
hencelest2str hence = hence |> maybe "NOTHING" f
  where
    f (RuleAlias hence') = hence' <&> quotOrUpper |> hsep
    f _ = ""
    quotOrUpper (MTT (T.toLower -> "and")) = "AND"
    quotOrUpper (MTT x) = x |> pretty2Qid
    quotOrUpper _ = ""

foldMapWithNewLines ::
  forall n a ann t.
  (Foldable t, Semigroup (CatWithNewLines n ann)) =>
  (a -> Doc ann) ->
  t a ->
  Doc ann
foldMapWithNewLines f docs = docs |> foldMap' f' |> coerce
  where
    f' :: a -> CatWithNewLines n ann
    f' x = x |> f |> coerce

-- Used to define the monoid <> op for CatWithNewLines
catWithNewLines ::
  forall n ann.
  Int ->
  CatWithNewLines n ann ->
  CatWithNewLines n ann ->
  CatWithNewLines n ann
catWithNewLines n x y = [x', lines', y'] |> mconcat |> coerce
  where
    (x', y') = (x, y) |> join bimap coerce2doc
    lines' = line |> replicate n |> mconcat
    coerce2doc :: CatWithNewLines n ann -> Doc ann
    coerce2doc = coerce

-- Boring utilities below.
newtype CatWithNewLines (n :: Nat) ann = CatWithNewLines (Doc ann)

instance
  Semigroup (CatWithNewLines n ann) =>
  Monoid (CatWithNewLines n ann)
  where
    mempty = CatWithNewLines ""

instance Semigroup (CatWithNewLines 1 ann) where
  (<>) = catWithNewLines 1

instance Semigroup (CatWithNewLines 2 ann) where
  (<>) = catWithNewLines 2