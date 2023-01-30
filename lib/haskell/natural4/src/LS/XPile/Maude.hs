{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-
  Work-in-progress transpiler to Maude.
  Note that since we do all the parsing and transpilation within Maude itself,
  all we do here is convert the list of rules to a textual, string
  representation that Maude can parse.
-}
module LS.XPile.Maude where

import Data.Coerce ( coerce )
import Data.Foldable ( Foldable(foldMap') )
import GHC.TypeLits

import Data.Text qualified as T

import LS.Types
    ( Deontic(DShant, DMust, DMay),
      TemporalConstraint(TemporalConstraint),
      RegKeywords(RParty) )
import LS.Rule
    ( Rule(..) )

import Flow ( (|>) )
import Prettyprinter
    ( cat, hsep, line, viaShow, Doc, Pretty(pretty) )


-- This function is still a work in progress.
rule2doc :: Rule -> Doc ann
rule2doc
  Regulative
    { rlabel = Just (_, _, ruleName),
      rkeyword = RParty,
      subj,
      deontic,
      action,
      temporal = Just tempConstr@(TemporalConstraint _ _ _),
      hence,
      lest
    } =
    [ ["RULE", ruleNameQid],
      ["PARTY", show2Qid subj],
      [deontic2str deontic, "DO", show2Qid action],
      [viaShow tempConstr],
      [show2Qid hence],
      [show2Qid lest]
    ]
    |> foldMapWithNewLines @1 hsep
    where
      deontic2str DMust = "MUST"
      deontic2str DMay = "MAY"
      deontic2str DShant = "SHANT"
      makeQid x = "'" <> x
      show2Qid x = x |> viaShow |> makeQid
      ruleNameQid = ruleName |> pretty |> makeQid

rule2doc _ = "unsupported"

rules2doc :: Foldable t => t Rule -> Doc ann
rules2doc rules = rules |> foldMapWithNewLines @2 rule2doc

rules2maudeStr :: Foldable t => t Rule -> String
rules2maudeStr rules = rules |> rules2doc |> show

test :: String
test = rules2maudeStr [ Regulative {..} ]
  where
    rlabel = Just (undefined, undefined, T.pack "rule")
    rkeyword = RParty
    subj = undefined
    deontic = DMust
    action = undefined
    temporal = Just (TemporalConstraint undefined undefined undefined)
    hence = Nothing
    lest = Nothing

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

-- Utilities.

newtype CatWithNewLines (n :: Nat) ann where
    CatWithNewLines :: forall n ann. Doc ann -> CatWithNewLines n ann

class Inhabited t where
    defaultElem :: t

instance (Semigroup t, Inhabited t) => Monoid t where
    mempty = defaultElem

instance Inhabited (CatWithNewLines n ann) where
    defaultElem = CatWithNewLines ""

instance Semigroup (CatWithNewLines 0 ann) where
  (<>) = catWithNewLines 0

instance Semigroup (CatWithNewLines 1 ann) where
  (<>) = catWithNewLines 1

instance Semigroup (CatWithNewLines 2 ann) where
  (<>) = catWithNewLines 2

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

catWithNewLines ::
  forall n ann.
  Int ->
  CatWithNewLines n ann ->
  CatWithNewLines n ann ->
  CatWithNewLines n ann
catWithNewLines n x y = [x', lines', y'] |> cat |> coerce
  where
    x' = coerce2doc x
    y' = coerce2doc y
    lines' = line |> repeat |> take n |> cat
    coerce2doc :: CatWithNewLines n ann -> Doc ann
    coerce2doc = coerce