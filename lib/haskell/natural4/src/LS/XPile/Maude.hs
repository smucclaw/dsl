{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-
  Work-in-progress transpiler to Maude.
  Note that since we do all the parsing and transpilation within Maude itself,
  all we do here is convert the list of rules to a textual, string
  representation that Maude can parse.
-}
module LS.XPile.Maude where

import Data.Text qualified as T

import LS.Types
    ( Deontic(DShant, DMust, DMay),
      TemporalConstraint(TemporalConstraint),
      RegKeywords(RParty) )
import LS.Rule
    ( Rule(..) )

import Flow ( (.>) , (|>) )
import Prettyprinter (Doc, Pretty (pretty), line, viaShow, hsep)

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
    |> mapThenCatWith hsep catWith1line
    where
      catWith1line = catWithNLines 1
      deontic2str DMust = "MUST"
      deontic2str DMay = "MAY"
      deontic2str DShant = "SHANT"
      makeQid x = "'" <> x
      show2Qid x = x |> viaShow |> makeQid
      ruleNameQid = ruleName |> pretty |> makeQid

rules2doc :: Foldable t => t Rule -> Doc ann
rules2doc = mapThenCatWith rule2doc catWith2lines
  where
    catWith2lines = catWithNLines 2

rules2maudeStr :: Foldable t => t Rule -> String
rules2maudeStr = rules2doc .> show

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

mapThenCatWith ::
  (Foldable t1, Monoid b) => (a -> t2) -> (t2 -> b -> b) -> t1 a -> b
mapThenCatWith f binop = foldr binop' mempty
  where
    x `binop'` y = f x `binop` y

catWithNLines :: Int -> Doc ann -> Doc ann -> Doc ann
catWithNLines n x y = x <> lines' <> y
  where
    lines' = line |> repeat |> take n |> mconcat