{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-
  Work-in-progress transpiler to Maude.
  Note that since we do all the parsing and transpilation within Maude itself,
  all we do here is convert the list of rules to a textual, string
  representation that Maude can parse.
-}
module LS.XPile.Maude where

import LS.Types
    ( Deontic(DShant, DMust, DMay),
      TemporalConstraint(TemporalConstraint),
      RegKeywords(RParty) )
import LS.Rule
    ( Rule(Regulative, lest, rlabel, rkeyword, deontic, action,
           temporal, hence) )

import Prettyprinter ( Doc, cat, vcat, viaShow, Pretty(pretty) )
import Flow ( (|>) )

-- This function is still a work in progress.
rule2text :: Rule -> Doc ann
rule2text
  Regulative
    { rlabel = Just (_, _, ruleName),
      rkeyword = RParty,
      deontic,
      action,
      temporal = Just tempConstr@(TemporalConstraint _ _ _),
      hence,
      lest
    } =
      [ ["RULE '", pretty ruleName],
        [deontic2str deontic, "DO '", viaShow action],
        [viaShow tempConstr],
        ["'", viaShow hence],
        ["'", viaShow lest]
      ]
      |$> cat |> vcat
    where
      deontic2str DMust = "MUST"
      deontic2str DMay = "MAY"
      deontic2str DShant = "SHANT"

rules2maude :: [Rule] -> String
rules2maude rules = rules |$> rule2text |> show

-- Utilities.

infixl 0 |$>

(|$>) :: Functor f => f a -> (a -> b) -> f b
x |$> f = fmap f x