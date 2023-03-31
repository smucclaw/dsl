{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LS.XPile.Maude.Rules
  ( rules2maudeStr,
  )
where

import Data.Coerce (coerce)
import Data.Either (rights)
import Data.Maybe (mapMaybe)
import Data.Monoid (Ap (Ap))
import Data.Validation (Validation, toEither)
import Flow ((|>))
import LS.Rule (Rule (..))
import LS.XPile.Maude.Rule (rule2doc)
import LS.XPile.Maude.Utils (text2qid, (|$>))
import Prettyprinter (Doc, concatWith, line, (<+>))

-- Main function to transpile rules to plaintext natural4 for Maude.
rules2maudeStr :: [Rule] -> String
rules2maudeStr rules = rules |> rules2doc |> show

{-
  This function happily swallows up rules that don't transpile properly and
  only outputs those that do to plaintext.
-}
rules2doc :: [Rule] -> Doc ann
rules2doc rules =
  startRule <> transpiledRules
    -- TODO:
    -- Don't just swallow up errors and turn them into mempty.
    -- Actually output a comment indicating what went wrong while transpiling
    -- those erraneous rules.
    |> (coerce :: [Ap (Validation a) b] -> [Validation a b])
    |$> toEither
    |> rights
    |> concatWith (<.>)
  where
    -- Find the first regulative rule and extracts its rule name.
    -- This returns a Maybe because there may not be any regulative rule.
    -- In such cases, we simply return mempty, the empty doc.
    -- Otherwise, we turn it into a quoted symbol and prepend START.
    startRule =
      rules
        |> mapMaybe rule2maybeStartRuleLabel
        |> take 1

    -- Transpile the rules to docs and collect all those that transpiled
    -- correctly, while ignoring erraneous ones.
    transpiledRules = rules |$> rule2doc

    rule2maybeStartRuleLabel Regulative {rlabel = Just (_, _, ruleName)} =
      "START" <+> text2qid ruleName |> pure |> Just
    rule2maybeStartRuleLabel _ = Nothing

    x <.> y = mconcat [x, ",", line, line, y]