{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module LS.XPile.Maude.Rules
  ( rules2maudeStr,
  )
where

import Control.Monad.Validate (Validate, runValidate)
import Data.Coerce (coerce)
import Data.Either (rights)
import Data.Maybe (mapMaybe)
import Data.MonoTraversable (Element, otoList)
import Data.Monoid (Ap (Ap))
import Data.Sequences as Seq (IsSequence)
import Flow ((|>))
import LS.Rule (Rule (..))
import LS.XPile.Maude.Rule (rule2doc)
import LS.XPile.Maude.Utils (text2qid, (|$>))
import Prettyprinter (Doc, concatWith, line, (<+>))
import Prettyprinter.Interpolate (di)

-- Main function to transpile rules to plaintext natural4 for Maude.
rules2maudeStr :: (IsSequence t, Element t ~ Rule) => t -> String
rules2maudeStr rules = rules |> rules2doc |> show

{-
  This function happily swallows up rules that don't transpile properly and
  only outputs those that do to plaintext.
-}
rules2doc :: (IsSequence t, Element t ~ Rule) => t -> Doc ann
rules2doc rules =
  startRule <> transpiledRules
    -- TODO:
    -- Don't just swallow up errors and turn them into mempty.
    -- Actually output a comment indicating what went wrong while transpiling
    -- those erraneous rules.
    |> (coerce :: [Ap (Validate a) b] -> [Validate a b])
    |$> runValidate
    |> rights
    |> concatWith (<.>)
  where
    -- Find the first regulative rule and extracts its rule name.
    -- If such a rule exists, we turn it into a quoted symbol and prepend START.
    startRule =
      rules'
        |> mapMaybe rule2maybeStartRuleLabel
        |> take 1

    -- Transpile the rules to docs and collect all those that transpiled
    -- correctly, while ignoring erraneous ones.
    transpiledRules = rules' |$> rule2doc

    rules' = otoList rules

    rule2maybeStartRuleLabel Regulative {rlabel = Just (_, _, ruleName)} =
      Just $ pure [di|START #{text2qid ruleName}|]
    rule2maybeStartRuleLabel _ = Nothing

    x <.> y = [di|#{x},\n\n#{y}|]