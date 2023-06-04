{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module LS.XPile.Maude.Rules
  ( rules2maudeStr,
  )
where

import Data.Coerce (coerce)
import Data.Either (rights)
import Data.Foldable qualified as Fold
import Data.Maybe (mapMaybe)
import Data.MonoTraversable (Element, otoList)
import Data.Sequences as Seq (IsSequence)
import Flow ((|>))
import LS.Rule (Rule (..))
import LS.Utils ((|$>), swallowErrs)
import LS.XPile.Maude.Rule (rule2doc)
import LS.XPile.Maude.Utils (text2qid)
import Prettyprinter (Doc, concatWith)
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
  startRule <> swallowErrs transpiledRules
    -- TODO:
    -- Don't just swallow up errors and turn them into mempty.
    -- Actually output a comment indicating what went wrong while transpiling
    -- those erraneous rules.
    |> concatWith (<.>)
  where
    -- Find the first regulative rule and extracts its rule name.
    -- If such a rule exists, we turn it into a quoted symbol and prepend START.
    startRule =
      rules'
        |> mapMaybe regRule2ruleName
        |> take 1
        |$> ruleName2startRule

    -- Transpile the rules to docs and collect all those that transpiled
    -- correctly, while ignoring erraneous ones.
    transpiledRules = rule2doc <$> rules'

    rules' = otoList rules

    regRule2ruleName Regulative {rlabel = Just (_, _, ruleName)} =
      Just ruleName
    regRule2ruleName _ = Nothing

    ruleName2startRule ruleName = [di|START #{text2qid ruleName}|]

    x <.> y = [di|#{x},\n\n#{y}|]