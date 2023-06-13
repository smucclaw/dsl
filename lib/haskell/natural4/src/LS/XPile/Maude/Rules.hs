{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

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
import LS.Utils ((|$>), swallowErrs, runMonoidValidate)
import LS.XPile.Maude.Rule (rule2doc)
import LS.XPile.Maude.Utils (text2qid)
import Prettyprinter (Doc, concatWith)
import Prettyprinter.Interpolate (di)
import Control.Monad.Validate (runValidate)
import Data.Monoid (Ap (..))
import Data.Bifunctor (Bifunctor(..))

-- Main function to transpile rules to plaintext natural4 for Maude.
rules2maudeStr :: (IsSequence t, Element t ~ Rule) => t -> String
rules2maudeStr rules = rules |> rules2doc |> show

{-
  This function happily swallows up rules that don't transpile properly and
  only outputs those that do to plaintext.
-}
rules2doc :: forall ann t. (IsSequence t, Element t ~ Rule) => t -> Doc ann
rules2doc (otoList -> rules :: [Rule]) =
    -- TODO:
    -- Don't just swallow up errors and turn them into mempty.
    -- Actually output a comment indicating what went wrong while transpiling
    -- those erraneous rules.
  concatWith (<.>) $ startRule <> (transpiledRules |$> snd |> swallowErrs)
  where
    -- Find the name of the first regulative rule which got transpiled correctly.
    -- If such a rule exists, we turn it into a quoted symbol and prepend START.
    startRule :: [Doc ann] =
      transpiledRules
        |> mapMaybe (
            \case
              ( Regulative {rlabel = Just (_, _, ruleName)},
                runMonoidValidate -> Right _
               ) -> Just ruleName
              _ -> Nothing
          )
        |> take 1
        |$> \ruleName -> [di|START #{text2qid ruleName}|]

    -- Transpile all rules to plaintext, keeping track of the original rule.
    transpiledRules = do
      rule <- rules
      pure (rule, rule2doc rule)

    x <.> y = [di|#{x},\n\n#{y}|]