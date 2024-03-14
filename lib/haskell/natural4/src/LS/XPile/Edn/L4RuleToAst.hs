{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.Edn.L4RuleToAst
  (l4RulesToProgram)
where

import Control.Arrow ((>>>))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Flow ((|>))
import LS.Rule (Rule (..))
import LS.TokenTable (MyToken (..))
import LS.Types (HornClause (..), MTExpr (..), RelationalPredicate (..))
import LS.Utils ((|$>))
import LS.XPile.Edn.Ast
  ( AstNode (..),
    pattern Program,
  )
import Prelude hiding (head)

l4RulesToProgram :: [Rule] -> AstNode metadata
l4RulesToProgram = foldMap l4RuleToAstNodes >>> Program Nothing

l4RuleToAstNodes :: Rule -> [AstNode metadata]
l4RuleToAstNodes Hornlike {keyword = Decide, given, clauses} = do
  HC {hHead, hBody} <- clauses

  let metadata = Nothing

      givens = case given of
        Nothing -> []
        Just (NE.toList -> given) ->
          given
            |> mapMaybe
                \case
                  (MTT varName NE.:| _, _) -> Just varName
                  _ -> Nothing

      head = case hHead of
        RPConstraint mtt rel mtt' -> undefined
        _ -> undefined

      body = hBody |$> undefined

  pure RuleFact {metadata, givens, head, body}

l4RuleToAstNodes _ = []