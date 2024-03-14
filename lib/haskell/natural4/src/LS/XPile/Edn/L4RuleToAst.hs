{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

module LS.XPile.Edn.L4RuleToAst
  (l4RuleToAstNodes, l4RulesToProgram)
where

import LS.Rule (Rule (..))
import LS.TokenTable (MyToken (..))
import LS.Types (HornClause (..))
import LS.XPile.Edn.Ast
  ( AstNode (..),
    pattern Program,
  )
import Prelude hiding (head)

l4RulesToProgram :: [Rule] -> AstNode metadata
l4RulesToProgram = Program Nothing . foldMap l4RuleToAstNodes

l4RuleToAstNodes :: Rule -> [AstNode metadata]
l4RuleToAstNodes Hornlike {keyword = Decide, given, clauses} = do
  HC {hHead, hBody} <- clauses

  let metadata = Nothing
      givens = undefined
      head = undefined
      body = undefined

  pure RuleFact {metadata, givens, head, body}

l4RuleToAstNodes _ = []