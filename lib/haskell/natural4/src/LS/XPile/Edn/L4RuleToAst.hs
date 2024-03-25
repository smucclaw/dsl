{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.Edn.L4RuleToAst (l4RulesToProgram) where

import Control.Arrow ((>>>))
import Control.Monad (join)
import Data.Bifunctor (Bifunctor (..))
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe, maybeToList)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Flow ((|>))
import LS.Rule (Rule (..))
import LS.TokenTable (MyToken (..))
import LS.Types
  ( HornClause (..),
    MTExpr (..),
    MultiTerm,
    RPRel (..),
    RelationalPredicate (..),
    TComparison (..),
  )
import LS.Utils ((|$>))
import LS.XPile.Edn.Ast
  ( AstNode (..),
    Op (..),
    pattern Bool,
    pattern InfixBinOp,
    pattern Integer,
    pattern Number,
    pattern Parens,
    pattern Program,
  )
import LS.XPile.Edn.RelToTextTable (relToTextTable)
import Language.Haskell.TH.Syntax (lift)
import Prelude hiding (head)

l4RulesToProgram :: [Rule] -> AstNode metadata
l4RulesToProgram = foldMap l4RuleToAstNodes >>> Program Nothing

l4RuleToAstNodes :: Rule -> [AstNode metadata]
l4RuleToAstNodes Hornlike {keyword = Decide, given, clauses} = do
  HC {hHead, hBody} <- clauses

  let metadata = Nothing
      givens =
        given
          |> maybeNonEmptyListToList
          |> mapMaybe \case
            (MTT varName NE.:| _, _) -> Just varName
            _ -> Nothing

      head = rpToAstNode metadata hHead
      body = hBody |$> undefined

  pure HornClause {metadata, givens, head, body}

l4RuleToAstNodes _ = []

maybeNonEmptyListToList :: Maybe (NE.NonEmpty a) -> [a]
maybeNonEmptyListToList = maybeToList >>> foldMap NE.toList

rpToAstNode :: Maybe metadata -> RelationalPredicate -> AstNode metadata
rpToAstNode metadata (RPMT multiTerm) = multiTermToAst metadata multiTerm

rpToAstNode metadata (RPConstraint mtt rel mtt') =
  InfixBinOp metadata op lhs rhs
  where
    (lhs, rhs) = join bimap (multiTermToAst Nothing) (mtt, mtt')
    op = $(lift relToTextTable) |> Map.findWithDefault [i|#{rel}|] rel

rpToAstNode _metadata _rp = undefined

multiTermToAst :: Maybe metadata -> MultiTerm -> AstNode metadata
multiTermToAst metadata =
  Parens metadata . map \case
    MTT text -> Text Nothing text
    MTI int -> Integer Nothing int
    MTF double -> Number Nothing double
    MTB bool -> Bool Nothing bool 