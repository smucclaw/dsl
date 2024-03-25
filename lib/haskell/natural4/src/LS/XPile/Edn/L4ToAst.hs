{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.Edn.L4ToAst (l4rulesToProgram) where

import AnyAll (BoolStruct, BoolStructF (..))
import Control.Arrow ((>>>))
import Control.Monad (join)
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Foldable (Recursive (..))
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe, maybeToList)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Flow ((|>))
import LS.Rule (Rule (..))
import LS.TokenTable (MyToken (Decide))
import LS.Types
  ( BoolStructR,
    HornClause (..),
    MTExpr (..),
    MultiTerm,
    RPRel (..),
    RelationalPredicate (..),
    TComparison (..), ParamText,
  )
import LS.Utils ((|$>))
import LS.XPile.Edn.Common.Ast
  ( AstNode (..),
    Op (..),
    pattern Bool,
    pattern InfixBinOp,
    pattern Integer,
    pattern Number,
    pattern Parens,
    pattern Program,
    pattern And,
    pattern Or,
    pattern Not,
  )
import LS.XPile.Edn.L4ToAst.RelToTextTable (relToTextTable)
import Language.Haskell.TH.Syntax (lift)
import Prelude hiding (head)

l4rulesToProgram :: [Rule] -> AstNode metadata
l4rulesToProgram = foldMap l4ruleToAstNodes >>> Program Nothing

l4ruleToAstNodes :: Rule -> [AstNode metadata]
l4ruleToAstNodes Hornlike {keyword = Decide, given, clauses} = do
  HC {hHead, hBody} <- clauses
  
  head <- relPredToAstNode metadata hHead
  body <- hBody |$> boolStructRToAstNode metadata |> maybeToList

  pure HornClause {metadata, givens, head, body}
  where
    metadata = Nothing
    givens = given |> givenToGivens

l4ruleToAstNodes _ = []

givenToGivens :: Maybe ParamText -> [T.Text]
givenToGivens =
  maybeNonEmptyListToList
    >>> mapMaybe \case
      (MTT varName NE.:| _, _) -> Just varName
      _ -> Nothing
  where
    maybeNonEmptyListToList :: Maybe (NE.NonEmpty a) -> [a]
    maybeNonEmptyListToList = maybeToList >>> foldMap NE.toList

relPredToAstNode ::
  forall metadata m.
  (MonadFail m) =>
  Maybe metadata ->
  RelationalPredicate ->
  m (AstNode metadata)
relPredToAstNode metadata = \case
  RPMT multiTerm -> pure $ multiTermToAst multiTerm

  RPConstraint multiTerm rel multiTerm' ->
    pure $ InfixBinOp metadata op lhs rhs
    where
      (lhs, rhs) = join bimap multiTermToAst (multiTerm, multiTerm')
      op = $(lift relToTextTable) |> Map.findWithDefault [i|#{rel}|] rel

  _ -> fail "Not supported"
  where
    multiTermToAst :: MultiTerm -> AstNode metadata =
      Parens metadata . map \case
        MTT text -> Text Nothing text
        MTI int -> Integer Nothing int
        MTF double -> Number Nothing double
        MTB bool -> Bool Nothing bool

boolStructRToAstNode ::
  MonadFail m => Maybe metadata -> BoolStructR -> m (AstNode metadata)
boolStructRToAstNode metadata = cata \case
  LeafF relPred -> relPredToAstNode metadata relPred
  NotF arg -> Not metadata <$> arg
  AllF _ children -> go And children
  AnyF _ children -> go Or children
  where
    go ctor children = do
      children <- sequenceA children
      pure $ ctor metadata children