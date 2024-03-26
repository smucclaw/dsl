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
import Control.Monad.Except (MonadError (..))
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
    ParamText,
    RPRel (..),
    RelationalPredicate (..),
    RelationalPredicateF (..),
    TComparison (..),
  )
import LS.Utils (eitherToList, (|$>))
import LS.XPile.Edn.Common.Ast
  ( AstNode (..),
    Op (..),
    pattern And,
    pattern Bool,
    pattern InfixBinOp,
    pattern Integer,
    pattern List,
    pattern Not,
    pattern Number,
    pattern Or,
    pattern Parens,
    pattern Program,
  )
import LS.XPile.Edn.Common.Utils (splitLast)
import LS.XPile.Edn.L4ToAst.MultiExprKeywords (multiExprKeywords)
import LS.XPile.Edn.L4ToAst.RPRelToTextTable (rpRelToTextTable)
import Language.Haskell.TH.Syntax qualified as TH
import Text.Regex.PCRE.Heavy qualified as PCRE
import Prelude hiding (head)

l4rulesToProgram :: [Rule] -> AstNode metadata
l4rulesToProgram =
  foldMap l4ruleToAstNodes
    >>> foldMap eitherToList
    >>> Program Nothing

l4ruleToAstNodes :: MonadError T.Text m => Rule -> [m (AstNode metadata)]
l4ruleToAstNodes Hornlike {keyword = Decide, given, clauses} =
  clauses |$> \HC {hHead, hBody} -> do
    head <- hHead |> relPredToAstNode metadata
    body <- hBody |> traverse (boolStructRToAstNode metadata)
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
  MonadError T.Text m =>
  Maybe metadata ->
  RelationalPredicate ->
  m (AstNode metadata)
relPredToAstNode metadata = cata \case
  RPMTF multiTerm -> pure $ parens $ multiTermToAstNodes multiTerm

  RPConstraintF
    lhsMultiTerm
    (rpRelToTextNode metadata -> Just rpRel)
    rhsMultiTerm ->
      pure $ parens $ lhs <> [rpRel] <> rhs
      where
        lhs = multiTermToAstNodes lhsMultiTerm
        rhs =
          rhsMultiTerm
            |> case rpRel of
              Text _ "IS" -> map capitaliseKeywordMTT
              _ -> id
            |> multiTermToAstNodes

        capitaliseKeywordMTT = \case
          (MTT text@(T.strip >>> (`elem` multiExprKeywords') -> True)) ->
            MTT $ T.toUpper text
          multiExpr -> multiExpr

        multiExprKeywords' = $(TH.lift multiExprKeywords)

  RPnaryF (rpRelToTextNode metadata -> Just rpRel) args -> do
    args <- sequenceA args
    pure $ parens case (rpRel, splitLast args) of
      -- Unparse stuff like (... IS SUM ...), (... IS PRODUCT ...),
      -- (... IS NOT IN ... ) etc.
      (Text _ "IS", Just (lhs, Parens _ (Text _ op : args))) ->
        [parens lhs, Text metadata [i|IS #{op}|], args']
        where
          args' =
            args
              |> if op PCRE.≈ [PCRE.re|^THE (SUM|PRODUCT|MIN|MAX) OF$|]
                then List metadata
                else parens

      _ -> rpRel : args

  _ -> throwError "Not supported"
  where
    parens = Parens metadata

    multiTermToAstNodes = map \case
      MTT text -> Text Nothing text
      MTI int -> Integer Nothing int
      MTF double -> Number Nothing double
      MTB bool -> Bool Nothing bool

rpRelToTextNode :: Maybe metadata -> RPRel -> Maybe (AstNode metadata)
rpRelToTextNode metadata =
  (`Map.lookup` rpRelToTextTable') >>> fmap (Text metadata)
  where
    rpRelToTextTable' = $(TH.lift rpRelToTextTable)

boolStructRToAstNode ::
  MonadError T.Text m => Maybe metadata -> BoolStructR -> m (AstNode metadata)
boolStructRToAstNode metadata = cata \case
  LeafF relPred -> relPredToAstNode metadata relPred
  NotF arg -> Not metadata <$> arg
  AllF _ children -> go And children
  AnyF _ children -> go Or children
  where
    go ctor children = do
      children <- sequenceA children
      pure $ ctor metadata children