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

import AnyAll (BoolStructF (..))
import Control.Arrow ((>>>))
import Control.Monad (join)
import Control.Monad.Except (MonadError (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Foldable.Monadic (cataM)
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty qualified as NE
import Data.Maybe (maybeToList)
import Data.Text qualified as T
import Flow ((|>))
import LS.Rule (Rule (..))
import LS.TokenTable (MyToken (Decide))
import LS.Types
  ( BoolStructR,
    HornClause (..),
    MTExpr (..),
    ParamText,
    ParamType (..),
    RPRel (..),
    RelationalPredicate (..),
    RelationalPredicateF (..),
    TypeSig (..),
  )
import LS.Utils (eitherToList, trimWhitespaces, (|$>))
import LS.XPile.Edn.Common.Ast
  ( AstNode (..),
    pattern And,
    pattern Bool,
    pattern Integer,
    pattern IsA,
    pattern IsOneOf,
    pattern Not,
    pattern Number,
    pattern Or,
    pattern Parens,
    pattern Program,
  )
import LS.XPile.Edn.Common.Utils (splitLast)
import LS.XPile.Edn.L4ToAst.MultiExprKeywords (multiExprKeywords)
import LS.XPile.Edn.L4ToAst.ToTextTables
  ( paramTypeToTextTable,
    rpRelToTextTable,
  )
import Language.Haskell.TH.Syntax qualified as TH
import Prelude hiding (head)

l4rulesToProgram :: [Rule] -> AstNode String
l4rulesToProgram =
  foldMap l4ruleToAstNodes
    >>> foldMap eitherToList
    >>> Program Nothing

l4ruleToAstNodes :: MonadError T.Text m => Rule -> [m (AstNode String)]
l4ruleToAstNodes = \case
  Hornlike {keyword = Decide, given, giveth, clauses} ->
    clauses |$> \HC {hHead, hBody} -> do
      givens <- givens
      giveths <- giveths
      head <- hHead |> relPredToAstNode metadata
      body <- hBody |> traverse (boolStructRToAstNode metadata)
      pure HornClause {metadata, givens, giveths, head, body}
      where
        metadata = Nothing
        (givens, giveths) =
          (given, giveth) |> join bimap (givenToAstNodes metadata)
  _ -> []

givenToAstNodes ::
  forall metadata m.
  MonadError T.Text m =>
  Maybe metadata ->
  Maybe ParamText ->
  m [AstNode metadata]
givenToAstNodes metadata =
  maybeNonEmptyListToList >>> traverse (uncurry varDeclToAstNode)
  where
    maybeNonEmptyListToList :: Maybe (NE.NonEmpty a) -> [a]
    maybeNonEmptyListToList = maybeToList >>> foldMap NE.toList

    varDeclToAstNode ::
      NE.NonEmpty MTExpr -> Maybe TypeSig -> m (AstNode metadata)
    varDeclToAstNode (MTT varName NE.:| _) = \case
      Nothing -> pure var

      Just
        ( SimpleType
            paramType'@(paramTypeToText -> Just paramType)
            entityType
          ) -> pure $ IsA metadata var typ
          where
            typ = Text metadata <$> case paramType' of
              TOne -> [entityType]
              _ -> [paramType, entityType]

      Just (InlineEnum TOne ((NE.toList -> multiTerm, Nothing) NE.:| [])) ->
        pure $ IsOneOf metadata var elements
        where
          elements = multiTerm |> multiTermToAstNodes metadata

      _ -> throwError "Not supported"
      where
        var = Text {metadata, text = trimWhitespaces varName}
        paramTypeToText = (`Map.lookup` $(TH.lift paramTypeToTextTable))

    varDeclToAstNode _ = const $ throwError "Not supported"

multiTermToAstNodes :: Maybe metadata -> [MTExpr] -> [AstNode metadata]
multiTermToAstNodes metadata = map \case
  MTT text ->
    Text {metadata, text = text |> trimWhitespaces |> keywordToUpper}
  MTI int -> Integer metadata int
  MTF double -> Number metadata double
  MTB bool -> Bool metadata bool
  where
    keywordToUpper text
      | text `elem` $(TH.lift multiExprKeywords) = T.toUpper text
      | otherwise = text

relPredToAstNode ::
  MonadError T.Text m =>
  Maybe metadata ->
  RelationalPredicate ->
  m (AstNode metadata)
relPredToAstNode metadata = cataM \case
  RPMTF multiTerm -> parens $ multiTermToAstNodes metadata multiTerm

  RPConstraintF
    (multiTermToAstNodes metadata -> lhs)
    (rpRelToTextNode -> Just rpRel)
    (multiTermToAstNodes metadata -> rhs) ->
      parens $ lhs <> [rpRel] <> rhs

  RPnaryF rpRel'@(rpRelToTextNode -> Just rpRel) args ->
    parens case (rpRel', splitLast args, args) of
      (RPis, Just (lhs, rhs), _) -> lhs <> [rpRel, rhs]
      (_, _, [Parens _ args]) -> rpRel : args
      _ -> rpRel : args

  _ -> throwError "Not supported"
  where
    parens = pure . Parens metadata

    rpRelToTextNode =
      (`Map.lookup` $(TH.lift rpRelToTextTable))
        >>> fmap \text -> Text {metadata, text}

boolStructRToAstNode ::
  MonadError T.Text m => Maybe metadata -> BoolStructR -> m (AstNode metadata)
boolStructRToAstNode metadata = cataM \case
  LeafF relPred -> relPredToAstNode metadata relPred
  NotF arg -> go Not arg
  AllF _ args -> go And args
  AnyF _ args -> go Or args
  where
    go ctor = pure . ctor metadata
