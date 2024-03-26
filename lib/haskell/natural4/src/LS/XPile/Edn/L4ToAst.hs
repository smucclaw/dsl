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
    pattern Not,
    pattern Number,
    pattern Or,
    pattern Parens,
    pattern Program,
  )
import LS.XPile.Edn.Common.Utils (splitLast)
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
  RPMTF multiTerm -> parens $ multiTermToAstNodes multiTerm

  RPConstraintF
    multiTerm
    rel@(rpRelToTextNode metadata -> Just relText)
    multiTerm' ->
      parens $ lhs <> [relText] <> rhs
      where
        lhs = multiTermToAstNodes multiTerm
        rhs =
          multiTerm'
            |> case rel of
              RPis -> map capitaliseKeywordMTT
              _ -> id
            |> multiTermToAstNodes

        capitaliseKeywordMTT (MTT text) =
          text
            |> T.strip
            |> PCRE.sub
              [PCRE.re|^((day|week|month|year)s?)$|]
              (\(text : _) -> T.toUpper text)
            |> MTT
        capitaliseKeywordMTT multiExpr = multiExpr

  -- Handle (... IS SUM ...) and (... IS PRODUCT ...)
  RPnaryF RPis (splitLast -> Just (relPreds, opWithArgs)) -> do
    relPreds <- sequenceA relPreds
    opWithArgs <- opWithArgs

    case opWithArgs of
      Parens _ (Text metadata' op : args) ->
        parens $ relPreds <> [Text metadata' [i|IS THE #{op} OF|]] <> args

  RPnaryF (rpRelToTextNode metadata -> Just relText) args -> do
    args <- sequenceA args
    parens $ relText : args

  _ -> throwError "Not supported"
  where
    parens = pure . Parens metadata

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