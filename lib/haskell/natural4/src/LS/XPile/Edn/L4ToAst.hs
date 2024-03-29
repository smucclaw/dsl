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
import Control.Monad.Except (MonadError (..))
import Data.Functor.Foldable.Monadic (cataM)
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
import LS.Utils (eitherToList, trimWhitespaces, (|$>))
import LS.XPile.Edn.Common.Ast
  ( AstNode (..),
    Op (..),
    pattern And,
    pattern Bool,
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

givenToGivens :: Maybe ParamText -> [(T.Text, Maybe T.Text)]
givenToGivens =
  maybeNonEmptyListToList
    >>> mapMaybe \case
      (MTT varName NE.:| _, typeSig) ->
        Just (trimWhitespaces varName, typeSig |$> const "Thing")
      _ -> Nothing
  where
    maybeNonEmptyListToList :: Maybe (NE.NonEmpty a) -> [a]
    maybeNonEmptyListToList = maybeToList >>> foldMap NE.toList

relPredToAstNode ::
  MonadError T.Text m =>
  Maybe metadata ->
  RelationalPredicate ->
  m (AstNode metadata)
relPredToAstNode metadata = cataM \case
  RPMTF multiTerm -> parens $ multiTermToAstNodes multiTerm

  RPConstraintF
    (multiTermToAstNodes -> lhs)
    (rpRelToTextNode metadata -> Just rpRel)
    (multiTermToAstNodes -> rhs) ->
      parens $ lhs <> [rpRel] <> rhs

  RPnaryF (rpRelToTextNode metadata -> Just rpRel) args ->
    parens case (rpRel, splitLast args, args) of
      (Text {text = "IS"}, Just (lhs, rhs), _) ->
        [Parens metadata lhs, rpRel, rhs]

      (_, _, [Parens _ args@(null -> False)]) ->
        [rpRel, args']
        where
          args' = case args of
            [arg] -> arg
            _ ->
              args
                |$> (\arg -> Parens metadata [arg])
                |> List metadata

      _ -> rpRel : args

  _ -> throwError "Not supported"
  where
    parens = pure . Parens metadata

    multiTermToAstNodes = map \case
      MTT text ->
        Text {metadata, text = text |> trimWhitespaces |> keywordToUpper}
      MTI int -> Integer metadata int
      MTF double -> Number metadata double
      MTB bool -> Bool metadata bool

    keywordToUpper text
      | text `elem` $(TH.lift multiExprKeywords) = T.toUpper text
      | otherwise = text

rpRelToTextNode :: Maybe metadata -> RPRel -> Maybe (AstNode metadata)
rpRelToTextNode metadata =
  rpRelToText >>> fmap \text -> Text {metadata, text}
  where
    rpRelToText = (`Map.lookup` $(TH.lift rpRelToTextTable))

boolStructRToAstNode ::
  MonadError T.Text m => Maybe metadata -> BoolStructR -> m (AstNode metadata)
boolStructRToAstNode metadata = cataM \case
  LeafF relPred -> relPredToAstNode metadata relPred
  NotF arg -> go Not arg
  AllF _ args -> go And args
  AnyF _ args -> go Or args
  where
    go ctor = pure . ctor metadata