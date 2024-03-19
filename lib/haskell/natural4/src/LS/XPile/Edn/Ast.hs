{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.Edn.Ast
  ( AstNode (..),
    AstNodeF (..),
    Op (..),
    pattern BoolTrue,
    pattern BoolFalse,
    pattern Number,
    pattern Integer,
    pattern Date,
    pattern PrefixOp,
    pattern UnaryOp,
    pattern InfixBinOp,
    pattern Fact,
    pattern Rule,
    pattern Program,
    pattern And,
    pattern Or,
    pattern Is,
    pattern Lt,
    pattern Leq,
    pattern Gt,
    pattern Geq,
    pattern Parens,
    pattern List,
    pattern Map,
    pattern Set,
  )
where

import Data.Coerce (coerce)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Hashable (Hashable)
import Data.String (IsString)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.Read qualified as TRead
import GHC.Generics (Generic)
import LS.XPile.Edn.Utils (listToPairs, pairsToList)

data AstNode metadata
  = HornClause
      { metadata :: Maybe metadata,
        givens :: [T.Text],
        head :: AstNode metadata,
        body :: Maybe (AstNode metadata)
      }
  | CompoundTerm
      { metadata :: Maybe metadata,
        op :: Op,
        children :: [AstNode metadata]
      }
  | Text {metadata :: Maybe metadata, text :: T.Text}
  deriving (Eq, Ord, Show, Generic, Hashable)

data Op
  = ParensOp
  | ListOp
  | MapOp
  | SetOp
  | AndOp
  | OrOp
  deriving (Eq, Ord, Show, Generic, Hashable)

makeBaseFunctor ''AstNode

pattern Parens :: Maybe metadata -> [AstNode metadata] -> AstNode metadata
pattern Parens {metadata, children} =
  CompoundTerm {metadata, op = ParensOp, children}

pattern List :: Maybe metadata -> [AstNode metadata] -> AstNode metadata
pattern List {metadata, elements} =
  CompoundTerm {metadata, op = ListOp, children = elements}

pattern Map ::
  Maybe metadata -> [(AstNode metadata, AstNode metadata)] -> AstNode metadata
pattern Map {metadata, kvPairs} <-
  CompoundTerm {metadata, op = MapOp, children = listToPairs -> kvPairs}
  where
    Map metadata kvPairs =
      CompoundTerm {metadata, op = MapOp, children = pairsToList kvPairs}

pattern Set :: Maybe metadata -> [AstNode metadata] -> AstNode metadata
pattern Set {metadata, elements} =
  CompoundTerm {metadata, op = SetOp, children = elements}

pattern Number :: Maybe metadata -> Double -> AstNode metadata
pattern Number {metadata, number} <-
  Text {metadata, text = TRead.double -> Right (number, "")}
  where
    Number metadata number = Text {metadata, text = [i|#{number}|]}

pattern BoolTrue :: Maybe metadata -> AstNode metadata
pattern BoolTrue {metadata} = Text {metadata, text = "true"}

pattern BoolFalse :: Maybe metadata -> AstNode metadata
pattern BoolFalse {metadata} = Text {metadata, text = "false"}

pattern Integer :: Maybe metadata -> Integer -> AstNode metadata
pattern Integer {metadata, int} <-
  Text {metadata, text = TRead.decimal -> Right (int, "")}
  where
    Integer metadata int = Text {metadata, text = [i|#{int}|]}

pattern Date ::
  Maybe metadata -> Integer -> Integer -> Integer -> AstNode metadata
pattern Date {metadata, year, month, day} =
  Parens
    { metadata,
      children = [Integer' year, Dash, Integer' month, Dash, Integer' day]
    }

pattern PrefixOp ::
  Maybe metadata -> T.Text -> [AstNode metadata] -> AstNode metadata
pattern PrefixOp {metadata, op, args} =
  Parens {metadata, children = [Text Nothing op, Parens Nothing args]}

pattern UnaryOp ::
  Maybe metadata -> T.Text -> AstNode metadata -> AstNode metadata
pattern UnaryOp {metadata, op, arg} = PrefixOp {metadata, op, args = [arg]}

pattern Not :: Maybe metadata -> AstNode metadata -> AstNode metadata
pattern Not metadata negated = UnaryOp metadata "NOT" negated

pattern Integer' :: Integer -> AstNode metadata
pattern Integer' {int} = Integer {metadata = Nothing, int}

pattern Dash :: AstNode metadata
pattern Dash = Text {metadata = Nothing, text = "-"}

pattern Fact ::
  Maybe metadata -> [T.Text] -> AstNode metadata -> AstNode metadata
pattern Fact {metadata, givens, head} =
  HornClause {metadata, givens, head, body = Nothing}

pattern Rule ::
  Maybe metadata ->
  [T.Text] ->
  AstNode metadata ->
  AstNode metadata ->
  AstNode metadata
pattern Rule {metadata, givens, head, body} =
  HornClause {metadata, givens, head, body = Just body}

pattern Program :: Maybe metadata -> [AstNode metadata] -> AstNode metadata
pattern Program {metadata, rules} = List {metadata, elements = rules}

pattern And :: Maybe metadata -> [AstNode metadata] -> AstNode metadata
pattern And {metadata, conjuncts} =
  CompoundTerm {metadata, op = AndOp, children = conjuncts}

pattern Or :: Maybe metadata -> [AstNode metadata] -> AstNode metadata
pattern Or {metadata, conjuncts} =
  CompoundTerm {metadata, op = OrOp, children = conjuncts}

pattern InfixBinOp ::
  Maybe metadata ->
  T.Text ->
  AstNode metadata ->
  AstNode metadata ->
  AstNode metadata
pattern InfixBinOp {metadata, binOp, lhs, rhs} =
  Parens
    { metadata,
      children = [lhs, Text {metadata = Nothing, text = binOp}, rhs]
    }

pattern Is ::
  Maybe metadata -> AstNode metadata -> AstNode metadata -> AstNode metadata
pattern Is {metadata, lhs, rhs} =
  InfixBinOp {metadata, lhs, binOp = "IS", rhs}

pattern Lt ::
  Maybe metadata -> AstNode metadata -> AstNode metadata -> AstNode metadata
pattern Lt {metadata, lhs, rhs} =
  InfixBinOp {metadata, lhs, binOp = "<", rhs}

pattern Leq ::
  Maybe metadata -> AstNode metadata -> AstNode metadata -> AstNode metadata
pattern Leq {metadata, lhs, rhs} =
  InfixBinOp {metadata, lhs, binOp = "<=", rhs}

pattern Gt ::
  Maybe metadata -> AstNode metadata -> AstNode metadata -> AstNode metadata
pattern Gt {metadata, lhs, rhs} =
  InfixBinOp {metadata, lhs, binOp = ">", rhs}

pattern Geq ::
  Maybe metadata -> AstNode metadata -> AstNode metadata -> AstNode metadata
pattern Geq {metadata, lhs, rhs} =
  InfixBinOp {metadata, lhs, binOp = ">=", rhs}