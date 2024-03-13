{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.Edn.Ast
  ( AstNode (..),
    AstNodeF (..),
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

-- newtype Variable = Variable { variable :: T.Text }
--   deriving (Eq, Ord, Show, Semigroup, Monoid, IsString, Generic, Hashable)

-- textsToVars :: [T.Text] -> [Variable]
-- textsToVars = coerce

data AstNode metadata
  = RuleFact
      { metadata :: Maybe metadata,
        givens :: [T.Text],
        head :: AstNode metadata,
        body :: Maybe (AstNode metadata)
      }
  | AndOr
      { metadata :: Maybe metadata,
        andOr :: T.Text,
        children :: [AstNode metadata]
      }
  | Parens {metadata :: Maybe metadata, children :: [AstNode metadata]}
  | List {metadata :: Maybe metadata, elements :: [AstNode metadata]}
  | Text {metadata :: Maybe metadata, text :: T.Text}

makeBaseFunctor ''AstNode

pattern Number :: Maybe metadata -> Double -> AstNode metadata
pattern Number metadata number <-
  Text metadata (TRead.double -> Right (number, ""))
  where
    Number metadata number = Text metadata [i|#{number}|]

pattern Integer :: Maybe metadata -> Integer -> AstNode metadata
pattern Integer metadata int <-
  Text metadata (TRead.decimal -> Right (int, ""))
  where
    Integer metadata int = Text metadata [i|#{int}|]

pattern Date :: Maybe metadata -> Integer -> Integer -> Integer -> AstNode metadata
pattern Date metadata year month day =
  Parens
    metadata [Int year, Dash, Int month, Dash, Int day]

pattern PrefixOp :: Maybe metadata -> T.Text -> [AstNode metadata] -> AstNode metadata
pattern PrefixOp metadata op args =
  Parens metadata [Text Nothing op, Parens Nothing args]

pattern UnaryOp :: Maybe metadata -> T.Text -> AstNode metadata -> AstNode metadata
pattern UnaryOp metadata op arg = PrefixOp metadata op [arg]

pattern Not :: Maybe metadata -> AstNode metadata -> AstNode metadata
pattern Not metadata negated = UnaryOp metadata "NOT" negated

pattern Int :: Integer -> AstNode metadata
pattern Int int = Integer Nothing int

pattern Dash :: AstNode metadata
pattern Dash = Text Nothing "-"

pattern Fact :: Maybe metadata -> [T.Text] -> AstNode metadata -> AstNode metadata
pattern Fact metadata givens head = RuleFact metadata givens head Nothing

pattern Rule :: Maybe metadata -> [T.Text] -> AstNode metadata -> AstNode metadata -> AstNode metadata
pattern Rule metadata givens head body = RuleFact metadata givens head (Just body)

pattern Program :: Maybe metadata -> [AstNode metadata] -> AstNode metadata
pattern Program metadata rules = List metadata rules

pattern And :: Maybe metadata -> [AstNode metadata] -> AstNode metadata
pattern And metadata conjuncts = AndOr metadata "AND" conjuncts

pattern Or :: Maybe metadata -> [AstNode metadata] -> AstNode metadata
pattern Or metadata conjuncts = AndOr metadata "OR" conjuncts

pattern InfixBinOp :: Maybe metadata -> T.Text -> AstNode metadata -> AstNode metadata -> AstNode metadata
pattern InfixBinOp metadata binOp lhs rhs =
  Parens metadata [lhs, Text Nothing binOp, rhs]

pattern Is :: Maybe metadata -> AstNode metadata -> AstNode metadata -> AstNode metadata
pattern Is metadata lhs rhs = InfixBinOp metadata "IS" lhs rhs

pattern Lt :: Maybe metadata -> AstNode metadata -> AstNode metadata -> AstNode metadata
pattern Lt metadata lhs rhs = InfixBinOp metadata "<" lhs rhs

pattern Leq :: Maybe metadata -> AstNode metadata -> AstNode metadata -> AstNode metadata
pattern Leq metadata lhs rhs = InfixBinOp metadata "<=" lhs rhs

pattern Gt :: Maybe metadata -> AstNode metadata -> AstNode metadata -> AstNode metadata
pattern Gt metadata lhs rhs = InfixBinOp metadata ">" lhs rhs

pattern Geq :: Maybe metadata -> AstNode metadata -> AstNode metadata -> AstNode metadata
pattern Geq metadata lhs rhs = InfixBinOp metadata ">=" lhs rhs