{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.Edn.Ast
where

import Data.EDN qualified as EDN
import Data.EDN.QQ (edn)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Text qualified as T
import Data.Text.Read qualified as TRead
import Data.Functor.Foldable (Recursive(..))
import Flow ((|>))
import Data.List (intersperse)
import Data.Either (rights)
import Data.String.Interpolate (i)

data AstNode metadata
  = Parenthesised {metadata :: Maybe metadata, children :: [AstNode metadata]}
  | List {metadata :: Maybe metadata, elements :: [AstNode metadata]}
  | Text {metadata :: Maybe metadata, text :: T.Text}
  | Variable {metadata :: Maybe metadata, variable :: T.Text}
  | Not {metadata :: Maybe metadata, negated :: AstNode metadata}
  | AndOr
      { metadata :: Maybe metadata,
        andOr :: T.Text,
        children :: [AstNode metadata]
      }
  | Rule
      { metadata :: Maybe metadata,
        head :: AstNode metadata,
        body :: AstNode metadata
      }
  -- deriving (Eq, Ord, Read, Show, Generic, Hashable)

makeBaseFunctor ''AstNode

astToEdn :: AstNode metadata -> T.Text
astToEdn = EDN.renderText . cata \case
  ParenthesisedF _ nodes -> EDN.toEDN nodes
  TextF _ text -> toSymbol text
  VariableF _ variable -> EDN.NoTag $ EDN.Symbol "var" variable
  AndOrF _ andOr children ->
    children |> intersperse (toSymbol andOr) |> EDN.toEDN
  NotF _ node -> EDN.toEDN [[edn|NOT|], node]
  RuleF _ head body -> EDN.toEDN [[edn|DECIDE|], head, [edn|IF|], body]
  ListF _ rules -> rules |> EDN.mkVec |> EDN.toEDN
  where
    toSymbol x = EDN.Symbol "" [i|#{x}|] |> EDN.NoTag

pattern Number :: Maybe metadata -> Double -> AstNode metadata
pattern Number metadata number <-
  Text metadata (TRead.double -> Right (number, ""))
  where
    Number metadata number = Text metadata [i|#{number}|]

pattern Integer :: Maybe metadata -> Integer -> AstNode metadata
pattern Integer metadata number <-
  Text metadata (TRead.decimal -> Right (number, ""))
  where
    Integer metadata number = Text metadata [i|#{number}|]

pattern Date :: Maybe metadata -> Integer -> Integer -> Integer -> AstNode metadata
pattern Date metadata year month day =
  Parenthesised
    metadata [Int year, Dash, Int month, Dash, Int day]

pattern Int :: Integer -> AstNode metadata
pattern Int int = Integer Nothing int

pattern Dash :: AstNode metadata
pattern Dash = Text Nothing "-"

pattern Fact :: Maybe metadata -> AstNode metadata -> AstNode metadata
pattern Fact metadata head = Rule metadata head (Parenthesised Nothing [])

pattern Program :: Maybe metadata -> [AstNode metadata] -> AstNode metadata
pattern Program metadata rules = List metadata rules

pattern And :: Maybe metadata -> [AstNode metadata] -> AstNode metadata
pattern And metadata conjuncts = AndOr metadata "AND" conjuncts

pattern Or :: Maybe metadata -> [AstNode metadata] -> AstNode metadata
pattern Or metadata conjuncts = AndOr metadata "OR" conjuncts

pattern BinOp :: T.Text -> Maybe metadata -> AstNode metadata -> AstNode metadata -> AstNode metadata
pattern BinOp binOp metadata lhs rhs =
  Parenthesised metadata [lhs, Text Nothing binOp, rhs]

pattern Is :: Maybe metadata -> AstNode metadata -> AstNode metadata -> AstNode metadata
pattern Is metadata lhs rhs = BinOp "IS" metadata lhs rhs

pattern Lt :: Maybe metadata -> AstNode metadata -> AstNode metadata -> AstNode metadata
pattern Lt metadata lhs rhs = BinOp "<" metadata lhs rhs

pattern Leq :: Maybe metadata -> AstNode metadata -> AstNode metadata -> AstNode metadata
pattern Leq metadata lhs rhs = BinOp "<=" metadata lhs rhs

pattern Gt :: Maybe metadata -> AstNode metadata -> AstNode metadata -> AstNode metadata
pattern Gt metadata lhs rhs = BinOp ">" metadata lhs rhs

pattern Geq :: Maybe metadata -> AstNode metadata -> AstNode metadata -> AstNode metadata
pattern Geq metadata lhs rhs = BinOp ">=" metadata lhs rhs

exampleProgram :: AstNode metadata
exampleProgram =
  Program
    Nothing
    [ Rule
        Nothing
        (Text Nothing "p")
        (And Nothing [Text Nothing "q", Text Nothing "r"]),
      Rule
        Nothing
        (Parenthesised Nothing [Variable Nothing "x", Text Nothing "is between 0 and 10 or is 100"])
        ( Or
            Nothing
            [ And
                  Nothing
                  [ Leq Nothing (Number Nothing 0) (Variable Nothing "x"),
                    Leq Nothing (Variable Nothing "x") (Number Nothing 10)
                  ],
              Is Nothing (Variable Nothing "x") (Number Nothing 100)
            ]
        ),
        Fact Nothing $ Parenthesised Nothing [Date Nothing 2023 1 10, Text Nothing "is a date"]
    ]

--- >>> astToEdn exampleProgram 
-- "[(DECIDE p IF (q AND r)) (DECIDE (var/x is between 0 and 10 or is 100) IF (((0.0 <= var/x) AND (var/x <= 10.0)) OR (var/x IS 100.0))) (DECIDE ((2023 - 1 - 10) is a date) IF ())]"
