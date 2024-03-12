{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.Edn.Ast where

import Data.EDN qualified as EDN
import Data.EDN.QQ (edn)
import Data.Functor.Foldable (Recursive (..))
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable)
import Data.List (intersperse)
import Data.String.Interpolate (i)
import Data.String.Interpolate.Conversion (Interpolatable, IsCustomSink)
import Data.Text qualified as T
import Data.Text.Read qualified as TRead
import Flow ((|>))
import GHC.Generics (Generic)
import LS.Utils ((|$>))

data AstNode metadata
  = RuleFact
      { metadata :: Maybe metadata,
        givens :: HashSet T.Text,
        head :: AstNode metadata,
        body :: Maybe (AstNode metadata)
      }
  -- | AnnotatedWithGivens { givens :: HashSet T.Text, node :: AstNode metadata}
  | AndOr
      { metadata :: Maybe metadata,
        andOr :: T.Text,
        children :: [AstNode metadata]
      }
  | Parens {metadata :: Maybe metadata, children :: [AstNode metadata]}
  | List {metadata :: Maybe metadata, elements :: [AstNode metadata]}
  | Text {metadata :: Maybe metadata, text :: T.Text}
  -- | Variable {metadata :: Maybe metadata, variable :: T.Text}
  deriving Show

makeBaseFunctor ''AstNode

astToEdnText ::
  Interpolatable (IsCustomSink t) T.Text t => AstNode metadata -> t
astToEdnText astNode = [i|#{astNode |> astToEdn |> EDN.renderText}|]

astToEdn :: forall metadata. AstNode metadata -> EDN.TaggedValue
astToEdn astNode = cata (flip go) astNode mempty
  where
    go :: HashSet T.Text -> AstNodeF metadata (HashSet T.Text -> EDN.TaggedValue) -> EDN.TaggedValue
    go givens = \case
      RuleFactF _ (HashSet.union givens -> givens') head body ->
        EDN.toEDN $ [edn|DECIDE|] : head givens' : ifBody
        where
          ifBody = case body of
            Just body -> [[edn|IF|], body givens']
            Nothing -> []
      AndOrF _ andOr children ->
        children |> aux (intersperse $ toSymbol andOr)
      ParensF _ nodes -> nodes |> aux id
      ListF _ rules -> rules |> aux EDN.mkVec
      TextF _ text -> text |> if text `elem` givens then toVar else toSymbol
      where
        toPrefixedSymbol prefix x = EDN.Symbol prefix [i|#{x}|] |> EDN.toEDN
        toSymbol = toPrefixedSymbol ""
        toVar = toPrefixedSymbol "var"

        aux ::
          (EDN.ToEDN a) =>
          ([EDN.TaggedValue] -> a) ->
          [HashSet T.Text -> EDN.TaggedValue] ->
          EDN.TaggedValue
        aux f x =
          x |$> ($ givens) |> f |> EDN.toEDN

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

pattern Fact :: Maybe metadata -> HashSet T.Text -> AstNode metadata -> AstNode metadata
pattern Fact metadata givens head = RuleFact metadata givens head Nothing

pattern Rule :: Maybe metadata -> HashSet T.Text -> AstNode metadata -> AstNode metadata -> AstNode metadata
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

exampleProgram :: AstNode metadata
exampleProgram =
  Program
    Nothing
    [ Rule
        Nothing
        mempty
        (Text Nothing "p")
        (And Nothing [Text Nothing "q", Text Nothing "r"]),
      Rule
        Nothing
        ["x"]
        (Parens Nothing [Text Nothing "x", Text Nothing "is between 0 and 10 or is 100"])
        ( Or
            Nothing
            [ And
                  Nothing
                  [ Leq Nothing (Number Nothing 0) (Text Nothing "x"),
                    Leq Nothing (Text Nothing "x") (Number Nothing 10)
                  ],
              Is Nothing (Text Nothing "x") (Number Nothing 100)
            ]
        ),
        Fact Nothing mempty $ Parens Nothing [Date Nothing 2023 1 10, Text Nothing "is a date"]
    ]

--- >>> (astToEdnText exampleProgram :: T.Text) 
-- "[(DECIDE p IF (q AND r)) (DECIDE (var/x is between 0 and 10 or is 100) IF (((0.0 <= var/x) AND (var/x <= 10.0)) OR (var/x IS 100.0))) (DECIDE ((2023 - 1 - 10) is a date))]"
