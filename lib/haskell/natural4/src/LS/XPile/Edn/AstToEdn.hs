{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.Edn.AstToEdn
  ( astToEdn,
  )
where

import Control.Arrow ((>>>))
import Control.Monad.Reader qualified as Reader
import Data.EDN qualified as EDN
import Data.EDN.QQ qualified as EDN
import Data.Functor.Foldable (Recursive (..))
import Data.List (intersperse)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.Read qualified as TRead
import Flow ((|>))
import GHC.Generics (Generic)
import LS.Utils ((|$>))
import LS.XPile.Edn.Ast
import LS.XPile.Edn.CPSTranspileM
  ( TranspileResult (..),
    logTranspileMsg,
    logTranspiledTo,
    runCPSTranspileM,
  )
import LS.XPile.Edn.Context (Context, (!?), (<++>))
import LS.XPile.Edn.MessageLog (MessageData (..), MessageLog, Severity (..))
import Prelude hiding (head)

-- Recursively transpile an AST node, threading an initial empty context
-- through recursive calls.
-- This context is:
-- 1. Used to lookup variables.
-- 2. Temporarily extended when recursing into the head and body of a
--    HornLike rule.
-- Technically, we traverse the AST via a paramorphism (ie primitive recursion),
-- augmented with a CPS'd state + reader monad (for logging and contexts
-- respectively) to invert the flow of control, so that we can suspend
-- computations under binders when traversing the AST bottom-up via the
-- paramorphism, and only resume them after extending the context at a binder
-- node higher up in the AST.
astToEdn :: AstNode metadata -> TranspileResult metadata
astToEdn = runCPSTranspileM . para \case
  RuleFactF
    { metadataF = metadata,
      givensF = givens,
      headF = (head, headCont),
      bodyF
    } -> Reader.local (givens <++>) do
      -- Temporarily extend the current context with the variables in givens,
      -- then resume the suspended continuations representing the head and body.
      headEdn <- headCont
      (body, ifBodyEdn) <- case bodyF of
        Just (body, bodyCont) -> do
          bodyEdn <- bodyCont
          pure (Just body, [[EDN.edn|IF|], bodyEdn])
        Nothing -> pure (Nothing, [])

      let result = [EDN.edn|DECIDE|] : headEdn : ifBodyEdn |> EDN.toEDN

      logTranspiledTo RuleFact {metadata, givens, head, body} result
      pure result

  CompoundTermF
    { metadataF = metadata,
      opF = op,
      childrenF = unzip -> (children, childrenConts)
    } -> do
      childrenEdns <- sequenceA childrenConts

      let result = childrenEdns |> case op of
            ParensOp -> EDN.toEDN
            ListOp -> EDN.mkVec >>> EDN.toEDN
            AndOp -> intersperse (toSymbol "AND") >>> EDN.toEDN
            OrOp -> intersperse (toSymbol "OR") >>> EDN.toEDN

      logTranspiledTo CompoundTerm {metadata, op, children} result
      pure result

  TextF {metadataF = metadata, textF = text} -> do
    context <- Reader.ask

    let result = text |> if text !? context then toVar else toSymbol

    logTranspiledTo Text {metadata, text} result
    pure result
  where
    toPrefixedSymbol prefix x = EDN.Symbol prefix [i|#{x}|] |> EDN.toEDN
    toSymbol = toPrefixedSymbol ""
    toVar = toPrefixedSymbol "var"

exampleProgram :: AstNode metadata
exampleProgram =
  Program
    Nothing
    [ Rule
        Nothing
        []
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

--- >>> astToEdn exampleProgram
-- TranspileResult {transpileResultEdnText = "[(DECIDE p IF (q AND r)) (DECIDE (var/x is between 0 and 10 or is 100) IF (((0.0 <= var/x) AND (var/x <= 10.0)) OR (var/x IS 100.0))) (DECIDE ((2023 - 1 - 10) is a date))]", transpileResultMessageLog = MessageLog {messageLog = [Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = Text {metadata = Nothing, text = "p"}, messageDataResult = "p"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = Text {metadata = Nothing, text = "q"}, messageDataResult = "q"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = Text {metadata = Nothing, text = "r"}, messageDataResult = "r"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = CompoundTerm {metadata = Nothing, op = AndOp, children = [Text {metadata = Nothing, text = "q"},Text {metadata = Nothing, text = "r"}]}, messageDataResult = "(q AND r)"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = RuleFact {metadata = Nothing, givens = [], head = Text {metadata = Nothing, text = "p"}, body = Just (CompoundTerm {metadata = Nothing, op = AndOp, children = [Text {metadata = Nothing, text = "q"},Text {metadata = Nothing, text = "r"}]})}, messageDataResult = "(DECIDE p IF (q AND r))"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = Text {metadata = Nothing, text = "x"}, messageDataResult = "var/x"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = Text {metadata = Nothing, text = "is between 0 and 10 or is 100"}, messageDataResult = "is between 0 and 10 or is 100"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "x"},Text {metadata = Nothing, text = "is between 0 and 10 or is 100"}]}, messageDataResult = "(var/x is between 0 and 10 or is 100)"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = Text {metadata = Nothing, text = "0.0"}, messageDataResult = "0.0"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = Text {metadata = Nothing, text = "<="}, messageDataResult = "<="}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = Text {metadata = Nothing, text = "x"}, messageDataResult = "var/x"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "0.0"},Text {metadata = Nothing, text = "<="},Text {metadata = Nothing, text = "x"}]}, messageDataResult = "(0.0 <= var/x)"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = Text {metadata = Nothing, text = "x"}, messageDataResult = "var/x"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = Text {metadata = Nothing, text = "<="}, messageDataResult = "<="}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = Text {metadata = Nothing, text = "10.0"}, messageDataResult = "10.0"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "x"},Text {metadata = Nothing, text = "<="},Text {metadata = Nothing, text = "10.0"}]}, messageDataResult = "(var/x <= 10.0)"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = CompoundTerm {metadata = Nothing, op = AndOp, children = [CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "0.0"},Text {metadata = Nothing, text = "<="},Text {metadata = Nothing, text = "x"}]},CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "x"},Text {metadata = Nothing, text = "<="},Text {metadata = Nothing, text = "10.0"}]}]}, messageDataResult = "((0.0 <= var/x) AND (var/x <= 10.0))"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = Text {metadata = Nothing, text = "x"}, messageDataResult = "var/x"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = Text {metadata = Nothing, text = "IS"}, messageDataResult = "IS"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = Text {metadata = Nothing, text = "100.0"}, messageDataResult = "100.0"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "x"},Text {metadata = Nothing, text = "IS"},Text {metadata = Nothing, text = "100.0"}]}, messageDataResult = "(var/x IS 100.0)"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = CompoundTerm {metadata = Nothing, op = OrOp, children = [CompoundTerm {metadata = Nothing, op = AndOp, children = [CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "0.0"},Text {metadata = Nothing, text = "<="},Text {metadata = Nothing, text = "x"}]},CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "x"},Text {metadata = Nothing, text = "<="},Text {metadata = Nothing, text = "10.0"}]}]},CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "x"},Text {metadata = Nothing, text = "IS"},Text {metadata = Nothing, text = "100.0"}]}]}, messageDataResult = "(((0.0 <= var/x) AND (var/x <= 10.0)) OR (var/x IS 100.0))"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = RuleFact {metadata = Nothing, givens = ["x"], head = CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "x"},Text {metadata = Nothing, text = "is between 0 and 10 or is 100"}]}, body = Just (CompoundTerm {metadata = Nothing, op = OrOp, children = [CompoundTerm {metadata = Nothing, op = AndOp, children = [CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "0.0"},Text {metadata = Nothing, text = "<="},Text {metadata = Nothing, text = "x"}]},CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "x"},Text {metadata = Nothing, text = "<="},Text {metadata = Nothing, text = "10.0"}]}]},CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "x"},Text {metadata = Nothing, text = "IS"},Text {metadata = Nothing, text = "100.0"}]}]})}, messageDataResult = "(DECIDE (var/x is between 0 and 10 or is 100) IF (((0.0 <= var/x) AND (var/x <= 10.0)) OR (var/x IS 100.0)))"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = Text {metadata = Nothing, text = "2023"}, messageDataResult = "2023"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = Text {metadata = Nothing, text = "-"}, messageDataResult = "-"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = Text {metadata = Nothing, text = "1"}, messageDataResult = "1"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = Text {metadata = Nothing, text = "-"}, messageDataResult = "-"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = Text {metadata = Nothing, text = "10"}, messageDataResult = "10"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "2023"},Text {metadata = Nothing, text = "-"},Text {metadata = Nothing, text = "1"},Text {metadata = Nothing, text = "-"},Text {metadata = Nothing, text = "10"}]}, messageDataResult = "(2023 - 1 - 10)"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = Text {metadata = Nothing, text = "is a date"}, messageDataResult = "is a date"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = CompoundTerm {metadata = Nothing, op = ParensOp, children = [CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "2023"},Text {metadata = Nothing, text = "-"},Text {metadata = Nothing, text = "1"},Text {metadata = Nothing, text = "-"},Text {metadata = Nothing, text = "10"}]},Text {metadata = Nothing, text = "is a date"}]}, messageDataResult = "((2023 - 1 - 10) is a date)"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = RuleFact {metadata = Nothing, givens = [], head = CompoundTerm {metadata = Nothing, op = ParensOp, children = [CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "2023"},Text {metadata = Nothing, text = "-"},Text {metadata = Nothing, text = "1"},Text {metadata = Nothing, text = "-"},Text {metadata = Nothing, text = "10"}]},Text {metadata = Nothing, text = "is a date"}]}, body = Nothing}, messageDataResult = "(DECIDE ((2023 - 1 - 10) is a date))"}},Message {messageSeverity = Info, messageData' = TranspiledTo {messageDataAstNode = CompoundTerm {metadata = Nothing, op = ListOp, children = [RuleFact {metadata = Nothing, givens = [], head = Text {metadata = Nothing, text = "p"}, body = Just (CompoundTerm {metadata = Nothing, op = AndOp, children = [Text {metadata = Nothing, text = "q"},Text {metadata = Nothing, text = "r"}]})},RuleFact {metadata = Nothing, givens = ["x"], head = CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "x"},Text {metadata = Nothing, text = "is between 0 and 10 or is 100"}]}, body = Just (CompoundTerm {metadata = Nothing, op = OrOp, children = [CompoundTerm {metadata = Nothing, op = AndOp, children = [CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "0.0"},Text {metadata = Nothing, text = "<="},Text {metadata = Nothing, text = "x"}]},CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "x"},Text {metadata = Nothing, text = "<="},Text {metadata = Nothing, text = "10.0"}]}]},CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "x"},Text {metadata = Nothing, text = "IS"},Text {metadata = Nothing, text = "100.0"}]}]})},RuleFact {metadata = Nothing, givens = [], head = CompoundTerm {metadata = Nothing, op = ParensOp, children = [CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "2023"},Text {metadata = Nothing, text = "-"},Text {metadata = Nothing, text = "1"},Text {metadata = Nothing, text = "-"},Text {metadata = Nothing, text = "10"}]},Text {metadata = Nothing, text = "is a date"}]}, body = Nothing}]}, messageDataResult = "[(DECIDE p IF (q AND r)) (DECIDE (var/x is between 0 and 10 or is 100) IF (((0.0 <= var/x) AND (var/x <= 10.0)) OR (var/x IS 100.0))) (DECIDE ((2023 - 1 - 10) is a date))]"}}]}}
