{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.Edn.AstToEdn
  ( astToEdnText,
    astToEdn,
  )
where

import Control.Arrow ((>>>))
import Control.Monad.State qualified as State
import Data.EDN qualified as EDN
import Data.EDN.QQ (edn)
import Data.Functor.Foldable (Recursive (..))
import Data.List (intersperse)
import Data.String.Interpolate (i)
import Data.String.Interpolate.Conversion (Interpolatable, IsCustomSink)
import Data.Text qualified as T
import Data.Text.Read qualified as TRead
import Flow ((|>))
import GHC.Generics (Generic)
import LS.Utils ((|$>))
import LS.XPile.Edn.Ast
import LS.XPile.Edn.CPSTranspileM
  ( TranspileState (..),
    logTranspileMsg,
    runCPSTranspileM,
    withExtendedCtx,
  )
import LS.XPile.Edn.Context (Context, (!?), (<++>))
import LS.XPile.Edn.MessageLog (Severity (..), MessageData (..), MessageLog)
import Prelude hiding (head)

astToEdnText ::
  Interpolatable (IsCustomSink t) T.Text t => AstNode metadata -> t
astToEdnText astNode = [i|#{astNode |> astToEdn |> fst |> EDN.renderText}|]

-- Recursively transpile an AST node, threading an initial empty context
-- through recursive calls.
-- This context is:
-- 1. Used to lookup variables.
-- 2. Temporarily extended when recursing into the head and body of a
--    HornLike rule.
-- Technically, we traverse the AST via a paramorphism (ie primitive recursion),
-- augmented with a combination of a CPS + State monad on contexts to invert
-- the flow of control, so that we can thread the context through each step in
-- a top-down manner, extending it as we go.
-- The key ideas are:
-- - We use the CPS monad to suspend computation of the head and body of a
--   HornLike rule, and then via the state monad, extend the current context
--   with the declarations found in the Givens.
--   We then resume the computations of the head and body and afterwards
--   and then restore the old context in the state monad.
-- - For arbitrary symbols, ie Texts, we look it up in the context, marking it
--   as a variable if we find it there, and treat it as an arbitrary atomic
--   symbol otherwise.
astToEdn :: AstNode metadata -> (EDN.TaggedValue, MessageLog metadata)
astToEdn =
  runCPSTranspileM . para \case
    RuleFactF
      { metadataF = metadata,
        givensF = givens,
        headF = (astHead, head),
        bodyF
      } -> withExtendedCtx givens do
        -- Temporarily extend the current context with the variables in givens,
        -- then resume the suspended continuations representing the head and body.
        head <- head
        (astBody, ifBody) <- case bodyF of
          Just (astBody, body) -> do
            body <- body
            pure (Just astBody, [[edn|IF|], body])
          Nothing -> pure (Nothing, [])

        let result = [edn|DECIDE|] : head : ifBody |> EDN.toEDN
            resultText = result |> EDN.renderText
            astNode = RuleFact {metadata, givens, head = astHead, body = astBody}

        logTranspileMsg Info TranspiledTo {astNode, result = resultText}
        pure result

    CompoundTermF
      { metadataF = metadata,
        opF = op,
        childrenF = unzip -> (astChildren, children)
      } -> do
        -- Recursively resume the children continuations.
        children <- sequenceA children

        let result =
              children |> case op of
                ParensOp -> EDN.toEDN
                ListOp -> EDN.mkVec >>> EDN.toEDN
                AndOp -> intersperse (toSymbol "AND") >>> EDN.toEDN
                OrOp -> intersperse (toSymbol "OR") >>> EDN.toEDN
            resultText = result |> EDN.renderText
            astNode = CompoundTerm {metadata, op, children = astChildren}

        logTranspileMsg Info TranspiledTo {astNode, result = resultText}
        pure result

    TextF {metadataF = metadata, textF = text} -> do
      state <- State.get

      let result = text |> if text !? state then toVar else toSymbol
          resultText = EDN.renderText result
          astNode = Text {metadata, text}

      logTranspileMsg Info TranspiledTo {astNode, result = resultText}
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
-- (NoTag (Vec [NoTag (List [NoTag (Symbol "" "DECIDE"),NoTag (Symbol "" "p"),NoTag (Symbol "" "IF"),NoTag (List [NoTag (Symbol "" "q"),NoTag (Symbol "" "AND"),NoTag (Symbol "" "r")])]),NoTag (List [NoTag (Symbol "" "DECIDE"),NoTag (List [NoTag (Symbol "var" "x"),NoTag (Symbol "" "is between 0 and 10 or is 100")]),NoTag (Symbol "" "IF"),NoTag (List [NoTag (List [NoTag (List [NoTag (Symbol "" "0.0"),NoTag (Symbol "" "<="),NoTag (Symbol "var" "x")]),NoTag (Symbol "" "AND"),NoTag (List [NoTag (Symbol "var" "x"),NoTag (Symbol "" "<="),NoTag (Symbol "" "10.0")])]),NoTag (Symbol "" "OR"),NoTag (List [NoTag (Symbol "var" "x"),NoTag (Symbol "" "IS"),NoTag (Symbol "" "100.0")])])]),NoTag (List [NoTag (Symbol "" "DECIDE"),NoTag (List [NoTag (List [NoTag (Symbol "" "2023"),NoTag (Symbol "" "-"),NoTag (Symbol "" "1"),NoTag (Symbol "" "-"),NoTag (Symbol "" "10")]),NoTag (Symbol "" "is a date")])])]),MessageLog {messageLog = [Message {severity = Info, messageData = TranspiledTo {astNode = Text {metadata = Nothing, text = "p"}, result = "p"}},Message {severity = Info, messageData = TranspiledTo {astNode = Text {metadata = Nothing, text = "q"}, result = "q"}},Message {severity = Info, messageData = TranspiledTo {astNode = Text {metadata = Nothing, text = "r"}, result = "r"}},Message {severity = Info, messageData = TranspiledTo {astNode = CompoundTerm {metadata = Nothing, op = AndOp, children = [Text {metadata = Nothing, text = "q"},Text {metadata = Nothing, text = "r"}]}, result = "(q AND r)"}},Message {severity = Info, messageData = TranspiledTo {astNode = RuleFact {metadata = Nothing, givens = [], head = Text {metadata = Nothing, text = "p"}, body = Just (CompoundTerm {metadata = Nothing, op = AndOp, children = [Text {metadata = Nothing, text = "q"},Text {metadata = Nothing, text = "r"}]})}, result = "(DECIDE p IF (q AND r))"}},Message {severity = Info, messageData = TranspiledTo {astNode = Text {metadata = Nothing, text = "x"}, result = "var/x"}},Message {severity = Info, messageData = TranspiledTo {astNode = Text {metadata = Nothing, text = "is between 0 and 10 or is 100"}, result = "is between 0 and 10 or is 100"}},Message {severity = Info, messageData = TranspiledTo {astNode = CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "x"},Text {metadata = Nothing, text = "is between 0 and 10 or is 100"}]}, result = "(var/x is between 0 and 10 or is 100)"}},Message {severity = Info, messageData = TranspiledTo {astNode = Text {metadata = Nothing, text = "0.0"}, result = "0.0"}},Message {severity = Info, messageData = TranspiledTo {astNode = Text {metadata = Nothing, text = "<="}, result = "<="}},Message {severity = Info, messageData = TranspiledTo {astNode = Text {metadata = Nothing, text = "x"}, result = "var/x"}},Message {severity = Info, messageData = TranspiledTo {astNode = CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "0.0"},Text {metadata = Nothing, text = "<="},Text {metadata = Nothing, text = "x"}]}, result = "(0.0 <= var/x)"}},Message {severity = Info, messageData = TranspiledTo {astNode = Text {metadata = Nothing, text = "x"}, result = "var/x"}},Message {severity = Info, messageData = TranspiledTo {astNode = Text {metadata = Nothing, text = "<="}, result = "<="}},Message {severity = Info, messageData = TranspiledTo {astNode = Text {metadata = Nothing, text = "10.0"}, result = "10.0"}},Message {severity = Info, messageData = TranspiledTo {astNode = CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "x"},Text {metadata = Nothing, text = "<="},Text {metadata = Nothing, text = "10.0"}]}, result = "(var/x <= 10.0)"}},Message {severity = Info, messageData = TranspiledTo {astNode = CompoundTerm {metadata = Nothing, op = AndOp, children = [CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "0.0"},Text {metadata = Nothing, text = "<="},Text {metadata = Nothing, text = "x"}]},CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "x"},Text {metadata = Nothing, text = "<="},Text {metadata = Nothing, text = "10.0"}]}]}, result = "((0.0 <= var/x) AND (var/x <= 10.0))"}},Message {severity = Info, messageData = TranspiledTo {astNode = Text {metadata = Nothing, text = "x"}, result = "var/x"}},Message {severity = Info, messageData = TranspiledTo {astNode = Text {metadata = Nothing, text = "IS"}, result = "IS"}},Message {severity = Info, messageData = TranspiledTo {astNode = Text {metadata = Nothing, text = "100.0"}, result = "100.0"}},Message {severity = Info, messageData = TranspiledTo {astNode = CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "x"},Text {metadata = Nothing, text = "IS"},Text {metadata = Nothing, text = "100.0"}]}, result = "(var/x IS 100.0)"}},Message {severity = Info, messageData = TranspiledTo {astNode = CompoundTerm {metadata = Nothing, op = OrOp, children = [CompoundTerm {metadata = Nothing, op = AndOp, children = [CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "0.0"},Text {metadata = Nothing, text = "<="},Text {metadata = Nothing, text = "x"}]},CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "x"},Text {metadata = Nothing, text = "<="},Text {metadata = Nothing, text = "10.0"}]}]},CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "x"},Text {metadata = Nothing, text = "IS"},Text {metadata = Nothing, text = "100.0"}]}]}, result = "(((0.0 <= var/x) AND (var/x <= 10.0)) OR (var/x IS 100.0))"}},Message {severity = Info, messageData = TranspiledTo {astNode = RuleFact {metadata = Nothing, givens = ["x"], head = CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "x"},Text {metadata = Nothing, text = "is between 0 and 10 or is 100"}]}, body = Just (CompoundTerm {metadata = Nothing, op = OrOp, children = [CompoundTerm {metadata = Nothing, op = AndOp, children = [CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "0.0"},Text {metadata = Nothing, text = "<="},Text {metadata = Nothing, text = "x"}]},CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "x"},Text {metadata = Nothing, text = "<="},Text {metadata = Nothing, text = "10.0"}]}]},CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "x"},Text {metadata = Nothing, text = "IS"},Text {metadata = Nothing, text = "100.0"}]}]})}, result = "(DECIDE (var/x is between 0 and 10 or is 100) IF (((0.0 <= var/x) AND (var/x <= 10.0)) OR (var/x IS 100.0)))"}},Message {severity = Info, messageData = TranspiledTo {astNode = Text {metadata = Nothing, text = "2023"}, result = "2023"}},Message {severity = Info, messageData = TranspiledTo {astNode = Text {metadata = Nothing, text = "-"}, result = "-"}},Message {severity = Info, messageData = TranspiledTo {astNode = Text {metadata = Nothing, text = "1"}, result = "1"}},Message {severity = Info, messageData = TranspiledTo {astNode = Text {metadata = Nothing, text = "-"}, result = "-"}},Message {severity = Info, messageData = TranspiledTo {astNode = Text {metadata = Nothing, text = "10"}, result = "10"}},Message {severity = Info, messageData = TranspiledTo {astNode = CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "2023"},Text {metadata = Nothing, text = "-"},Text {metadata = Nothing, text = "1"},Text {metadata = Nothing, text = "-"},Text {metadata = Nothing, text = "10"}]}, result = "(2023 - 1 - 10)"}},Message {severity = Info, messageData = TranspiledTo {astNode = Text {metadata = Nothing, text = "is a date"}, result = "is a date"}},Message {severity = Info, messageData = TranspiledTo {astNode = CompoundTerm {metadata = Nothing, op = ParensOp, children = [CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "2023"},Text {metadata = Nothing, text = "-"},Text {metadata = Nothing, text = "1"},Text {metadata = Nothing, text = "-"},Text {metadata = Nothing, text = "10"}]},Text {metadata = Nothing, text = "is a date"}]}, result = "((2023 - 1 - 10) is a date)"}},Message {severity = Info, messageData = TranspiledTo {astNode = RuleFact {metadata = Nothing, givens = [], head = CompoundTerm {metadata = Nothing, op = ParensOp, children = [CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "2023"},Text {metadata = Nothing, text = "-"},Text {metadata = Nothing, text = "1"},Text {metadata = Nothing, text = "-"},Text {metadata = Nothing, text = "10"}]},Text {metadata = Nothing, text = "is a date"}]}, body = Nothing}, result = "(DECIDE ((2023 - 1 - 10) is a date))"}},Message {severity = Info, messageData = TranspiledTo {astNode = CompoundTerm {metadata = Nothing, op = ListOp, children = [RuleFact {metadata = Nothing, givens = [], head = Text {metadata = Nothing, text = "p"}, body = Just (CompoundTerm {metadata = Nothing, op = AndOp, children = [Text {metadata = Nothing, text = "q"},Text {metadata = Nothing, text = "r"}]})},RuleFact {metadata = Nothing, givens = ["x"], head = CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "x"},Text {metadata = Nothing, text = "is between 0 and 10 or is 100"}]}, body = Just (CompoundTerm {metadata = Nothing, op = OrOp, children = [CompoundTerm {metadata = Nothing, op = AndOp, children = [CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "0.0"},Text {metadata = Nothing, text = "<="},Text {metadata = Nothing, text = "x"}]},CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "x"},Text {metadata = Nothing, text = "<="},Text {metadata = Nothing, text = "10.0"}]}]},CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "x"},Text {metadata = Nothing, text = "IS"},Text {metadata = Nothing, text = "100.0"}]}]})},RuleFact {metadata = Nothing, givens = [], head = CompoundTerm {metadata = Nothing, op = ParensOp, children = [CompoundTerm {metadata = Nothing, op = ParensOp, children = [Text {metadata = Nothing, text = "2023"},Text {metadata = Nothing, text = "-"},Text {metadata = Nothing, text = "1"},Text {metadata = Nothing, text = "-"},Text {metadata = Nothing, text = "10"}]},Text {metadata = Nothing, text = "is a date"}]}, body = Nothing}]}, result = "[(DECIDE p IF (q AND r)) (DECIDE (var/x is between 0 and 10 or is 100) IF (((0.0 <= var/x) AND (var/x <= 10.0)) OR (var/x IS 100.0))) (DECIDE ((2023 - 1 - 10) is a date))]"}}]})
