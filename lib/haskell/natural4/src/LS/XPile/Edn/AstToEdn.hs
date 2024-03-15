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
import Data.String.Interpolate.Conversion (Interpolatable, IsCustomSink)
import Data.Text qualified as T
import Data.Text.Read qualified as TRead
import Flow ((|>))
import GHC.Generics (Generic)
import LS.Utils ((|$>))
import LS.XPile.Edn.Ast
import LS.XPile.Edn.CPSTranspileM
  ( TranspileResult (..),
    edn,
    logTranspileMsg,
    logTranspiledTo,
    runCPSTranspileM,
  )
import LS.XPile.Edn.Context (Context, (!?), (<++>))
import LS.XPile.Edn.MessageLog (MessageData (..), MessageLog, Severity (..))
import Optics qualified
import Prelude hiding (head)

-- astToEdnText ::
--   Interpolatable (IsCustomSink t) T.Text t => AstNode metadata -> t
-- astToEdnText =
--   astToEdn
--     >>> Optics.view edn
--     >>> EDN.renderText
--     >>> \x -> [i|#{x}|]

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
-- "[(DECIDE p IF (q AND r)) (DECIDE (var/x is between 0 and 10 or is 100) IF (((0.0 <= var/x) AND (var/x <= 10.0)) OR (var/x IS 100.0))) (DECIDE ((2023 - 1 - 10) is a date))]"
