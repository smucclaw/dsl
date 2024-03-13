{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

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
import LS.XPile.Edn.MessageLog (Severity (..))

astToEdnText ::
  Interpolatable (IsCustomSink t) T.Text t => AstNode metadata -> t
astToEdnText astNode = [i|#{astNode |> astToEdn |> EDN.renderText}|]

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
astToEdn :: AstNode metadata -> EDN.TaggedValue
astToEdn = runCPSTranspileM . para \case
  RuleFactF {givensF = givens, headF = (_, head), bodyF} -> do
    -- Temporarily extend the current context with the variables in givens,
    -- then resume the suspended continuations representing the head and body.
    withExtendedCtx givens do
      logTranspileMsg Info undefined
      head <- head
      ifBody <- case bodyF of
        Just (_, body) -> do
          body <- body
          pure [[edn|IF|], body]
        Nothing -> pure []
      [edn|DECIDE|] : head : ifBody |> EDN.toEDN |> pure

  AndOrF {andOrF = andOr, childrenF = children} ->
    children |> recurseAndThen (intersperse $ toSymbol andOr)

  ParensF {childrenF = children} -> children |> recurseAndThen id

  ListF {elementsF = elements} -> elements |> recurseAndThen EDN.mkVec

  TextF {textF = text} -> do
    state <- State.get
    text |> (if text !? state then toVar else toSymbol) |> pure
  where
    toPrefixedSymbol prefix x = EDN.Symbol prefix [i|#{x}|] |> EDN.toEDN
    toSymbol = toPrefixedSymbol ""
    toVar = toPrefixedSymbol "var"

    recurseAndThen f xs = do
      xs <- traverse snd xs
      xs |> f |> EDN.toEDN |> pure

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

--- >>> (astToEdnText exampleProgram :: T.Text) 
-- "[(DECIDE p IF (q AND r)) (DECIDE (var/x is between 0 and 10 or is 100) IF (((0.0 <= var/x) AND (var/x <= 10.0)) OR (var/x IS 100.0))) (DECIDE ((2023 - 1 - 10) is a date))]"
