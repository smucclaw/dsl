{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.Edn.AstToEdn
  ( astNodeToEdn,
  )
where

import Control.Arrow ((>>>))
import Control.Monad.Reader qualified as Reader
import Data.EDN qualified as EDN
import Data.EDN.QQ qualified as EDN
import Data.Functor.Foldable (Recursive (..))
import Data.Functor.Foldable.Monadic (paraM)
import Data.List (intersperse)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.Read qualified as TRead
import Flow ((|>))
import GHC.Generics (Generic)
import LS.Utils ((|$>))
import LS.XPile.Edn.AstToEdn.MessageLog
  ( MessageData (..),
    MessageLog,
    Severity (..),
  )
import LS.XPile.Edn.Common.Ast
import LS.XPile.Edn.Common.Utils (listToPairs)
import Text.Regex.PCRE.Heavy qualified as PCRE
import Prelude hiding (head)

astNodeToEdn :: AstNode metadata -> EDN.TaggedValue
astNodeToEdn = cata \case
  HornClauseF {metadataF, givensF, headF, bodyF} ->
    -- logTranspiledTo HornClause {metadata, givens, head, body} resultEdn
    EDN.toEDN $ givens <> ([EDN.edn|DECIDE|] : headF : ifBody)
    where
      ifBody = bodyF |> foldMap \bodyEdn -> [[EDN.edn|IF|], bodyEdn]

      givens
        | null givensF = []
        | otherwise = [EDN.edn|GIVEN|] : (uncurry givenToEdn <$> givensF)

      givenToEdn :: T.Text -> Maybe T.Text -> EDN.TaggedValue
      givenToEdn (toSymbol -> var) = maybe var \typ ->
        [var, [EDN.edn|IS|], [EDN.edn|A|], toSymbol typ]
          |$> EDN.toEDN
          |> EDN.mkVec
          |> EDN.toEDN

  CompoundTermF {metadataF, opF, childrenF} ->
  -- logTranspiledTo CompoundTerm {metadata, op, children} resultEdn
    childrenF |> case opF of
      ParensOp -> EDN.toEDN
      ListOp -> EDN.mkVec >>> EDN.toEDN
      MapOp -> listToPairs >>> EDN.mkMap >>> EDN.toEDN
      SetOp -> EDN.mkSet >>> EDN.toEDN
      AndOp -> intersperseToEdn "AND"
      OrOp -> intersperseToEdn "OR"
    where
      intersperseToEdn text = intersperse (toSymbol text) >>> EDN.toEDN

  TextF {metadataF = metadata, textF = text} -> toSymbol text
    -- logTranspiledTo Text {metadata, text} resultEdn
  where
    toSymbol = replaceText >>> EDN.Symbol "" >>> EDN.toEDN

    replaceText =
      PCRE.gsub [PCRE.re|;|] ("*semicolon*" :: T.Text)
        >>> PCRE.gsub [PCRE.re|#_|] ("*hash_underscore*" :: T.Text)
        >>> PCRE.gsub
          [PCRE.re|([a-zA-Z].*(?<!\s))/([a-zA-Z].*(?<!\s))|]
          \(x : y : _ :: [T.Text]) -> [i|#{x}*slash*#{y}|] :: T.Text

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
        [("x", Just "Integer"), ("y", Nothing), ("xs", Just "LIST OF Integer")]
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
      Fact Nothing [] $
        Parens Nothing [Date Nothing 2023 1 10, Text Nothing "is a date"]
    ]

--- >>> exampleProgram |> astNodeToEdn |> EDN.renderText
-- "[(DECIDE p IF (q AND r)) (GIVEN [x IS A Integer] y [xs IS A LIST OF Integer] DECIDE (x is between 0 and 10 or is 100) IF (((0.0 <= x) AND (x <= 10.0)) OR (x IS 100.0))) (DECIDE ((2023 - 1 - 10) is a date))]"
