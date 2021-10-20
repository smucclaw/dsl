{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Error where

import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Text.Megaparsec.Pos
import Text.Megaparsec

import qualified Text.PrettyPrint.Boxes as Box
import           Text.PrettyPrint.Boxes hiding ((<>))
import Data.Function

import BasicTypes (MyStream, myStreamInput)
import Data.Vector (imap, foldl')
import qualified Data.Text.Lazy as Text
import Control.Arrow ((>>>))

-- custom version of https://hackage.haskell.org/package/megaparsec-9.2.0/docs/src/Text.Megaparsec.Error.html#errorBundlePretty
errorBundlePrettyCustom ::
  forall e .
  ShowErrorComponent e =>
  -- | Parse error bundle to display
  ParseErrorBundle MyStream e ->
  -- | Textual rendition of the bundle
  String
errorBundlePrettyCustom ParseErrorBundle {..} =
  let (r, _) = foldl f (id, bundlePosState) bundleErrors
   in drop 1 (r "")
  where
    f ::
      (ShowS, PosState MyStream) ->
      ParseError MyStream e ->
      (ShowS, PosState MyStream)
    f (o, !pst) e = (o . (outChunk ++), pst')
      where
        (_msline, pst') = reachOffset (errorOffset e) pst
        epos = pstateSourcePos pst'
        row = unPos (sourceLine epos) - 1
        col = unPos (sourceColumn epos) - 1
        excelTable = pst & pstateInput & myStreamInput
        excelTableMarked =
          imap (\i -> if i == row then imap (\j -> if j == col then ("✳ " <>) . (<> "") else id) else id ) excelTable
          & fmap (fmap Text.unpack)
        foldMax = foldl' max 1
        maxLength = foldMax (fmap (foldMax . fmap length) excelTableMarked) & fromIntegral @_ @Int
        boxRepresentation = excelTableMarked
          & fmap (fmap (Box.alignHoriz Box.left maxLength . Box.text) >>> hsep 1 Box.left)
          & vcat Box.left & Box.render
        outChunk =
          "\n" <> sourcePosPretty epos <> ":\n"
          <> parseErrorTextPretty e
          <> boxRepresentation <> "\n"

----------------------------------------------------------------------------
-- Helpers

-- | Pretty-print an 'ErrorItem'.
showErrorItem :: VisualStream s => Proxy s -> ErrorItem (Token s) -> String
showErrorItem pxy_ = \case
  Tokens ts -> showTokens pxy_ ts
  Label label_ -> NE.toList label_
  EndOfInput -> "end of input"

-- | Get length of the “pointer” to display under a given 'ErrorItem'.
errorItemLength :: VisualStream s => Proxy s -> ErrorItem (Token s) -> Int
errorItemLength pxy_ = \case
  Tokens ts -> tokensLength pxy_ ts
  _ -> 1

-- | Pretty-print an 'ErrorFancy'.
showErrorFancy :: ShowErrorComponent e => ErrorFancy e -> String
showErrorFancy = \case
  ErrorFail msg -> msg
  ErrorIndentation ord ref actual ->
    "incorrect indentation (got " <> show (unPos actual)
      <> ", should be "
      <> p
      <> show (unPos ref)
      <> ")"
    where
      p = case ord of
        LT -> "less than "
        EQ -> "equal to "
        GT -> "greater than "
  ErrorCustom a -> showErrorComponent a

-- | Get length of the “pointer” to display under a given 'ErrorFancy'.
errorFancyLength :: ShowErrorComponent e => ErrorFancy e -> Int
errorFancyLength = \case
  ErrorCustom a -> errorComponentLen a
  _ -> 1
