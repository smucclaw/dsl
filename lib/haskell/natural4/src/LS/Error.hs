{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

{-|

Show parser errors with more helpful context.

-}

module LS.Error where

import Control.Arrow ((>>>))
import Data.Function ( (&) )
import Data.List.NonEmpty qualified as NE
import Data.Proxy ( Proxy(..) )
import Data.Set qualified as Set
import Data.String.Interpolate (i, __i)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LT
import Data.Vector (foldl1', imap)
import Data.Vector qualified as V
import Data.Void (Void)
import LS.BasicTypes
  ( MyStream (MyStream, unMyStream),
    MyToken (Other),
    WithPos (pos, tokenVal),
    myStreamInput,
    renderToken,
  )
import Text.Megaparsec
    ( Stream(Token),
      TraversableStream(reachOffset),
      VisualStream(tokensLength),
      SourcePos(sourceColumn, sourceLine),
      errorOffset,
      parseErrorTextPretty,
      showErrorItem,
      sourcePosPretty,
      unPos,
      ErrorFancy(..),
      ErrorItem(Tokens),
      ParseError(..),
      ParseErrorBundle(..),
      ShowErrorComponent(..),
      PosState(pstateInput, pstateSourcePos) )
import Text.Megaparsec.Pos ()
import Text.Pretty.Simple (pStringNoColor)
import Text.PrettyPrint.Boxes ( hsep, vcat )
import Text.PrettyPrint.Boxes qualified as Box

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
        numCols = maximum $ fmap length excelTable
        paddedExcelTable = excelTable & fmap \x -> x <> V.replicate (numCols - length x) ""
        excelTableMarked =
          imap (\i -> if i == row then imap (\j -> if j == col then ("✳ " <>) else id) else id ) paddedExcelTable
          & fmap (fmap Text.unpack)
          -- & fmap (fmap (Text.unpack. ("(" <>) . (<>")")))
        -- foldMax = foldl' _ 1
        maxAllowedWidth = 35
        maxLengths = fmap (fmap (min maxAllowedWidth . length)) excelTableMarked & foldl1' (V.zipWith max) & fmap (fromIntegral @_ @Int)
        boxRepresentation = excelTableMarked
          -- & sequenceA -- NOTE: This only works if the table is actually rectangular and doesn't have jagged rows
          -- & fmap (vcat Box.left . fmap Box.text )
          & fmap (imap (\c -> Box.alignHoriz Box.left (maxLengths V.! c) . Box.para Box.left maxAllowedWidth) >>> hsep 3 Box.left)
          & vcat Box.left & Box.render
        outChunk =
          [__i|
            #{sourcePosPretty epos}:
            #{parseErrorTextPretty e}
            #{boxRepresentation}
            Stream:
            #{xpRenderStream (insertStarAt epos $ pstateInput pst)}
          |]

insertStarAt :: SourcePos -> MyStream -> MyStream
insertStarAt sp (MyStream vec wps) = MyStream vec (foldMap insertIt wps)
  where
    insertIt :: WithPos MyToken -> [WithPos MyToken]
    insertIt t | pos t == sp = [Other "✳" <$ t, t]
               | otherwise = [t]

----------------------------------------------------------------------------
-- Helpers

xrenderStream :: MyStream -> String
xrenderStream stream = unwords $ renderToken . tokenVal <$> unMyStream stream

xpRenderStream :: MyStream -> String
xpRenderStream = Text.unpack . LT.toStrict . pStringNoColor . xrenderStream

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
    [i|incorrect indentation (got #{unPos actual}, should be #{p} #{unPos ref})|]
    where
      p :: String = case ord of
        LT -> "less than "
        EQ -> "equal to "
        GT -> "greater than "
  ErrorCustom a -> showErrorComponent a

-- | Get length of the “pointer” to display under a given 'ErrorFancy'.
errorFancyLength :: ShowErrorComponent e => ErrorFancy e -> Int
errorFancyLength = \case
  ErrorCustom a -> errorComponentLen a
  _ -> 1

--------

-- | Oneline error message for debug purposes.
onelineErrorMsg :: ParseError MyStream Void -> String
onelineErrorMsg (TrivialError _ Nothing set) = 
  [i|Expecting: #{unwords (map onelineErrorItem $ Set.toList set)}|]

onelineErrorMsg (TrivialError _ (Just ei) set) =
  [i|Unexpected #{onelineErrorItem ei} Expecting: #{unwords (map onelineErrorItem $ Set.toList set)}|]

onelineErrorMsg (FancyError _ set) = unwords $ map showFancy $ Set.toList set
  where
    showFancy :: ErrorFancy Void -> String
    showFancy (ErrorFail s) = [i|Fail: #{s}|]
    showFancy (ErrorIndentation ord pos pos') = [i|Indent error: #{pos} should be #{ord} #{pos'}|]
    showFancy (ErrorCustom vo) = case vo of {}

onelineErrorItem :: ErrorItem (WithPos MyToken) -> String
onelineErrorItem = showErrorItem @MyStream Proxy
