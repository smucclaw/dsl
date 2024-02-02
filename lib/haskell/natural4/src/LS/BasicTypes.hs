{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Foundational types imported by all other modules.
--
-- These are largely:
--
-- * tokens for the parser and renderer
-- * stream for the parser
module LS.BasicTypes
  ( MyStream (..),
    MyToken (..),
    RawStanza,
    WithPos (..),
    liftMyToken,
    renderToken,
    toTokens,
  )
where

import Control.Arrow ((>>>))
import Data.Char (toUpper)
import Data.HashMap.Strict qualified as Map
import Data.Hashable (Hashable)
import Data.List qualified as DL
import Data.List.NonEmpty qualified as NE
import Data.Proxy (Proxy (..))
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Vector qualified as V
import GHC.Generics (Generic)
import LS.TokenTable (MyToken (..), tokenTable)
import Language.Haskell.TH.Syntax (lift)
import Text.Megaparsec
  ( PosState
      ( PosState,
        pstateInput,
        pstateLinePrefix,
        pstateOffset,
        pstateSourcePos,
        pstateTabWidth
      ),
    SourcePos (sourceLine),
    Stream (..),
    TraversableStream (reachOffset),
    VisualStream (..),
    initialPos,
  )
import Text.Regex.PCRE.Heavy qualified as PCRE

type RawStanza = V.Vector (V.Vector Text.Text) -- "did I stammer?"

data MyStream = MyStream
  { myStreamInput :: RawStanza  -- for showing offending lines
  , unMyStream :: [WithPos MyToken]
  }
  deriving Show

-- the Rule types employ these tokens, which are meaningful to L4.
--
toTokens :: Text.Text -> [MyToken]
toTokens (\txt -> Map.lookup [i|#{txt}|] $(lift tokenTable) -> Just tokens) =
  tokens

toTokens (PCRE.scan [PCRE.re|^-(§|¶)$|] -> [(_, [c])]) =
  [RuleMarker (-1) c]

toTokens s@(PCRE.scan [PCRE.re|^(§|¶)+$|] -> [(_, [c])]) =
  [RuleMarker (Text.length s) c]

toTokens (PCRE.scan [PCRE.re|^H([1-9](\d)*)$|] -> [(_, [n, _])]) =
  [RuleMarker (read $ Text.unpack n) "H"]

-- we recognize numbers
-- let's not recognize numbers yet; treat them as strings to be pOtherVal'ed.
toTokens (Text.unpack >>> reads -> [(n, "")]) = [TNumber n]

-- any other value becomes an Other -- "walks", "runs", "eats", "drinks"
toTokens txt = [Other txt]

-- note: we choose not to treat NOTIFY as keyword.
-- we parse it downstream when dealing with actions.
-- note: we choose not to tokenize LESS and PLUS here as keywords;
-- they represent set subtraction and union but we deal with them later.

-- INTERNAL PLUMBING
-- we use a custom input stream where the positions of the tokens are the x,y cell coordinates of a spreadsheet.
-- thanks to CSV the initial lexing is already done so we don't need to do a first-pass character-by-character lexing.
-- but we still may need to do a chunk-by-chunk parse of the cell contents.
--
-- the following was cribbed from Mark Karpov's MegaParsec tutorial,
-- https://markkarpov.com/tutorial/megaparsec.html#working-with-custom-input-streams

instance Stream MyStream where
  type Token  MyStream = WithPos MyToken
  type Tokens MyStream = [WithPos MyToken]
  tokenToChunk Proxy x = [x]
  tokensToChunk Proxy xs = xs
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  take1_ (MyStream _ []) = Nothing
  take1_ (MyStream str (t:ts)) = Just
    ( t
    -- , MyStream (drop (tokensLength pxy (t:|[])) str) ts
    , MyStream str ts
    )
  takeN_ n (MyStream str s)
    | n <= 0    = Just ([], MyStream str s)
    | null s    = Nothing
    | otherwise =
        let (x, s') = splitAt n s
        in case NE.nonEmpty x of
          Nothing -> Just (x, MyStream str s')
          -- Just nex -> Just (x, MyStream (drop (tokensLength pxy nex) str) s')
          Just _nex -> Just (x, MyStream str s')
  takeWhile_ f (MyStream str s) =
    let (x, s') = DL.span f s
    in case NE.nonEmpty x of
      Nothing -> (x, MyStream str s')
      -- Just nex -> (x, MyStream (drop (tokensLength pxy nex) str) s')
      Just _nex -> (x, MyStream str s')

instance VisualStream MyStream where
  tokensLength Proxy xs = sum (tokenLength <$> xs)
  -- showTokens Proxy (x NE.:| []) = show (tokenVal x)
  showTokens Proxy = unwords
    . NE.toList
    . fmap showTokenWithContext

showTokenWithContext :: WithPos MyToken -> String
showTokenWithContext WithPos {tokenVal = t} = showMyToken t
-- showTokenWithContext WithPos {parserCtx = Nothing, tokenVal = t} = showMyToken t
-- showTokenWithContext WithPos {parserCtx = Just ctx, tokenVal = t}
--   = "\n    " ++ showMyToken t ++ "\t: " ++ intercalate " - " (reverse ctx)
  -- = "\n    " ++ intercalate " -> " (reverse ctx) ++ " -> " ++ showMyToken t

instance TraversableStream MyStream where
  reachOffset o PosState {..} =
    ( Just (prefix <> restOfLine)
    , PosState
        { pstateInput = MyStream
            -- { myStreamInput = postStr
            { myStreamInput = myStreamInput pstateInput
            , unMyStream = post
            }
        , pstateOffset = max pstateOffset o
        , pstateSourcePos = newSourcePos
        , pstateTabWidth = pstateTabWidth
        , pstateLinePrefix = prefix
        }
    )
    where
      prefix
        | sameLine = pstateLinePrefix <> preLine
        | otherwise = preLine
      sameLine = sourceLine newSourcePos == sourceLine pstateSourcePos
      newSourcePos =
        case post of
          [] -> if null pre then pstateSourcePos else pos (last pre)
          (x:_) -> pos x
      (pre, post) = splitAt (o - pstateOffset) (unMyStream pstateInput)
      -- (preStr, postStr) = splitAt tokensConsumed (myStreamInput pstateInput)
      (preStr, postStr) = ("<not implemented #173a>", "<not implemented #173b>")
      preLine = reverse . takeWhile (/= '\n') . reverse $ preStr
      _tokensConsumed =
        case NE.nonEmpty pre of
          Nothing -> 0
          Just nePre -> tokensLength pxy nePre
      restOfLine = takeWhile (/= '\n') postStr

data WithPos a = WithPos
  { pos :: SourcePos
  , tokenLength :: Int
  , parserCtx :: Maybe [String]
  , tokenVal :: a
  } deriving (Eq, Ord, Show, Functor)

pxy :: Proxy MyStream
pxy = Proxy

showMyToken :: MyToken -> String
-- showMyToken = show
showMyToken = renderToken

renderToken :: MyToken -> String
renderToken ScenarioTok = "SCENARIO"
renderToken TokAll = "ALL"
renderToken MPNot = "NOT"
renderToken TokAnd = "&&"
renderToken TokOr  = "||"
renderToken TokLT = "<"
renderToken TokLTE = "<="
renderToken TokGT = ">"
renderToken TokGTE = ">="
renderToken TokIn = "IN"
renderToken TokNotIn = "NOT IN"
renderToken TokEQ = "=="
renderToken TokTrue = "TRUE"
renderToken TokFalse = "FALSE"
renderToken GoDeeper = "("
renderToken UnDeeper = ")"
renderToken SetPlus = "PLUS"
renderToken SetLess = "LESS"
renderToken A_An = "A"
renderToken (TNumber n) = show n
renderToken OneOf = "ONE OF"
renderToken TypeSeparator = "::"
renderToken (Other txt) = show txt
renderToken (RuleMarker 0 txt) = [i|§0#{txt}|]
renderToken (RuleMarker n "H") = [i|H#{n}|]
renderToken (RuleMarker n txt) = mconcat $ replicate n $ Text.unpack txt

renderToken Semicolon = ";;"

renderToken SubjectTo = "SUBJECT TO"
renderToken TokMin = "MIN"
renderToken TokMax = "MAX"
renderToken TokSum = "SUM"
renderToken TokProduct = "PRODUCT"
renderToken FMap = "MAP"

renderToken tok = toUpper <$> show tok

liftMyToken :: [String] -> MyToken -> WithPos MyToken
liftMyToken = WithPos pos 0 . Just
  where
    pos = initialPos ""
