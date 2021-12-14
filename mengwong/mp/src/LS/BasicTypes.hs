{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module LS.BasicTypes where
import Data.Proxy
import qualified Data.Text.Lazy as Text
import Text.Megaparsec
import qualified Data.List.NonEmpty as NE
import qualified Data.List as DL
import qualified Data.Vector as V
import Data.Aeson (ToJSON)
import GHC.Generics

type RawStanza = V.Vector (V.Vector Text.Text) -- "did I stammer?"

data MyStream = MyStream
  { myStreamInput :: RawStanza  -- for showing offending lines
  , unMyStream :: [WithPos MyToken]
  }
  deriving Show

data MyToken = Every | Party | TokAll
             | Who
             | Must | May | Shant
             | If | When | Always | Never
             | Or | And | MPNot
             | Before | After | By | On | Eventually -- TVague is a temporal constraint but not a token
             | Means | Includes  | Is
             | Given | Having | Upon
             | Define | OneOf | Holds
             | Decide
             | A_An
             | Deem | As | Has
             | TypeSeparator -- ::, TYPE, AS, shrug
             | One | Optional | List0 | List1 -- list-like modifiers, List1=NonEmpty
             | Distinct -- entity modifier in GIVEN
             | Unless
             | Hence | Lest | Fulfilled | Breach
             | TNumber Integer
             | Other Text.Text
             | Do
             | Checkbox
             | Aka -- also known as, for AKA Receiving Party
             | Empty | EOL
             | RuleMarker Int Text.Text
             | Expect
             | TokLT | TokLTE | TokGT | TokGTE | TokIn | TokNotIn | TokEQ
             | Otherwise
             | EOF
             | GoDeeper | UnDeeper | NoDeeper
  deriving (Ord, Eq, Show, Generic, ToJSON)

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
  showTokens Proxy = unwords
    . NE.toList
    . fmap (showMyToken . tokenVal)
  tokensLength Proxy xs = sum (tokenLength <$> xs)

instance TraversableStream MyStream where
  reachOffset o PosState {..} =
    ( Just (prefix ++ restOfLine)
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
      prefix =
        if sameLine
          then pstateLinePrefix ++ preLine
          else preLine
      sameLine = sourceLine newSourcePos == sourceLine pstateSourcePos
      newSourcePos =
        case post of
          [] -> if null pre then pstateSourcePos else endPos (last pre)
          (x:_) -> startPos x
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
  { startPos :: SourcePos
  , endPos :: SourcePos
  , tokenLength :: Int
  , tokenVal :: a
  } deriving (Eq, Ord, Show)




pxy :: Proxy MyStream
pxy = Proxy

showMyToken :: MyToken -> String
showMyToken = show

liftMyToken :: MyToken -> WithPos MyToken
liftMyToken = WithPos pos pos 0
  where
    pos = initialPos ""
