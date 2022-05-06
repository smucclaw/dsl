{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DeriveFunctor #-}

module LS.BasicTypes where
import Data.Proxy
import qualified Data.Text.Lazy as Text
import Text.Megaparsec
import qualified Data.List.NonEmpty as NE
import qualified Data.List as DL
import qualified Data.Vector as V
import Data.Aeson (ToJSON)
import GHC.Generics
import Data.Char (toUpper)

type RawStanza = V.Vector (V.Vector Text.Text) -- "did I stammer?"

data MyStream = MyStream
  { myStreamInput :: RawStanza  -- for showing offending lines
  , unMyStream :: [WithPos MyToken]
  }
  deriving Show

data MyToken = Every | Party | TokAll
             | Who | Which | Whose
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
             | Hence | Lest | Fulfilled | Breach | Goto
             | TNumber Integer
             | Other Text.Text
             | Do
             | Checkbox
             | Aka -- also known as, for AKA Receiving Party
             | Typically -- to provide default values
             | Empty | EOL
             | RuleMarker Int Text.Text
             | Expect | ScenarioTok
             | TokLT | TokLTE | TokGT | TokGTE | TokIn | TokNotIn | TokEQ
             | Otherwise
             | SOF | EOF
             | GoDeeper | UnDeeper
             | SetPlus | SetLess -- set union and subtraction
             | Where -- like in Haskell
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
  -- showTokens Proxy (x NE.:| []) = show (tokenVal x)
  showTokens Proxy xs = unwords
    . NE.toList
    . fmap (showMyToken . tokenVal) $ xs
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
renderToken TokLT = "<"
renderToken TokLTE = "<="
renderToken TokGT = ">"
renderToken TokGTE = ">="
renderToken TokIn = "IN"
renderToken TokNotIn = "NOT IN"
renderToken TokEQ = "=="
renderToken Checkbox = ""
renderToken GoDeeper = "("
renderToken UnDeeper = ")"
renderToken SetPlus = "PLUS"
renderToken SetLess = "LESS"
renderToken A_An = "A"
renderToken (TNumber n) = show n
renderToken OneOf = "ONE OF"
renderToken TypeSeparator = "::"
renderToken (Other txt) = show txt
renderToken (RuleMarker 0 txt) = "§0" ++ Text.unpack txt
renderToken (RuleMarker n txt) = concat $ replicate n (Text.unpack txt)
renderToken tok = map toUpper (show tok)


liftMyToken :: [String] -> MyToken -> WithPos MyToken
liftMyToken = WithPos pos 0 . Just
  where
    pos = initialPos ""
