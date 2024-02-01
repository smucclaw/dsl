{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

{-|
Foundational types imported by all other modules.

These are largely:

* tokens for the parser and renderer
* stream for the parser
-}

module LS.BasicTypes
  ( MyStream (..),
    MyToken (..),
    RawStanza,
    WithPos (..),
    liftMyToken,
    renderToken,
    toToken
  )
where

import Data.Aeson (ToJSON)
import Data.Char (toUpper)
import Data.Hashable (Hashable)
import Data.List qualified as DL
import Data.List.NonEmpty qualified as NE
import Data.Proxy (Proxy (..))
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Vector qualified as V
import GHC.Generics (Generic)
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

data MyToken = Every | Party | TokAll
             | Who | Which | Whose
             | Must | May | Shant
             | If | When | Always | Never
             | Or | And | MPNot
             | Before | After | By | On | Eventually -- TVague is a temporal constraint but not a token
             | Means | Includes  | Is
             | Given | Giveth | Having | Upon
             | Declare | Define | OneOf | Holds
             | Decide
             | A_An
             | Deem | As | Has
             -- | AsOf -- used to evaluate a term not under the live context but at some previous time
             | TypeSeparator -- ::, TYPE, AS, shrug
             | One | Optional | List0 | List1 -- list-like modifiers, List1=NonEmpty
             |                   Set0 | Set1  -- set; set.nonempty
             | Distinct -- entity modifier in GIVEN
             | Unless
             | Hence | Lest | Fulfilled | Breach | Goto
             | Then | Else
             | TNumber Float
             | Other Text.Text
             | Do | FMap
             | TokTrue | TokFalse
             | Aka -- also known as, for AKA Receiving Party
             | Typically -- to provide default values
             | Empty | EOL
             | RuleMarker Int Text.Text
             | Expect | ScenarioTok
             | TokLT | TokLTE | TokGT | TokGTE | TokIn | TokNotIn | TokEQ | TokAnd | TokOr | TokSum | TokProduct | TokMin | TokMax
             | Notwithstanding | Despite | SubjectTo
             | Otherwise
             | SOF | EOF
             | GoDeeper | UnDeeper
             | SetPlus | SetLess -- set union and subtraction
             | Where -- like in Haskell
             | Semicolon -- rule separator
  deriving (Ord, Eq, Show, Generic, Hashable, ToJSON)

-- the Rule types employ these tokens, which are meaningful to L4.
--
toToken :: Text.Text -> [MyToken]

-- start a regulative rule
toToken "EVERY" =  pure Every
toToken "PARTY" =  pure Party
toToken "ALL"   =  pure TokAll -- when parties are treated as a collective, e.g. ALL diners. TokAll means "Token All"

-- start a boolstruct
toToken "ALWAYS" = pure Always
toToken "NEVER"  = pure Never

-- qualify a subject
toToken "WHO" =    pure Who
toToken "WHICH" =  pure Which
toToken "WHOSE" =  pure Whose

toToken "WHEN" =   pure When
toToken "IF" =     pure If
toToken "UPON" =   pure Upon
toToken "GIVEN" =  pure Given
toToken "GIVETH" = pure Giveth
toToken "HAVING" = pure Having

toToken "MEANS" =  pure Means -- "infix"-starts a constitutive rule "Name MEANS x OR y OR z"
toToken "INCLUDES" =  pure Includes
toToken "IS" =     pure Is

-- boolean connectors
toToken "OR" =     pure Or
toToken ((PCRE.≈ [PCRE.re|^(AND|\.\.\.|…)$|]) -> True) =
  pure And -- Elipses are CNL sugar to allow phrases to follow
toToken ((PCRE.≈ [PCRE.re|^(UNLESS|EXCEPT|IF NOT)$|]) -> True) = pure Unless
toToken "NOT"    = pure MPNot

-- set operators
toToken "PLUS"   = pure SetPlus
toToken "LESS"   = pure SetLess

-- deontics
toToken "MUST" =   pure Must
toToken "MAY" =    pure May
toToken "SHANT" =  pure Shant

-- temporals
toToken ((PCRE.≈ [PCRE.re|^(UNTIL|BEFORE)$|]) -> True)  = pure Before  -- <
toToken "WITHIN" = pure Before  -- <=
toToken "AFTER"  = pure After   -- >
toToken "BY"     = pure By
toToken ((PCRE.≈ [PCRE.re|^(ON|AT)$|]) -> True)  = pure On -- ==
toToken "EVENTUALLY" = pure Eventually

-- the rest of the regulative rule
toToken ((PCRE.≈ [PCRE.re|^(➔|->|DO|PERFORM)$|]) -> True)      = pure Do

-- for discarding
toToken "" =       pure Empty
toToken "TRUE" =   pure TokTrue
toToken "FALSE" =  pure TokFalse
toToken "HOLDS" =  pure Holds

-- regulative chains
toToken ((PCRE.≈ [PCRE.re|^(HENCE|THUS)$|]) -> True)      = pure Hence

-- alternative formulations intended to be closer to natural language
-- for the obligation case
toToken "IF FULFILLED"      = pure Hence
toToken ((PCRE.≈ [PCRE.re|^IF (NOT FULFILLED|VIOLATED)$|]) -> True)      = pure Lest
-- for the permission case
toToken ((PCRE.≈ [PCRE.re|^IF EXERCI(S|Z)ED$|]) -> True)      = pure Hence
toToken ((PCRE.≈ [PCRE.re|^IF NOT EXERCI(S|Z)ED$|]) -> True)      = pure Lest
-- for the prohibition case
toToken "IF PROHIBITION VIOLATED"      = pure Lest
toToken ((PCRE.≈ [PCRE.re|^IF (PROHIBITION NOT|NOT (PROHIBITION)?) VIOLATED$|]) -> True)      = pure Hence

-- mutable state variables are modified by UPON THEN ELSE
toToken     "THEN" = pure Then
toToken ((PCRE.≈ [PCRE.re|^(((X)?OR |X)?ELSE)$|]) -> True)      = pure Else

-- trivial contracts
toToken  "FULFILLED" = pure Fulfilled
toToken  "BREACH" = pure Breach

toToken     "LEST" = pure Lest
toToken  "GOTO" = pure Goto

toToken ";"      = pure EOL

toToken ((PCRE.≈ [PCRE.re|^(:(:)?|TYPE|IS (A(N)?|THE))$|]) -> True) =
  [TypeSeparator, A_An]
toToken ((PCRE.≈ [PCRE.re|^(A(N)?|THE)$|]) -> True) = pure A_An -- [TODO] this is going to break entirely innocent end-user phrasing like 7 8 9 A B C D E

toToken "DECLARE"   = pure Declare
toToken "DEFINE"    = pure Define -- [TODO] rephrase DEFINE to support DECIDE and possibly overloaded DATA?
toToken "DATA"      = pure Define
toToken "DECIDE"    = pure Decide
toToken ((PCRE.≈ [PCRE.re|^(ONEOF|((I|A)?S )?ONE OF)$|]) -> True) =
  pure OneOf
toToken "DEEM"      = pure Deem
toToken "HAS"       = pure Has

toToken "ONE"       = pure One
toToken "OPTIONAL"  = pure Optional

toToken "LIST0"     = pure List0
toToken ((PCRE.≈ [PCRE.re|^(LIST(1|( )?OF)?)$|]) -> True) = pure List1

toToken "SET0"     = pure Set0
toToken ((PCRE.≈ [PCRE.re|^(SET(1|( )?OF)?)$|]) -> True) = pure Set1

toToken "MAP"       = pure FMap

toToken "AKA"       = pure Aka
toToken "TYPICALLY" = pure Typically

toToken ((PCRE.≈ [PCRE.re|^(CLAUSE|SECTION)$|]) -> True) =
  pure $ RuleMarker   1  "§"

toToken (PCRE.scan [PCRE.re|^-(§|¶)$|] -> [(_, [c])]) =
  pure $ RuleMarker (-1) c

toToken s@(PCRE.scan [PCRE.re|^(§|¶|H)+$|] -> [(_, [c])]) =
  pure $ RuleMarker (Text.length s) c

toToken "SCENARIO"  = pure ScenarioTok
toToken "EXPECT"    = pure Expect
toToken "<"         = pure TokLT
toToken ((PCRE.≈ [PCRE.re|^MIN( OF)?$|]) -> True)     = pure TokMin
toToken ((PCRE.≈ [PCRE.re|^(<=|=>)?$|]) -> True)      = pure TokLTE
toToken ">"         = pure TokGT
toToken ((PCRE.≈ [PCRE.re|^MAX( OF)?$|]) -> True)     = pure TokMax
toToken ">="        = pure TokGTE
toToken "&&"        = pure TokAnd
toToken "||"        = pure TokOr
toToken ((PCRE.≈ [PCRE.re|^SUM( OF)?$|]) -> True)     = pure TokSum
toToken ((PCRE.≈ [PCRE.re|^PRODUCT( OF)?$|]) -> True) = pure TokProduct
toToken ((PCRE.≈ [PCRE.re|^=(=)?(=)?$|]) -> True)     = pure TokEQ
toToken "IN"        = pure TokIn
toToken "NOT IN"    = pure TokNotIn

-- rule priority interactions and "defeasibility"
toToken "SUBJECT TO" = pure SubjectTo
toToken "DESPITE"    = pure Despite
toToken "NOTWITHSTANDING" = pure Notwithstanding

toToken "OTHERWISE" = pure Otherwise

toToken "WHERE"     = pure Where

toToken ";;"        = pure Semicolon

-- we recognize numbers
-- let's not recognize numbers yet; treat them as strings to be pOtherVal'ed.
toToken (reads . Text.unpack -> [(n, "")]) = pure $ TNumber n

-- any other value becomes an Other -- "walks", "runs", "eats", "drinks"
toToken x = pure $ Other x

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
