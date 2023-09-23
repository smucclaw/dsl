{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

{-|
Foundational types imported by all other modules.

These are largely:

* tokens for the parser and renderer
* stream for the parser
-}

module LS.BasicTypes where

import Data.Aeson (ToJSON)
import Data.Char (toUpper)
import Data.Hashable (Hashable)
import qualified Data.List as DL
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import qualified Data.Vector as V
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

-- import Data.List (intercalate)

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
  deriving (Ord, Eq, Show, Generic, ToJSON)

instance Hashable MyToken

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
toToken "AND" =    pure And
toToken "..." =    pure And   -- CNL sugar to allow phrases to follow
toToken "…"   =    pure And   -- CNL sugar to allow phrases to follow -- this is unicode for ellipsis
toToken "UNLESS" = pure Unless
toToken "EXCEPT" = pure Unless
toToken "IF NOT" = pure Unless
toToken "NOT"    = pure MPNot

-- set operators
toToken "PLUS"   = pure SetPlus
toToken "LESS"   = pure SetLess

-- deontics
toToken "MUST" =   pure Must
toToken "MAY" =    pure May
toToken "SHANT" =  pure Shant

-- temporals
toToken "UNTIL"  = pure Before  -- <
toToken "BEFORE" = pure Before  -- <
toToken "WITHIN" = pure Before  -- <=
toToken "AFTER"  = pure After   -- >
toToken "BY"     = pure By
toToken "ON"     = pure On      -- ==
toToken "AT"     = pure On      -- ==
toToken "EVENTUALLY" = pure Eventually

-- the rest of the regulative rule
toToken "➔"       =     pure Do
toToken "->"      =     pure Do
toToken "DO"      =     pure Do
toToken "PERFORM" =     pure Do

-- for discarding
toToken "" =       pure Empty
toToken "TRUE" =   pure TokTrue
toToken "FALSE" =  pure TokFalse
toToken "HOLDS" =  pure Holds

-- regulative chains
toToken "HENCE" = pure Hence
toToken "THUS"  = pure Hence

-- alternative formulations intended to be closer to natural language
-- for the obligation case
toToken "IF FULFILLED"      = pure Hence
toToken "IF NOT FULFILLED"  = pure Lest
toToken "IF VIOLATED"       = pure Lest
-- for the permission case
toToken "IF EXERCISED"      = pure Hence
toToken "IF EXERCIZED"      = pure Hence
toToken "IF NOT EXERCISED"  = pure Lest
toToken "IF NOT EXERCIZED"  = pure Lest
-- for the prohibition case
toToken "IF PROHIBITION VIOLATED"      = pure Lest
toToken "IF PROHIBITION NOT VIOLATED"  = pure Hence
toToken "IF NOT PROHIBITION VIOLATED"  = pure Hence
toToken "IF NOT VIOLATED"              = pure Hence


-- mutable state variables are modified by UPON THEN ELSE
toToken     "THEN" = pure Then
toToken     "ELSE" = pure Else
toToken  "OR ELSE" = pure Else
toToken "XOR ELSE" = pure Else
toToken    "XELSE" = pure Else

-- trivial contracts
toToken  "FULFILLED" = pure Fulfilled
toToken  "BREACH" = pure Breach

toToken     "LEST" = pure Lest
toToken  "GOTO" = pure Goto

toToken ";"      = pure EOL

toToken ":"      = [TypeSeparator, A_An]
toToken "::"     = [TypeSeparator, A_An]
toToken "TYPE"   = [TypeSeparator, A_An]
toToken "IS A"   = [TypeSeparator, A_An]
toToken "IS AN"  = [TypeSeparator, A_An]
toToken "IS THE" = [TypeSeparator, A_An]
toToken "A"      = pure A_An -- [TODO] this is going to break entirely innocent end-user phrasing like 7 8 9 A B C D E
toToken "AN"     = pure A_An
toToken "THE"    = pure A_An

toToken "DECLARE"   = pure Declare
toToken "DEFINE"    = pure Define -- [TODO] rephrase DEFINE to support DECIDE and possibly overloaded DATA?
toToken "DATA"      = pure Define
toToken "DECIDE"    = pure Decide
toToken "ONEOF"    = pure OneOf
toToken "ONE OF"    = pure OneOf
toToken "IS ONE OF" = pure OneOf
toToken "AS ONE OF" = pure OneOf
toToken "DEEM"      = pure Deem
toToken "HAS"       = pure Has

toToken "ONE"       = pure One
toToken "OPTIONAL"  = pure Optional
toToken "LIST0"     = pure List0
toToken "LIST1"     = pure List1
toToken "LIST OF"   = pure List1
toToken "LISTOF"    = pure List1
toToken "LIST"      = pure List1

toToken "MAP"       = pure FMap

toToken "AKA"       = pure Aka
toToken "TYPICALLY" = pure Typically

toToken "-§"        = pure $ RuleMarker (-1) "§"
toToken "SECTION"   = pure $ RuleMarker   1  "§"
toToken "§"         = pure $ RuleMarker   1  "§"
toToken "§§"        = pure $ RuleMarker   2  "§"
toToken "§§§"       = pure $ RuleMarker   3  "§"
toToken "§§§§"      = pure $ RuleMarker   4  "§"
toToken "§§§§§"     = pure $ RuleMarker   5  "§"
toToken "§§§§§§"    = pure $ RuleMarker   6  "§"

toToken "SCENARIO"  = pure ScenarioTok
toToken "EXPECT"    = pure Expect
toToken "<"         = pure TokLT
toToken "MIN"       = pure TokMin;     toToken "MIN OF"    = pure TokMin
toToken "=<"        = pure TokLTE
toToken "<="        = pure TokLTE
toToken ">"         = pure TokGT
toToken "MAX"       = pure TokMax;     toToken "MAX OF"    = pure TokMax
toToken ">="        = pure TokGTE
toToken "="         = pure TokEQ
toToken "&&"        = pure TokAnd
toToken "||"        = pure TokOr
toToken "SUM"       = pure TokSum;     toToken "SUM OF"     = pure TokSum
toToken "PRODUCT"   = pure TokProduct; toToken "PRODUCT OF" = pure TokProduct
toToken "=="        = pure TokEQ
toToken "==="       = pure TokEQ
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
toToken s | [(n,"")] <- reads $ Text.unpack s = pure $ TNumber n

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
  -- showTokens Proxy (x NE.:| []) = show (tokenVal x)
  showTokens Proxy xs = unwords
    . NE.toList
    . fmap showTokenWithContext $ xs
  tokensLength Proxy xs = sum (tokenLength <$> xs)

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
renderToken (RuleMarker 0 txt) = "§0" ++ Text.unpack txt
renderToken (RuleMarker n txt) = concat $ replicate n (Text.unpack txt)

renderToken Semicolon = ";;"

renderToken SubjectTo = "SUBJECT TO"
renderToken TokMin = "MIN"
renderToken TokMax = "MAX"
renderToken TokSum = "SUM"
renderToken TokProduct = "PRODUCT"
renderToken FMap = "MAP"


renderToken tok = map toUpper (show tok)

liftMyToken :: [String] -> MyToken -> WithPos MyToken
liftMyToken = WithPos pos 0 . Just
  where
    pos = initialPos ""
