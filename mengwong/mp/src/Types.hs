{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Proxy
import qualified Data.Text.Lazy as Text
import Text.Megaparsec
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List as DL
import qualified Data.Vector as V
import Data.Void (Void)
import qualified Data.Set           as Set
import Control.Monad
import qualified AnyAll as AA

type RawStanza = V.Vector (V.Vector Text.Text) -- "did I stammer?"
type Parser = Parsec Void MyStream
type Depth = Int
type Preamble = MyToken
type BoolRules = (BoolStruct, [Rule])

data Rule = Regulative
            { every    :: EntityType         -- every person
            , who      :: BoolStruct         -- walks and (eats or drinks)
            , deontic  :: Deontic            -- must
            , action   :: ActionType         -- sing
            , temporal :: Maybe TemporalConstraint -- before midnight
            }
          | Constitutive
            { term :: ConstitutiveTerm
            , cond :: BoolStruct
            }
          deriving (Eq, Show)
-- everything is stringly typed at the moment but as this code matures these will become more specialized.
type TemporalConstraint = Text.Text
type ConstitutiveTerm = Text.Text
type EntityType = Text.Text
type ActionType = Text.Text
type BoolStruct = AA.Item Text.Text
data Deontic = DMust | DMay | DShant
  deriving (Eq, Show)

data RunConfig = RC { debug     :: Bool
                    , callDepth :: Int
                    }

-- the Rule types employ these tokens, which are meaningful to L4.
--
toToken :: Text.Text -> MyToken

-- start a regulative rule
toToken "EVERY" =  Every

-- start a boolstruct
toToken "ALWAYS" = Always
toToken "WHO" =    Who
toToken "WHEN" =   When
toToken "IF" =     If
toToken "MEANS" =  Means -- "infix"-starts a constitutive rule "Term MEANS x OR y OR z"
toToken "IS" =     Is

-- boolean connectors
toToken "OR" =     Or
toToken "AND" =    And
toToken "UNLESS" = Unless

-- deontics
toToken "MUST" =   Must
toToken "MAY" =    May
toToken "SHANT" =  Shant

-- the rest of the regulative rule
toToken "➔"       =      Do
toToken "->"      =     Do
toToken "DO"      =     Do
toToken "PERFORM" =     Do

-- for discarding
toToken "" =       Empty
toToken "TRUE" =   Checkbox
toToken "FALSE" =  Checkbox

-- we recognize numbers
toToken s | [(n,"")] <- reads $ Text.unpack s = Number n

-- any other value becomes an Other -- "walks", "runs", "eats", "drinks"
toToken x = Other x

data MyToken = Every | Who | Means | When | Is | Always
             | Must | May | Shant | If | Or | And
             | Unless
             | Number Int
             | Other Text.Text
             | Do
             | Checkbox
             | Empty
  deriving (Ord, Eq, Show)
 


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
      tokensConsumed =
        case NE.nonEmpty pre of
          Nothing -> 0
          Just nePre -> tokensLength pxy nePre
      restOfLine = takeWhile (/= '\n') postStr

pxy :: Proxy MyStream
pxy = Proxy

showMyToken :: MyToken -> String
showMyToken = show

liftMyToken :: MyToken -> WithPos MyToken
liftMyToken = WithPos pos pos 0
  where
    pos = initialPos ""

pToken :: MyToken -> Parser MyToken
pToken c = pTokenMatch (== c) c

dToken :: Depth -> MyToken -> Parser MyToken
dToken d c = do
  currentX <- lookAhead pXLocation
  guard $ currentX >= d
  pTokenMatch (== c) c

pXLocation :: Parser Depth
pXLocation = token test Set.empty <?> "nested Boolstruct"
  where
    test (WithPos (SourcePos _ _y x) _ _ _) = Just (unPos x)


pTokenMatch :: (MyToken -> Bool) -> MyToken -> Parser MyToken
pTokenMatch f c = token test (Set.singleton . Tokens . nes . liftMyToken $ c)
  where
    test (WithPos _ _ _ x) =
      if f x
        then Just x
        else Nothing
    nes x = x :| []

data WithPos a = WithPos
  { startPos :: SourcePos
  , endPos :: SourcePos
  , tokenLength :: Int
  , tokenVal :: a
  } deriving (Eq, Ord, Show)

data MyStream = MyStream
  { myStreamInput :: RawStanza  -- for showing offending lines
  , unMyStream :: [WithPos MyToken]
  }
  deriving Show
