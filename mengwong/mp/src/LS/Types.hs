{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LS.Types ( module LS.BasicTypes
                , module LS.Types) where

import qualified Data.Text.Lazy as Text
import Text.Megaparsec
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import Data.Void (Void)
import qualified Data.Set           as Set
import Control.Monad
import qualified AnyAll as AA
import Control.Monad.Reader (ReaderT (runReaderT), asks)
import Data.Aeson (ToJSON)
import GHC.Generics

import LS.BasicTypes
import Control.Monad.Writer.Lazy (WriterT (runWriterT))
import Data.Monoid (Endo (Endo))
import Data.Bifunctor (second)

type PlainParser = ReaderT RunConfig (Parsec Void MyStream)
-- A parser generates a list of rules (in the "appendix", representing nested rules defined inline) and optionally some other value
type Parser = WriterT (DList Rule) PlainParser
type Depth = Int
type Preamble = MyToken
type BoolRules = BoolStruct
type BoolRulesP = BoolStructP
type BoolStruct = AA.Item Text.Text
type BoolStructP = AA.Item ParamText

-- | Like [a] but with faster concatenation.
newtype DList a = DList (Endo [a])
  deriving newtype (Semigroup, Monoid)

singeltonDL :: a -> DList a
singeltonDL a = DList $ Endo (a:)

listToDL :: [a] -> DList a
listToDL as = DList $ Endo (as ++)

dlToList :: DList a -> [a]
dlToList (DList (Endo f)) = f []

runMyParser :: ((a, [Rule]) -> b) -> RunConfig -> Parser a -> String -> MyStream -> Either (ParseErrorBundle MyStream Void) b
runMyParser f rc p = runParser (runReaderT (f . second dlToList <$> runWriterT (p <* eof)) rc)

data RuleBody = RuleBody { rbaction   :: BoolStructP -- pay(to=Seller, amount=$100)
                         , rbpbrs     :: [(Preamble, BoolRulesP)] -- not subject to the party
                         , rbpbrneg   :: [(Preamble, BoolRulesP)] -- negative global conditions
                         , rbdeon     :: Deontic
                         , rbtemporal :: Maybe (TemporalConstraint Text.Text)
                         , rbupon     :: [(Preamble, BoolRulesP)] -- Upon  event conditions -- TODO, figure out how these are joined; or should we ban multiple UPONs?
                         , rbgiven    :: [(Preamble, ParamText)] -- Given
                         , rbhaving   :: Maybe ParamText
                         }
                      deriving (Eq, Show, Generic)

data Rule = Regulative
  -- TODO: preserve Every vs Party as keyword, just like in Constitutive
            { every    :: ConstitutiveName         -- every person
            , who      :: Maybe BoolStructP         -- who walks and (eats or drinks)
            , cond     :: Maybe BoolStructP         -- if it is a saturday
            , deontic  :: Deontic            -- must
            , action   :: BoolStructP          -- fart loudly AND run away
            , temporal :: Maybe (TemporalConstraint Text.Text) -- Before "midnight"
            , hence    :: Maybe Rule
            , lest     :: Maybe Rule
            , rlabel   :: Maybe Text.Text
            , lsource  :: Maybe Text.Text
            , srcref   :: Maybe SrcRef
            , upon     :: [BoolStructP] -- UPON entering the club (event prereq trigger)
            , given    :: Maybe ParamText
            , having   :: Maybe ParamText  -- HAVING sung...
            , orig     :: [(Preamble, BoolStructP)]
            }
          | Constitutive
            { name     :: ConstitutiveName     -- the thing we are defining
            , keyword  :: MyToken       -- Means, Includes, Is, Deem
            , letbind  :: BoolStructP   -- might be just a bunch of words to be parsed downstream
            , cond     :: Maybe BoolStructP -- a boolstruct set of conditions representing When/If/Unless
            , given    :: Maybe ParamText
            , rlabel   :: Maybe Text.Text
            , lsource  :: Maybe Text.Text
            , srcref   :: Maybe SrcRef
            , orig     :: [(Preamble, BoolStructP)]
            }
          | TypeDecl
            { name     :: ConstitutiveName         --      DEFINE Sign
            , super    :: Maybe TypeSig     --                  :: Thing
            , has      :: Maybe [(ParamText, Maybe TypeSig)] -- HAS foo :: List Hand \n bar :: Optional Restaurant
            , enums    :: Maybe ParamText  -- ONE OF rock, paper, scissors (basically, disjoint subtypes)
            , rlabel   :: Maybe Text.Text
            , lsource  :: Maybe Text.Text
            , srcref   :: Maybe SrcRef
            }
          | DefNameAlias -- inline alias, like     some thing AKA Thing
            { name   :: ConstitutiveName -- "Thing"
            , detail :: ConstitutiveName        -- "some thing"
            , nlhint :: Maybe Text.Text  -- "lang=en number=singular"
            , srcref :: Maybe SrcRef
            }
          | RegAlias Text.Text -- internal softlink to a regulative rule label
          | ConAlias Text.Text -- internal softlink to a constitutive rule label
          | RegFulfilled  -- trivial top
          | RegBreach     -- trivial bottom
          deriving (Eq, Show, Generic, ToJSON)

newtype RelName = RN { getName :: ConstitutiveName }

noLabel, noLSource :: Maybe Text.Text
noLabel   = Nothing
noLSource = Nothing
noSrcRef :: Maybe SrcRef
noSrcRef  = Nothing
noDeem   :: Maybe ParamText
noDeem = Nothing

data ParamType = TOne | TOptional | TList0 | TList1
  deriving (Eq, Show, Generic, ToJSON)

-- everything is stringly typed at the moment but as this code matures these will become more specialized.
data TemporalConstraint a = TBefore a
                          | TAfter  a
                          | TBy     a
                          | TOn     a
                          deriving (Eq, Show, Generic, ToJSON)
type ConstitutiveName = Text.Text
type EntityType = Text.Text

data TypeSig = SimpleType ParamType EntityType
             | InlineEnum ParamType ParamText
             deriving (Eq, Show, Generic, ToJSON)

-- is this a NonEmpty (NonEmpty Text.Text)
-- or a Tree (Text.Text)
-- type ParamText' = Tree Text.Text -- function(arg0, arg1=[val2,val3], arg4=[val5,val6])
--                                   -- Node "action" [ Node "eat"   [ Node "ice cream" [] ]
--                                   --               , Node "arg1"  [ Node "val2" [], Node "val3" [] ]
--                                   --               , Node "arg4"  [ Node "val5" [], Node "val6" [] ] ]

type ParamText = NonEmpty (NonEmpty Text.Text) -- but consider the Tree alternative above

text2pt :: a -> NonEmpty (NonEmpty a)
text2pt = pure . pure

pt2text :: NonEmpty (NonEmpty Text.Text) -> Text.Text
pt2text x = Text.unwords $ concatMap toList $ toList x

bsp2text :: BoolStructP -> Text.Text
bsp2text (AA.Leaf pt)  = pt2text pt
bsp2text (AA.Not  x)   = "not " <> bsp2text x
bsp2text (AA.Any xs) = "any (" <> Text.unwords (bsp2text <$> xs) <> ")"
bsp2text (AA.All xs) = "all (" <> Text.unwords (bsp2text <$> xs) <> ")"
-- and possibily we want to have interspersed BoolStructs along the way

data Deontic = DMust | DMay | DShant
  deriving (Eq, Show, Generic, ToJSON)

data SrcRef = SrcRef { url      :: Text.Text
                     , short    :: Text.Text
                     , srcrow   :: Int
                     , srccol   :: Int
                     , version  :: Maybe Text.Text
                     }
              deriving (Eq, Show, Generic, ToJSON)

mkTC :: MyToken -> Text.Text -> Maybe (TemporalConstraint Text.Text)
mkTC Before     tt = Just $ TBefore tt
mkTC After      tt = Just $ TAfter  tt
mkTC By         tt = Just $ TBy     tt
mkTC On         tt = Just $ TOn     tt
mkTC Eventually _  = Nothing
mkTC x      y  = error $ "mkTC: can't create temporal constraint from " ++ show x ++ ", " ++ show y

data RunConfig = RC { debug     :: Bool
                    , callDepth :: Int
                    , parseCallStack :: [String]
                    , sourceURL :: Text.Text
                    , asJSON    :: Bool
                    , toNLG     :: Bool
                    , toBabyL4  :: Bool
                    , toProlog  :: Bool
                    }

nestLevel :: RunConfig -> Int
nestLevel = length . parseCallStack

increaseNestLevel :: String -> RunConfig -> RunConfig
increaseNestLevel name rc = rc { parseCallStack = name : parseCallStack rc }

magicKeywords :: [Text.Text]
magicKeywords = Text.words "EVERY PARTY MUST MAY WHEN INCLUDES MEANS IS IF UNLESS DEFINE"

-- the Rule types employ these tokens, which are meaningful to L4.
--
toToken :: Text.Text -> MyToken

-- start a regulative rule
toToken "EVERY" =  Every
toToken "PARTY" =  Party

-- start a boolstruct
toToken "ALWAYS" = Always
toToken "NEVER"  = Never
toToken "WHO" =    Who
toToken "WHEN" =   When
toToken "IF" =     If
toToken "UPON" =   Upon
toToken "GIVEN" =  Given
toToken "HAVING" = Having

toToken "MEANS" =  Means -- "infix"-starts a constitutive rule "Name MEANS x OR y OR z"
toToken "INCLUDES" =  Includes
toToken "IS" =     Is

-- boolean connectors
toToken "OR" =     Or
toToken "AND" =    And
toToken "UNLESS" = Unless
toToken "IF NOT" = Unless
toToken "NOT"    = MPNot

-- deontics
toToken "MUST" =   Must
toToken "MAY" =    May
toToken "SHANT" =  Shant

-- temporals
toToken "BEFORE" = Before
toToken "WITHIN" = Before
toToken "AFTER" =  After
toToken "BY" =  By
toToken "ON" =  On
toToken "EVENTUALLY" = Eventually

-- the rest of the regulative rule
toToken "➔"       =     Do
toToken "->"      =     Do
toToken "DO"      =     Do
toToken "PERFORM" =     Do

-- for discarding
toToken "" =       Empty
toToken "TRUE" =   Checkbox
toToken "FALSE" =  Checkbox
toToken "HOLDS" =  Holds

-- regulative chains
toToken "HENCE" = Hence
toToken  "THEN" = Hence
-- trivial contracts
toToken  "FULFILLED" = Fulfilled
toToken  "BREACH" = Breach

toToken     "LEST" = Lest
toToken     "ELSE" = Lest
toToken  "OR ELSE" = Lest
toToken "XOR ELSE" = Lest
toToken    "XELSE" = Lest

toToken ";"      = EOL

toToken ":"      = TypeSeparator
toToken "::"     = TypeSeparator
toToken "TYPE"   = TypeSeparator
toToken "A"      = A_An
toToken "AN"     = A_An

toToken "DEFINE"    = Define
toToken "ONE OF"    = OneOf
toToken "AS ONE OF" = OneOf
toToken "DEEM"      = Deem
toToken "HAS"       = Has

toToken "ONE"       = One
toToken "OPTIONAL"  = Optional
toToken "LIST0"     = List0
toToken "LIST1"     = List1

toToken "AKA"       = Aka

-- we recognize numbers
toToken s | [(n,"")] <- reads $ Text.unpack s = TNumber n

-- any other value becomes an Other -- "walks", "runs", "eats", "drinks"
toToken x = Other x

pToken :: MyToken -> Parser MyToken
pToken c = checkDepth >> pTokenMatch (== c) c

-- | check that the next token is at at least the current level of indentation
checkDepth :: Parser ()
checkDepth = do
  depth <- asks callDepth
  leftX <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.
  guard $ leftX >= depth

pXLocation :: Parser Depth
pXLocation = token test Set.empty <|> pure 0 <?> "x location"
  where
    test (WithPos (SourcePos _ _y x) _ _ _) = Just (unPos x)

pYLocation :: Parser Depth
pYLocation = token test Set.empty <?> "y location"
  where
    test (WithPos (SourcePos _ y _x) _ _ _) = Just (unPos y)


pTokenMatch :: (MyToken -> Bool) -> MyToken -> Parser MyToken
pTokenMatch f c = token test (Set.singleton . Tokens . nes . liftMyToken $ c)
  where
    test (WithPos _ _ _ x) =
      if f x
        then Just x
        else Nothing
    nes x = x :| []

