{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module L4.Types ( module L4.BasicTypes
                , module L4.Types) where

import qualified Data.Text.Lazy as Text
import Text.Megaparsec
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Void (Void)
import qualified Data.Set           as Set
import Control.Monad
import qualified AnyAll as AA
import Control.Monad.Reader (ReaderT (runReaderT), asks)
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics

import L4.BasicTypes
import Control.Monad.Writer.Lazy (WriterT (runWriterT))
import Data.Monoid (Endo (Endo))
import Data.Bifunctor (second)

type PlainParser = ReaderT RunConfig (Parsec Void MyStream)
-- A parser generates a list of rules and optionally some other value
type Parser = WriterT (DList Rule) PlainParser
type Depth = Int
type Preamble = MyToken
type BoolRules = Maybe BoolStruct
type BoolStruct = AA.Item Text.Text

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

data Rule = Regulative
            { every    :: EntityType         -- every person
            , who      :: Maybe BoolStruct         -- who walks and (eats or drinks)
            , cond     :: Maybe BoolStruct         -- if it is a saturday
            , deontic  :: Deontic            -- must
            , action   :: ActionType         -- sing
            , temporal :: Maybe (TemporalConstraint Text.Text) -- Before "midnight"
            , hence    :: Maybe Rule
            , lest     :: Maybe Rule
            , rlabel   :: Maybe Text.Text
            , lsource  :: Maybe Text.Text
            , srcref   :: Maybe SrcRef
            , upon     :: Maybe BoolStruct   -- UPON entering the club (event prereq trigger)
            , given    :: Maybe BoolStruct   -- GIVEN an Entertainment flag was previously set in the history trace
            }
          | Constitutive
            { term     :: ConstitutiveTerm
            , cond     :: Maybe BoolStruct
            , rlabel   :: Maybe Text.Text
            , lsource  :: Maybe Text.Text
            , srcref   :: Maybe SrcRef
            }
          | DefTermAlias -- inline alias, like     some thing ("Thing")
            { term   :: ConstitutiveTerm -- "Thing"
            , detail :: Text.Text        -- "some thing"
            , nlhint :: Maybe Text.Text  -- "lang=en number=singular"
            , srcref :: Maybe SrcRef
            }
          | RegAlias Text.Text -- internal softlink to a regulative rule label
          | ConAlias Text.Text -- internal softlink to a constitutive rule label
          | RegFulfilled  -- trivial top
          | RegBreach     -- trivial bottom
          deriving (Eq, Show, Generic, ToJSON, FromJSON)

noLabel, noLSource :: Maybe Text.Text
noLabel   = Nothing
noLSource = Nothing
noSrcRef :: Maybe SrcRef
noSrcRef  = Nothing

-- everything is stringly typed at the moment but as this code matures these will become more specialized.
data TemporalConstraint a = TBefore a
                          | TAfter  a
                          | TBy     a
                          | TOn     a
                          deriving (Eq, Show, Generic, ToJSON, FromJSON)
type ConstitutiveTerm = Text.Text
type EntityType = Text.Text
type ActionType = (Text.Text,[(Text.Text,[Text.Text])])
data Deontic = DMust | DMay | DShant
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data SrcRef = SrcRef { url      :: Text.Text
                     , short    :: Text.Text
                     , srcrow   :: Int
                     , srccol   :: Int
                     , version  :: Maybe Text.Text
                     }
              deriving (Eq, Show, Generic, ToJSON, FromJSON)

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
                    }

nestLevel :: RunConfig -> Int
nestLevel = length . parseCallStack

increaseNestLevel :: String -> RunConfig -> RunConfig
increaseNestLevel name rc = rc { parseCallStack = name : parseCallStack rc }

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

toToken "MEANS" =  Means -- "infix"-starts a constitutive rule "Term MEANS x OR y OR z"
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

-- we recognize numbers
toToken s | [(n,"")] <- reads $ Text.unpack s = TNumber n

-- any other value becomes an Other -- "walks", "runs", "eats", "drinks"
toToken x = Other x


pToken :: MyToken -> Parser MyToken
pToken c = pTokenMatch (== c) c

dToken :: MyToken -> Parser MyToken
dToken c = do
  d <- asks callDepth
  currentX <- lookAhead pXLocation
  guard $ currentX >= d
  pTokenMatch (== c) c

pXLocation :: Parser Depth
pXLocation = token test Set.empty <?> "x location"
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

