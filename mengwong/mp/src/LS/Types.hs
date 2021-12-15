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
import Data.List.NonEmpty (NonEmpty ((:|)), toList, fromList)
import Data.Void (Void)
import qualified Data.Set           as Set
import Control.Monad
import qualified AnyAll as AA
import Control.Monad.Reader (ReaderT (runReaderT), asks)
import Data.Aeson (ToJSON)
import GHC.Generics
import Debug.Trace

import LS.BasicTypes
import Control.Monad.Writer.Lazy (WriterT (runWriterT))
import Data.Monoid (Endo (Endo))
import Data.Bifunctor (second)

type PlainParser = ReaderT RunConfig (Parsec Void MyStream)
-- A parser generates a list of rules (in the "appendix", representing nested rules defined inline) and optionally some other value
type Parser = WriterT (DList Rule) PlainParser
type Depth = Int
type Preamble = MyToken
type BoolStruct = AA.Item Text.Text
type BoolStructP = AA.Item ParamText
type BoolStructR = AA.Item RelationalPredicate

mkLeaf :: a -> AA.Item (NonEmpty (NonEmpty a, Maybe TypeSig))
mkLeaf = AA.Leaf . text2pt

mkLeafR :: Text.Text -> BoolStructR
mkLeafR = AA.Leaf . RPParamText . text2pt

-- remove the TypeSig from a ParamText
untypePT :: ParamText -> NonEmpty (NonEmpty Text.Text)
untypePT = fmap fst

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

-- intermediate form for a deontic rule
data RuleBody = RuleBody { rbaction   :: BoolStructP -- pay(to=Seller, amount=$100)
                         , rbpbrs     :: [(Preamble, BoolStructR)] -- not subject to the party
                         , rbpbrneg   :: [(Preamble, BoolStructR)] -- negative global conditions
                         , rbdeon     :: Deontic
                         , rbtemporal :: Maybe (TemporalConstraint Text.Text)
                         , rbupon     :: [(Preamble, ParamText)] -- Upon  event conditions -- TODO, figure out how these are joined; or should we ban multiple UPONs?
                         , rbgiven    :: [(Preamble, ParamText)] -- Given
                         , rbhaving   :: Maybe ParamText
                         , rbkeyname  :: (Preamble, BoolStructP)   -- Every man AND woman
                         , rbwho      :: Maybe (Preamble, BoolStructR)   -- WHO seeks eternal life in me
                         }
                      deriving (Eq, Show, Generic)

ruleName :: Rule -> RuleName
ruleName Regulative { subj  = x } = [bsp2text x]
ruleName x = name x

type RuleLabel = (Text.Text   --  "§"
                 ,Int         --   1
                 ,Text.Text   --  "My First Rule"
                 )

-- maybe we should have a proper dict orientation here
data KW a = KW { dictK :: MyToken
               , dictV :: a }

data Rule = Regulative
            { subj     :: BoolStructP               -- man AND woman AND child
            , keyword  :: MyToken                   -- Every | Party | TokAll
            , who      :: Maybe BoolStructR         -- who walks and (eats or drinks)
            , cond     :: Maybe BoolStructR         -- if it is a saturday
            , deontic  :: Deontic            -- must
            , action   :: BoolStructP          -- fart loudly AND run away
            , temporal :: Maybe (TemporalConstraint Text.Text) -- Before "midnight"
            , hence    :: Maybe Rule
            , lest     :: Maybe Rule
            , rlabel   :: Maybe RuleLabel
            , lsource  :: Maybe Text.Text
            , srcref   :: Maybe SrcRef
            , upon     :: Maybe ParamText
            , given    :: Maybe ParamText
            , having   :: Maybe ParamText  -- HAVING sung...
            }
          | Constitutive
            { name     :: RuleName   -- the thing we are defining
            , keyword  :: MyToken       -- Means, Includes, Is, Deem, Decide
            , letbind  :: BoolStructR
            , cond     :: Maybe BoolStructR -- a boolstruct set of conditions representing When/If/Unless
            , given    :: Maybe ParamText
            , rlabel   :: Maybe RuleLabel
            , lsource  :: Maybe Text.Text
            , srcref   :: Maybe SrcRef
            }
          | Hornlike
            { name     :: RuleName
            , keyword  :: MyToken
            , given    :: Maybe ParamText
            , upon     :: Maybe ParamText
            , clauses  :: [HornClause2]
            , rlabel   :: Maybe RuleLabel
            , lsource  :: Maybe Text.Text
            , srcref   :: Maybe SrcRef
            }
          | TypeDecl
            { name     :: RuleName  --      DEFINE Sign
            , super    :: Maybe TypeSig     --                  :: Thing
            , has      :: Maybe [Rule]      -- HAS foo :: List Hand \n bar :: Optional Restaurant
            , enums    :: Maybe ParamText   -- ONE OF rock, paper, scissors (basically, disjoint subtypes)
            , given    :: Maybe ParamText
            , upon     :: Maybe ParamText
            , rlabel   :: Maybe RuleLabel
            , lsource  :: Maybe Text.Text
            , srcref   :: Maybe SrcRef
            }
          | Scenario
            { scgiven  :: [RelationalPredicate]
            , expect   :: [HornClause]      -- investment is savings when dependents is 5
            , rlabel   :: Maybe RuleLabel
            , lsource  :: Maybe Text.Text
            , srcref   :: Maybe SrcRef
            }
          | DefNameAlias -- inline alias, like     some thing AKA Thing
            { name   :: RuleName  -- "Thing" -- the thing usually said as ("Thing")
            , detail :: RuleName  -- ["some", "thing"]
            , nlhint :: Maybe Text.Text   -- "lang=en number=singular"
            , srcref :: Maybe SrcRef
            }
          | RuleAlias Text.Text -- internal softlink to a rule label (rlabel), e.g. HENCE NextStep
          | RuleGroup { rlabel :: Maybe RuleLabel }  -- § NextStep
          | RegFulfilled  -- trivial top
          | RegBreach     -- trivial bottom
          -- | CaseStm       -- work in progress
          -- { name   :: RuleName
          -- , limbs  :: [(Maybe BoolStructP -- cond
          --              ,ParamText         -- result
          --              )]
          -- , eqtest :: Maybe ParamText
          -- }
          | NotARule [MyToken]
          deriving (Eq, Show, Generic, ToJSON)

data HornClause2 = HC2
  { hHead :: RelationalPredicate
  , hBody :: Maybe BoolStructR
  }
  deriving (Eq, Show, Generic, ToJSON)
  
data IsPredicate = IP ParamText ParamText
  deriving (Eq, Show, Generic, ToJSON)

-- Prologgy stuff
data HornClause = HC
  { relPred :: RelationalPredicate
  , relWhen :: Maybe HornBody
  }
  deriving (Eq, Show, Generic, ToJSON)

type HornRP = AA.Item RelationalPredicate

data HornBody = HBRP HornRP
              | HBITE { hbif   :: HornRP
                      , hbthen :: HornRP
                      , hbelse :: HornRP } 
  deriving (Eq, Show, Generic, ToJSON)

data RelationalPredicate = RPParamText ParamText
                         | RPConstraint MultiTerm RPRel MultiTerm
                         | RPBoolStructR MultiTerm RPRel BoolStructR
  deriving (Eq, Show, Generic, ToJSON)

rel2txt :: RPRel -> Text.Text
rel2txt RPis      = "relIs"
rel2txt RPeq      = "relEq"
rel2txt RPlt      = "relLT"
rel2txt RPlte     = "relLTE"
rel2txt RPgt      = "relGT"
rel2txt RPgte     = "relGTE"
rel2txt RPelem    = "relIn"
rel2txt RPnotElem = "relNotIn"

rp2texts :: RelationalPredicate -> MultiTerm
rp2texts (RPParamText    pt)            = pt2multiterm pt
rp2texts (RPConstraint   mt1 rel mt2)   = mt1 ++ [rel2txt rel] ++ mt2
rp2texts (RPBoolStructR  mt1 rel bsr)   = mt1 ++ [rel2txt rel] ++ [bsr2text bsr]

rp2text :: RelationalPredicate -> Text.Text
rp2text = Text.unwords . rp2texts

text2rp :: Text.Text -> RelationalPredicate
text2rp = RPParamText . text2pt

pt2multiterm :: ParamText -> MultiTerm
pt2multiterm pt = toList $ Text.unwords . toList <$> untypePT pt

-- head here is super fragile, will runtime crash
rpFirstWord :: RelationalPredicate -> Text.Text
rpFirstWord = head . rp2texts

data RPRel = RPis | RPeq | RPlt | RPlte | RPgt | RPgte | RPelem | RPnotElem
  deriving (Eq, Show, Generic, ToJSON)

newtype RelName = RN { getName :: RuleName }

noLabel :: Maybe (Text.Text, Int, Text.Text)
noLabel   = Nothing
noLSource :: Maybe Text.Text
noLSource = Nothing
noSrcRef :: Maybe SrcRef
noSrcRef  = Nothing
noDeem   :: Maybe ParamText
noDeem = Nothing

data ParamType = TOne | TOptional | TList0 | TList1
  deriving (Eq, Show, Generic, ToJSON)

-- everything is stringly typed at the moment but as this code matures these will become more specialized.
data TComparison = TBefore | TAfter | TBy | TOn | TVague
                          deriving (Eq, Show, Generic, ToJSON)

data TemporalConstraint a = TemporalConstraint TComparison Integer a
                          deriving (Eq, Show, Generic, ToJSON)
type RuleName   = MultiTerm
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

multiterm2pt :: MultiTerm -> ParamText
multiterm2pt x = pure (fromList x, Nothing)

multiterm2bsr :: Rule -> BoolStructR
multiterm2bsr = AA.Leaf . RPParamText . multiterm2pt . name

type MultiTerm = [Text.Text]
type TypedMulti = (NonEmpty Text.Text, Maybe TypeSig)
type ParamText = NonEmpty TypedMulti -- but consider the Tree alternative above

text2pt :: a -> NonEmpty (NonEmpty a, Maybe TypeSig)
text2pt x = pure (pure x, Nothing)

pt2text :: ParamText -> Text.Text
pt2text x = Text.unwords $ concatMap (toList . fst) $ toList x

bsp2text :: BoolStructP -> Text.Text
bsp2text (AA.Not                    x ) = Text.unwords ["not", bsp2text x]
bsp2text (AA.Leaf                   x ) = Text.unwords $ concatMap toList $ fst <$> x
bsp2text (AA.Any (Just (AA.Pre p1       )) xs) = Text.unwords $ p1 : (bsp2text <$> xs)
bsp2text (AA.Any (Just (AA.PrePost p1 p2)) xs) = Text.unwords $ p1 : (bsp2text <$> xs) <> [p2]
bsp2text (AA.Any Nothing                   xs) = "any of:-" <> Text.unwords (bsp2text <$> xs)
bsp2text (AA.All (Just (AA.Pre p1       )) xs) = Text.unwords $ p1 : (bsp2text <$> xs)
bsp2text (AA.All (Just (AA.PrePost p1 p2)) xs) = Text.unwords $ p1 : (bsp2text <$> xs) <> [p2]
bsp2text (AA.All Nothing                   xs) = "all of:-" <> Text.unwords (bsp2text <$> xs)

bsr2text :: BoolStructR -> Text.Text
bsr2text (AA.Not                    x ) = Text.unwords ["not", bsr2text x]
bsr2text (AA.Leaf                   x ) = rp2text x
bsr2text (AA.Any (Just (AA.Pre p1       )) xs) = Text.unwords $ p1 : (bsr2text <$> xs)
bsr2text (AA.Any (Just (AA.PrePost p1 p2)) xs) = Text.unwords $ p1 : (bsr2text <$> xs) <> [p2]
bsr2text (AA.Any Nothing                   xs) = "any of:-" <> Text.unwords (bsr2text <$> xs)
bsr2text (AA.All (Just (AA.Pre p1       )) xs) = Text.unwords $ p1 : (bsr2text <$> xs)
bsr2text (AA.All (Just (AA.PrePost p1 p2)) xs) = Text.unwords $ p1 : (bsr2text <$> xs) <> [p2]
bsr2text (AA.All Nothing                   xs) = "all of:-" <> Text.unwords (bsr2text <$> xs)

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


mkTComp :: MyToken -> Maybe TComparison
mkTComp Before     = Just TBefore 
mkTComp After      = Just TAfter  
mkTComp By         = Just TBy     
mkTComp On         = Just TOn     
mkTComp Eventually = Nothing
mkTComp x          = error $ "mkTC: can't create temporal constraint from " ++ show x ++ " -- this should be handled by a Vaguely"

mkTC :: MyToken -> Integer -> Text.Text -> Maybe (TemporalConstraint Text.Text)
mkTC tok   tt unit = TemporalConstraint <$> mkTComp tok <*> pure tt <*> pure unit
-- TODO: Consider supporting non-integer time constraints

data RunConfig = RC { debug     :: Bool
                    , callDepth :: Int
                    , parseCallStack :: [String]
                    , sourceURL :: Text.Text
                    , asJSON    :: Bool
                    , toNLG     :: Bool
                    , toBabyL4  :: Bool
                    , toProlog  :: Bool
                    , toUppaal  :: Bool
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
toToken "ALL"   =  TokAll -- when parties are treated as a collective, e.g. ALL diners. TokAll means "Token All"

-- start a boolstruct
toToken "ALWAYS" = Always
toToken "NEVER"  = Never
toToken "WHO" =    Who
toToken "WHICH" =  Who
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
toToken "BEFORE" = Before  -- <
toToken "WITHIN" = Before  -- <=
toToken "AFTER"  = After   -- >
toToken "BY"     = By
toToken "ON"     = On      -- ==
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
toToken "IS A"   = TypeSeparator
toToken "IS AN"  = TypeSeparator
toToken "A"      = A_An
toToken "AN"     = A_An

toToken "DEFINE"    = Define
toToken "DECIDE"    = Decide
toToken "ONE OF"    = OneOf
toToken "AS ONE OF" = OneOf
toToken "DEEM"      = Deem
toToken "HAS"       = Has

toToken "ONE"       = One
toToken "OPTIONAL"  = Optional
toToken "LIST0"     = List0
toToken "LIST1"     = List1

toToken "AKA"       = Aka

toToken "-§"        = RuleMarker (-1) "§"
toToken "SECTION"   = RuleMarker   1  "§"
toToken "§"         = RuleMarker   1  "§"
toToken "§§"        = RuleMarker   2  "§"
toToken "§§§"       = RuleMarker   3  "§"
toToken "§§§§"      = RuleMarker   4  "§"
toToken "§§§§§"     = RuleMarker   5  "§"
toToken "§§§§§§"    = RuleMarker   6  "§"

toToken "EXPECT"    = Expect
toToken "<"         = TokLT
toToken "=<"        = TokLTE
toToken "<="        = TokLTE
toToken ">"         = TokGT
toToken ">="        = TokGTE
toToken "="         = TokEQ
toToken "=="        = TokEQ
toToken "IN"        = TokIn
toToken "NOT IN"    = TokNotIn

toToken "OTHERWISE" = Otherwise

-- we recognize numbers
-- let's not recognize numbers yet; treat them as strings to be pOtherVal'ed.
toToken s | [(n,"")] <- reads $ Text.unpack s = TNumber n

-- any other value becomes an Other -- "walks", "runs", "eats", "drinks"
toToken x = Other x

pToken :: MyToken -> Parser MyToken
pToken c = checkDepth >> pTokenMatch (== c) c

pTokenAnyDepth :: MyToken -> Parser MyToken
pTokenAnyDepth c = pTokenMatch (== c) c

-- | check that the next token is at at least the current level of indentation
checkDepth :: Parser ()
checkDepth = do
  depth <- asks callDepth
  leftX <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.
  if leftX <  depth
    then myTraceM $ "checkDepth: current location " ++ show leftX ++ " is left of minimum depth " ++ show depth ++ "; considering parse fail"
    else myTraceM $ "checkDepth: current location " ++ show leftX ++ " is right of minimum depth " ++ show depth ++ "; guard succeeds"
  guard $ leftX >= depth

myTraceM :: String -> Parser ()
myTraceM x = whenDebug $ do
  nestDepth <- asks nestLevel
  traceM $ indentShow nestDepth <> x
  where
    indentShow depth = concat $ replicate depth "| "

whenDebug :: Parser () -> Parser ()
whenDebug act = do
  isDebug <- asks debug
  when isDebug act


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

