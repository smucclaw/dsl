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
import qualified Data.Tree as Tree

import LS.BasicTypes
import Control.Monad.Writer.Lazy (WriterT (runWriterT))
import Data.Monoid (Endo (Endo))
import Data.Bifunctor (second)

type PlainParser = ReaderT RunConfig (Parsec Void MyStream)
-- A parser generates a list of rules (in the "appendix", representing nested rules defined inline) and optionally some other value
type Parser = WriterT (DList Rule) PlainParser
type Depth = Int
type Preamble = MyToken
type BoolStruct  = AA.Item Text.Text
type BoolStructP = AA.Item ParamText
type BoolStructR = AA.Item RelationalPredicate

type MultiTerm = [Text.Text]                          --- | apple | orange | banana
type KVsPair = (NonEmpty Text.Text, Maybe TypeSig)    --- so really there are multiple Values
type TypedMulti = KVsPair                             --- | apple | orange | banana | :: | Fruit   |
type ParamText = NonEmpty TypedMulti                  --- | notify | the government |    |         |
                                                      --- |        | immediately    | :: | Urgency |

text2pt :: Text.Text -> ParamText
text2pt x = pure (pure x, Nothing)

pt2text :: ParamText -> Text.Text
pt2text x = Text.unwords $ concatMap (toList . fst) $ toList x

type PTree = Tree.Tree TypedMulti -- Node (["notify" :| "the government"], Nothing) [ Node (["immediately" :| [], Urgency) [] ]

mkPTree :: TypedMulti -> [PTree] -> PTree
mkPTree = Tree.Node

mkLeaf :: Text.Text -> AA.Item ParamText
mkLeaf = AA.Leaf . text2pt

mkLeafR :: Text.Text -> BoolStructR
mkLeafR x = AA.Leaf $ RPMT [x]

-- remove the TypeSig from a ParamText
untypePT :: ParamText -> NonEmpty (NonEmpty Text.Text)
untypePT = fmap fst

tm2mt :: TypedMulti -> MultiTerm
tm2mt = toList . fst

mt2tm :: MultiTerm -> TypedMulti
mt2tm x = (fromList x, Nothing)

mt2pt :: MultiTerm -> ParamText
mt2pt ts = pure (fromList ts, Nothing)

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
                         , rbwhere    :: [Rule]      -- Hornlike rules only, please       -- WHERE sky IS blue WHEN day IS thursday -- basically an inlineconstitutiverule but shoehorned into a hornlike until we get such rules working again
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
            , who      :: Maybe BoolStructR         -- WHO walks and (eats or drinks)
            , cond     :: Maybe BoolStructR         -- IF it is a saturday
            , deontic  :: Deontic            -- must
            , action   :: BoolStructP               -- fart loudly AND run away
            , temporal :: Maybe (TemporalConstraint Text.Text) -- Before "midnight"
            , hence    :: Maybe Rule
            , lest     :: Maybe Rule
            , rlabel   :: Maybe RuleLabel
            , lsource  :: Maybe Text.Text
            , srcref   :: Maybe SrcRef
            , upon     :: Maybe ParamText
            , given    :: Maybe ParamText
            , having   :: Maybe ParamText  -- HAVING sung...
            , wwhere   :: [Rule]
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
            { name     :: RuleName           -- colour
            , keyword  :: MyToken            -- decide / define / means
            , given    :: Maybe ParamText    -- applicant has submitted fee
            , upon     :: Maybe ParamText    -- second request occurs
            , clauses  :: [HornClause2]      -- colour IS blue WHEN fee > $10 ; colour IS green WHEN fee > $20 AND approver IS happy
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
            , expect   :: [HornClause2]      -- investment is savings when dependents is 5
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
          | RuleAlias RuleName -- internal softlink to a rule label (rlabel), e.g. HENCE NextStep
          | RuleGroup { rlabel :: Maybe RuleLabel
                      , srcref :: Maybe SrcRef }  -- § NextStep
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

data RelationalPredicate = RPParamText   ParamText                     -- cloudless blue sky
                         | RPMT MultiTerm -- intended to replace RPParamText. consider TypedMulti?
                         | RPConstraint  MultiTerm RPRel MultiTerm     -- eyes IS blue
                         | RPBoolStructR MultiTerm RPRel BoolStructR   -- eyes IS (left IS blue
                                                                       --          AND
                                                                       --          right IS brown)
  deriving (Eq, Show, Generic, ToJSON)
                 -- RPBoolStructR (["eyes"] RPis (AA.Leaf (RPParamText ("blue" :| [], Nothing))))
                 -- would need to reduce to
                 -- RPConstraint ["eyes"] Rpis ["blue"]

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
rp2texts (RPMT           mt)            = mt
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

data NatLang = NLen

tc2nl :: NatLang -> Maybe (TemporalConstraint Text.Text) -> Text.Text
tc2nl NLen Nothing = "eventually"
tc2nl NLen (Just (TemporalConstraint TBefore n t)) = Text.unwords [ "before", Text.pack (show n), t ]
tc2nl NLen (Just (TemporalConstraint TBy     n t)) = Text.unwords [ "by",     Text.pack (show n), t ]
tc2nl NLen (Just (TemporalConstraint TAfter  n t)) = Text.unwords [ "after",  Text.pack (show n), t ]
tc2nl NLen (Just (TemporalConstraint TOn     n t)) = Text.unwords [ "on",     Text.pack (show n), t ]
tc2nl NLen (Just (TemporalConstraint TVague  n t)) = Text.unwords [ "around", Text.pack (show n), t ]


data RunConfig = RC { debug     :: Bool
                    , callDepth :: Int
                    , oldDepth  :: Int
                    , parseCallStack :: [String]
                    , sourceURL :: Text.Text
                    , asJSON    :: Bool
                    , toNLG     :: Bool
                    , toBabyL4  :: Bool
                    , toProlog  :: Bool
                    , toUppaal  :: Bool
                    , saveAKA   :: Bool
                    , wantNotRules :: Bool
                    }

nestLevel :: RunConfig -> Int
nestLevel = length . parseCallStack

increaseNestLevel :: String -> RunConfig -> RunConfig
increaseNestLevel name rc = rc { parseCallStack = name : parseCallStack rc }

magicKeywords :: [Text.Text]
magicKeywords = Text.words "EVERY PARTY MUST MAY WHEN INCLUDES MEANS IS IF UNLESS DEFINE"

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
toToken "HAVING" = pure Having

toToken "MEANS" =  pure Means -- "infix"-starts a constitutive rule "Name MEANS x OR y OR z"
toToken "INCLUDES" =  pure Includes
toToken "IS" =     pure Is

-- boolean connectors
toToken "OR" =     pure Or
toToken "AND" =    pure And
toToken "UNLESS" = pure Unless
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
toToken "BEFORE" = pure Before  -- <
toToken "WITHIN" = pure Before  -- <=
toToken "AFTER"  = pure After   -- >
toToken "BY"     = pure By
toToken "ON"     = pure On      -- ==
toToken "EVENTUALLY" = pure Eventually

-- the rest of the regulative rule
toToken "➔"       =     pure Do
toToken "->"      =     pure Do
toToken "DO"      =     pure Do
toToken "PERFORM" =     pure Do

-- for discarding
toToken "" =       pure Empty
toToken "TRUE" =   pure Checkbox
toToken "FALSE" =  pure Checkbox
toToken "HOLDS" =  pure Holds

-- regulative chains
toToken "HENCE" = pure Hence
toToken  "THEN" = pure Hence
-- trivial contracts
toToken  "FULFILLED" = pure Fulfilled
toToken  "BREACH" = pure Breach

toToken     "LEST" = pure Lest
toToken     "ELSE" = pure Lest
toToken  "OR ELSE" = pure Lest
toToken "XOR ELSE" = pure Lest
toToken    "XELSE" = pure Lest
toToken  "GOTO" = pure Goto

toToken ";"      = pure EOL

toToken ":"      = [TypeSeparator, A_An]
toToken "::"     = [TypeSeparator, A_An]
toToken "TYPE"   = [TypeSeparator, A_An]
toToken "IS A"   = [TypeSeparator, A_An]
toToken "IS AN"  = [TypeSeparator, A_An]
toToken "A"      = pure A_An
toToken "AN"     = pure A_An

toToken "DEFINE"    = pure Define
toToken "DECIDE"    = pure Decide
toToken "ONE OF"    = pure OneOf
toToken "AS ONE OF" = pure OneOf
toToken "DEEM"      = pure Deem
toToken "HAS"       = pure Has

toToken "ONE"       = pure One
toToken "OPTIONAL"  = pure Optional
toToken "LIST0"     = pure List0
toToken "LIST1"     = pure List1

toToken "AKA"       = pure Aka

toToken "-§"        = pure $ RuleMarker (-1) "§"
toToken "SECTION"   = pure $ RuleMarker   1  "§"
toToken "§"         = pure $ RuleMarker   1  "§"
toToken "§§"        = pure $ RuleMarker   2  "§"
toToken "§§§"       = pure $ RuleMarker   3  "§"
toToken "§§§§"      = pure $ RuleMarker   4  "§"
toToken "§§§§§"     = pure $ RuleMarker   5  "§"
toToken "§§§§§§"    = pure $ RuleMarker   6  "§"

toToken "EXPECT"    = pure Expect
toToken "<"         = pure TokLT
toToken "=<"        = pure TokLTE
toToken "<="        = pure TokLTE
toToken ">"         = pure TokGT
toToken ">="        = pure TokGTE
toToken "="         = pure TokEQ
toToken "=="        = pure TokEQ
toToken "==="       = pure TokEQ
toToken "IN"        = pure TokIn
toToken "NOT IN"    = pure TokNotIn

toToken "OTHERWISE" = pure Otherwise

toToken "WHERE"     = pure Where

-- we recognize numbers
-- let's not recognize numbers yet; treat them as strings to be pOtherVal'ed.
toToken s | [(n,"")] <- reads $ Text.unpack s = pure $ TNumber n

-- any other value becomes an Other -- "walks", "runs", "eats", "drinks"
toToken x = pure $ Other x


whenDebug :: Parser () -> Parser ()
whenDebug act = do
  isDebug <- asks debug
  when isDebug act


pXLocation :: Parser Depth
pXLocation = token test Set.empty <|> pure 0 <?> "x location"
  where
    test (WithPos (SourcePos _ _y x) _ _ _) = Just (unPos x)

pYLocation :: Parser Depth
pYLocation = token test Set.empty <|> pure 0 <?> "y location"
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

