{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Types used by the Legal Spreadsheets parser, interpreter, and transpilers.
-}

module LS.Types ( module LS.BasicTypes
                , module LS.Types) where

import qualified Data.Text as Text
import Text.Megaparsec
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty ((:|)), toList, fromList)
import Data.Void (Void)
import qualified Data.Set           as Set
import Control.Monad
import qualified AnyAll as AA
import Control.Monad.Reader (ReaderT (runReaderT), asks)
import Data.Aeson (ToJSON)
import GHC.Generics
import qualified Data.Tree as Tree
import qualified Data.Map as Map

import LS.BasicTypes
import Control.Monad.Writer.Lazy (WriterT (runWriterT))
import Data.Monoid (Endo (Endo))
import Data.Bifunctor (second)

type PlainParser = ReaderT RunConfig (Parsec Void MyStream)
-- A parser generates a list of rules (in the "appendix", representing nested rules defined inline) and optionally some other value
type Parser = WriterT (DList Rule) PlainParser
type Depth = Int
type Preamble = MyToken

type KVsPair = (NonEmpty Text.Text, Maybe TypeSig)    --- so really there are multiple Values
type TypedMulti = KVsPair                             --- | apple | orange | banana | :: | Fruit   |

-- * BoolStructs wrap Phrasal types

type BoolStructT  = AA.OptionallyLabeledBoolStruct Text.Text
type BoolStructP = AA.OptionallyLabeledBoolStruct ParamText
type BoolStructR = AA.OptionallyLabeledBoolStruct RelationalPredicate


type MultiTerm = [Text.Text]                          --- | apple | orange | banana

-- $phrasetypes

-- | @ParamText@ contains /parameterized text/.
--
-- Suppose we want to say, "pay the Vendor the amount of $10, by bank transfer".
--
-- Conceptually, that has the structure of a function call with both positional and named arguments.
-- We put the named arguments in an attribute dictionary object:
--
-- > pay( the Vendor, { amount = $10,
-- >                    by     = bank transfer } )
--
-- In a table, we would say:
--
-- > | ... | pay | the Vendor |               |    |   |               |
-- > |     |     | amount     | $10           | IS | A | Consideration |
-- > |     |     | by         | bank transfer |    |   |               |
--
-- Parsed into a ParamText, we have a 'NonEmpty' list of 'TypedMulti', which are themselves a `NonEmpty Text` tupled with a `Maybe TypeSig`.
--
-- > action = (      "pay"    :| ["the Vendor"]       , Nothing )
-- >          :| [ ( "amount" :| ["$10"] )            , SimpleType TOne "Consideration" )
-- >             , ( "by"     :| ["bank transfer"] )  , Nothing )
-- >             ] )
--
-- Linguistically, the first word ("pay") must always be a verb. If it is a transitive verb, the direct object follows on the same line. If it is an intransitive verb, the rest of the line can be blank. On subsequent lines, we give optional attributes: a "prepositional" keyword, followed by one or more optional parameters to that keyword.
--
-- 'ParamText' is primarily used in `Rule.action` keywords, which take a 'BoolStructP', whose base type is @ParamText@. That means that multiple ParamTexts can be connected together using @AND@ and @OR@ keywords.
--
-- In the simplest case, a ParamText could be simply a single intransitive verb, e.g. "walk", with no further arguments.
--
-- > action = ( "walk" :| [] , Nothing )
--

type ParamText = NonEmpty TypedMulti               --- | notify | the government |    |         |
                                                   --- |        | immediately    | :: | Urgency |
-- see PrettyPrinter for newtypes based on ParamText
text2pt :: Text.Text -> ParamText
text2pt x = pure (pure x, Nothing)

pt2text :: ParamText -> Text.Text
pt2text x = Text.unwords $ concatMap (toList . fst) $ toList x

type PTree = Tree.Tree TypedMulti -- Node (["notify" :| "the government"], Nothing) [ Node (["immediately" :| [], Urgency) [] ]

mkPTree :: TypedMulti -> [PTree] -> PTree
mkPTree = Tree.Node

mkLeafPT :: Text.Text -> BoolStructP
mkLeafPT = AA.Leaf . text2pt

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

mt2text :: MultiTerm -> Text.Text
mt2text = Text.unwords

-- | Like [a] but with faster concatenation.
newtype DList a = DList (Endo [a])
  deriving newtype (Semigroup, Monoid)

instance Show a => Show (DList a) where
  show = show . dlToList

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
                         , rbupon     :: [(Preamble, ParamText)] -- Upon  event conditions -- [TODO], figure out how these are joined; or should we ban multiple UPONs?
                         , rbgiven    :: [(Preamble, ParamText)] -- Given
                         , rbhaving   :: Maybe ParamText
                         , rbkeyname  :: (RegKeywords, BoolStructP)   -- Every man AND woman
                         , rbwho      :: Maybe (Preamble, BoolStructR)   -- WHO seeks eternal life in me
                         , rbwhere    :: [Rule]      -- Hornlike rules only, please       -- WHERE sky IS blue WHEN day IS thursday -- basically an inlineconstitutiverule but shoehorned into a hornlike until we get such rules working again
                         }
                      deriving (Eq, Ord, Show, Generic)

-- | find some unique name for the rule for purposes of scoping the symbol table.
-- if a rule label is provided, we use that.
-- if it's not provided, we use the name.
-- NOTE: we currently do not detect name collisions. In a future, more sophisticated version of this code, we would track the path to the rule.

ruleLabelName :: Rule -> RuleName
ruleLabelName r = maybe (ruleName r) (\x-> [rl2text x]) (getRlabel r)

getRlabel :: Rule -> Maybe RuleLabel
getRlabel r@Regulative{}    = rlabel r
getRlabel r@Constitutive {} = rlabel r
getRlabel r@Hornlike {}     = rlabel r
getRlabel r@TypeDecl {}     = rlabel r
getRlabel r@Scenario {}     = rlabel r
getRlabel r@RuleGroup {}    = rlabel r
-- getRlabel r@DefNameAlias {} = Nothing
-- getRlabel r@DefTypically {} = Nothing
-- getRlabel r@(RuleAlias a)   = Nothing
-- getRlabel r@RegFulfilled    = Nothing
-- getRlabel r@RegBreach       = Nothing
getRlabel _                 = Nothing

ruleName :: Rule -> RuleName
ruleName Regulative { subj  = x } = [bsp2text x]
ruleName (RuleAlias rn) = rn
ruleName RegFulfilled = ["FULFILLED"]
ruleName RegBreach    = ["BREACH"]
ruleName x = name x

type RuleLabel = (Text.Text   --  "§"
                 ,Int         --   1
                 ,Text.Text   --  "My First Rule"
                 )

rl2text :: RuleLabel -> Text.Text
rl2text (_sectionSymbol, _numSymbols, ruleText) = ruleText

-- sometimes we just want to convert either the rulelabel or the rulename to text
rlrn2text :: Rule -> Text.Text
rlrn2text r = Text.unwords $ ruleLabelName r

-- maybe we should have a proper dict orientation here
data KW a = KW { dictK :: MyToken
               , dictV :: a }

data RegKeywords =
  REvery | RParty | RTokAll
  deriving (Eq, Ord, Show, Generic, ToJSON)

class HasToken a where
  tokenOf :: a -> MyToken

instance HasToken RegKeywords where
  tokenOf REvery = Every
  tokenOf RParty = Party
  tokenOf RTokAll = TokAll

-- [TODO]: we need to start preserving the keywords for each preamble*, because maybe this is a "which" not a "who"
data Rule = Regulative
            { subj     :: BoolStructP               -- man AND woman AND child
            , rkeyword :: RegKeywords               -- Every | Party | TokAll
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
--            , given    :: [ParamText]      -- input parameters basically, or the type thereof
            , given    :: Maybe ParamText
            , having   :: Maybe ParamText  -- HAVING sung...
            , wwhere   :: [Rule]
            , defaults :: [RelationalPredicate] -- SomeConstant IS 500 ; MentalCapacity TYPICALLY True
            , symtab   :: [RelationalPredicate] -- SomeConstant IS 500 ; MentalCapacity TYPICALLY True
            }
          | Constitutive
            { name     :: RuleName   -- the thing we are defining
            , keyword  :: MyToken       -- Means, Includes, Is, Deem, Decide
            , letbind  :: BoolStructR
            , cond     :: Maybe BoolStructR -- a boolstruct set of conditions representing When/If/Unless
--            , given    :: [ParamText]
            , given    :: Maybe ParamText
        --  , having   :: Maybe ParamText    -- event trace history predicate: applicant has submitted fee
            , rlabel   :: Maybe RuleLabel
            , lsource  :: Maybe Text.Text
-- [TODO]   , wwhere   :: [Rule]
            , srcref   :: Maybe SrcRef
            , defaults :: [RelationalPredicate] -- SomeConstant IS 500 ; MentalCapacity TYPICALLY True
            , symtab   :: [RelationalPredicate] -- SomeConstant IS 500 ; MentalCapacity TYPICALLY True
            }
          | Hornlike
            { name     :: RuleName           -- MyInstance
            , super    :: Maybe TypeSig         -- IS A Superclass
            , keyword  :: MyToken            -- decide / define / means
            , given    :: Maybe ParamText    -- a:Applicant, p:Person, l:Lender -- the signature of the input
        --  , having   :: Maybe ParamText    -- event trace history predicate: applicant has submitted fee
            , upon     :: Maybe ParamText    -- second request occurs
            , clauses  :: [HornClause2]      -- colour IS blue WHEN fee > $10 ; colour IS green WHEN fee > $20 AND approver IS happy
            , rlabel   :: Maybe RuleLabel
            , lsource  :: Maybe Text.Text
-- [TODO]   , wwhere   :: [Rule]
            , srcref   :: Maybe SrcRef
            , defaults :: [RelationalPredicate] -- SomeConstant IS 500 ; MentalCapacity TYPICALLY True
            , symtab   :: [RelationalPredicate] -- SomeConstant IS 500 ; MentalCapacity TYPICALLY True
            }
          | TypeDecl
            { name     :: RuleName              -- DECLARE Class
            , super    :: Maybe TypeSig         -- IS A Superclass
            , has      :: [Rule]      -- HAS foo :: List Hand \n bar :: Optional Restaurant
            , enums    :: Maybe ParamText   -- ONE OF rock, paper, scissors (basically, disjoint subtypes)
--            , given    :: [ParamText]
            , given    :: Maybe ParamText
            , upon     :: Maybe ParamText
            , rlabel   :: Maybe RuleLabel
            , lsource  :: Maybe Text.Text
-- [TODO]   , wwhere   :: [Rule]
            , srcref   :: Maybe SrcRef
            , defaults :: [RelationalPredicate] -- SomeConstant IS 500 ; MentalCapacity TYPICALLY True
            , symtab   :: [RelationalPredicate] -- SomeConstant IS 500 ; MentalCapacity TYPICALLY True
            }
          | Scenario
            { scgiven  :: [RelationalPredicate]
            , expect   :: [Expect]              -- ExpRP (RPConstraint ["investment"] RPis ["savings"])
--          , upon ?.... we want to say things like WHEN an event happens / THEN some state transition occurs
--          , redrule  :: [Rule]                -- a test could return a reduction of existing rules
            , rlabel   :: Maybe RuleLabel
            , lsource  :: Maybe Text.Text
-- [TODO]   , wwhere   :: [Rule]
            , srcref   :: Maybe SrcRef
            , defaults :: [RelationalPredicate] -- SomeConstant IS 500 ; MentalCapacity TYPICALLY True
            , symtab   :: [RelationalPredicate] -- SomeConstant IS 500 ; MentalCapacity TYPICALLY True
            }
          | DefNameAlias -- inline alias, like     some thing AKA Thing
            { name   :: RuleName  -- "Thing" -- the thing usually said as ("Thing")
            , detail :: RuleName  -- ["some", "thing"]
            , nlhint :: Maybe Text.Text   -- "lang=en number=singular"
            , srcref :: Maybe SrcRef
            }
          | DefTypically -- inline default assignment, like     some hemisphere TYPICALLY North
            { name   :: RuleName  -- the name of the enclosing rule scope context -- a bit tricky to retrieve so typically just the termhead for now. [FIXME]
            , defaults :: [RelationalPredicate] -- usually an RPParamText or RPMT. higher order not quite explored yet.
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
          deriving (Eq, Ord, Show, Generic, ToJSON)

defaultReg, defaultCon, defaultHorn :: Rule
defaultReg = Regulative
  { subj = mkLeafPT "person"
  , rkeyword = REvery
  , who = Nothing
  , cond = Nothing
  , deontic = DMust
  , action = mkLeafPT "sing"
  , temporal = Nothing
  , hence = Nothing
  , lest = Nothing
  , rlabel = Nothing
  , lsource = Nothing
  , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing})
  , upon = Nothing
  , given = Nothing
  , having = Nothing
  , wwhere = []
  , defaults = []
  , symtab   = []
  }

defaultCon = Constitutive
  { name = []
  , keyword = Means
  , letbind = mkLeafR "Undefined"
  , cond = Nothing
  , rlabel = Nothing
  , lsource = Nothing
  , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing})
  , given = Nothing
  , defaults = []
  , symtab   = []
  }

defaultHorn = Hornlike
  { name = []
  , super = Nothing
  , keyword = Means
  , given = Nothing
  , upon  = Nothing
  , clauses = []
  , rlabel = Nothing
  , lsource = Nothing
  , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing})
  , defaults = []
  , symtab   = []
  }

-- | does a rule have a Given attribute?
hasGiven :: Rule -> Bool
hasGiven     Hornlike{} = True
hasGiven   Regulative{} = True
hasGiven     TypeDecl{} = True
hasGiven Constitutive{} = True
hasGiven             __ = False

-- | does a rule have Clauses?
-- [TODO] it's beginning to look like we need to break out the Rule Types into different types not just constructors
hasClauses :: Rule -> Bool
hasClauses     Hornlike{} = True
hasClauses             __ = False

getDecisionHeads :: Rule -> [MultiTerm]
getDecisionHeads Hornlike{..} = [ rpHead hhead
                                | HC2 hhead _hbody <- clauses ]
getDecisionHeads _ = []

data Expect = ExpRP      RelationalPredicate
            | ExpDeontic Rule -- regulative rule
            deriving (Eq, Ord, Show, Generic, ToJSON)

data HornClause2 = HC2
  { hHead :: RelationalPredicate
  , hBody :: Maybe BoolStructR
  }
  deriving (Eq, Ord, Show, Generic, ToJSON)

data IsPredicate = IP ParamText ParamText
  deriving (Eq, Ord, Show, Generic, ToJSON)

class PrependHead a where
  -- Used to prepend what was first interpreted to be a label to an item
  prependHead :: Text.Text -> a -> a

instance PrependHead Text.Text where
  prependHead s = ((s <> " ") <>)
instance PrependHead ParamText where
  prependHead s ((xs, ts) :| xss) = (pure s <> xs, ts) :| xss

instance PrependHead RelationalPredicate where
  prependHead s (RPParamText ne)        = RPParamText (prependHead s ne)
  prependHead s (RPMT txts)             = RPMT (s : txts)
  prependHead s (RPConstraint l rr r)   = RPConstraint (s : l) rr r
  prependHead s (RPBoolStructR l rr it) = RPBoolStructR (s : l) rr it
  prependHead s (RPnary rel rp)         = RPnary rel $ prependHead s rp

-- | the catch-all datatype used for decision elements, action specifications, and just strings of text wrapped as RP.
--
-- the simplest form is a MultiTerm wrapped in an RPMT:
-- `foo | bar baz` turns into `RPMT ["foo", "bar baz"]`
--
-- the next simplest form, a (one-or-more-line ParamText) wrapped in an RPPT, allows type annotation:
-- `foo | IS A | Potato` turns into `RPPT [ ("foo" :| [], SimpleType TOne "Potato") ]`
--
-- the next two allow you to actually express relations, which is why we call this a relational predicate!
--
-- something like `Bob | IS | your uncle` gets read into an `RPConstraint ["Bob"] RPis ["your uncle"]`
-- Other relations available inside the RPRel are the usual `<`, `>`, `<=`, `>=`, etc.
-- Currently every RPConstraint is a binary relation.
-- There is a strong argument that we should allow simple constraint relations of any arity.
-- [TODO] this we would rewrite the form of an `RPConstraint` to `RPConstraint RPRel [ MultiTerm ]`
--
-- The final form is a recursion: we have a Boolean Struct of RelationalPredicates, so we can do a full treelike thing:
-- `RPBoolStructR "Uncle" RPis (AA.Any Nothing [ AA.Leaf $ RPMT ["your", "mother's", "brother" ]
--                                             , AA.Leaf $ RPMT ["your", "father's", "brother" ]
--                                             , AA.Leaf $ RPMT ["some random old guy you want to call uncle" ]
--                                             ])`
-- So how do we say something like:
-- `EXPECT    NOT Sky IS Blue`
--
--
--
-- We would use a BoolStructR:
--
--    AA.Not (AA.Leaf (RPConstraint ["Sky"] RPis ["Blue"]))
--
-- In another universe we could recurse the RPConstraints and have an `RPConstraint (Not (RPConstraint (Is Sky Blue)))`
-- [TODO] Let's think about refactoring to that in future.

data RelationalPredicate = RPParamText   ParamText                     -- cloudless blue sky
                         | RPMT MultiTerm  -- intended to replace RPParamText. consider TypedMulti?
                         | RPConstraint  MultiTerm RPRel MultiTerm     -- eyes IS blue
                         | RPBoolStructR MultiTerm RPRel BoolStructR   -- eyes IS (left IS blue AND right IS brown)
                         | RPnary RPRel RelationalPredicate -- RPnary RPnot (RPnary RPis ["the sky", "blue"]
                        -- [TODO] consider adding a new approach, actually a very old Lispy approach

                     --  | RPDefault      in practice we use RPMT ["OTHERWISE"], but if we ever refactor, we would want an RPDefault
  deriving (Eq, Ord, Show, Generic, ToJSON)
                 -- RPBoolStructR (["eyes"] RPis (AA.Leaf (RPParamText ("blue" :| [], Nothing))))
                 -- would need to reduce to
                 -- RPConstraint ["eyes"] Rpis ["blue"]

rel2txt :: RPRel -> Text.Text
rel2txt RPis      = "Is"
rel2txt RPhas     = "relHas"
rel2txt RPeq      = "relEq"
rel2txt RPlt      = "relLT"
rel2txt RPlte     = "relLTE"
rel2txt RPgt      = "relGT"
rel2txt RPgte     = "relGTE"
rel2txt RPelem    = "relIn"
rel2txt RPnotElem = "relNotIn"
rel2txt RPnot     = "relNot"

rel2op :: RPRel -> Text.Text
rel2op RPis      = "IS"
rel2op RPhas     = ".?"
rel2op RPeq      = "=="
rel2op RPlt      = "<"
rel2op RPlte     = "<="
rel2op RPgt      = ">"
rel2op RPgte     = ">="
rel2op RPelem    = "IN"
rel2op RPnotElem = "NOT IN"
rel2op RPnot     = "NOT"

rp2texts :: RelationalPredicate -> MultiTerm
rp2texts (RPParamText    pt)            = pt2multiterm pt
rp2texts (RPMT           mt)            = mt
rp2texts (RPConstraint   mt1 rel mt2)   = mt1 ++ [rel2txt rel] ++ mt2
rp2texts (RPBoolStructR  mt1 rel bsr)   = mt1 ++ [rel2txt rel] ++ [bsr2text bsr]
rp2texts (RPnary         rel rp)        = rel2txt rel : rp2texts rp

-- | pull out all the body leaves of RelationalRredicates as multiterms
rp2bodytexts :: RelationalPredicate -> [MultiTerm]
rp2bodytexts (RPParamText    pt)            = [pt2multiterm pt]
rp2bodytexts (RPMT           mt)            = [mt]
rp2bodytexts (RPConstraint   mt1 rel mt2)   = [mt1, [rel2op rel], mt2]
rp2bodytexts (RPBoolStructR  mt1 rel bsr)   = [mt1 ++ rel2op rel : bod
                                              | bod <- concatMap rp2bodytexts (AA.extractLeaves bsr) ]

rp2text :: RelationalPredicate -> Text.Text
rp2text = Text.unwords . rp2texts

text2rp :: Text.Text -> RelationalPredicate
text2rp = RPParamText . text2pt

pt2multiterm :: ParamText -> MultiTerm
pt2multiterm pt = toList $ Text.unwords . toList <$> untypePT pt

rpFirstWord :: RelationalPredicate -> Text.Text
rpFirstWord rp =
  case rp2texts rp of
    []  -> ""
    x:_ -> x

-- the "key-like" part of a relationalpredicate, used for TYPICALLY value assignment
rpHead :: RelationalPredicate -> MultiTerm
rpHead (RPParamText    pt)            = pt2multiterm pt
rpHead (RPMT           mt)            = mt
rpHead (RPConstraint   mt1 _rel _mt2) = mt1
rpHead (RPBoolStructR  mt1 _rel _bsr) = mt1
rpHead (RPnary         rel rp)        = rel2op rel : rpHead rp

data RPRel = RPis | RPhas | RPeq | RPlt | RPlte | RPgt | RPgte | RPelem | RPnotElem | RPnot
  deriving (Eq, Ord, Show, Generic, ToJSON)

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
  deriving (Eq, Ord, Show, Generic, ToJSON)

-- everything is stringly typed at the moment but as this code matures these will become more specialized.
data TComparison = TBefore | TAfter | TBy | TOn | TVague
                          deriving (Eq, Ord, Show, Generic, ToJSON)

data TemporalConstraint a = TemporalConstraint TComparison (Maybe Integer) a
                          deriving (Eq, Ord, Show, Generic, ToJSON)
type RuleName   = MultiTerm
type EntityType = Text.Text

data TypeSig = SimpleType ParamType EntityType
             | InlineEnum ParamType ParamText
             deriving (Eq, Ord, Show, Generic, ToJSON)

-- for use by the interpreter

type VarPath = [TypedMulti]

data InterpreterOptions = IOpts
  { enums2decls :: Bool -- ^ convert inlineEnums in a class declaration to top-level decls? Used by corel4.
  }
  deriving (Eq, Ord, Show)

defaultInterpreterOptions :: InterpreterOptions
defaultInterpreterOptions = IOpts
  { enums2decls = False
  }

data Interpreted = L4I
  { classtable :: ClsTab
  , scopetable :: ScopeTabs
  , origrules  :: [Rule]
  }
  deriving (Eq, Ord, Show)

-- | a basic symbol table to track "variable names" and their associated types.

getUnderlyingType :: TypeSig -> Either String EntityType
getUnderlyingType   (SimpleType TOne      s1) = Right s1
getUnderlyingType   (SimpleType TOptional s1) = Right s1
getUnderlyingType   (SimpleType TList0    s1) = Right s1
getUnderlyingType   (SimpleType TList1    s1) = Right s1
getUnderlyingType   (InlineEnum _pt1      __) = Left "type declaration cannot inherit from _enum_ superclass"

-- what's the difference between SymTab, ClsTab, and ScopeTabs?

-- | ClsTab: things that are explicitly defined in a Type Declaration (DECLARE ... HAS ...) end up in the ClsTab
-- and they qualify to be used as types on the RHS of a :: definition which could appear anywhere.
newtype ClsTab = CT ClassHierarchyMap
  -- a class has attributes; those attributes live in a map keyed by classname.
  -- the fst part is the type of the class -- X IS A Y basically means X extends Y, but more complex types are possible, e.g. X :: LIST1 Y
  -- the snd part is the recursive HAS containing attributes of the class
  deriving (Show, Ord, Eq, Generic)

unCT :: ClsTab -> ClassHierarchyMap
unCT (CT x) = x

type TypedClass = (Inferrable TypeSig, ClsTab)

type ClassHierarchyMap = Map.Map EntityType TypedClass

-- | ScopeTabs: In the course of a program we will sometimes see ad-hoc variables used in GIVEN and elsewhere.
-- those end up in the ScopeTabs object returned by the `symbolTable` function.

-- We also see explicit variable definitions given by (DEFINE ... HAS ...). These also end up in ScopeTabs.
-- If such a definition appears under a WHERE limb of another rule, it is scoped to that rule.

-- If it is given at top level, then it is under ... global scope, which is represented by Rulename=[]
-- The keys to ScopeTabs are from ruleLabelName.

type ScopeTabs = Map.Map RuleName SymTab

--  | SymTabs are a helper data structure used by ScopeTabs.
-- the fst contains type-related information.
-- the snd contains value-related information.

-- this type is getting pretty hefty, soon it'll be time to give it a proper type definition.

type SymTab = Map.Map MultiTerm (Inferrable TypeSig, [HornClause2])

-- | The explicitly annotated types from the L4 source text are recorded in the fst of Inferrable.
--   The confirmed & inferred types after the type checker & inferrer has run, are recorded in the snd of Inferrable.
--   If type checking / inference have not been implemented the snd will be empty.
type Inferrable ts = (Maybe ts, [ts])

thisAttributes, extendedAttributes :: ClsTab -> EntityType -> Maybe ClsTab

-- | attributes defined in the type declaration for this class specifically
thisAttributes (CT clstab) subclass = do
  ((_mts, _tss), ct) <- Map.lookup subclass clstab
  return ct

extendedAttributes o@(CT clstab) subclass = do
  ((_mts, _tss), CT ct) <- Map.lookup subclass clstab
  let eAttrs = case (extendedAttributes o <$> clsParent o subclass) of
                 Nothing               -> Map.empty
                 (Just Nothing)        -> Map.empty
                 (Just (Just (CT ea))) -> ea
  return $ CT $ ct <> eAttrs

-- get out whatever type signature has been user defined or inferred.
getSymType :: Inferrable ts -> Maybe ts
getSymType (Just x, _)    = Just x
getSymType (Nothing, x:_) = Just x
getSymType (Nothing, [])  = Nothing

-- a subclass extends a superclass.
-- but if the type definition for the class is anything other than the simple TOne, it's actually a polymorphic newtype and not a superclass
clsParent :: ClsTab -> EntityType -> Maybe EntityType
clsParent (CT clstab) subclass = do
  ((mts, tss), _st) <- Map.lookup subclass clstab
  case getUnderlyingType <$> getSymType (mts, tss) of
    Just (Right s1) -> Just s1
    Just (Left _)   -> Nothing
    Nothing         -> Nothing

-- is this a NonEmpty (NonEmpty Text.Text)
-- or a Tree (Text.Text)
-- type ParamText' = Tree Text.Text -- function(arg0, arg1=[val2,val3], arg4=[val5,val6])
--                                   -- Node "action" [ Node "eat"   [ Node "ice cream" [] ]
--                                   --               , Node "arg1"  [ Node "val2" [], Node "val3" [] ]
--                                   --               , Node "arg4"  [ Node "val5" [], Node "val6" [] ] ]

multiterm2pt :: MultiTerm -> ParamText
multiterm2pt x = pure (fromList x, Nothing)

multiterm2bsr :: Rule -> BoolStructR
multiterm2bsr = AA.mkLeaf . RPParamText . multiterm2pt . name

multiterm2bsr' :: MultiTerm -> BoolStructR
multiterm2bsr' = AA.mkLeaf . RPParamText . multiterm2pt

bsp2text :: BoolStructP -> Text.Text
bsp2text (AA.Not                    x ) = Text.unwords ["not", bsp2text x]
bsp2text (AA.Leaf                   x ) = Text.unwords $ concatMap toList $ fst <$> x
bsp2text (AA.Any (Just (AA.Pre p1       )) xs) = Text.unwords $ p1 : (bsp2text <$> xs)
bsp2text (AA.Any (Just (AA.PrePost p1 p2)) xs) = Text.unwords $ p1 : (bsp2text <$> xs) <> [p2]
bsp2text (AA.Any Nothing                   xs) = "any of:-" <> Text.unwords (bsp2text <$> xs)
bsp2text (AA.All (Just (AA.Pre p1       )) xs) = Text.unwords $ p1 : (bsp2text <$> xs)
bsp2text (AA.All (Just (AA.PrePost p1 p2)) xs) = Text.unwords $ p1 : (bsp2text <$> xs) <> [p2]
bsp2text (AA.All Nothing                   xs) = "all of:-" <> Text.unwords (bsp2text <$> xs)

bsr2text, bsr2textnl :: BoolStructR -> Text.Text
bsr2text   = bsr2text' Text.unwords
bsr2textnl = bsr2text' (Text.intercalate "\\n")

bsr2text' :: ([Text.Text] -> Text.Text) -> BoolStructR -> Text.Text
bsr2text'  joiner (AA.Not                           x ) = joiner ["not", bsr2text x]
bsr2text' _joiner (AA.Leaf                          x ) = rp2text x
bsr2text'  joiner (AA.Any (Just (AA.Pre p1       )) xs) = joiner $ p1 : (bsr2text <$> xs)
bsr2text'  joiner (AA.Any (Just (AA.PrePost p1 p2)) xs) = joiner $ p1 : (bsr2text <$> xs) <> [p2]
bsr2text'  joiner (AA.Any Nothing                   xs) = joiner ("any of:-" : (bsr2text <$> xs))
bsr2text'  joiner (AA.All (Just (AA.Pre p1       )) xs) = joiner $ p1 : (bsr2text <$> xs)
bsr2text'  joiner (AA.All (Just (AA.PrePost p1 p2)) xs) = joiner $ p1 : (bsr2text <$> xs) <> [p2]
bsr2text'  joiner (AA.All Nothing                   xs) = joiner ("all of:-" : (bsr2text <$> xs))

-- and possibily we want to have interspersed BoolStructs along the way

data Deontic = DMust | DMay | DShant
  deriving (Eq, Ord, Show, Generic, ToJSON)

data SrcRef = SrcRef { url      :: Text.Text
                     , short    :: Text.Text
                     , srcrow   :: Int
                     , srccol   :: Int
                     , version  :: Maybe Text.Text
                     }
              deriving (Eq, Ord, Show, Generic, ToJSON)


mkTComp :: MyToken -> Maybe TComparison
mkTComp Before     = Just TBefore
mkTComp After      = Just TAfter
mkTComp By         = Just TBy
mkTComp On         = Just TOn
mkTComp Eventually = Nothing
mkTComp x          = error $ "mkTC: can't create temporal constraint from " ++ show x ++ " -- this should be handled by a Vaguely"

mkTC :: MyToken -> Maybe Integer -> Text.Text -> Maybe (TemporalConstraint Text.Text)
mkTC tok   tt unit = TemporalConstraint <$> mkTComp tok <*> Just tt <*> pure unit
-- [TODO]: Consider supporting non-integer time constraints

data NatLang = NLen

tc2nl :: NatLang -> Maybe (TemporalConstraint Text.Text) -> Text.Text
tc2nl NLen Nothing = "eventually"
tc2nl NLen (Just (TemporalConstraint TBefore n t)) = Text.unwords [ "before", (maybe "" (Text.pack . show) n), t ]
tc2nl NLen (Just (TemporalConstraint TBy     n t)) = Text.unwords [ "by",     (maybe "" (Text.pack . show) n), t ]
tc2nl NLen (Just (TemporalConstraint TAfter  n t)) = Text.unwords [ "after",  (maybe "" (Text.pack . show) n), t ]
tc2nl NLen (Just (TemporalConstraint TOn     n t)) = Text.unwords [ "on",     (maybe "" (Text.pack . show) n), t ]
tc2nl NLen (Just (TemporalConstraint TVague  n t)) = Text.unwords [ "around", (maybe "" (Text.pack . show) n), t ]


data RunConfig = RC { debug     :: Bool
                    , printstream   :: Bool
                    , callDepth :: Int
                    , oldDepth  :: Int
                    , parseCallStack :: [String]
                    , sourceURL :: Text.Text
                    , asJSON    :: Bool
                    , toNLG     :: Bool
                    , toBabyL4  :: Bool
                    , toProlog  :: Bool
                    , toUppaal  :: Bool
                    , toHTML    :: Bool
                    , toPDF     :: Bool
                    , saveAKA   :: Bool
                    , wantNotRules :: Bool
                    , toGrounds :: Bool
                    , toVue     :: Bool
                    , toTS      :: Bool
                    , extendedGrounds :: Bool
                    , toChecklist :: Bool
                    , runNLGtests :: Bool
                    } deriving (Show, Ord, Eq)

defaultRC :: RunConfig
defaultRC = RC
        { debug = False
        , printstream = False
        , callDepth = 0
        , oldDepth = 0
        , parseCallStack = []
        , sourceURL = "STDIN"
        , asJSON = False
        , toNLG = False
        , toBabyL4 = False
        , toProlog = False
        , toUppaal = False
        , saveAKA = False
        , wantNotRules = False
        , toGrounds = False
        , toVue = False
        , toTS = False
        , extendedGrounds = False
        , toChecklist = False
        , runNLGtests = False
        , toHTML = False
        , toPDF = False
        }
nestLevel :: RunConfig -> Int
nestLevel = length . parseCallStack

increaseNestLevel :: String -> RunConfig -> RunConfig
increaseNestLevel name rc = rc { parseCallStack = name : parseCallStack rc }

magicKeywords :: [Text.Text]
magicKeywords = Text.words "EVERY PARTY MUST MAY WHEN INCLUDES MEANS IS IF UNLESS DEFINE"



whenDebug :: Parser () -> Parser ()
whenDebug act = do
  isDebug <- asks debug
  when isDebug act

pGetTokenPos :: Parser (WithPos ())
pGetTokenPos = token test Set.empty <?> "some token"
  where
    test tok = Just (() <$ tok)

pXLocation :: Parser Depth
pXLocation = token test Set.empty <|> pure 0 <?> "x location"
  where
    test WithPos {pos= SourcePos _ _y x} = Just (unPos x)

pYLocation :: Parser Depth
pYLocation = token test Set.empty <|> pure 0 <?> "y location"
  where
    test WithPos{pos= SourcePos _ y _x } = Just (unPos y)


pTokenMatch :: (MyToken -> Bool) -> NonEmpty MyToken -> Parser MyToken
pTokenMatch f c = do
  ctx <- asks parseCallStack
  token test $ Set.singleton $ Tokens $ liftMyToken ctx <$> c
  where
    test WithPos {tokenVal = x} =
      if f x
        then Just x
        else Nothing

enumLabels, enumLabels_ :: ParamText -> [Text.Text]
enumLabels nelist = concat $ NE.toList $ NE.toList . fst <$> nelist

enumLabels_ = fmap (Text.replace " " "_") . enumLabels
