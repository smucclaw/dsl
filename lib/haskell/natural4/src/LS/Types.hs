{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-|
Types used by the Legal Spreadsheets parser, interpreter, and transpilers.
-}

module LS.Types
  ( module LS.BasicTypes,
    module LS.Types,
  )
where

import AnyAll (mkLeaf)
import AnyAll qualified as AA
import Control.Monad.Reader (ReaderT (runReaderT), asks)
-- import Control.Monad.Writer.Lazy (WriterT (runWriterT))
import Data.Aeson (ToJSON)
import Data.Bifunctor (second)
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty ((:|)), fromList, toList)
import Data.List.NonEmpty qualified as NE
import Data.Monoid (Endo (Endo))
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Tree qualified as Tree
import Data.Void (Void)
import Flow ((|>))
import GHC.Generics (Generic)
import LS.BasicTypes
import Optics ()
import Safe (headMay)
import Text.Megaparsec (Parsec)

type PlainParser = ReaderT RunConfig (Parsec Void MyStream)
-- A parser generates a list of rules (in the "appendix", representing nested rules defined inline) and optionally some other value
type Depth = Int
type Preamble = MyToken

--- so really there are multiple Values
--- | apple | orange | banana | :: | Fruit   |
type TypedMulti = (NonEmpty MTExpr, Maybe TypeSig)

-- * BoolStructs wrap Phrasal types

type BoolStructT  = AA.OptionallyLabeledBoolStruct Text.Text
type BoolStructP = AA.OptionallyLabeledBoolStruct ParamText
type BoolStructR = AA.OptionallyLabeledBoolStruct RelationalPredicate

-- | the relations in a RelationalPredicate
data RPRel = RPis | RPhas | RPeq | RPlt | RPlte | RPgt | RPgte | RPelem | RPnotElem | RPnot | RPand | RPor | RPsum | RPproduct | RPsubjectTo
           | RPmin | RPmax
           | RPmap
           | RPTC TComparison -- ^ temporal constraint as part of a relational predicate; note there is a separate `TemporalConstraint` type.
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance Hashable RPRel

-- | Previously `MultiTerm`s were just @[Text]@.
-- We give them a long-overdue upgrade to match a handful of cell types that are native to spreadsheets
data MTExpr = MTT Text.Text -- ^ Text string
            | MTI Integer   -- ^ Integer
            | MTF Float     -- ^ Float
            | MTB Bool      -- ^ Boolean
--            | MTC Text.Text -- ^ Currency money
--            | MTD Text.Text -- ^ Date
            deriving (Eq, Ord, Show, Generic, ToJSON)

instance Hashable MTExpr

-- | the parser returns a list of MTExpr, to be parsed further at some later point
type MultiTerm = [MTExpr] --- | apple | banana | 100 | $100 | 1 Feb 1970

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
-- | notify | the government |    |         |
-- |        | immediately    | :: | Urgency |
type ParamText = NonEmpty TypedMulti

text2pt :: Text.Text -> ParamText
text2pt x = pure (pure (MTT x), Nothing)

mtexpr2text :: MTExpr -> Text.Text
mtexpr2text (MTT t) = t
mtexpr2text (MTI n) = Text.pack $ show n
mtexpr2text (MTF n) = Text.pack $ show n
mtexpr2text (MTB True) = "TRUE"
mtexpr2text (MTB False) = "FALSE"

pt2text :: ParamText -> Text.Text
pt2text = Text.unwords . fmap mtexpr2text . toList . (fst =<<)

type PTree = Tree.Tree TypedMulti -- Node ([MTT "notify" :| MTT "the government"], Nothing) [ Node ([MTT "immediately" :| [], Urgency) [] ]

mkPTree :: TypedMulti -> [PTree] -> PTree
mkPTree = Tree.Node

mkLeafPT :: Text.Text -> BoolStructP
mkLeafPT = AA.Leaf . text2pt

mkLeafR :: Text.Text -> BoolStructR
mkLeafR x = AA.Leaf $ RPMT [MTT x]

-- remove the TypeSig from a ParamText
untypePT :: ParamText -> NonEmpty (NonEmpty MTExpr)
untypePT = fmap fst

tm2mt :: TypedMulti -> MultiTerm
tm2mt = toList . fst

mt2tm :: MultiTerm -> TypedMulti
mt2tm x = (fromList x, Nothing)

mt2pt :: MultiTerm -> ParamText
mt2pt ts = pure (fromList ts, Nothing)

mt2text :: MultiTerm -> Text.Text
mt2text = Text.unwords . fmap mtexpr2text

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


-- maybe we should have a proper dict orientation here
data KW a = KW { dictK :: MyToken
               , dictV :: a }

data RegKeywords =
  REvery | RParty | RTokAll
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance Hashable RegKeywords

class HasToken a where
  tokenOf :: a -> MyToken

instance HasToken RegKeywords where
  tokenOf REvery = Every
  tokenOf RParty = Party
  tokenOf RTokAll = TokAll

-- [TODO]: we need to start preserving the keywords for each preamble*, because maybe this is a "which" not a "who"

data HornClause a = HC
  { hHead :: RelationalPredicate
  , hBody :: Maybe a
  }
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance Hashable a => Hashable (HornClause a)

type HornClause2 = HornClause BoolStructR

data IsPredicate = IP ParamText ParamText
  deriving (Eq, Ord, Show, Generic, ToJSON)

class PrependHead a where
  -- Used to prepend what was first interpreted to be a label to an item
  prependHead :: Text.Text -> a -> a

instance PrependHead MTExpr where
  prependHead t (MTT mtt) = MTT (prependHead t mtt)
  prependHead t (MTI mti) = MTT (prependHead t (Text.pack . show $ mti))
  prependHead t (MTF mtn) = MTT (prependHead t (Text.pack . show $ mtn))
  prependHead t (MTB mtb) = MTT (prependHead t (Text.pack . show $ mtb))

instance PrependHead Text.Text where
  prependHead s = ((s <> " ") <>)
instance PrependHead ParamText where
  prependHead s ((xs, ts) :| xss) = (pure (MTT s) <> xs, ts) :| xss

instance PrependHead RelationalPredicate where
  prependHead s (RPParamText ne)        = RPParamText (prependHead s ne)
  prependHead s (RPMT mtes)             = RPMT (MTT s : mtes)
  prependHead s (RPConstraint l rr r)   = RPConstraint (MTT s : l) rr r
  prependHead s (RPBoolStructR l rr it) = RPBoolStructR (MTT s : l) rr it
  prependHead s (RPnary rel rps)        = RPnary rel (RPMT [MTT s] : rps)

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
                         | RPnary RPRel [RelationalPredicate] -- "NEVER GO FULL LISP!" "we went full Lisp".
                           -- RPnary RPnot [RPnary RPis [MTT ["the sky"], MTT ["blue"]]
                        -- [TODO] consider adding a new approach, actually a very old Lispy approach

                     --  | RPDefault      in practice we use RPMT ["OTHERWISE"], but if we ever refactor, we would want an RPDefault
  deriving (Eq, Ord, Show, Generic, ToJSON)
                 -- RPBoolStructR (["eyes"] RPis (AA.Leaf (RPParamText ("blue" :| [], Nothing))))
                 -- would need to reduce to
                 -- RPConstraint ["eyes"] Rpis ["blue"]

instance Hashable RelationalPredicate

mkRpmt :: [Text.Text] -> RelationalPredicate
mkRpmt a = RPMT (MTT <$> a)

mkRpmtLeaf :: [Text.Text] -> BoolStructR
mkRpmtLeaf a = mkLeaf (mkRpmt a)

-- | [TODO] figure out why there are two very similar functions, this and `rel2op`
rel2txt :: RPRel -> Text.Text
rel2txt RPis      = "IS"
rel2txt RPhas     = "HAS" -- "relHas"
rel2txt RPeq      = "=="  -- "relEq"
rel2txt RPlt      = "<"   -- "relLT"
rel2txt RPlte     = "<="  -- "relLTE"
rel2txt RPgt      = ">"   -- "relGT"
rel2txt RPgte     = ">="  -- "relGTE"
rel2txt RPelem    = "IN"  -- "relIn"
rel2txt RPnotElem = "NOT IN" -- "relNotIn"
rel2txt RPnot     = "NOT"    -- "relNot"
rel2txt RPand     = "&&"    -- "relAnd"
rel2txt RPor      = "||"    -- "relOr"
rel2txt RPmap     = "MAP"
rel2txt RPmin     = "MIN"
rel2txt RPmax     = "MAX"
rel2txt RPsum     = "SUM"
rel2txt RPproduct = "PRODUCT"
rel2txt (RPTC TBefore) = "BEFORE"
rel2txt (RPTC TAfter ) = "AFTER"
rel2txt (RPTC TBy    ) = "BY"
rel2txt (RPTC TOn)     = "ON"
rel2txt (RPTC TVague)  = "ABOUT"

-- | [TODO] figure out why there are two very similar functions, this and `rel2txt`
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
rel2op RPand     = "&&"
rel2op RPor      = "||"
rel2op x         = rel2txt x

rp2mt :: RelationalPredicate -> MultiTerm
rp2mt (RPParamText    pt)            = pt2multiterm pt
rp2mt (RPMT           mt)            = mt
rp2mt (RPConstraint   mt1 rel mt2)   = mt1 ++ [MTT $ rel2txt rel] ++ mt2
rp2mt (RPBoolStructR  mt1 rel bsr)   = mt1 ++ [MTT $ rel2txt rel] ++ [MTT $ bsr2text bsr] -- [TODO] is there some better way to bsr2mtexpr?
rp2mt (RPnary         rel rps)       = MTT (rel2txt rel) : foldMap rp2mt rps

-- | pull out all the body leaves of RelationalRredicates as multiterms
rp2bodytexts :: RelationalPredicate -> [MultiTerm]
rp2bodytexts (RPParamText    pt)            = [pt2multiterm pt]
rp2bodytexts (RPMT           mt)            = [mt]
rp2bodytexts (RPConstraint   mt1 rel mt2)   = [mt1 ++ [MTT $ rel2op rel] ++ mt2]
rp2bodytexts (RPBoolStructR  mt1 rel bsr)   = [mt1 ++ MTT (rel2op rel) : bod
                                              | bod <- foldMap rp2bodytexts (AA.extractLeaves bsr) ]
rp2bodytexts (RPnary         rel rps)       = [MTT (rel2op rel), MTT "("] : foldMap rp2bodytexts rps ++ [[MTT ")"]]

rp2text :: RelationalPredicate -> Text.Text
rp2text = Text.unwords . fmap mtexpr2text . rp2mt

text2rp :: Text.Text -> RelationalPredicate
text2rp = RPParamText . text2pt

pt2multiterm :: ParamText -> MultiTerm
pt2multiterm pt = concat (toList (toList <$> untypePT pt))

-- the "key-like" part of a relationalpredicate, used for TYPICALLY value assignment
rpHead :: RelationalPredicate -> MultiTerm
rpHead (RPParamText    pt)            = pt2multiterm pt
rpHead (RPMT           mt)            = mt
rpHead (RPConstraint   mt1 _rel _mt2) = mt1
rpHead (RPBoolStructR  mt1 _rel _bsr) = mt1
-- [TODO] this is lossy, why not keep it in relationalpredicate? can we MTR?
rpHead (RPnary         rel [])        = [ MTT (rel2op rel) ]
rpHead (RPnary         rel (rp:_))    =   MTT (rel2op rel) : rpHead rp

newtype RelName = RN { getName :: RuleName }

noLabel :: Maybe (Text.Text, Int, Text.Text)
noLabel   = Nothing
noLSource :: Maybe Text.Text
noLSource = Nothing
noSrcRef :: Maybe SrcRef
noSrcRef  = Nothing
noDeem   :: Maybe ParamText
noDeem = Nothing

data ParamType = TOne | TOptional | TList0 | TList1 | TSet0 | TSet1
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance Hashable ParamType

-- everything is stringly typed at the moment but as this code matures these will become more specialized.
data TComparison = TBefore | TAfter | TBy | TOn | TVague
                 deriving (Eq, Ord, Show, Generic, ToJSON)

instance Hashable TComparison

data TemporalConstraint a = TemporalConstraint TComparison (Maybe Integer) a
                          deriving (Eq, Ord, Show, Generic, ToJSON)

instance Hashable a => Hashable (TemporalConstraint a)

type RuleName   = MultiTerm
type EntityType = Text.Text
type EntityName = Text.Text

data TypeSig = SimpleType ParamType EntityType
             | InlineEnum ParamType ParamText
             deriving (Eq, Ord, Show, Generic, ToJSON)

instance Hashable TypeSig

-- for use by the interpreter

type VarPath = [TypedMulti]

data InterpreterOptions = IOpts
  { enums2decls :: Bool -- ^ convert inlineEnums in a class declaration to top-level decls? Used by corel4.
  }
  deriving (Eq, Ord, Show)

-- [TODO] consider using typeclass Default https://hackage.haskell.org/package/data-default
defaultInterpreterOptions :: InterpreterOptions
defaultInterpreterOptions = IOpts
  { enums2decls = False
  }

-- | a basic symbol table to track "variable names" and their associated types.

getUnderlyingType :: TypeSig -> Either String EntityType
getUnderlyingType   (SimpleType TOne      s1) = Right s1
getUnderlyingType   (SimpleType TOptional s1) = Right s1
getUnderlyingType   (SimpleType TList0    s1) = Right s1
getUnderlyingType   (SimpleType TList1    s1) = Right s1
getUnderlyingType   (SimpleType TSet0     s1) = Right s1
getUnderlyingType   (SimpleType TSet1     s1) = Right s1
getUnderlyingType   (InlineEnum _pt1      __) = Left "type declaration cannot inherit from _enum_ superclass"

-- * what's the difference between SymTab, ClsTab, and ScopeTabs?

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

type ClassHierarchyMap = Map.HashMap EntityType TypedClass

-- | ScopeTabs: In the course of a program we will sometimes see ad-hoc variables used in GIVEN and elsewhere.
-- those end up in the ScopeTabs object returned by the `symbolTable` function.

-- We also see explicit variable definitions given by (DEFINE ... HAS ...). These also end up in ScopeTabs.
-- If such a definition appears under a WHERE limb of another rule, it is scoped to that rule.

-- If it is given at top level, then it is under ... global scope, which is represented by Rulename=[]
-- The keys to ScopeTabs are from ruleLabelName.

type ScopeTabs = Map.HashMap RuleName SymTab

--  | SymTabs are a helper data structure used by ScopeTabs.
-- the fst contains type-related information.
-- the snd contains value-related information.

-- this type is getting pretty hefty, soon it'll be time to give it a proper type definition.

type SymTab = Map.HashMap MultiTerm (Inferrable TypeSig, [HornClause2])

-- | The explicitly annotated types from the L4 source text are recorded in the fst of Inferrable.
--   The confirmed & inferred types after the type checker & inferrer has run, are recorded in the snd of Inferrable.
--   If type checking / inference have not been implemented the snd will be empty.
type Inferrable ts = (Maybe ts, [ts])

defaultInferrableTypeSig = (Nothing, [])

thisAttributes, extendedAttributes :: ClsTab -> EntityType -> Maybe ClsTab

-- | attributes defined in the type declaration for this class specifically
thisAttributes (CT clstab) subclass = do
  ((_mts, _tss), ct) <- Map.lookup subclass clstab
  return ct

-- | attributes including superclass attributes
extendedAttributes o@(CT clstab) subclass = do
  ((_mts, _tss), CT ct) <- Map.lookup subclass clstab
  let eAttrs = case extendedAttributes o <$> clsParent o subclass of
                 Nothing               -> Map.empty
                 Just Nothing        -> Map.empty
                 Just (Just (CT ea)) -> ea
  return $ CT $ ct <> eAttrs

-- | get out whatever type signature has been user defined or inferred.
getSymType :: Inferrable ts -> Maybe ts
getSymType (Just x, _)    = Just x
getSymType (_, xs) = headMay xs

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

multiterm2bsr' :: MultiTerm -> BoolStructR
multiterm2bsr' = AA.mkLeaf . RPParamText . multiterm2pt

bsp2text :: BoolStructP -> Text.Text
bsp2text (AA.Not                    x ) = [i|not #{bsp2text x}|]
bsp2text (AA.Leaf                   x ) = Text.unwords $ mtexpr2text <$> foldMap (toList . fst) x
bsp2text (AA.Any (Just (AA.Pre p1       )) xs) = Text.unwords $ p1 : (bsp2text <$> xs)
bsp2text (AA.Any (Just (AA.PrePost p1 p2)) xs) = Text.unwords $ p1 : (bsp2text <$> xs) <> [p2]
bsp2text (AA.Any Nothing                   xs) = [i|any of:-#{Text.unwords $ bsp2text <$> xs}|]
bsp2text (AA.All (Just (AA.Pre p1       )) xs) = Text.unwords $ p1 : (bsp2text <$> xs)
bsp2text (AA.All (Just (AA.PrePost p1 p2)) xs) = [i|#{Text.unwords $ p1 : (bsp2text <$> xs)} #{p2}|]
bsp2text (AA.All Nothing                   xs) = [i|all of:-#{Text.unwords $ bsp2text <$> xs}|]

bsr2text, bsr2textnl :: BoolStructR -> Text.Text
bsr2text   = bsr2text' Text.unwords
bsr2textnl = bsr2text' (Text.intercalate "\\n")

bsr2text' :: ([Text.Text] -> Text.Text) -> BoolStructR -> Text.Text
bsr2text'  joiner (AA.Not                           x ) = joiner ["not",       bsr2text' joiner x]
bsr2text' _joiner (AA.Leaf                          x ) = rp2text x
bsr2text'  joiner (AA.Any (Just (AA.Pre p1       )) xs) = joiner $        p1 : (bsr2text' joiner <$> xs)
bsr2text'  joiner (AA.Any (Just (AA.PrePost p1 p2)) xs) = joiner $        p1 : (bsr2text' joiner <$> xs) <> [p2]
bsr2text'  joiner (AA.Any Nothing                   xs) = joiner ("any of:-" : (bsr2text' joiner <$> xs))
bsr2text'  joiner (AA.All (Just (AA.Pre p1       )) xs) = joiner $        p1 : (bsr2text' joiner <$> xs)
bsr2text'  joiner (AA.All (Just (AA.PrePost p1 p2)) xs) = joiner $        p1 : (bsr2text' joiner <$> xs) <> [p2]
bsr2text'  joiner (AA.All Nothing                   xs) = joiner ("all of:-" : (bsr2text' joiner <$> xs))

-- and possibily we want to have interspersed BoolStructs along the way

data Deontic = DMust | DMay | DShant
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance Hashable Deontic

data SrcRef = SrcRef { url      :: Text.Text
                     , short    :: Text.Text
                     , srcrow   :: Int
                     , srccol   :: Int
                     , version  :: Maybe Text.Text
                     }
              deriving (Eq, Ord, Show, Generic, ToJSON)

instance Hashable SrcRef

mkTComp :: MyToken -> Maybe TComparison
mkTComp Before     = Just TBefore
mkTComp After      = Just TAfter
mkTComp By         = Just TBy
mkTComp On         = Just TOn
mkTComp Eventually = Nothing
mkTComp x          = error [i|mkTC: can't create temporal constraint from #{x} -- this should be handled by a Vaguely|]

mkTC :: MyToken -> Maybe Integer -> Text.Text -> Maybe (TemporalConstraint Text.Text)
mkTC tok   tt unit = TemporalConstraint <$> mkTComp tok <*> Just tt <*> pure unit
-- [TODO]: Consider supporting non-integer time constraints

data NatLang = NLen

tc2nl :: NatLang -> Maybe (TemporalConstraint Text.Text) -> Text.Text
tc2nl NLen Nothing = "eventually"
tc2nl NLen (Just (TemporalConstraint tComparison n t)) =
  [i|{tComaparisonTxt} #{maybe "" show n} #{t}|]
  where
    tComparisonTxt :: Text.Text = case tComparison of
      TVague -> "around"
      _ -> tComparison |> show |> Text.pack |> Text.tail |> Text.toLower

data RunConfig = RC { debug     :: Bool
                    , printstream   :: Bool
                    , callDepth :: Int
                    , oldDepth  :: Int
                    , parseCallStack :: [String]
                    , sourceURL :: Text.Text
                    , asJSON    :: Bool
                    , toNLG     :: Bool
                    , toBabyL4  :: Bool
                    , toASP     :: Bool
                    , toProlog  :: Bool
                    , toPrologTp :: Bool
                    , toJsonTp  :: Bool
                    , toJsonUI  :: Bool
                    , toMaude :: Bool
                    , toLogicalEnglish :: Bool
                    , toMathLang :: Bool
                    , toSCasp   :: Bool
                    , toUppaal  :: Bool
                    , toHTML    :: Bool
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
        , toASP    = False
        , toProlog = False
        , toPrologTp = False
        , toJsonTp = False
        , toJsonUI = False
        , toMaude = False
        , toLogicalEnglish = False
        , toMathLang = False
        , toSCasp  = False
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
        }

nestLevel :: RunConfig -> Int
nestLevel = length . parseCallStack

increaseNestLevel :: String -> RunConfig -> RunConfig
increaseNestLevel name rc = rc { parseCallStack = name : parseCallStack rc }

magicKeywords :: Set.HashSet Text.Text
magicKeywords = Set.fromList
  [ "EVERY",
    "PARTY",
    "MUST,",
    "MAY",
    "WHEN",
    "INCLUDES",
    "MEANS",
    "IS",
    "IF",
    "UNLESS",
    "DEFINE"
  ]

-- | we actually want @[Text]@ here not just `MultiTerm`
enumLabels, enumLabels_ :: ParamText -> [Text.Text]
enumLabels nelist = fmap mtexpr2text $ mconcat $ NE.toList $ NE.toList . fst <$> nelist

enumLabels_ = fmap (Text.replace " " "_") . enumLabels
