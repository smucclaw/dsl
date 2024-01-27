{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LS.Rule
  ( Rule (..),
    RuleBody (..),
    RuleLabel,
    Interpreted (..),
    Parser,
    ValuePredicate (..),
    Expect (..),
    RuleGraph,
    RuleGraphEdgeLabel,
    defaultHorn,
    defaultL4I,
    defaultReg,
    defaultTypeDecl,
    defaultValuePredicate,
    dummyRef,
    extractMTExprs,
    getRlabel,
    getDecisionHeads,
    hasClauses,
    hasGiven,
    hasGiveth,
    isFact,
    mkTestSrcRef,
    multiterm2bsr,
    pGetTokenPos,
    pTokenMatch,
    pXLocation,
    pYLocation,
    rl2text,
    ruleLabelName,
    ruleName,
    ruleConstructor,
    runMyParser,
    srccol1,
    srcrow2,
    srctest,
    whenDebug,
  )
where

import AnyAll qualified as AA
import Control.Monad (void, when)
import Control.Monad.Reader (ReaderT (runReaderT), asks)
import Control.Monad.Writer.Lazy (WriterT (runWriterT))
import Data.Aeson (ToJSON)
import Data.Bifunctor (second)
import Data.Generics.Product.Types (HasTypes, types)
import Data.Graph.Inductive (Gr, empty)
import Data.HashMap.Strict qualified as Map
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Void (Void)
import Flow ((|>))
import GHC.Generics (Generic)
import LS.Types
  ( BoolStructP,
    BoolStructR,
    ClsTab (..),
    DList,
    Deontic (DMust),
    Depth,
    EntityName,
    HornClause (HC, hBody),
    HornClause2,
    Inferrable,
    MTExpr (..),
    MultiTerm,
    MyStream,
    MyToken (Means),
    ParamText,
    PlainParser,
    Preamble,
    RPRel (..),
    RegKeywords (REvery),
    RelationalPredicate (RPMT, RPParamText),
    RuleName,
    RunConfig (debug, parseCallStack),
    ScopeTabs,
    SrcRef (..),
    TemporalConstraint,
    TypeSig,
    WithPos (WithPos, pos, tokenVal),
    bsp2text,
    defaultInferrableTypeSig,
    dlToList,
    liftMyToken,
    mkLeafPT,
    mkLeafR,
    mt2text,
    multiterm2pt,
    rpHead,
  )
import LS.XPile.Logging (XPileLogW)
import Optics hiding (has, (|>)) -- the Rule record has a `has` field
import System.FilePath ((</>))
import Text.Megaparsec
  ( ErrorItem (Tokens),
    MonadParsec (eof, token),
    ParseErrorBundle,
    SourcePos (SourcePos),
    runParser,
    unPos,
    (<?>),
    (<|>),
  )

-- $setup
-- >>> :set -XDataKinds -XFlexibleContexts -XTypeApplications -XTypeFamilies -XDeriveGeneric
-- >>> import Optics qualified as O (has)
-- >>> import Data.Generics.Sum.Constructors

{- | 
[TODO] refactoring: these should be broken out into their own (new)types and have Rule include them all.
We should take advantage of NoFieldSelectors to reduce the hazards here


The deriving Generics stuff allows us to do things like
>>> O.has (_Ctor @"Regulative") defaultHorn
False
>>> O.has (_Ctor @"Regulative") defaultReg
True

as well as extracting givens from a rule in an easy way (see the Logical English code).

See https://hackage.haskell.org/package/generic-optics-2.2.1.0/docs/Data-Generics-Sum-Constructors.html 
for an explanation of how the Generics and optics stuff works
-}
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
            , given    :: Maybe ParamText    -- a:Applicant, p:Person, l:Lender       -- the type signature of the input
            , giveth   :: Maybe ParamText    -- m:Amount,   mp:Principal, mi:Interest -- the type signature of the output
        --  , having   :: Maybe ParamText    -- event trace history predicate: applicant has submitted fee
            , upon     :: Maybe ParamText    -- second request occurs
            , clauses  :: [HornClause2]      -- colour IS blue WHEN fee > $10 ; colour IS green WHEN fee > $20 AND approver IS happy
            , rlabel   :: Maybe RuleLabel
            , lsource  :: Maybe Text.Text
            , wwhere   :: [Rule]
            , srcref   :: Maybe SrcRef
            , defaults :: [RelationalPredicate] -- SomeConstant IS 500 ; MentalCapacity TYPICALLY True
            , symtab   :: [RelationalPredicate] -- SomeConstant IS 500 ; MentalCapacity TYPICALLY True
            }
          | TypeDecl
            { name     :: RuleName              -- DECLARE Class
            , super    :: Maybe TypeSig         -- IS A Superclass
            , has      :: [Rule]      -- HAS foo :: List Hand \n bar :: Optional Restaurant
            , enums    :: Maybe ParamText   -- ONE OF rock, paper, scissors (basically, disjoint subtypes)
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
          | RegFulfilled  -- trivial top -- success / fulfilled. used as the default HENCE branch of a MAY / MUST
          | RegBreach     -- trivial bottom -- failure / breach. used as the default LEST branch of a MUST.
          -- | CaseStm       -- work in progress
          -- { name   :: RuleName
          -- , limbs  :: [(Maybe BoolStructP -- cond
          --              ,ParamText         -- result
          --              )]
          -- , eqtest :: Maybe ParamText
          -- }
          | NotARule [MyToken]
          deriving (Eq, Ord, Show, Generic, ToJSON)

instance Hashable Rule

type Parser = WriterT (DList Rule) PlainParser


-- | the more responsible version of head . words . show
ruleConstructor :: Rule -> String
ruleConstructor Regulative{}   = "Regulative"
ruleConstructor Constitutive{} = "Constitutive"
ruleConstructor Hornlike{}     = "Hornlike"
ruleConstructor TypeDecl{}     = "TypeDecl"
ruleConstructor Scenario{}     = "Scenario"
ruleConstructor DefNameAlias{} = "DefNameAlias"
ruleConstructor DefTypically{} = "DefTypically"
ruleConstructor RuleAlias{}    = "RuleAlias"
ruleConstructor RuleGroup{}    = "RuleGroup"
ruleConstructor RegFulfilled   = "RegFulfilled"
ruleConstructor RegBreach      = "RegBreach"
ruleConstructor _              = "NotARule"

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
ruleLabelName rule =
  rule |> getRlabel |> maybe (ruleName rule) \x -> [MTT $ rl2text x]

getRlabel :: Rule -> Maybe RuleLabel
getRlabel Regulative {rlabel}   = rlabel
getRlabel Constitutive {rlabel} = rlabel
getRlabel Hornlike {rlabel}     = rlabel
getRlabel TypeDecl {rlabel}     = rlabel
getRlabel Scenario {rlabel}     = rlabel
getRlabel RuleGroup {rlabel}    = rlabel
-- getRlabel r@DefNameAlias {} = Nothing
-- getRlabel r@DefTypically {} = Nothing
-- getRlabel r@(RuleAlias a)   = Nothing
-- getRlabel r@RegFulfilled    = Nothing
-- getRlabel r@RegBreach       = Nothing
getRlabel _                     = Nothing

ruleName :: Rule -> RuleName
ruleName Regulative {subj}    = [MTT $ bsp2text subj]
ruleName (RuleAlias ruleName) = ruleName
ruleName RegFulfilled         = [MTT "FULFILLED"]
ruleName RegBreach            = [MTT "BREACH"]
ruleName Constitutive {name}  = name
ruleName Hornlike {name}      = name
ruleName TypeDecl {name}      = name
ruleName DefNameAlias {name}  = name
ruleName DefTypically {name}  = name
ruleName _                    = []

type RuleLabel = (Text.Text   --  "§"
                 ,Int         --   1
                 ,Text.Text   --  "My First Rule"
                 )

rl2text :: RuleLabel -> Text.Text
rl2text (_sectionSymbol, _numSymbols, ruleText) = ruleText

-- sometimes we just want to convert either the rulelabel or the rulename to text
rlrn2text :: Rule -> Text.Text
rlrn2text r = mt2text $ ruleLabelName r

mkTestSrcRef :: Int -> Int ->Maybe SrcRef
mkTestSrcRef row col = Just (SrcRef {url = Text.pack $ "test" </> "Spec", short = Text.pack $ "test" </> "Spec", srcrow = row, srccol = col, version = Nothing})

dummyRef :: Maybe SrcRef
dummyRef = mkTestSrcRef 1 1

defaultReg :: Rule
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
  , srcref = dummyRef
  , upon = Nothing
  , given = Nothing
  , having = Nothing
  , wwhere = []
  , defaults = []
  , symtab   = []
  }

defaultCon :: Rule
defaultCon = Constitutive
  { name = []
  , keyword = Means
  , letbind = mkLeafR "Undefined"
  , cond = Nothing
  , rlabel = Nothing
  , lsource = Nothing
  , srcref = dummyRef
  , given = Nothing
  , defaults = []
  , symtab   = []
  }

defaultHorn :: Rule
defaultHorn = Hornlike
  { name = []
  , super = Nothing
  , keyword = Means
  , given  = Nothing
  , giveth = Nothing
  , upon  = Nothing
  , clauses = []
  , wwhere = []
  , rlabel = Nothing
  , lsource = Nothing
  , srcref = dummyRef
  , defaults = []
  , symtab   = []
  }

defaultTypeDecl :: Rule
defaultTypeDecl =
  TypeDecl
    { name = [],
      super = Nothing,
      has = [],
      enums = Nothing,
      given = Nothing,
      upon = Nothing,
      rlabel = Nothing,
      lsource = Nothing,
      srcref = Nothing,
      defaults = [],
      symtab = []
    }

-- | Extract the MTExprs from, e.g., the Maybe ParamTexts (e.g. from the given field)
extractMTExprs :: forall s. (HasTypes s MTExpr) => s -> [MTExpr]
extractMTExprs = toListOf (types @MTExpr)


-- | does a rule have a Given attribute?
hasGiven :: Rule -> Bool
hasGiven     Hornlike{} = True
hasGiven   Regulative{} = True
hasGiven     TypeDecl{} = True
hasGiven Constitutive{} = True
hasGiven             __ = False

-- | does a rule have a Giveth attribute? this should go away upon refactoring the Rule type to avoid multiple record selectors
hasGiveth :: Rule -> Bool
hasGiveth Hornlike{} = True
hasGiveth _          = False

-- | does a rule have Clauses?
-- [TODO] it's beginning to look like we need to break out the Rule Types into different types not just constructors
hasClauses :: Rule -> Bool
hasClauses     Hornlike{} = True
hasClauses             __ = False

-- | is a decision rule a predicate or is it a fact?
-- this may be fragile -- we believe that a rule is a fact if it has exactly one horn clause
-- whose body is a Nothing.
isFact :: Rule -> Bool
isFact r
  | hasClauses r = ruleNameIsNumeric (name r) || ((length (clauses r) == 1) && all ((Nothing ==) . hBody) (clauses r))
  | otherwise = False
  where
    -- when we have a numeric fact, it shows up with a name like [ MTI 0 ]
    ruleNameIsNumeric = all ( \case
                                MTI _ -> True
                                _     -> False )
getDecisionHeads :: Rule -> [MultiTerm]
getDecisionHeads Hornlike{..} = [ rpHead hhead
                                | HC hhead _hbody <- clauses ]
getDecisionHeads _ = []

data Expect = ExpRP      RelationalPredicate
            | ExpDeontic Rule -- regulative rule
            deriving (Eq, Ord, Show, Generic, ToJSON)

instance Hashable Expect

-- | This is generated by the Interpreter and handed around to different transpilers.
--
-- In the future we may add to this list of attributes the following frequently called functions:
--
-- * `LS.Interpreter.qaHornsT` -- currently a function run against `l4i`
--
-- * `LS.Interpreter.qaHornsR` -- currently a function run against `l4i`

data Interpreted = L4I {
  -- | all the `DECLARE`d classes
  classtable :: ClsTab

  -- | all the rule scopes; the exact semantics are not
  -- entirely nailed down as we don't have a rigorous notion of scope
  -- yet. This is intended to reflect the real-world usage of "for the
  -- purposes of this section, X means Y". We might think of that as a
  -- nested block scope for @let@ bindings.
  , scopetable :: ScopeTabs

  -- | the original rules on which the interpretation was based. You
  -- may sometimes see functions that take both 'Interpreted' and
  -- @[Rule]@; the latter is technically redundant and can be safely
  -- eliminated. [TODO].
  , origrules  :: [Rule]

  -- | valuepredicates contain the bulk of the top-level decision logic, and can be easily expressed as instance or class methosd.
  , valuePreds :: [ValuePredicate]

  -- | rule decision graph gets used by multiple transpilers, so it lives here
  , ruleGraph :: RuleGraph
  , ruleGraphErr :: XPileLogW
  }
  deriving (Eq, Show)

-- | default L4I
defaultL4I :: Interpreted
defaultL4I = L4I
  { classtable = CT Map.empty
  , scopetable = Map.empty
  , origrules = mempty
  , valuePreds = mempty
  , ruleGraph = empty
  , ruleGraphErr = mempty
  }


-- | when the input says @DECIDE ClassA's RecordAttr's AttributeNAME IS foo WHEN bar@
-- we rewrite that to a `ValuePredicate`.
data ValuePredicate = ValPred
  { moduleName :: [EntityName]  -- MoneyLib
  , scopeName  :: [EntityName]  -- DollarJurisdictions
  , objPath    :: [EntityName]  -- ClassA, ClassB, RecordAttrName --> ClassA.ClassB
                  -- If this list is null, then the "attribute" is toplevel / module-global
  , attrName   ::  EntityName   -- ClassA, ClassB, RecordAttrName --> RecordAttrName
  , attrRel    ::  Maybe RPRel  -- 
  , attrVal    ::  Maybe RelationalPredicate
  , attrCond   ::  Maybe BoolStructR
  , attrIType  ::  Inferrable TypeSig
  , origBSR    ::  Maybe BoolStructR
  , origHC     ::  Maybe HornClause2
  , origRule   ::  Maybe Rule
  }
  deriving (Show, Eq, Ord, Generic)

defaultValuePredicate = ValPred
  { moduleName = []
  , scopeName  = []
  , objPath    = []
  , attrName   = "defaultAttrName"
  , attrRel    = Just   RPis
  , attrVal    = Just $ RPMT [MTT "defaultAttrVal"]
  , attrCond   = Nothing
  , attrIType  = defaultInferrableTypeSig
  , origBSR    = Nothing
  , origHC     = Nothing
  , origRule   = Nothing
  }




-- | structure the rules as a graph.
-- in the simple case, the graph is one or more trees, each rooted at a "top-level" rule which is not "used" by any another rule.
--
-- if we walk the roots, we will sooner or later encounter all the decision elements relevant to each root.
-- in a less simple case, the graph is cyclic! everything depends on everything else! but we can recognize that as an error condition.
--
-- note that a regulative rule R1 HENCE R2 is recorded as a single rule, even if we think of the R2 as a separate rule
-- perhaps we should have a notion of anonymous rules, that are internally labelled and structured, so R2 is equal to R1 in the graph.

type RuleGraphEdgeLabel = ()
type RuleGraph = Gr Rule RuleGraphEdgeLabel



multiterm2bsr :: Rule -> BoolStructR
multiterm2bsr = AA.mkLeaf . RPParamText . multiterm2pt . name

pGetTokenPos :: Parser (WithPos ())
pGetTokenPos = token test Set.empty <?> "some token"
  where
    test tok = Just $ void tok

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
    test WithPos {tokenVal = x}
      | f x = Just x
      | otherwise = Nothing

whenDebug :: Parser () -> Parser ()
whenDebug act = do
  isDebug <- asks debug
  when isDebug act

srctest :: Int -> Int -> Rule -> Rule
srctest srow scol r = r { srcref = Just (SrcRef {url = Text.pack $ "test" </> "Spec", short = Text.pack $ "test" </> "Spec", srcrow = srow, srccol = scol, version = Nothing }) }

srcrow_ :: Rule -> Rule
srcrow_   w = w { srcref = Nothing, hence = srcrow_ <$> hence w, lest = srcrow_ <$> lest w }

srcrow1' :: Rule -> Rule
srcrow1'  w = w { srcref = (\x -> x  { srcrow = 1 }) <$> srcref defaultReg }

srcrow1 :: Rule -> Rule
srcrow1     = srcrow' 1

srcrow2 :: Rule -> Rule
srcrow2     = srcrow' 2

srcrow' :: Int -> Rule -> Rule
srcrow' n w = w { srcref = (\x -> x  { srcrow = n }) <$> srcref w }

srccol1 :: Rule -> Rule
srccol1     = srccol' 1

srccol2 :: Rule -> Rule
srccol2     = srccol' 2

srccol' :: Int -> Rule -> Rule
srccol' n w = w { srcref = (\x -> x  { srccol = n }) <$> srcref w }

