{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LS.Rule where

import AnyAll qualified as AA
import Control.Monad (when)
import Control.Monad.Reader (ReaderT (runReaderT), asks)
import Control.Monad.Writer.Lazy (WriterT (runWriterT))
import Data.Aeson (ToJSON)
import Data.Bifunctor (second)
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
    ClsTab,
    DList,
    Deontic (DMust),
    Depth,
    HornClause (HC),
    HornClause2,
    MTExpr (MTT),
    MultiTerm,
    MyStream,
    MyToken (Means),
    ParamText,
    PlainParser,
    Preamble,
    RegKeywords (REvery),
    RelationalPredicate (RPParamText),
    RuleName,
    RunConfig (debug, parseCallStack),
    ScopeTabs,
    SrcRef (..),
    TemporalConstraint,
    TypeSig,
    WithPos (WithPos, pos, tokenVal),
    bsp2text,
    dlToList,
    liftMyToken,
    mkLeafPT,
    mkLeafR,
    mt2text,
    multiterm2pt,
    rpHead,
  )
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
  rule |> getRlabel |> maybe (ruleName rule) (\x -> [MTT $ rl2text x]) 

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
mkTestSrcRef row col = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = row, srccol = col, version = Nothing})

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
  }
  deriving (Eq, Ord, Show)

multiterm2bsr :: Rule -> BoolStructR
multiterm2bsr = AA.mkLeaf . RPParamText . multiterm2pt . name

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

whenDebug :: Parser () -> Parser ()
whenDebug act = do
  isDebug <- asks debug
  when isDebug act

srctest :: Int -> Int -> Rule -> Rule
srctest srow scol r = r { srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = srow, srccol = scol, version = Nothing }) }

srcrow_ :: Rule -> Rule
srcrow_   w = w { srcref = Nothing, hence = srcrow_ <$> (hence w), lest = srcrow_ <$> (lest w) }

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

