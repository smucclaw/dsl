{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module LS.Renamer (
  -- * Renamed Rule types
  RnRule (..),
  RnHornlike (..),
  RnTypeDecl (..),
  RnHornClause (..),
  RnTypedMulti (..),
  RnMultiTerm,
  RnExpr (..),
  RnName (..),
  RnNameType (..),
  RnLit (..),
  RnRelationalPredicate (..),
  RnBoolStructR,
  OccName,
  Unique,
  mkSimpleOccName,

  -- * Renamer Monad and runners
  RenamerResult (..),
  rnResultScope,
  Renamer (..),
  runRenamerFor,

  -- * Logging
  Tracer,
  liftRenamerTracer,
  Log (..),

  -- * Renamer Errors
  RenamerError (..),
  AssertionError (..),
  renderRenamerError,

  -- * Scope checking types
  Scope (..),
  emptyScope,
  scScopeTable,
  scUniqueSupply,
  newUnique,
  lookupName,
  lookupExistingName,
  lookupOrInsertName,
  insertName,
  insertFunction,
  lookupExistingFunction,
  ScopeTable (..),
  stVariables,
  stFunction,
  unionScopeTable,
  differenceScopeTable,
  emptyScopeTable,
  FuncInfo (..),

  -- * Assertion helpers
  assertEmptyList,
  assertSingletonMultiTerm,
  assertNoTypeSignature,

  -- * Utilities for analyzing the L4 AST
  toObjectPath,

  -- * Debugging helpers
  renameRuleTopLevel,
) where

import AnyAll.BoolStruct qualified as AA
import LS.Rule (Rule, RuleLabel)
import LS.Rule qualified as Rule
import LS.Types (MyToken, RuleName, SrcRef)
import LS.Types qualified as LS

import Control.Monad.Error.Class as Error
import Control.Monad.Extra (foldM, fromMaybeM)
import Control.Monad.State.Strict (MonadState, MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except qualified as Except
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.State.Strict qualified as State
import Data.Foldable (traverse_)
import Data.Foldable qualified as Foldable
import Data.Functor (void)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy.IO qualified as TL
import Data.Tuple (Solo (MkSolo))
import GHC.Generics (Generic)
import LS.Log (traceWith)
import LS.Log qualified as Log
import Optics hiding (has)
import Prettyprinter
import Text.Pretty.Simple qualified as Pretty

-- ----------------------------------------------------------------------------
-- Types specific to the renamer phase
-- ----------------------------------------------------------------------------

-- | A rename rule is the same as a 'Rule' but names that occur in the rule
-- are resolved and renamed.
-- This aims to provide common ground for transpilers, s.t. a transpiler can
-- assume a name is already defined, and language ambiguities are resolved.
-- Further, this representation aims to be usable for typechecking.
data RnRule
  = Hornlike RnHornlike
  | TypeDecl RnTypeDecl
  deriving (Eq, Ord, Show, Generic)

type RnBoolStructR = AA.OptionallyLabeledBoolStruct RnRelationalPredicate

-- | Corresponds to 'HornClause2', which is equivalent to @HornClause BoolStructR@.
--
-- We don't seem to require any parameterization.
data RnHornClause = RnHornClause
  { rnHcHead :: RnRelationalPredicate
  , rnHcBody :: Maybe RnBoolStructR
  }
  deriving (Eq, Ord, Show, Generic)

type RnRuleName = RnMultiTerm
type RnEntityType = RnName

data RnHornlike = RnHornlike
  { name :: RnRuleName -- MyInstance
  , super :: Maybe RnTypeSig -- IS A Superclass
  , keyword :: MyToken -- decide / define / means
  , given :: Maybe RnParamText -- a:Applicant, p:Person, l:Lender       -- the type signature of the input
  , giveth :: Maybe RnParamText -- m:Amount,   mp:Principal, mi:Interest -- the type signature of the output
  , upon :: Maybe RnParamText -- second request occurs
  , clauses :: [RnHornClause] -- colour IS blue WHEN fee > $10 ; colour IS green WHEN fee > $20 AND approver IS happy
  , rlabel :: Maybe RuleLabel
  , lsource :: Maybe Text
  , wwhere :: [RnRule]
  , srcref :: Maybe SrcRef
  , defaults :: [RnRelationalPredicate] -- SomeConstant IS 500 ; MentalCapacity TYPICALLY True
  , symtab :: [RnRelationalPredicate] -- SomeConstant IS 500 ; MentalCapacity TYPICALLY True
  }
  deriving (Eq, Ord, Show, Generic)

data RnTypeDecl = RnTypeDecl
  { name :: RnRuleName -- MyInstance
  , super :: Maybe RnTypeSig -- IS A Superclass
  , has :: [RnRule] -- HAS foo :: List Hand, bar :: Optional Restaurant
  , enums :: Maybe RnParamText -- ONE OF rock, paper, scissors (basically, disjoint subtypes)
  , given :: Maybe RnParamText
  , upon :: Maybe RnParamText
  , rlabel :: Maybe RuleLabel
  , lsource :: Maybe Text.Text
  , srcref :: Maybe SrcRef
  , defaults :: [RnRelationalPredicate] -- SomeConstant IS 500 ; MentalCapacity TYPICALLY True
  , symtab :: [RnRelationalPredicate] -- SomeConstant IS 500 ; MentalCapacity TYPICALLY True
  }
  deriving (Eq, Ord, Show, Generic)

data RnTypeSig
  = RnSimpleType LS.ParamType RnEntityType
  | RnInlineEnum LS.ParamType RnParamText
  deriving (Eq, Ord, Show, Generic)

newtype RnParamText = RnParamText
  { mkParamText :: NonEmpty RnTypedMulti
  }
  deriving (Eq, Ord, Show, Generic)

data RnTypedMulti = RnTypedMulti
  { rnTypedMultiExpr :: NonEmpty RnExpr
  , rnTypedMultiTypeSig :: Maybe RnTypeSig
  }
  deriving (Eq, Ord, Show, Generic)

-- | A name is something that can be resolved as either a variable, function, or enum.
data RnName = RnName
  { rnOccName :: OccName
  , rnUniqueId :: Unique
  , rnNameType :: RnNameType
  -- TODO: add the binding scope for scope checking
  -- , rnBindingScope :: BindingScope
  }
  deriving (Eq, Ord, Show, Generic)

data RnNameType
  = RnSelector
  | RnFunction
  | RnVariable
  | RnType
  | RnEnum
  | RnBuiltin
  deriving (Eq, Ord, Show, Generic)

data RnExpr
  = RnExprName RnName
  | RnExprLit RnLit
  deriving (Eq, Ord, Show, Generic)

data RnLit
  = RnInt Integer
  | RnDouble Double
  | RnBool Bool
  | RnString Text
  deriving (Eq, Ord, Show, Generic)

type RnMultiTerm = [RnExpr]

data RnRelationalPredicate
  = -- | Might be something like a record access.
    RnRelationalTerm RnMultiTerm
  | RnConstraint RnMultiTerm LS.RPRel RnMultiTerm
  | RnBoolStructR RnMultiTerm LS.RPRel RnBoolStructR
  | RnNary LS.RPRel [RnRelationalPredicate]
  deriving (Eq, Ord, Show, Generic)

-- ----------------------------------------------------------------------------
-- Typed Errors
-- ----------------------------------------------------------------------------

data RenamerError
  = UnsupportedRule Text Rule
  | UnsupportedRPParamText LS.ParamText
  | UnsupportedUpon LS.ParamText
  | UnknownMultiTerms LS.MultiTerm
  | FixArityFunctionNotFound RnName [RnExpr]
  | ArityErrorLeft !Int RnName [RnExpr]
  | ArityErrorRight !Int RnName [RnExpr]
  | UnexpectedNameNotFound OccName
  | UnexpectedRnNameNotFound RnName
  | InsertNameUnexpectedType RnNameType RnNameType
  | LookupOrInsertNameUnexpectedType RnNameType RnNameType
  | AssertErr AssertionError
  deriving (Show, Eq, Ord)

-- | Validation Errors
data AssertionError
  = -- | List is expected to be empty, but it wasn't!
    -- The 'Text' parameter is a textual representation of the list that not
    -- empty! We could use existentials (and we used to), but that makes deriving
    -- more difficult, so I opted to the simpler solution for now.
    UnexpectedNonEmptyList Text.Text
  | -- | List is expected to be singleton list, but it wasn't!
    -- The 'Text' parameter is a textual representation of the list that not
    -- empty! We could use existentials (and we used to), but that makes deriving
    -- more difficult, so I opted to the simpler solution for now.
    UnexpectedNonSingletonList Text.Text
  | UnexpectedTypeSignature LS.TypedMulti
  deriving (Show, Eq, Ord)

renderRenamerError :: RenamerError -> Text.Text
renderRenamerError = \case
  UnsupportedRule herald r -> herald <> ": Unsupported rule: " <> Text.pack (show r)
  UnsupportedRPParamText rp -> "Received 'RPParamText', we can't handle that yet. Got: " <> Text.pack (show rp)
  UnsupportedUpon pText -> "Clause \"UPON\" is currently unsupported: " <> Text.pack (show pText)
  UnknownMultiTerms mts -> "While scanning a multi term in a top-level DECIDE clause, we encountered an unsupported pattern: " <> Text.pack (show mts)
  FixArityFunctionNotFound name l ->
    "Invariant violated, function " <> Text.pack (show name) <> " reported, but not found in " <> Text.pack (show l)
  ArityErrorLeft expected name l ->
    "Not enough elements in left hand side of function "
      <> Text.pack (show name)
      <> ". Required: "
      <> Text.pack (show expected)
      <> " but got: "
      <> Text.pack (show (length l))
      <> " ("
      <> Text.pack (show l)
      <> ")"
  ArityErrorRight expected name l ->
    "Not enough elements in right hand side of function "
      <> Text.pack (show name)
      <> ". Required: "
      <> Text.pack (show expected)
      <> " but got: "
      <> Text.pack (show (length l))
      <> " ("
      <> Text.pack (show l)
      <> ")"
  -- Scope Error
  UnexpectedNameNotFound occName ->
    "Assumption violated, OccName not found: " <> Text.pack (show occName)
  UnexpectedRnNameNotFound rnName ->
    "Assumption violated, RnName not found: " <> Text.pack (show rnName)
  InsertNameUnexpectedType expected actual ->
    "Invariant violated, trying to insert an incorrect RnNameType for a resolved name. Got: "
      <> Text.pack (show actual)
      <> " but expected: "
      <> Text.pack (show expected)
  LookupOrInsertNameUnexpectedType expected actual ->
    "Invariant violated, trying to insert or lookup an incorrect RnNameType for a resolved name. Got: "
      <> Text.pack (show actual)
      <> " but expected: "
      <> Text.pack (show expected)
  AssertErr err -> renderAssertionError err

renderAssertionError :: AssertionError -> Text.Text
renderAssertionError = \case
  -- Validation Errrors
  UnexpectedNonEmptyList xs ->
    "Expected an empty list, but got: " <> xs
  UnexpectedNonSingletonList xs ->
    "Expected an singleton list, but got: " <> xs
  UnexpectedTypeSignature tm ->
    "Expected no type signature, but got: " <> Text.pack (show tm)

-- ----------------------------------------------------------------------------
-- Renamer Main Types.
-- Defines the 'Renamer' Monad and utility functions.
-- ----------------------------------------------------------------------------

newtype Renamer a = Renamer {runRenamer :: ExceptT RenamerError (StateT Scope IO) a}
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadState Scope, MonadError RenamerError)

type Tracer a = Log.Tracer Renamer a

liftRenamerTracer :: Log.Tracer IO a -> Tracer a
liftRenamerTracer tracer = Log.Tracer $ \msg -> do
  Renamer $ lift $ lift $ Log.runTracer tracer msg

data RenamerResult a
  = RenamerFail RenamerError Scope
  | RenamerSuccess a Scope
  deriving (Show, Eq, Ord)
  deriving (Functor, Traversable, Foldable)

rnResultScope :: RenamerResult a -> Scope
rnResultScope (RenamerFail _ s) = s
rnResultScope (RenamerSuccess _ s) = s

-- ----------------------------------------------------------------------------
-- Log Messages
-- ----------------------------------------------------------------------------

data Log
  = LogNewRnName RnName
  | LogNewFuncInfo RnName FuncInfo
  | LogScopeTableForRule RuleName ScopeTable

instance Pretty Log where
  pretty = \case
    LogNewRnName name ->
      "Renamed name:"
        <+> (prettyMultiTerm $ rnOccName name)
        <+> "with id"
        <+> pretty (rnUniqueId name)
        <+> "with type"
        <+> pretty (rnNameType name)
    LogNewFuncInfo name funcInfo ->
      "New Function Information for" <+> prettyMultiTerm (rnOccName name) <> ":" <+> pretty (_funcArity funcInfo)
    LogScopeTableForRule name sc ->
      "Renaming Rule with name"
        <+> prettyMultiTerm name
        <+> "with"
        <+> pretty (Pretty.pShow sc)

prettyMultiTerm :: (Traversable f) => f LS.MTExpr -> Doc ann
prettyMultiTerm = list . Foldable.toList . fmap prettyMT

prettyMT :: LS.MTExpr -> Doc ann
prettyMT (LS.MTT t) = pretty t
prettyMT (LS.MTI int) = pretty int
prettyMT (LS.MTF float) = pretty float
prettyMT (LS.MTB boolean) = pretty boolean

instance Pretty RnNameType where
  pretty = \case
    RnSelector -> "Selector"
    RnFunction -> "Function"
    RnVariable -> "Variable"
    RnType -> "Type"
    RnEnum -> "Enum"
    RnBuiltin -> "Builtin"

-- ----------------------------------------------------------------------------
-- Scope tables
-- ----------------------------------------------------------------------------

type Unique = Int

-- | An unresolved name as it occurs in the original source.
type OccName = NonEmpty LS.MTExpr

mkSimpleOccName :: Text -> OccName
mkSimpleOccName = NE.singleton . LS.MTT

data FuncInfo = FuncInfo
  { _funcArity :: (Int, Int)
  -- ^ Arity of a function. The first component means how many parameters
  -- are allowed before the function, the second component how many parameters
  -- are allowed afterwards.
  -- For example @(1, 1)@ is a simple infix function of the form @x f y@ where @f@
  -- is the name of the function.
  }
  deriving (Eq, Ord, Show)

data Scope = Scope
  { _scScopeTable :: ScopeTable
  , _scUniqueSupply :: Unique
  -- ^ next unique value that we can use
  }
  deriving (Eq, Ord, Show)

-- | A 'ScopeTable' keeps tab on the variables and functions that occur in a
-- program.
--
-- Invariant:
--
-- Every name that gets resolved to an 'RnName' with 'RnNameType' being
-- 'RnFunction' should have additional 'FuncInfo' in '_stFunction'.
data ScopeTable = ScopeTable
  { _stVariables :: Map OccName RnName
  -- ^ all names currently in scope
  , _stFunction :: Map RnName FuncInfo
  -- ^ additional information for resolved functions
  }
  deriving (Eq, Ord, Show)

unionScopeTable :: ScopeTable -> ScopeTable -> ScopeTable
unionScopeTable tbl1 tbl2 =
  ScopeTable
    { _stVariables = Map.union tbl1._stVariables tbl2._stVariables
    , _stFunction = Map.union tbl1._stFunction tbl2._stFunction
    }

differenceScopeTable :: ScopeTable -> ScopeTable -> ScopeTable
differenceScopeTable tbl1 tbl2 =
  ScopeTable
    { _stVariables = Map.difference tbl1._stVariables tbl2._stVariables
    , _stFunction = Map.difference tbl1._stFunction tbl2._stFunction
    }

emptyScopeTable :: ScopeTable
emptyScopeTable =
  ScopeTable
    { _stVariables = Map.empty
    , _stFunction = Map.empty
    }

makeFieldsNoPrefix 'Scope
makeFieldsNoPrefix 'ScopeTable
makeFieldsNoPrefix 'FuncInfo

emptyScope :: Scope
emptyScope =
  Scope
    { _scScopeTable = emptyScopeTable
    , _scUniqueSupply = 0
    }

newUnique :: Renamer Unique
newUnique = do
  u <- use scUniqueSupply
  modifying' scUniqueSupply (+ 1)
  pure u

-- | Lookup the given name in the 'ScopeTable'.
lookupName :: OccName -> Renamer (Maybe RnName)
lookupName occName =
  use (scScopeTable % stVariables % at occName)

-- | Look up the name associated with a given 'OccName' and assert it has
-- the correct 'RnNameType'.
--
-- This can be used when the 'OccName' *must* be present in the 'ScopeTable',
-- otherwise an assumption has been violated.
-- If the name cannot be found, or the name is not of the expected type, we
-- throw an exception.
lookupExistingName :: OccName -> RnNameType -> Renamer RnName
lookupExistingName occName nameType = do
  mRnName <- lookupName occName
  case mRnName of
    Nothing -> throwError $ UnexpectedNameNotFound occName
    Just name
      | name.rnNameType == nameType -> pure name
      | otherwise ->
          throwError $ InsertNameUnexpectedType (rnNameType name) nameType

-- | Either inserts a new name of the given type, or checks that the name
-- is already in scope with the given type.
--
-- Fails if the name type does not match.
lookupOrInsertName :: Tracer Log -> OccName -> RnNameType -> Renamer RnName
lookupOrInsertName tracer occName nameType =
  lookupName occName >>= \case
    Nothing -> insertName tracer occName nameType
    Just name
      | rnNameType name == nameType -> pure name
      | otherwise ->
          throwError $ LookupOrInsertNameUnexpectedType (rnNameType name) nameType

-- | Insert an occurrence name into the current 'ScopeTable'.
-- The new 'OccName' will overwrite (shadow?) any existing names.
insertName :: Tracer Log -> OccName -> RnNameType -> Renamer RnName
insertName tracer occName nameType = do
  n <- newUnique
  let
    rnName =
      RnName
        { rnUniqueId = n
        , rnOccName = occName
        , rnNameType = nameType
        }
  traceWith tracer $ LogNewRnName rnName
  assign'
    ( scScopeTable
        % stVariables
        % at occName
    )
    (Just rnName)
  pure rnName

-- | Insert an function meta information into the current 'ScopeTable'.
insertFunction :: Tracer Log -> RnName -> FuncInfo -> Renamer ()
insertFunction tracer rnFnName funcInfo = do
  traceWith tracer $ LogNewFuncInfo rnFnName funcInfo
  assign'
    ( scScopeTable
        % stFunction
        % at rnFnName
    )
    (Just funcInfo)

lookupExistingFunction :: RnName -> Renamer FuncInfo
lookupExistingFunction rnFnName = do
  funcInfoM <- use (scScopeTable % stFunction % at rnFnName)
  case funcInfoM of
    Nothing -> throwError $ UnexpectedRnNameNotFound rnFnName
    Just funcInfo -> pure funcInfo

-- | Execute a 'Renamer' action, but record which 'RnName's and 'FuncInfo's
-- were introduced during this action.
--
-- Note, this operation is rather expensive, so use it with caution!
recordScopeTable :: Renamer a -> Renamer (a, ScopeTable)
recordScopeTable act = do
  orig <- use scScopeTable
  a <- act
  origWithNew <- use scScopeTable
  pure (a, origWithNew `differenceScopeTable` orig)

recordScopeTable_ :: Renamer a -> Renamer ScopeTable
recordScopeTable_ = fmap snd . recordScopeTable

-- ----------------------------------------------------------------------------
-- Helper types for local context
-- ----------------------------------------------------------------------------

-- | Intermediate context when renaming a '[MultiTerm]'.
data MultiTermContext = MultiTermContext
  { _multiTermContextInSelector :: Bool
  -- ^ Did the previous 'MultiTerm' introduce a selector chain?
  -- A selector chain is introduced, if the multi term has a genitive suffix.
  -- For example: @[MTT "book's", MTT "title"]@, when @"title"@ is renamed,
  -- the 'multiTermContextInSelector' is set expected to be to 'True', so that
  -- we can infer that @"title"@ is a 'RnSelector'.
  , _multiTermContextFunctionCall :: Maybe RnName
  -- ^ While renaming a 'MultiTerm', did we encounter a function application?
  -- If so, we want to fix the call convention from infix/postfix to prefix!
  }

makeFields 'MultiTermContext

inSelectorContext :: MultiTermContext -> MultiTermContext
inSelectorContext mtc = mtc & inSelector .~ True

notInSelectorContext :: MultiTermContext -> MultiTermContext
notInSelectorContext mtc = mtc & inSelector .~ False

-- ----------------------------------------------------------------------------
-- Top Level Definitions
-- ----------------------------------------------------------------------------

renameRuleTopLevel :: Rule -> IO ()
renameRuleTopLevel rule = do
  TL.putStrLn $ Pretty.pShow rule
  renamerResult <- runRenamerFor (liftRenamerTracer Log.prettyTracer) (MkSolo rule)
  TL.putStrLn $ Pretty.pShow $ rnResultScope renamerResult
  case renamerResult of
    RenamerFail err _ -> Text.putStrLn $ renderRenamerError err
    RenamerSuccess (MkSolo rnRules) _ -> TL.putStrLn $ Pretty.pShow rnRules

runRenamerFor :: (Traversable f) => Tracer Log -> f Rule -> IO (RenamerResult (f RnRule))
runRenamerFor tracer rules = do
  (resE, scope) <- State.runStateT (Except.runExceptT (runRenamer $ renameRules tracer rules)) emptyScope
  pure $ case resE of
    Left err -> RenamerFail err scope
    Right rnRules -> RenamerSuccess rnRules scope

-- ----------------------------------------------------------------------------
-- Resolve functions and their respective arities
-- ----------------------------------------------------------------------------

-- | Scan the structure of 'Rule' to find declarations that affect other rules.
--
-- We identify the following names that can be referenced from other rules:
--
-- 1. Functions and variables in the head of 'HornClauses'.
-- 2. Names declared in 'GIVETH' clauses.
-- 3. Types and selectors defined via 'DEFINE'
--
-- 'scanRule' produces a 'ScopeTable' of items that are exported from this rule.
-- Further, 'scanRule' may only *add* new names to the 'ScopeTable'.
scanRule :: Tracer Log -> Rule -> Renamer ScopeTable
scanRule tracer rule@Rule.Hornlike{} = do
  scanGivens tracer rule.given
  exports <- recordScopeTable_ $ do
    scanGiveths tracer rule.giveth
    traverse_ (scanHornClause tracer) rule.clauses
  pure exports
scanRule _ r@Rule.Regulative{} = throwError $ UnsupportedRule "scanRule" r
scanRule _ r@Rule.Constitutive{} = throwError $ UnsupportedRule "scanRule" r
scanRule tracer rule@Rule.TypeDecl{} = do
  traverse_ (scanTypeSignature tracer) rule.super
  scanEnums tracer rule.enums
  scanGivens tracer rule.given
  traverse_ (scanRule tracer) rule.has
  scanTypeDeclName tracer rule.name
  typeScope <- use scScopeTable
  pure typeScope
scanRule _ r@Rule.Scenario{} = throwError $ UnsupportedRule "scanRule" r
scanRule _ r@Rule.DefNameAlias{} = throwError $ UnsupportedRule "scanRule" r
scanRule _ r@Rule.DefTypically{} = throwError $ UnsupportedRule "scanRule" r
scanRule _ r@Rule.RuleAlias{} = throwError $ UnsupportedRule "scanRule" r
scanRule _ r@Rule.RuleGroup{} = throwError $ UnsupportedRule "scanRule" r
scanRule _ r@Rule.RegFulfilled{} = throwError $ UnsupportedRule "scanRule" r
scanRule _ r@Rule.RegBreach{} = throwError $ UnsupportedRule "scanRule" r
scanRule _ r@Rule.NotARule{} = throwError $ UnsupportedRule "scanRule" r

-- | Scan a 'LS.HornClause2' for declarations of variables and functions.
scanHornClause :: Tracer Log -> LS.HornClause2 -> Renamer ()
scanHornClause tracer hc = do
  scanDecideHeadClause tracer hc.hHead

-- | Scan the head of relational predicates that occur in
-- the head of @DECIDE clauses@, e.g. @DECIDE foo IS bar@.
--
-- We detect the occurrence of @IS@ and treat it in a special way,
-- and in the case of a multi-term, we use 'scanDecideMultiTerm'
-- which allows the *introduction* of variables.
scanDecideHeadClause :: Tracer Log -> LS.RelationalPredicate -> Renamer ()
scanDecideHeadClause tracer = \case
  LS.RPParamText pText -> throwError $ UnsupportedRPParamText pText --  $ "Received 'RPParamText', we can't handle that yet. Got: " <> show pText
  LS.RPMT mt -> scanDecideMultiTerm tracer mt
  LS.RPConstraint lhs _predicate _rhs -> do
    scanDecideMultiTerm tracer lhs
  LS.RPBoolStructR lhs _predicate _rhs -> do
    scanDecideMultiTerm tracer lhs
  LS.RPnary LS.RPis (lhs : _rhs) -> do
    -- When the assignment has multiple complicated relational predicates,
    -- it is translated to this 'RPNary'. Then the first element is before the 'IS'
    -- and the rest after.
    -- Example:
    -- @f x IS SUM(x, x, x)@
    -- is parsed to @RPnary RPis [[f, x], [RPnary RPSum [x, x, x]]]@
    -- ignoring some details.
    -- Thus, we scan the first item of 'IS' predicates.
    scanDecideHeadClause tracer lhs
  LS.RPnary _predicate _rhs -> do
    pure ()

-- | Scan a top-level occurrence of 'LS.MultiTerm'.
--
-- This is slightly special, as this may be the definition site of functions.
--
-- For now, we accept the following 'LS.MultiTerm''s for function definitions:
--
-- * @f x1 x2 ...@: function @f@ in prefix with a variable number of parameters @x1, x2, ...@
-- * @x1 x2 ... f@: function @f@ in postfix with a variable number of parameters @x1, x2, ...@
-- * @x1 x2 ... f y1 y2 ...@: function @f@ in infix with a variable number of prefix
--   parameters @x1, x2, ...@ and a variable numbers of postfix parameters @y1 y2 ...@.
--
-- Note, to be recognized as a function, variables must have been specified by 'GIVEN'
-- clauses and the function name must be unbound in its current scope.
--
-- Additionally, we recognize the following forms:
--
-- * @f's x's y's z@: An attribute path from variable @f@ to something that has a @z@ attribute.
-- * @x@: a variable, might be bound ad-hoc
--
-- Note, this doesn't accept literals such as '42' or '3.5f' or True or False.
scanDecideMultiTerm :: Tracer Log -> LS.MultiTerm -> Renamer ()
scanDecideMultiTerm tracer mt = do
  scopeTable <- use scScopeTable
  case mt of
    attrs
      | Just (obj, objAttrs) <- toObjectPath attrs -> do
          -- DECIDE x IS ...
          -- DECIDE x's y's z IS ...
          _ <- lookupOrInsertName tracer (mkSimpleOccName obj) RnVariable
          traverse_ (\attr -> RnExprName <$> lookupOrInsertName tracer (mkSimpleOccName attr) RnSelector) objAttrs
    fnDecl
      | Just (fnOccName, preArgs, postArgs) <- scanForFunctionDecl scopeTable fnDecl -> do
          rnF <- lookupOrInsertName tracer fnOccName RnFunction
          insertFunction tracer rnF (FuncInfo{_funcArity = (preArgs, postArgs)})
    unknownPattern -> throwError $ UnknownMultiTerms unknownPattern

-- throwError $ "While scanning a multi term in a top-level DECIDE clause, we encountered an unsupported pattern: " <> show unknownPattern

-- | Check whether this could be a function like structure.
--
-- It might be, if all the variables are already bound, and the function name
-- is unbound or already known as a function.
--
-- ANDRES: It surprises me that we do not have to check whether
-- the arity matches.
scanForFunctionDecl :: ScopeTable -> LS.MultiTerm -> Maybe (OccName, Int, Int)
scanForFunctionDecl scopeTable mts = do
  let
    (preVars, fnWithArgs) = List.break (not . isVariable) mts
  (fnTerm, postVars) <- case fnWithArgs of
    [] -> Nothing
    (LS.MTT fnTerm : postTerms) ->
      if all isVariable postTerms
        then Just (fnTerm, postTerms)
        else Nothing
    _terms -> Nothing

  pure (mkSimpleOccName fnTerm, length preVars, length postVars)
 where
  isVariable (LS.MTT x) = case Map.lookup (mkSimpleOccName x) (scopeTable ^. stVariables) of
    Nothing -> False
    Just rnName
      | rnName.rnNameType == RnVariable -> True
      | otherwise -> False
  isVariable _ = False

scanGiveths ::
  Tracer Log ->
  Maybe LS.ParamText ->
  Renamer ()
scanGiveths = scanGivens

scanEnums ::
  Tracer Log ->
  Maybe LS.ParamText ->
  Renamer ()
scanEnums tracer = traverse_ (scanGivenInlineEnumParamText tracer)

scanGivens ::
  Tracer Log ->
  Maybe LS.ParamText ->
  Renamer ()
scanGivens _ Nothing = pure ()
scanGivens tracer (Just givens) = do
  traverse_ (scanGiven tracer) givens

scanGiven :: Tracer Log -> LS.TypedMulti -> Renamer ()
scanGiven tracer (mtExprs, typeSig) = do
  scanGivenMultiTerm tracer mtExprs
  traverse_ (scanTypeSignature tracer) typeSig

scanGivenMultiTerm :: Tracer Log -> NonEmpty LS.MTExpr -> Renamer ()
scanGivenMultiTerm tracer mtExprs = do
  mt <- assertSingletonMultiTerm mtExprs
  void $ insertName tracer (pure mt) RnVariable

scanTypeSignature ::
  Tracer Log ->
  LS.TypeSig ->
  Renamer ()
scanTypeSignature tracer sig = case sig of
  LS.SimpleType _pType entityType -> do
    scanEntityType entityType
  LS.InlineEnum _pType paramText -> do
    -- TODO: error handling, would we accept an enum such as `a IS ONE OF 1, 2, 3`?
    -- Only if we treat them as text, which might be confusing, as user might infer
    -- this to be some kind of type checked number type.
    scanGivenInlineEnumParamText tracer paramText
 where
  scanEntityType :: LS.EntityType -> Renamer ()
  scanEntityType eType =
    -- This can either refer to an existing entity type, or define a new,
    -- ad-hoc, entity type. We just assume that multiple ad-hoc definitions
    -- of the same name in the same scope must be consistent.
    void $ lookupOrInsertName tracer (mkSimpleOccName eType) RnType

-- | Scan for names in the enum definition.
--
-- Why not reuse 'scanGivens'? It is basically the same type!
-- Well, we don't handle arbitrary nested type signatures.
-- In fact, it is a bit dubious we have them at all!
-- The following seems to be possible in theory:
--
-- @
-- GIVEN x IS ONE OF foo IS ONE OF foobar, foobaz
-- @
--
-- What would that suppose to mean? So, for now, we only allow enum definitions
-- to be of the following form:
--
-- @
-- GIVEN x IS ONE OF foo, bar, `foo baz`
-- @
--
-- This means 'x' is one of three possible enum values 'foo', 'bar'
-- and 'foo baz'.
--
-- TODO: We reuse this for Type declarations as well, are nested type signatures allowed in this case?
-- Even in that case, since 'TypeDecl''s 'has' is a list of 'TypeDecl''s, it seems like
-- there is no arbitrary nesting.
--
-- ANDRES: I think the fact that type signatures allow nested
-- type signatures is a shortcoming of the input syntax that should
-- be fixed at that level.
scanGivenInlineEnumParamText :: Tracer Log -> LS.ParamText -> Renamer ()
scanGivenInlineEnumParamText tracer params = do
  let
    scanEach tm = do
      mt <- assertNoTypeSignature tm
      enumNames <- traverse (\t -> insertName tracer (NE.singleton t) RnEnum) mt
      pure $
        RnTypedMulti
          { rnTypedMultiExpr = fmap RnExprName enumNames
          , rnTypedMultiTypeSig = Nothing
          }

  traverse_ scanEach params

scanTypeDeclName :: Tracer Log -> RuleName -> Renamer ()
scanTypeDeclName tracer mtexprs = do
  mt <- assertSingletonMultiTerm mtexprs
  void $ insertName tracer (NE.singleton mt) RnType

-- ----------------------------------------------------------------------------
-- Renamer passes
-- ----------------------------------------------------------------------------

-- |
-- Lexical Scoping rules for hornlike rules:
--
-- GIVETH's are global
-- GIVEN's are local
-- DECIDE head term in "IS" clauses is global
renameRules :: (Traversable f) => Tracer Log -> f Rule -> Renamer (f RnRule)
renameRules tracer rules = do
  rulesWithLocalDefs <-
    traverse
      ( \r -> do
          prev <- use scScopeTable
          exportedScope <- scanRule tracer r
          fullRuleScope <- use scScopeTable
          assign' scScopeTable (prev `unionScopeTable` exportedScope)
          pure (r, fullRuleScope)
      )
      rules
  traverse
    ( \(r, ruleScope) -> do
        orig <- use scScopeTable
        modifying' scScopeTable (`unionScopeTable` ruleScope)
        rnRule <- renameRule tracer r
        assign' scScopeTable orig
        pure rnRule
    )
    rulesWithLocalDefs

renameRule :: Tracer Log -> Rule -> Renamer RnRule
renameRule tracer rule@Rule.Hornlike{} = do
  traceWith tracer . LogScopeTableForRule rule.name =<< use scScopeTable
  super <- traverse renameTypeSignature rule.super
  given <- renameGivens rule.given
  giveth <- renameGiveths rule.giveth
  wwhere <- renameLocalRules tracer rule.wwhere
  upon <- renameUpons rule.upon
  defaults <- assertEmptyList rule.defaults
  symtab <- assertEmptyList rule.symtab
  clauses <- traverse (renameHornClause tracer) rule.clauses
  name <- renameMultiTerm tracer rule.name
  pure $
    Hornlike
      RnHornlike
        { name
        , super
        , keyword = rule.keyword
        , given
        , giveth
        , upon
        , clauses
        , rlabel = rule.rlabel
        , lsource = rule.lsource
        , wwhere
        , srcref = rule.srcref
        , defaults
        , symtab
        }
renameRule _ r@Rule.Regulative{} = throwError $ UnsupportedRule "renameRule" r
renameRule _ r@Rule.Constitutive{} = throwError $ UnsupportedRule "renameRule" r
renameRule tracer rule@Rule.TypeDecl{} = do
  traceWith tracer . LogScopeTableForRule rule.name =<< use scScopeTable
  super <- traverse renameTypeSignature rule.super
  defaults <- assertEmptyList rule.defaults
  enums <- renameEnums rule.enums
  given <- renameGivens rule.given
  upon <- renameUpons rule.upon
  symtab <- assertEmptyList rule.symtab
  has <- traverse (renameRule tracer) rule.has
  name <- renameTypeDeclName rule.name
  pure $
    TypeDecl
      RnTypeDecl
        { name
        , super
        , has
        , enums
        , given
        , upon
        , rlabel = rule.rlabel
        , lsource = rule.lsource
        , srcref = rule.srcref
        , defaults
        , symtab
        }
renameRule _ r@Rule.Scenario{} = throwError $ UnsupportedRule "renameRule" r
renameRule _ r@Rule.DefNameAlias{} = throwError $ UnsupportedRule "renameRule" r
renameRule _ r@Rule.DefTypically{} = throwError $ UnsupportedRule "renameRule" r
renameRule _ r@Rule.RuleAlias{} = throwError $ UnsupportedRule "renameRule" r
renameRule _ r@Rule.RuleGroup{} = throwError $ UnsupportedRule "renameRule" r
renameRule _ r@Rule.RegFulfilled{} = throwError $ UnsupportedRule "renameRule" r
renameRule _ r@Rule.RegBreach{} = throwError $ UnsupportedRule "renameRule" r
renameRule _ r@Rule.NotARule{} = throwError $ UnsupportedRule "renameRule" r

renameLocalRules :: Tracer Log -> [Rule] -> Renamer [RnRule]
renameLocalRules = renameRules

renameTypeDeclName :: RuleName -> Renamer RnRuleName
renameTypeDeclName mtexprs = do
  mt <- assertSingletonMultiTerm mtexprs
  rnTyName <- lookupExistingName (NE.singleton mt) RnType
  pure [RnExprName rnTyName]

renameUpons ::
  Maybe LS.ParamText ->
  Renamer (Maybe RnParamText)
renameUpons Nothing = pure Nothing
renameUpons (Just xs) = throwError $ UnsupportedUpon xs

renameGiveths ::
  Maybe LS.ParamText ->
  Renamer (Maybe RnParamText)
renameGiveths = renameGivens

renameEnums ::
  Maybe LS.ParamText ->
  Renamer (Maybe RnParamText)
renameEnums = traverse renameGivenInlineEnumParamText

renameGivens ::
  Maybe LS.ParamText ->
  Renamer (Maybe RnParamText)
renameGivens Nothing = pure Nothing
renameGivens (Just givens) = do
  rnGivens <- traverse renameGiven givens
  pure $ Just $ RnParamText rnGivens

renameGiven :: LS.TypedMulti -> Renamer RnTypedMulti
renameGiven (mtExprs, typeSig) = do
  rnMtExprs <- renameGivenMultiTerm mtExprs
  rnTypeSig <- traverse renameTypeSignature typeSig
  pure $ RnTypedMulti (NE.singleton $ RnExprName rnMtExprs) rnTypeSig

renameGivenMultiTerm :: NonEmpty LS.MTExpr -> Renamer RnName
renameGivenMultiTerm mtExprs = do
  mt <- assertSingletonMultiTerm mtExprs
  lookupExistingName (pure mt) RnVariable

renameTypeSignature ::
  LS.TypeSig ->
  Renamer RnTypeSig
renameTypeSignature sig = case sig of
  LS.SimpleType pType entityType -> do
    rnEntityType <- renameEntityType entityType
    pure $ RnSimpleType pType rnEntityType
  LS.InlineEnum pType paramText -> do
    -- TODO: error handling, would we accept an enum such as `a IS ONE OF 1, 2, 3`?
    -- Only if we treat them as text, which might be confusing, as user might infer
    -- this to be some kind of type checked number type.
    rnParamText <- renameGivenInlineEnumParamText paramText
    pure $ RnInlineEnum pType rnParamText
 where
  renameEntityType :: LS.EntityType -> Renamer RnEntityType
  renameEntityType eType =
    lookupExistingName (mkSimpleOccName eType) RnType

renameGivenInlineEnumParamText :: LS.ParamText -> Renamer RnParamText
renameGivenInlineEnumParamText params = do
  let
    renameEach tm = do
      mt <- assertNoTypeSignature tm
      enumNames <- traverse (\t -> lookupExistingName (NE.singleton t) RnEnum) mt
      pure $
        RnTypedMulti
          { rnTypedMultiExpr = fmap RnExprName enumNames
          , rnTypedMultiTypeSig = Nothing
          }

  rnParams <- traverse renameEach params
  pure $ RnParamText rnParams

renameHornClause :: Tracer Log -> LS.HornClause2 -> Renamer RnHornClause
renameHornClause tracer hc = do
  rnHead <- renameRelationalPredicate tracer hc.hHead
  rnBody <- traverse (renameBoolStruct tracer) hc.hBody
  pure $
    RnHornClause
      { rnHcHead = rnHead
      , rnHcBody = rnBody
      }

renameRelationalPredicate :: Tracer Log -> LS.RelationalPredicate -> Renamer RnRelationalPredicate
renameRelationalPredicate tracer = \case
  LS.RPParamText pText ->
    throwError $ UnsupportedRPParamText pText
  LS.RPMT mt -> RnRelationalTerm <$> renameMultiTerm tracer mt
  LS.RPConstraint lhs relationalPredicate rhs -> do
    rnLhs <- renameMultiTerm tracer lhs
    rnRhs <- renameMultiTerm tracer rhs
    pure $ RnConstraint rnLhs relationalPredicate rnRhs
  LS.RPBoolStructR lhs relationalPredicate rhs -> do
    rnLhs <- renameMultiTerm tracer lhs
    rnRhs <- renameBoolStruct tracer rhs
    pure $ RnBoolStructR rnLhs relationalPredicate rnRhs
  LS.RPnary relationalPredicate rhs -> do
    rnRhs <- traverse (renameRelationalPredicate tracer) rhs
    pure $ RnNary relationalPredicate rnRhs

renameBoolStruct :: Tracer Log -> LS.BoolStructR -> Renamer RnBoolStructR
renameBoolStruct tracer = \case
  AA.Leaf p -> AA.Leaf <$> renameRelationalPredicate tracer p
  AA.All lbl cs -> do
    rnBoolStruct <- traverse (renameBoolStruct tracer) cs
    pure $ AA.All lbl rnBoolStruct
  AA.Any lbl cs -> do
    rnBoolStruct <- traverse (renameBoolStruct tracer) cs
    pure $ AA.Any lbl rnBoolStruct
  AA.Not cs -> AA.Not <$> (renameBoolStruct tracer) cs

-- | Rename a 'LS.MultiTerm' and turn each 'LS.MTExpr' into a 'RnExpr'.
--
-- Renaming a list of 'LS.MTExpr' cannot be done without keeping intermediate
-- state. Take for example this input:
--
-- @[MTT "x's", MTT "y's", MTT "z"]@
--
-- In this example, @x's@ and @y's@ can be relatively unambiguously renamed,
-- but @z@ is tricky. Without context, @z@ could be a variable, a string
-- constant... or perhaps even a function name! No way to tell, as the
-- text fragment of the 'LS.MultiTerm' is ambiguous.
--
-- To resolve this ambiguity, we keep track of intermediate state in
-- 'MultiTermContext'. Using this intermediate state, we can clearly
-- disambiguate @z@ as being a 'RnSelector', as it is the last element
-- of a "selector chain".
--
-- Further, we analyze whether we encounter a function application. If so,
-- we fix the function application to its prefix form.
-- For example, @[MTT "x", MTT "f"]@ will be changed @[MTT "f", MTT "x"]@,
-- if and only if @"f"@ is a known function variable in scope with associated
-- arity information.
renameMultiTerm :: Tracer Log -> LS.MultiTerm -> Renamer RnMultiTerm
renameMultiTerm tracer multiTerms = do
  (reversedRnMultiTerms, ctx) <-
    foldM
      ( \(results, state) mt -> do
          (rnExpr, newState) <- renameMultiTermExpression tracer state mt
          pure (rnExpr : results, newState)
      )
      ([], initialMultiTermContext)
      multiTerms
  let
    rnMultiTerms = reverse reversedRnMultiTerms
  fixFixity ctx rnMultiTerms
 where
  fixFixity ctx rnMultiTerms = case ctx ^. functionCall of
    Nothing -> pure rnMultiTerms
    Just fnName -> do
      funcInfo <- lookupExistingFunction fnName
      let
        (preNum, postNum) = funcInfo ^. funcArity
      (lhs, fnExpr, rhs) <- findFunctionApplication fnName rnMultiTerms
      (leftNonArgs, leftArgs) <- processLhs fnName preNum lhs
      (rightNonArgs, rightArgs) <- processRhs fnName postNum rhs
      pure $ reverse leftNonArgs <> [fnExpr] <> leftArgs <> rightArgs <> rightNonArgs

  findFunctionApplication fnName rnMultiTerms = do
    let
      (preArgs, postArgsWithName) = List.break (== (RnExprName fnName)) rnMultiTerms
    case postArgsWithName of
      [] -> throwError $ FixArityFunctionNotFound fnName rnMultiTerms
      -- throwError "fixFixity: Invariant violated, function name reported, but none found."
      (fnExpr : postArgs) -> pure (preArgs, fnExpr, postArgs)

  processLhs name n lhs = do
    case safeSplitAt n (reverse lhs) of
      Nothing ->
        throwError $ ArityErrorLeft n name lhs
      Just (args, nonArgs) -> pure (reverse nonArgs, reverse args)

  processRhs name n rhs = do
    case safeSplitAt n rhs of
      Nothing ->
        throwError $ ArityErrorRight n name rhs
      Just (nonArgs, args) -> pure (nonArgs, args)

  initialMultiTermContext =
    MultiTermContext
      { _multiTermContextInSelector = False
      , _multiTermContextFunctionCall = Nothing
      }

-- | Rename a single 'LS.MTExpr' to a 'RnExpr'.
renameMultiTermExpression :: Tracer Log -> MultiTermContext -> LS.MTExpr -> Renamer (RnExpr, MultiTermContext)
renameMultiTermExpression tracer ctx = \case
  -- TODO: this could be an expression such as "2+2" (for whatever reason), so perhaps
  -- we need to parse this further. Allegedly, we also want to support
  -- expressions nested into one csv-cell, for example:
  --
  -- >>> MT "f x y"
  --
  -- where 'f' is a function.
  -- We ignore this for now, though.
  LS.MTT name -> case isGenitive name of
    Nothing -> do
      let
        ctx' = notInSelectorContext ctx
      lookupName (mkSimpleOccName name) >>= \case
        Just rnName -> do
          let
            ctx'' =
              if rnName.rnNameType == RnFunction
                then ctx' & functionCall ?~ rnName
                else ctx'
          pure (RnExprName rnName, ctx'')
        Nothing
          | Just literal <- isTextLiteral name ->
              pure (RnExprLit $ RnString literal, ctx')
          | isL4BuiltIn name -> do
              -- ANDRES: I'm not convinced that built-ins should be renamed, and
              -- if we already detected that they're built-ins, perhaps we should
              -- just use a different dedicated constructor for this case.
              rnName <- RnExprName <$> rnL4Builtin tracer name
              pure (rnName, ctx')
          | ctx ^. inSelector -> do
              rnName <- RnExprName <$> insertName tracer (mkSimpleOccName name) RnSelector
              pure (rnName, ctx')
          | otherwise -> do
              -- If this is not a selector, or a known variable, we infer
              -- it is a string type. This is ok, because users can
              -- disambiguate variables and string literals by enclosing the
              -- literal in quotes, e.g. @"This is a string"@
              --
              pure (RnExprLit $ RnString name, ctx')
    Just nameSelector -> do
      -- Is this name known already?
      -- If not, we assume this is a selector we haven't encountered before.
      -- Take for example this function:
      --
      -- @
      --   GIVEN x DECIDE f x IS x's y's z
      -- @
      --
      -- Then 'y' and 'z' are anonymous selectors for 'x'.
      rnName <- fromMaybeM (lookupOrInsertName tracer (mkSimpleOccName nameSelector) RnSelector) (lookupName (mkSimpleOccName nameSelector))
      pure (RnExprName rnName, inSelectorContext ctx)
  LS.MTI int -> pure (RnExprLit $ RnInt int, notInSelectorContext ctx)
  LS.MTF double -> pure (RnExprLit $ RnDouble double, notInSelectorContext ctx)
  LS.MTB bool -> pure (RnExprLit $ RnBool bool, notInSelectorContext ctx)
 where
  -- There is no doubt this is a text literal, if it is enclosed in quotes.
  -- Strips away the quotes.
  isTextLiteral t = do
    ('"', t') <- uncons t
    (t'', '"') <- unsnoc t'
    pure t''

-- ----------------------------------------------------------------------------
-- Builtins
-- ----------------------------------------------------------------------------

isL4BuiltIn :: Text -> Bool
isL4BuiltIn name = Set.member name (Set.fromList l4Builtins)

rnL4Builtin :: Tracer Log -> Text -> Renamer RnName
rnL4Builtin tracer name = do
  lookupOrInsertName tracer (mkSimpleOccName name) RnBuiltin

l4Builtins :: [Text]
l4Builtins = [oTHERWISE]

oTHERWISE :: Text
oTHERWISE = "OTHERWISE"

-- ----------------------------------------------------------------------------
-- Assertions and helpers.
-- These allow us to express expectations and clean up the code
-- by giving us exactly what we need, followed by throwErroring if assumptions are violated.
-- ----------------------------------------------------------------------------

assertSingletonMultiTerm :: (Show (f LS.MTExpr), Foldable f) => f LS.MTExpr -> Renamer LS.MTExpr
assertSingletonMultiTerm xs = case Foldable.toList xs of
  [x] -> pure x
  _ -> throwError $ AssertErr $ UnexpectedNonSingletonList (Text.pack $ show xs)

assertNoTypeSignature :: LS.TypedMulti -> Renamer (NonEmpty LS.MTExpr)
assertNoTypeSignature tm@(_, Just _) = throwError $ AssertErr $ UnexpectedTypeSignature tm
assertNoTypeSignature (mtt, Nothing) = do
  pure mtt

-- | If we can't handle renaming certain list of things, we just hope that
-- the parser doesn't give us a list with any elements.
-- We throwError if the list is not @'null'@.
assertEmptyList :: (Show a) => [a] -> Renamer [b]
assertEmptyList [] = pure []
assertEmptyList xs = throwError $ AssertErr $ UnexpectedNonEmptyList (Text.pack $ show xs)

-- ----------------------------------------------------------------------------
-- Helper utils non specific to the renamer.
-- Should be moved out into a general purpose function.
-- ----------------------------------------------------------------------------

-- | Given a 'LS.MultiTerm', check whether it has the form of an attribute
-- selector.
--
-- >>> :set -XOverloadedStrings
--
-- >>> toObjectPath [LS.MTT "x's", LS.MTT "z"]
-- Just ("x",["z"])
--
-- Special case when the last text fragment still has a "'s".
-- Should we allow this?
--
-- >>> toObjectPath [LS.MTT "x's", LS.MTT "z's"]
-- Just ("x",["z's"])
--
-- >>> toObjectPath [LS.MTT "x's", LS.MTT "y's", LS.MTT "z"]
-- Just ("x",["y","z"])
--
-- >>> toObjectPath [LS.MTT "f", LS.MTT "x", LS.MTT "y"]
-- Nothing
--
-- >>> toObjectPath []
-- Nothing
--
-- >>> toObjectPath [LS.MTT "y's"]
-- Nothing
--
-- >>> toObjectPath [LS.MTT "y"]
-- Just ("y",[])
toObjectPath :: LS.MultiTerm -> Maybe (Text, [Text])
toObjectPath [] = Nothing
toObjectPath [LS.MTT varName] = case isGenitive varName of
  Nothing -> Just (varName, [])
  Just _ -> Nothing
toObjectPath (varNameInGenitive : attrs) = do
  varName <- LS.isMtexprText varNameInGenitive >>= isGenitive
  textAttrsInGenitive <- traverse LS.isMtexprText attrs
  textAttrs <- applyToInit isGenitive textAttrsInGenitive
  pure (varName, textAttrs)
 where
  applyToInit :: (a -> Maybe a) -> [a] -> Maybe [a]
  applyToInit _ [] = Nothing
  applyToInit _ [x] = Just [x]
  applyToInit f (x : xs) = (:) <$> f x <*> applyToInit f xs

-- | Is a text message in genitive form?
isGenitive :: Text -> Maybe Text
isGenitive = Text.stripSuffix genitiveSuffix

genitiveSuffix :: Text
genitiveSuffix = Text.pack "'s"

-- | Like 'splitAt', but produces a 'Nothing' if there are not enough elements.
safeSplitAt :: Int -> [a] -> Maybe ([a], [a])
safeSplitAt i _as
  | i < 0 = Nothing
safeSplitAt i as =
  case go i as [] of
    Nothing -> Nothing
    Just (lhs, rhs) -> Just (reverse lhs, rhs)
 where
  go 0 xs lhs = Just (lhs, xs)
  go _n [] _lhs = Nothing
  go n (x : xs) lhs = go (n - 1) xs (x : lhs)
