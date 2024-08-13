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
  RnHornClause(..),
  RnTypedMulti(..),
  RnMultiTerm,
  RnExpr(..),
  RnName(..),
  RnNameType(..),
  RnLit(..),
  RnRelationalPredicate(..),
  RnBoolStructR,
  OccName,
  Unique,
  mkSimpleOccName,
  -- * Renamer Monad and runners
  Renamer(..),
  runRenamerFor,

  -- * Scope checking types
  Scope (..),
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
  -- * Debugging helpers
  renameRuleTopLevel,
) where

import AnyAll.BoolStruct qualified as AA
import LS.Rule (Rule, RuleLabel)
import LS.Rule qualified as Rule
import LS.Types (MyToken, RuleName, SrcRef)
import LS.Types qualified as LS

import Control.Monad.Error.Class
import Control.Monad.Extra (foldM, fromMaybeM)
import Control.Monad.State.Strict (MonadState)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except qualified as Except
import Control.Monad.Trans.State.Strict (State)
import Control.Monad.Trans.State.Strict qualified as State (runState)
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
import Data.Text.Lazy.IO qualified as TL
import GHC.Generics (Generic)
import Optics hiding (has)
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
-- Scope tables
-- ----------------------------------------------------------------------------

newtype Renamer a = Renamer {runRenamer :: ExceptT String (State Scope) a}
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadState Scope, MonadError String)

type Unique = Int

-- | An unresolved name as it occurs in the original source.
type OccName = NonEmpty LS.MTExpr

data FuncInfo = FuncInfo
  { _funcArity :: (Int, Int)
  -- ^ Arity of a function. The first component means how many parameters
  -- are allowed before the function, the second component how many parameters
  -- are allowed afterwards.
  -- For example @(1, 1)@ is a simple infix function of the form @x f y@ where @f@
  -- is the name of the function.
  }
  deriving (Eq, Ord, Show)

mkSimpleOccName :: Text -> OccName
mkSimpleOccName = NE.singleton . LS.MTT
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
    Nothing -> throwError $ "lookupExistingName: Assumptions violated, name wasn't found: " <> show occName
    Just name
      | name.rnNameType == nameType -> pure name
      | otherwise ->
          throwError $
            "lookupExistingName: Invariant violated, trying to insert a different name type for a name that's already known. Got: "
              <> show nameType
              <> " but expected: "
              <> show (rnNameType name)

-- | Either inserts a new name of the given type, or checks that the name
-- is already in scope with the given type.
--
-- Fails if the name type does not match.
lookupOrInsertName :: OccName -> RnNameType -> Renamer RnName
lookupOrInsertName occName nameType =
  lookupName occName >>= \case
    Nothing -> insertName occName nameType
    Just name
      | rnNameType name == nameType -> pure name
      | otherwise ->
          throwError $
            "lookupOrInsertName: Invariant violated, trying to insert a different name type for a name that's already known. Got: "
              <> show nameType
              <> " but expected: "
              <> show (rnNameType name)

-- | Insert an occurrence name into the current 'ScopeTable'.
-- The new 'OccName' will overwrite (shadow?) any existing names.
insertName :: OccName -> RnNameType -> Renamer RnName
insertName occName nameType = do
  n <- newUnique
  let
    rnName =
      RnName
        { rnUniqueId = n
        , rnOccName = occName
        , rnNameType = nameType
        }
  assign'
    ( scScopeTable
        % stVariables
        % at occName
    )
    (Just rnName)
  pure rnName

-- | Insert an function meta information into the current 'ScopeTable'.
insertFunction :: RnName -> FuncInfo -> Renamer ()
insertFunction rnFnName funcInfo =
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
    Nothing -> throwError $ "lookupExistingFunction: Assumptions violated, function name wasn't found: " <> show rnFnName
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
  -- ^ During renaming a 'MultiTerm', did we encounter a function application?
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
  let
    (res, s) = runRenamerFor [rule]
  TL.putStrLn $ Pretty.pShow s
  case res of
    Left err -> putStrLn err
    Right rnRules -> TL.putStrLn $ Pretty.pShow $ head rnRules

runRenamerFor :: (Traversable f) => f Rule -> (Either String (f RnRule), Scope)
runRenamerFor rule =
  let
    (resE, scope) = State.runState (Except.runExceptT (runRenamer $ renameRules rule)) emptyScope
  in
    (resE, scope)

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
scanRule :: Rule -> Renamer ScopeTable
scanRule rule@Rule.Hornlike{} = do
  scanGivens rule.given
  exports <- recordScopeTable_ $ do
    scanGiveths rule.giveth
    traverse_ scanHornClause rule.clauses
  pure exports
scanRule r@Rule.Regulative{} = throwError $ "scanRule: Unsupported rule: " <> show r
scanRule r@Rule.Constitutive{} = throwError $ "scanRule: Unsupported rule: " <> show r
scanRule rule@Rule.TypeDecl{} = do
  traverse_ scanTypeSignature rule.super
  scanEnums rule.enums
  scanGivens rule.given
  traverse_ scanRule rule.has
  scanTypeDeclName rule.name
  typeScope <- use scScopeTable
  pure typeScope
scanRule r@Rule.Scenario{} = throwError $ "scanRule: Unsupported rule: " <> show r
scanRule r@Rule.DefNameAlias{} = throwError $ "scanRule: Unsupported rule: " <> show r
scanRule r@Rule.DefTypically{} = throwError $ "scanRule: Unsupported rule: " <> show r
scanRule r@Rule.RuleAlias{} = throwError $ "scanRule: Unsupported rule: " <> show r
scanRule r@Rule.RuleGroup{} = throwError $ "scanRule: Unsupported rule: " <> show r
scanRule r@Rule.RegFulfilled{} = throwError $ "scanRule: Unsupported rule: " <> show r
scanRule r@Rule.RegBreach{} = throwError $ "scanRule: Unsupported rule: " <> show r
scanRule r@Rule.NotARule{} = throwError $ "scanRule: Unsupported rule: " <> show r

-- | Scan a 'LS.HornClause2' for declarations of variables and functions.
scanHornClause :: LS.HornClause2 -> Renamer ()
scanHornClause hc = do
  scanDecideHeadClause hc.hHead

-- | Scan the head of relational predicates that occur in
-- the head of @DECIDE clauses@, e.g. @DECIDE foo IS bar@.
--
-- We detect the occurrence of @IS@ and treat it in a special way,
-- and in the case of a multi-term, we use 'scanDecideMultiTerm'
-- which allows the *introduction* of variables.
scanDecideHeadClause :: LS.RelationalPredicate -> Renamer ()
scanDecideHeadClause = \case
  LS.RPParamText pText -> throwError $ "Received 'RPParamText', we can't handle that yet. Got: " <> show pText
  LS.RPMT mt -> scanDecideMultiTerm mt
  LS.RPConstraint lhs _predicate _rhs -> do
    scanDecideMultiTerm lhs
  LS.RPBoolStructR lhs _predicate _rhs -> do
    scanDecideMultiTerm lhs
  LS.RPnary LS.RPis (lhs : _rhs) -> do
    -- When the assignment has multiple complicated relational predicates,
    -- it is translated to this 'RPNary'. Then the first element is before the 'IS'
    -- and the rest after.
    -- Example:
    -- @f x IS SUM(x, x, x)@
    -- is parsed to @RPnary RPis [[f, x], [RPnary RPSum [x, x, x]]]@
    -- ignoring some details.
    -- Thus, we scan the first item of 'IS' predicates.
    scanDecideHeadClause lhs
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
scanDecideMultiTerm :: LS.MultiTerm -> Renamer ()
scanDecideMultiTerm mt = do
  scopeTable <- use scScopeTable
  case mt of
    attrs
      | Just (obj, objAttrs) <- toObjectPath attrs -> do
          -- DECIDE x IS ...
          -- DECIDE x's y's z IS ...
          _ <- lookupOrInsertName (mkSimpleOccName obj) RnVariable
          traverse_ (\attr -> RnExprName <$> lookupOrInsertName (mkSimpleOccName attr) RnSelector) objAttrs
    fnDecl
      | Just (fnOccName, preArgs, postArgs) <- scanForFunctionDecl scopeTable fnDecl -> do
          rnF <- lookupOrInsertName fnOccName RnFunction
          insertFunction rnF (FuncInfo{_funcArity = (preArgs, postArgs)})
    unknownPattern -> throwError $ "While scanning a multi term in a top-level DECIDE clause, we encountered an unsupported pattern: " <> show unknownPattern

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
  Maybe LS.ParamText ->
  Renamer ()
scanGiveths = scanGivens

scanEnums ::
  Maybe LS.ParamText ->
  Renamer ()
scanEnums = traverse_ scanGivenInlineEnumParamText

scanGivens ::
  Maybe LS.ParamText ->
  Renamer ()
scanGivens Nothing = pure ()
scanGivens (Just givens) = do
  traverse_ scanGiven givens

scanGiven :: LS.TypedMulti -> Renamer ()
scanGiven (mtExprs, typeSig) = do
  scanGivenMultiTerm mtExprs
  traverse_ scanTypeSignature typeSig

scanGivenMultiTerm :: NonEmpty LS.MTExpr -> Renamer ()
scanGivenMultiTerm mtExprs = do
  mt <- assertSingletonMultiTerm mtExprs
  void $ insertName (pure mt) RnVariable

scanTypeSignature ::
  LS.TypeSig ->
  Renamer ()
scanTypeSignature sig = case sig of
  LS.SimpleType _pType entityType -> do
    scanEntityType entityType
  LS.InlineEnum _pType paramText -> do
    -- TODO: error handling, would we accept an enum such as `a IS ONE OF 1, 2, 3`?
    -- Only if we treat them as text, which might be confusing, as user might infer
    -- this to be some kind of type checked number type.
    scanGivenInlineEnumParamText paramText
 where
  scanEntityType :: LS.EntityType -> Renamer ()
  scanEntityType eType =
    -- This can either refer to an existing entity type, or define a new,
    -- ad-hoc, entity type. We just assume that multiple ad-hoc definitions
    -- of the same name in the same scope must be consistent.
    void $ lookupOrInsertName (mkSimpleOccName eType) RnType

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
scanGivenInlineEnumParamText :: LS.ParamText -> Renamer ()
scanGivenInlineEnumParamText params = do
  let
    scanEach tm = do
      mt <- assertNoTypeSignature tm
      enumNames <- traverse (\t -> insertName (NE.singleton t) RnEnum) mt
      pure $
        RnTypedMulti
          { rnTypedMultiExpr = fmap RnExprName enumNames
          , rnTypedMultiTypeSig = Nothing
          }

  traverse_ scanEach params

scanTypeDeclName :: RuleName -> Renamer ()
scanTypeDeclName mtexprs = do
  mt <- assertSingletonMultiTerm mtexprs
  void $ insertName (NE.singleton mt) RnType

-- ----------------------------------------------------------------------------
-- Renamer passes
-- ----------------------------------------------------------------------------

-- |
-- Lexical Scoping rules for hornlike rules:
--
-- GIVETH's are global
-- GIVEN's are local
-- DECIDE head term in "IS" clauses is global
renameRules :: (Traversable f) => f Rule -> Renamer (f RnRule)
renameRules rules = do
  rulesWithLocalDefs <-
    traverse
      ( \r -> do
          prev <- use scScopeTable
          exportedScope <- scanRule r
          fullRuleScope <- use scScopeTable
          assign' scScopeTable (prev `unionScopeTable` exportedScope)
          pure (r, fullRuleScope)
      )
      rules
  traverse
    ( \(r, ruleScope) -> do
        orig <- use scScopeTable
        modifying' scScopeTable (`unionScopeTable` ruleScope)
        rnRule <- renameRule r
        assign' scScopeTable orig
        pure rnRule
    )
    rulesWithLocalDefs

renameRule :: Rule -> Renamer RnRule
renameRule rule@Rule.Hornlike{} = do
  super <- traverse renameTypeSignature rule.super
  given <- renameGivens rule.given
  giveth <- renameGiveths rule.giveth
  wwhere <- renameLocalRules rule.wwhere
  upon <- renameUpons rule.upon
  defaults <- assertEmptyList rule.defaults
  symtab <- assertEmptyList rule.symtab
  clauses <- traverse renameHornClause rule.clauses
  name <- renameMultiTerm rule.name
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
renameRule r@Rule.Regulative{} = throwError $ "renameRule: Unsupported rule: " <> show r
renameRule r@Rule.Constitutive{} = throwError $ "renameRule: Unsupported rule: " <> show r
renameRule rule@Rule.TypeDecl{} = do
  super <- traverse renameTypeSignature rule.super
  defaults <- assertEmptyList rule.defaults
  enums <- renameEnums rule.enums
  given <- renameGivens rule.given
  upon <- renameUpons rule.upon
  symtab <- assertEmptyList rule.symtab
  has <- traverse renameRule rule.has
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
renameRule r@Rule.Scenario{} = throwError $ "renameRule: Unsupported rule: " <> show r
renameRule r@Rule.DefNameAlias{} = throwError $ "renameRule: Unsupported rule: " <> show r
renameRule r@Rule.DefTypically{} = throwError $ "renameRule: Unsupported rule: " <> show r
renameRule r@Rule.RuleAlias{} = throwError $ "renameRule: Unsupported rule: " <> show r
renameRule r@Rule.RuleGroup{} = throwError $ "renameRule: Unsupported rule: " <> show r
renameRule r@Rule.RegFulfilled{} = throwError $ "renameRule: Unsupported rule: " <> show r
renameRule r@Rule.RegBreach{} = throwError $ "renameRule: Unsupported rule: " <> show r
renameRule r@Rule.NotARule{} = throwError $ "renameRule: Unsupported rule: " <> show r

renameLocalRules :: [Rule] -> Renamer [RnRule]
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
renameUpons (Just xs) = throwError $ "Unsupported \"UPON\", got: " <> show xs

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

renameHornClause :: LS.HornClause2 -> Renamer RnHornClause
renameHornClause hc = do
  rnHead <- renameRelationalPredicate hc.hHead
  rnBody <- traverse renameBoolStruct hc.hBody
  pure $
    RnHornClause
      { rnHcHead = rnHead
      , rnHcBody = rnBody
      }

renameRelationalPredicate :: LS.RelationalPredicate -> Renamer RnRelationalPredicate
renameRelationalPredicate = \case
  LS.RPParamText pText -> throwError $ "Received 'RPParamText', we can't handle that yet. Got: " <> show pText
  LS.RPMT mt -> RnRelationalTerm <$> renameMultiTerm mt
  LS.RPConstraint lhs relationalPredicate rhs -> do
    rnLhs <- renameMultiTerm lhs
    rnRhs <- renameMultiTerm rhs
    pure $ RnConstraint rnLhs relationalPredicate rnRhs
  LS.RPBoolStructR lhs relationalPredicate rhs -> do
    rnLhs <- renameMultiTerm lhs
    rnRhs <- renameBoolStruct rhs
    pure $ RnBoolStructR rnLhs relationalPredicate rnRhs
  LS.RPnary relationalPredicate rhs -> do
    rnRhs <- traverse renameRelationalPredicate rhs
    pure $ RnNary relationalPredicate rnRhs

renameBoolStruct :: LS.BoolStructR -> Renamer RnBoolStructR
renameBoolStruct = \case
  AA.Leaf p -> AA.Leaf <$> renameRelationalPredicate p
  AA.All lbl cs -> do
    rnBoolStruct <- traverse renameBoolStruct cs
    pure $ AA.All lbl rnBoolStruct
  AA.Any lbl cs -> do
    rnBoolStruct <- traverse renameBoolStruct cs
    pure $ AA.Any lbl rnBoolStruct
  AA.Not cs -> AA.Not <$> renameBoolStruct cs

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
renameMultiTerm :: LS.MultiTerm -> Renamer RnMultiTerm
renameMultiTerm multiTerms = do
  (reversedRnMultiTerms, ctx) <-
    foldM
      ( \(results, state) mt -> do
          (rnExpr, newState) <- renameMultiTermExpression state mt
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
      (leftNonArgs, leftArgs) <- processLhs preNum lhs
      (rightNonArgs, rightArgs) <- processRhs postNum rhs
      pure $ reverse leftNonArgs <> [fnExpr] <> leftArgs <> rightArgs <> rightNonArgs

  findFunctionApplication fnName rnMultiTerms = do
    let
      (preArgs, postArgsWithName) = List.break (== (RnExprName fnName)) rnMultiTerms
    case postArgsWithName of
      [] -> throwError "fixFixity: Invariant violated, function name reported, but none found."
      (fnExpr : postArgs) -> pure (preArgs, fnExpr, postArgs)

  processLhs n lhs = do
    case safeSplitAt n (reverse lhs) of
      Nothing ->
        throwError $
          "Not enough elements in left hand side of function application. Required: "
            <> show n
            <> " but got: "
            <> show (length lhs)
      Just (args, nonArgs) -> pure (reverse nonArgs, reverse args)

  processRhs n rhs = do
    case safeSplitAt n rhs of
      Nothing ->
        throwError $
          "Not enough elements in left hand side of function application. Required: "
            <> show n
            <> " but got: "
            <> show (length rhs)
      Just (nonArgs, args) -> pure (nonArgs, args)

  initialMultiTermContext =
    MultiTermContext
      { _multiTermContextInSelector = False
      , _multiTermContextFunctionCall = Nothing
      }

-- | Rename a single 'LS.MTExpr' to a 'RnExpr'.
renameMultiTermExpression :: MultiTermContext -> LS.MTExpr -> Renamer (RnExpr, MultiTermContext)
renameMultiTermExpression ctx = \case
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
              rnName <- RnExprName <$> rnL4Builtin name
              pure (rnName, ctx')
          | ctx ^. inSelector -> do
              rnName <- RnExprName <$> insertName (mkSimpleOccName name) RnSelector
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
      rnName <- fromMaybeM (lookupOrInsertName (mkSimpleOccName nameSelector) RnSelector) (lookupName (mkSimpleOccName nameSelector))
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

rnL4Builtin :: Text -> Renamer RnName
rnL4Builtin name = do
  lookupOrInsertName (mkSimpleOccName name) RnBuiltin

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
  _ -> throwError $ "Expected singleton but got: " <> show xs

assertNoTypeSignature :: LS.TypedMulti -> Renamer (NonEmpty LS.MTExpr)
assertNoTypeSignature tm@(_, Just _) = throwError $ "Expected no type signature but got: " <> show tm
assertNoTypeSignature (mtt, Nothing) = do
  pure mtt

-- | If we can't handle renaming certain list of things, we just hope that
-- the parser doesn't give us a list with any elements.
-- We throwError if the list is not @'null'@.
assertEmptyList :: (Show a, MonadError String m) => [a] -> m [b]
assertEmptyList [] = pure []
assertEmptyList xs = throwError $ "Expected an empty list, but got: " <> show xs

-- ----------------------------------------------------------------------------
-- Helper utils non specific to the renamer.
-- Should be moved out into a general purpose function.
-- ----------------------------------------------------------------------------

-- | Given a 'LS.MultiTerm', check whether it has the form of an attribute
-- selector.
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
