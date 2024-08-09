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
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module LS.Renamer where

import Prelude hiding (lookup)

import AnyAll.BoolStruct qualified as AA
import LS.Rule (Rule, RuleLabel)
import LS.Rule qualified as Rule
import LS.Types (MyToken, RuleName, SrcRef)
import LS.Types qualified as LS

import Control.Monad.Error.Class
import Control.Monad.Extra (foldM, fromMaybeM)
import Control.Monad.State.Strict (MonadState)
import Control.Monad.State.Strict qualified as State
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except qualified as Except
import Control.Monad.Trans.State.Strict (State)
import Control.Monad.Trans.State.Strict qualified as State (runStateT)
import Data.Foldable qualified as Foldable
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy.IO qualified as TL
import GHC.Generics (Generic)
import GHC.List qualified as List
import Optics hiding (has)
import Text.Pretty.Simple qualified as Pretty

-- ----------------------------------------------------------------------------
-- Types specific to the renamer phase
-- ----------------------------------------------------------------------------

-- | A rename rule is the same as a 'Rule' but
data RnRule
  = Hornlike RnHornlike
  | TypeDecl RnTypeDecl
  deriving (Eq, Ord, Show, Generic)

type RnBoolStructR = AA.OptionallyLabeledBoolStruct RnRelationalPredicate

-- | Corresponds to 'HornClause2', which is equivalent to @HornClause BoolStructR@.
data RnHornClause = RnHornClause
  { rnHcHead :: RnRelationalPredicate
  , rnHcBody :: Maybe RnBoolStructR
  }
  deriving (Eq, Ord, Show, Generic)

type RnRuleName = RnMultiTerm
type RnFullRuleName = [RnRuleOccName]
type RnRuleOccName = [Text]
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
  , rnRuleOccName :: RnFullRuleName
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
-- Utility data structures. To be moved outside of this module. Perhaps use
-- third-party library?
-- ----------------------------------------------------------------------------

-- rename (Let decls expr) = do { ns <- scanNames decls; rdecls <- withExtendedScope ns (traverse renameDecl decls); rexpr <- withExtendedScope ns (renameExpr expr); pure (RLet rdecls rexpr) }

data Trie k v = Trie
  { _trieValue :: !(Maybe v)
  , _trieChildren :: !(Map k (Trie k v))
  }
  deriving (Eq, Ord, Foldable, Functor, Show, Traversable)

empty :: Trie k v
empty = Trie Nothing Map.empty

singleton :: (Eq k) => [k] -> v -> Trie k v
singleton [] x = Trie (Just x) Map.empty
singleton (k : ks) x = Trie Nothing (Map.singleton k (singleton ks x))

unionWith ::
  (Eq k, Ord k) =>
  (v -> v -> v) ->
  Trie k v ->
  Trie k v ->
  Trie k v
unionWith f (Trie v1 c1) (Trie v2 c2) =
  Trie v $ Map.unionWith (unionWith f) c1 c2
 where
  v = case (v1, v2) of
    (Nothing, _) -> v2
    (_, Nothing) -> v1
    (Just x, Just y) -> Just (f x y)

unionsWith ::
  (Eq k, Ord k) =>
  (v -> v -> v) ->
  [Trie k v] ->
  Trie k v
unionsWith f = List.foldl' (unionWith f) empty

prefix :: (Eq k, Ord k) => [k] -> Trie k v -> Trie k v
prefix [] trie = trie
prefix (k : ks) trie = Trie Nothing $ Map.singleton k (prefix ks trie)

lookup :: (Eq k, Ord k) => [k] -> Trie k v -> Maybe v
lookup [] (Trie Nothing _) = Nothing
lookup [] (Trie (Just x) _) = Just x
lookup (k : ks) (Trie _ children) = do
  trie <- Map.lookup k children
  lookup ks trie

-- alterF :: Functor f
--        => (Maybe a -> f (Maybe a)) -> [k] -> Trie k a -> f (Trie k a)
-- -- This implementation was stolen from 'Control.Lens.At'.
-- alterF f [] (Trie Nothing _) = Nothing
-- alterF f [] (Trie (Just x) _) = Just x
-- alterF f (k : ks) (Trie _ children) = do
--   trie <- alterF f ks children
--   lookup ks trie

-- instance Ord k => At (Trie k a) where
--   at k = lensVL $ \f -> alterF f k
--   {-# INLINE at #-}

-- type instance Index (Trie k a) = [k]
-- type instance IxValue (Trie k a) = a
-- -- Default implementation uses Map.alterF
-- instance Ord k => Ixed (Trie k a)

-- ----------------------------------------------------------------------------
-- Scope tables
-- ----------------------------------------------------------------------------

newtype Renamer a = Renamer {runRenamer :: ExceptT String (State Scope) a}
  deriving (Functor, Applicative, Monad)
  deriving newtype (MonadState Scope, MonadError String)

type Unique = Int

-- | An unresolved name as it occurs in the original source.
type OccName = NonEmpty LS.MTExpr

data FuncInfo = FuncInfo
  { funcArity :: (Int, Int)
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
  { _scScopeTable :: Trie RnRuleOccName ScopeTable
  , _scUnique :: Unique
  -- ^ next unique value that we can use
  , _scBindingScope :: BindingScope
  , _scRuleOccName :: RnFullRuleName
  }
  deriving (Eq, Ord, Show)

data BindingScope
  = TopLevelScope
  | WhereScope
  | GivenScope
  | GivethScope
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

emptyScopeTable :: ScopeTable
emptyScopeTable =
  ScopeTable
    { _stVariables = Map.empty
    , _stFunction = Map.empty
    }

makeFieldsNoPrefix 'Scope
makeFields 'Trie
makeFieldsNoPrefix 'ScopeTable

emptyScope :: Scope
emptyScope =
  Scope
    { _scScopeTable = empty
    , _scUnique = 0
    , _scBindingScope = TopLevelScope
    , _scRuleOccName = []
    }

prefixScope :: RuleName -> Scope -> Scope
prefixScope = undefined

newUniqueM :: Renamer Unique
newUniqueM = do
  u <- use scUnique
  modifying' scUnique (+ 1)
  pure u

lookupName :: OccName -> Renamer (Maybe RnName)
lookupName occName = do
  ruleOccName <- use scRuleOccName
  State.gets $ \s -> do
    (scope :: ScopeTable) <- lookup ruleOccName s._scScopeTable
    occName `Map.lookup` scope._stVariables

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

insertName :: OccName -> RnNameType -> Renamer RnName
insertName occName nameType = do
  n <- newUniqueM
  ruleOccName <- use scRuleOccName
  let
    rnName =
      RnName
        { rnUniqueId = n
        , rnOccName = occName
        , rnRuleOccName = ruleOccName
        , rnNameType = nameType
        }

  State.modify' $ \s ->
    let
      st =
        unionWith
          unionScopeTable
          s._scScopeTable
          ( singleton ruleOccName $
              ScopeTable
                { _stVariables = Map.singleton occName rnName
                , _stFunction = Map.empty
                }
          )
    in
      s{_scScopeTable = st}

  pure rnName

unionScopeTable :: ScopeTable -> ScopeTable -> ScopeTable
unionScopeTable sc1 sc2 =
  ScopeTable
    { _stVariables =
        Map.unionWith
          (\a b -> if a == b then a else error "Invariant unionWith")
          sc1._stVariables
          sc2._stVariables
    , _stFunction =
        Map.unionWith
          (\a b -> if a == b then a else error "Invariant unionWith")
          sc1._stFunction
          sc2._stFunction
    }

insertFunction :: RnName -> FuncInfo -> Renamer ()
insertFunction rnFnName funcInfo = do
  ruleOccName <- use scRuleOccName
  State.modify' $ \s ->
    let
      st =
        unionWith
          const
          s._scScopeTable
          ( singleton ruleOccName $
              ScopeTable
                { _stVariables = Map.empty
                , _stFunction = Map.singleton rnFnName funcInfo
                }
          )
    in
      s{_scScopeTable = st}

lookupFunction :: RnName -> Renamer (Maybe FuncInfo)
lookupFunction rnFnName = do
  ruleOccName <- use scRuleOccName
  State.gets $ \s -> do
    (scope :: ScopeTable) <- lookup ruleOccName s._scScopeTable
    rnFnName `Map.lookup` scope._stFunction

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
  }

makeFieldsNoPrefix 'MultiTermContext

inSelectorContext :: MultiTermContext -> MultiTermContext
inSelectorContext mtc = mtc & multiTermContextInSelector .~ True

notInSelectorContext :: MultiTermContext -> MultiTermContext
notInSelectorContext mtc = mtc & multiTermContextInSelector .~ False

-- ----------------------------------------------------------------------------
-- Top Level Definitions
-- ----------------------------------------------------------------------------

renameRuleTopLevel :: Rule -> IO ()
renameRuleTopLevel rule = do
  TL.putStrLn $ Pretty.pShow rule
  let
    (res, s) = renameRuleTopLevel' rule
  TL.putStrLn $ Pretty.pShow s
  case res of
    Left err -> putStrLn err
    Right rnRule -> TL.putStrLn $ Pretty.pShow rnRule

renameRuleTopLevel' :: Rule -> (Either String RnRule, Scope)
renameRuleTopLevel' rule =
  State.runState (Except.runExceptT (runRenamer $ renameRule rule)) emptyScope

-- ----------------------------------------------------------------------------
-- Resolve functions and their respective arities
-- ----------------------------------------------------------------------------

-- fixFunctionFixity :: (MonadState Scope m, MonadError String m) => RnRule -> m RnRule
-- fixFunctionFixity

-- ----------------------------------------------------------------------------
-- Renamer passes
-- ----------------------------------------------------------------------------

renameRule :: Rule -> Renamer RnRule
renameRule rule@Rule.Hornlike{} = do
  super <- traverse renameTypeSignature rule.super
  given <- renameGivens rule.given
  giveth <- renameGiveths rule.giveth
  upon <- renameUpons rule.upon
  wwhere <- traverse renameRule rule.wwhere
  defaults <- assertEmptyList rule.defaults
  symtab <- assertEmptyList rule.symtab
  clauses <- traverse renameHornClause rule.clauses
  name <- renameMultiTerm rule.name
  pure $
    Hornlike
      RnHornlike
        { name = name
        , super = super
        , keyword = rule.keyword
        , given = given
        , giveth = giveth
        , upon = upon
        , clauses = clauses
        , rlabel = rule.rlabel
        , lsource = rule.lsource
        , wwhere = wwhere
        , srcref = rule.srcref
        , defaults = defaults
        , symtab = symtab
        }
renameRule r@Rule.Regulative{} = throwError $ "Unsupported rule: " <> show r
renameRule r@Rule.Constitutive{} = throwError $ "Unsupported rule: " <> show r
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
renameRule r@Rule.Scenario{} = throwError $ "Unsupported rule: " <> show r
renameRule r@Rule.DefNameAlias{} = throwError $ "Unsupported rule: " <> show r
renameRule r@Rule.DefTypically{} = throwError $ "Unsupported rule: " <> show r
renameRule r@Rule.RuleAlias{} = throwError $ "Unsupported rule: " <> show r
renameRule r@Rule.RuleGroup{} = throwError $ "Unsupported rule: " <> show r
renameRule r@Rule.RegFulfilled{} = throwError $ "Unsupported rule: " <> show r
renameRule r@Rule.RegBreach{} = throwError $ "Unsupported rule: " <> show r
renameRule r@Rule.NotARule{} = throwError $ "Unsupported rule: " <> show r

renameTypeDeclName :: RuleName -> Renamer RnRuleName
renameTypeDeclName mtexprs = do
  mt <- assertSingletonMultiTerm mtexprs
  rnTyName <- insertName (NE.singleton mt) RnType
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
  insertName (pure mt) RnVariable

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
    -- This can either refer to an existing entity type, or define a new,
    -- ad-hoc, entity type. We just assume that multiple ad-hoc definitions
    -- of the same name in the same scope must be consistent.
    lookupOrInsertName (mkSimpleOccName eType) RnType

-- | Rename an enum definition.
--
-- Why not reuse 'renameGivens'? It is basically the same type!
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
-- GIVEN x IS ONE OF foo, bar, foo baz
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
renameGivenInlineEnumParamText :: LS.ParamText -> Renamer RnParamText
renameGivenInlineEnumParamText params = do
  let
    renameEach tm = do
      mt <- assertNoTypeSignature tm
      _t <- assertMultiExprIsOnlyText mt -- unclear if we really want this
      enumNames <- traverse (\t -> insertName (NE.singleton t) RnEnum) mt
      pure $
        RnTypedMulti
          { rnTypedMultiExpr = fmap RnExprName enumNames
          , rnTypedMultiTypeSig = Nothing
          }

  rnParams <- traverse renameEach params
  pure $ RnParamText rnParams

renameHornClause :: LS.HornClause2 -> Renamer RnHornClause
renameHornClause hc = do
  rnHead <- renameDecideHeadClause hc.hHead
  rnBody <- traverse renameBoolStruct hc.hBody
  pure $
    RnHornClause
      { rnHcHead = rnHead
      , rnHcBody = rnBody
      }

-- Special renaming function for the relational predicates that occur in
-- the head of @DECIDE clauses@, e.g. @DECIDE foo IS bar@.
--
-- We detect the occurrence of @IS@ and treat it in a special way,
-- and in the case of a multi-term, we use 'renameDecideMultiTerm'
-- which allows the *introduction* of variables rather than just referencing
-- them.
--
renameDecideHeadClause :: LS.RelationalPredicate -> Renamer RnRelationalPredicate
renameDecideHeadClause = \case
  LS.RPParamText pText -> throwError $ "Received 'RPParamText', we can't handle that yet. Got: " <> show pText
  LS.RPMT mt -> RnRelationalTerm <$> renameDecideMultiTerm mt
  LS.RPConstraint lhs relationalPredicate rhs -> do
    rnLhs <- renameDecideMultiTerm lhs
    rnRhs <- renameMultiTerm rhs
    pure $ RnConstraint rnLhs relationalPredicate rnRhs
  LS.RPBoolStructR lhs relationalPredicate rhs -> do
    rnLhs <- renameDecideMultiTerm lhs
    rnRhs <- renameBoolStruct rhs
    pure $ RnBoolStructR rnLhs relationalPredicate rnRhs
  LS.RPnary LS.RPis (lhs : rhs) -> do
    -- When the assignment has multiple complicated relational predicates,
    -- it is translated to this 'RPNary'. Then the first element is before the 'IS'
    -- and the rest after.
    -- Example:
    -- @f x IS SUM(x, x, x)@
    -- is parsed to @RPnary RPis [[f, x], [RPnary RPSum [x, x, x]]]@
    -- ignoring some details.
    rnLhs <- renameDecideHeadClause lhs
    rnRhs <- traverse renameRelationalPredicate rhs
    pure $ RnNary LS.RPis (rnLhs : rnRhs)
  LS.RPnary relationalPredicate rhs -> do
    rnRhs <- traverse renameRelationalPredicate rhs
    pure $ RnNary relationalPredicate rnRhs

-- | Rename a top-level occurrence of 'LS.MultiTerm'.
--
-- This is slightly special, as this may be the definition site of functions.
--
-- For now, we accept the following 'LS.MultiTerm''s for function definitions:
--
-- * @f x@: function @f@ in prefix with parameter @x@
-- * @x f@: function @f@ in postfix with parameter @x@
-- * @f x y@: function @f@ in prefix with parameters @x@ and @y@
-- * @x f y@: function @f@ in infix with parameters @x@ and @y@
--
-- Note, to be recognized as a function, variables must have been specified by 'GIVEN'
-- clauses and the function name must be unbound in its current scope.
--
-- Additionally, we recognize the following forms:
--
-- * @f's x's y's z@: An attribute path from @f@ to something that has a @z@ attribute.
-- * @x@: a variable, might be bound ad-hoc
--
-- Note, this doesn't accept literals such as '42' or '3.5f'.
renameDecideMultiTerm :: LS.MultiTerm -> Renamer RnMultiTerm
renameDecideMultiTerm mt = do
  ruleOccName <- use scRuleOccName
  scopeTable <- (fromMaybe emptyScopeTable . lookup ruleOccName) <$> use scScopeTable
  case mt of
    attrs
      | Just (obj, objAttrs) <- toObjectPath attrs -> do
          -- DECIDE x IS ...
          -- DECIDE x's y's z IS ...
          rnName <- lookupOrInsertName (mkSimpleOccName obj) RnVariable
          rnObjAttrs <- mapM (\attr -> RnExprName <$> lookupOrInsertName (mkSimpleOccName attr) RnSelector) objAttrs
          pure $ RnExprName rnName : rnObjAttrs

    -- ANDRES: I think we should generalise this to something like
    -- the following:
    --
    -- If we have a list of names x_1, x_2, ... x_n, check if
    -- there is an x_i such that all x_j with i /= j are known
    -- (givens), and x_i is either unknown, or already known as
    -- a function.
    --
    -- I'm not completely sure if this is enough, because we probably
    -- should be more precise about shadowing existing functions ...
    --
    [LS.MTT f, LS.MTT x]
      | Just [rnX] <- variableAndFunction scopeTable [x] f -> do
          rnF <- lookupOrInsertName (mkSimpleOccName f) RnFunction
          insertFunction rnF (FuncInfo{funcArity = (0, 1)})
          pure $ [RnExprName rnF, RnExprName rnX]
    [LS.MTT x, LS.MTT f]
      | Just [rnX] <- variableAndFunction scopeTable [x] f -> do
          rnF <- lookupOrInsertName (mkSimpleOccName f) RnFunction
          insertFunction rnF (FuncInfo{funcArity = (1, 0)})
          pure $ [RnExprName rnF, RnExprName rnX]
    [LS.MTT x, LS.MTT f, LS.MTT y]
      | Just [rnX, rnY] <- variableAndFunction scopeTable [x, y] f -> do
          rnF <- lookupOrInsertName (mkSimpleOccName f) RnFunction
          insertFunction rnF (FuncInfo{funcArity = (1, 1)})
          pure $ [RnExprName rnF, RnExprName rnX, RnExprName rnY]
    [LS.MTT f, LS.MTT x, LS.MTT y]
      | Just [rnX, rnY] <- variableAndFunction scopeTable [x, y] f -> do
          rnF <- lookupOrInsertName (mkSimpleOccName f) RnFunction
          insertFunction rnF (FuncInfo{funcArity = (0, 2)})
          pure $ [RnExprName rnF, RnExprName rnX, RnExprName rnY]
    [] -> throwError "renameDecideMultiTerm: Unexpected empty list of MultiTerm"
    unknownPattern -> throwError $ "While renaming a multi term in a top-level DECIDE clause, we encountered an unsupported pattern: " <> show unknownPattern

-- | Check whether this could be a function like structure.
--
-- It might be, if all the variables are already bound, and the function name
-- is unbound or already known as a function.
--
-- ANDRES: It surprises me that we do not have to check whether
-- the arity matches.
variableAndFunction :: ScopeTable -> [Text] -> Text -> Maybe [RnName]
variableAndFunction st variables function = do
  -- TODO: this is wrong, only consider arguments in the GIVEN's, otherwise
  -- that's name shadowing. E.g.
  --
  -- @
  -- GIVEN x DECIDE f x y IS SUM(x, y) WHERE y IS 5
  -- @
  rnBoundVariables <- traverse ((`Map.lookup` st._stVariables) . mkSimpleOccName) variables
  case mkSimpleOccName function `Map.lookup` st._stVariables of
    -- The function name must be either unbound, or
    -- registered as a function.
    Just fnName
      | fnName.rnNameType == RnFunction -> Just rnBoundVariables
      | otherwise -> Nothing
    Nothing -> Just rnBoundVariables

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

renameMultiTerm :: LS.MultiTerm -> Renamer RnMultiTerm
renameMultiTerm multiTerms = do
  (results, _finalCtx) <-
    foldM
      ( \(results, state) mt -> do
          (rnExpr, newState) <- renameMultiTermExpression state mt
          pure (rnExpr : results, newState)
      )
      ([], initialMultiTermContext)
      multiTerms
  pure $ reverse results
 where
  initialMultiTermContext =
    MultiTermContext
      { _multiTermContextInSelector = False
      }

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
      lookupName (mkSimpleOccName name) >>= \case
        Just rnName -> pure (RnExprName rnName, notInSelectorContext ctx)
        Nothing
          | Just literal <- isTextLiteral name ->
              pure (RnExprLit $ RnString literal, notInSelectorContext ctx)
          | isL4BuiltIn name -> do
              -- ANDRES: I'm not convinced that built-ins should be renamed, and
              -- if we already detected that they're built-ins, perhaps we should
              -- just use a different dedicated constructor for this case.
              rnName <- RnExprName <$> rnL4Builtin name
              pure (rnName, notInSelectorContext ctx)
          | ctx ^. multiTermContextInSelector -> do
              rnName <- RnExprName <$> insertName (mkSimpleOccName name) RnSelector
              pure (rnName, notInSelectorContext ctx)
          | otherwise -> do
              -- If this is not a selector, or a known variable, we infer
              -- it is a string type. This is ok, because users can
              -- disambiguate variables and string literals by enclosing the
              -- literal in quotes, e.g. @"This is a string"@
              --
              pure (RnExprLit $ RnString name, notInSelectorContext ctx)
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

-- | Assert the collection of 'MTExpr' consists only of text fragments.
--
-- TODO: This is lossy, we can't reconstruct the 'NonEmpty LS.MTExpr' given the
-- text. Fix this! It is likely wrong, too.
assertMultiExprIsOnlyText :: NonEmpty LS.MTExpr -> Renamer Text
assertMultiExprIsOnlyText mtt = do
  xs <- traverse assertExprIsText mtt
  pure $ Text.unwords $ NE.toList xs

assertSingletonMultiTerm :: (Show (f LS.MTExpr), Foldable f) => f LS.MTExpr -> Renamer LS.MTExpr
assertSingletonMultiTerm xs = case Foldable.toList xs of
  [x] -> pure x
  _ -> throwError $ "Expected singleton but got: " <> show xs

assertMultiExprIsText :: NonEmpty LS.MTExpr -> Renamer Text
assertMultiExprIsText mts = do
  mt <- assertSingletonMultiTerm mts
  assertExprIsText mt

assertExprIsText :: LS.MTExpr -> Renamer Text
assertExprIsText (LS.MTT t) = pure t
assertExprIsText mt = throwError $ "Expected MTT but got: " <> show mt

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
