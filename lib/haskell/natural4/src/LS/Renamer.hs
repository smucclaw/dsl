{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module LS.Renamer where

import AnyAll.BoolStruct qualified as AA
import LS.Rule (Rule, RuleLabel)
import LS.Rule qualified as Rule
import LS.Types (MyToken, RuleName, SrcRef)
import LS.Types qualified as LS
import TextuaL4.ParTextuaL qualified as Parser
import TextuaL4.Transform qualified as Parser

import Control.Monad.Error.Class
import Control.Monad.Extra (foldM, fromMaybeM)
import Control.Monad.State.Class qualified as State
import Control.Monad.State.Strict (MonadState)
import Control.Monad.Trans.Except qualified as Except
import Control.Monad.Trans.State.Strict qualified as State (runState)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy.IO qualified as TL
import GHC.Generics (Generic)
import Optics
import Text.Pretty.Simple qualified as Pretty

-- ----------------------------------------------------------------------------
-- Types specific to the renamer phase
-- ----------------------------------------------------------------------------

-- | A rename rule is the same as a 'Rule' but
data RnRule
  = Hornlike RnHornlike
  deriving (Eq, Ord, Show, Generic)

type RnBoolStructR = AA.OptionallyLabeledBoolStruct RnRelationalPredicate

data RnHornClause = RnHornClause
  { rnHcHead :: RnRelationalPredicate
  , rnHcBody :: Maybe RnBoolStructR
  }
  deriving (Eq, Ord, Show, Generic)

type RnRuleName = RnMultiTerm

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

type RnEntityType = RnName

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

type StVariable = Text
type Unique = Int
type OccName = NonEmpty LS.MTExpr
type FuncOccName = Text

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
  { _scScopeTable :: ScopeTable
  , _scUnique :: Unique
  }
  deriving (Eq, Ord, Show)

data BindingScope
  = ToplevelScope
  | WhereScope
  | GivenScope
  | GivethScope
  deriving (Eq, Ord, Show)

data ScopeTable = ScopeTable
  { _stVariables :: Map OccName RnName
  , _stFunction :: Map FuncOccName FuncInfo
  }
  deriving (Eq, Ord, Show)

makeFieldsNoPrefix 'Scope
makeFieldsNoPrefix 'ScopeTable

emptyScope :: Scope
emptyScope =
  Scope
    { _scScopeTable =
        ScopeTable
          { _stVariables = Map.empty
          , _stFunction = Map.empty
          }
    , _scUnique = 0
    }

prefixScope :: RuleName -> Scope -> Scope
prefixScope = undefined

newUniqueM :: (MonadState Scope m) => m Unique
newUniqueM = do
  u <- State.gets _scUnique
  State.modify' (\s -> s & scUnique %~ (+ 1))
  pure u

lookupName :: (MonadState Scope m) => OccName -> m (Maybe RnName)
lookupName occName = do
  st <- State.gets _scScopeTable
  pure $ Map.lookup occName (_stVariables st)

lookupOrInsertName :: (MonadState Scope m, MonadError String m) => OccName -> RnNameType -> m RnName
lookupOrInsertName occName nameType = do
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

insertName :: (MonadState Scope m) => OccName -> RnNameType -> m RnName
insertName occName nameType = do
  n <- newUniqueM
  let
    rnName =
      RnName
        { rnUniqueId = n
        , rnOccName = occName
        , rnNameType = nameType
        }
  State.modify' $ \(s :: Scope) ->
    s
      & scScopeTable
      % stVariables
      % at occName
      .~ Just rnName
  pure rnName

insertFunction :: (MonadState Scope m) => FuncOccName -> FuncInfo -> m ()
insertFunction funcOccName funcInfo = do
  State.modify' $ \s ->
    s
      & scScopeTable
      % stFunction
      % at funcOccName
      .~ Just funcInfo

lookupFunction :: (MonadState Scope m) => FuncOccName -> m (Maybe FuncInfo)
lookupFunction funcOccName =
  State.gets $ \s ->
    s ^. scScopeTable % stFunction % at funcOccName

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
  State.runState (Except.runExceptT (renameRule rule)) emptyScope

-- ----------------------------------------------------------------------------
-- Resolve functions and their respective arities
-- ----------------------------------------------------------------------------

-- fixRuleArity :: (MonadState Scope m, MonadError String m) => Rule -> m RnRule

-- ----------------------------------------------------------------------------
-- Renamer passes
-- ----------------------------------------------------------------------------

renameRule :: (MonadState Scope m, MonadError String m) => Rule -> m RnRule
renameRule rule@Rule.Hornlike{} = do
  super <- traverse renameTypeSignature rule.super
  given <- renameGivens rule.given
  giveth <- renameGiveths rule.giveth
  upons <- renameUpons rule.upon
  wwhere <- traverse renameRule rule.wwhere
  defaults <- assertEmptyList rule.defaults
  symtab <- assertEmptyList rule.symtab
  clauses <- traverse renameHornClause rule.clauses
  pure $
    Hornlike
      RnHornlike
        { name = []
        , super = super
        , keyword = rule.keyword
        , given = given
        , giveth = giveth
        , upon = upons
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
renameRule r@Rule.TypeDecl{} = throwError $ "Unsupported rule: " <> show r
renameRule r@Rule.Scenario{} = throwError $ "Unsupported rule: " <> show r
renameRule r@Rule.DefNameAlias{} = throwError $ "Unsupported rule: " <> show r
renameRule r@Rule.DefTypically{} = throwError $ "Unsupported rule: " <> show r
renameRule r@Rule.RuleAlias{} = throwError $ "Unsupported rule: " <> show r
renameRule r@Rule.RuleGroup{} = throwError $ "Unsupported rule: " <> show r
renameRule r@Rule.RegFulfilled{} = throwError $ "Unsupported rule: " <> show r
renameRule r@Rule.RegBreach{} = throwError $ "Unsupported rule: " <> show r
renameRule r@Rule.NotARule{} = throwError $ "Unsupported rule: " <> show r

renameUpons ::
  forall m.
  (MonadState Scope m, MonadError String m) =>
  Maybe LS.ParamText ->
  m (Maybe RnParamText)
renameUpons Nothing = pure Nothing
renameUpons (Just xs) = throwError $ "Unsupported \"UPON\", got: " <> show xs

renameGiveths ::
  forall m.
  (MonadState Scope m, MonadError String m) =>
  Maybe LS.ParamText ->
  m (Maybe RnParamText)
renameGiveths = renameGivens

renameGivens ::
  forall m.
  (MonadState Scope m, MonadError String m) =>
  Maybe LS.ParamText ->
  m (Maybe RnParamText)
renameGivens Nothing = pure Nothing
renameGivens (Just givens) = do
  rnGivens <- mapM renameGiven givens
  pure $ Just $ RnParamText rnGivens
 where
  renameGiven (mtExprs, typeSig) = do
    rnMtExprs <- renameGivenMultiTerm mtExprs
    rnTypeSig <- traverse renameTypeSignature typeSig
    pure $ RnTypedMulti (NE.singleton $ RnExprName rnMtExprs) rnTypeSig

  renameGivenMultiTerm mtExprs = do
    mt <- assertSingletonMultiTerm mtExprs
    insertName (pure mt) RnVariable

renameTypeSignature ::
  forall m.
  (MonadState Scope m, MonadError String m) =>
  LS.TypeSig ->
  m RnTypeSig
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
  renameEntityType :: LS.EntityType -> m RnEntityType
  renameEntityType eType =
    -- This is might be a new entity type. However, we allow ad-hoc type definitions.
    -- Thus, insert a new entity type. This definition defines one name for all
    -- 'EntityType's with the same name over the whole program.
    lookupOrInsertName (mkSimpleOccName eType) RnType

  -- Why not reuse 'renameParamText'? It is basically the same type!
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
  renameGivenInlineEnumParamText :: LS.ParamText -> m RnParamText
  renameGivenInlineEnumParamText params = do
    let
      renameEach tm = do
        mt <- assertNoTypeSignature tm
        _t <- assertMultiExprIsOnlyText mt
        enumName <- insertName mt RnEnum
        pure $
          RnTypedMulti
            { rnTypedMultiExpr = NE.singleton $ RnExprName enumName
            , rnTypedMultiTypeSig = Nothing
            }

    rnParams <- mapM renameEach params
    pure $ RnParamText rnParams

renameHornClause :: (MonadState Scope m, MonadError String m) => LS.HornClause2 -> m RnHornClause
renameHornClause hc = do
  rnHead <- renameDecideHeadClause hc.hHead
  rnBody <- traverse renameBoolStruct hc.hBody
  pure $
    RnHornClause
      { rnHcHead = rnHead
      , rnHcBody = rnBody
      }

renameDecideHeadClause :: (MonadState Scope m, MonadError String m) => LS.RelationalPredicate -> m RnRelationalPredicate
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
renameDecideMultiTerm :: (MonadState Scope m, MonadError String m) => LS.MultiTerm -> m RnMultiTerm
renameDecideMultiTerm mt = do
  scopeTable <- State.gets _scScopeTable
  case mt of
    attrs@(_ : _)
      | Just (obj, objAttrs) <- toObjectPath attrs -> do
          rnName <- lookupOrInsertName (mkSimpleOccName obj) RnVariable
          rnObjAttrs <- mapM (\attr -> RnExprName <$> lookupOrInsertName (mkSimpleOccName attr) RnSelector) objAttrs
          pure $ RnExprName rnName : rnObjAttrs
    [LS.MTT x] -> do
      rnName <- lookupOrInsertName (mkSimpleOccName x) RnVariable
      pure [RnExprName rnName]
    [LS.MTT f, LS.MTT x]
      | Just [rnX] <- variableAndFunction scopeTable [x] f -> do
          insertFunction f (FuncInfo{funcArity = (0, 1)})
          rnF <- lookupOrInsertName (mkSimpleOccName f) RnFunction
          pure $ [RnExprName rnF, RnExprName rnX]
    [LS.MTT x, LS.MTT f]
      | Just [rnX] <- variableAndFunction scopeTable [x] f -> do
          insertFunction f (FuncInfo{funcArity = (1, 0)})
          rnF <- lookupOrInsertName (mkSimpleOccName f) RnFunction
          pure $ [RnExprName rnF, RnExprName rnX]
    [LS.MTT x, LS.MTT f, LS.MTT y]
      | Just [rnX, rnY] <- variableAndFunction scopeTable [x, y] f -> do
          insertFunction f (FuncInfo{funcArity = (1, 1)})
          rnF <- lookupOrInsertName (mkSimpleOccName f) RnFunction
          pure $ [RnExprName rnF, RnExprName rnX, RnExprName rnY]
    [LS.MTT f, LS.MTT x, LS.MTT y]
      | Just [rnX, rnY] <- variableAndFunction scopeTable [x, y] f -> do
          insertFunction f (FuncInfo{funcArity = (0, 2)})
          rnF <- lookupOrInsertName (mkSimpleOccName f) RnFunction
          pure $ [RnExprName rnF, RnExprName rnX, RnExprName rnY]
    [] -> throwError "renameDecideMultiTerm: Unexpected empty list of MultiTerm"
    unknownPattern -> throwError $ "While renaming a multi term in a top-level DECIDE clause, we encountered an unsupported pattern: " <> show unknownPattern

-- | Check whether this could be a function like structure.
--
-- It might be, if all the variables are already bound, and the function name
-- is unbound or already known as a function.
variableAndFunction :: ScopeTable -> [Text] -> Text -> Maybe [RnName]
variableAndFunction st variables function = do
  -- TODO: this is wrong, only consider arguments in the GIVEN's, otherwise
  -- that's name shadowing. E.g.
  --
  -- @
  -- GIVEN x DECIDE f x y IS SUM(x, y) WHERE y IS 5
  -- @
  rnBoundVariables <- traverse ((`Map.lookup` _stVariables st) . mkSimpleOccName) variables
  case mkSimpleOccName function `Map.lookup` _stVariables st of
    -- The function name must be either unbound, or
    -- registered as a function.
    Just fnName
      | fnName.rnNameType == RnFunction -> Just rnBoundVariables
      | otherwise -> Nothing
    Nothing -> Just rnBoundVariables

renameRelationalPredicate :: (MonadState Scope m, MonadError String m) => LS.RelationalPredicate -> m RnRelationalPredicate
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

renameBoolStruct :: (MonadState Scope m, MonadError String m) => LS.BoolStructR -> m RnBoolStructR
renameBoolStruct = \case
  AA.Leaf p -> AA.Leaf <$> renameRelationalPredicate p
  AA.All lbl cs -> do
    rnBoolStruct <- traverse renameBoolStruct cs
    pure $ AA.All lbl rnBoolStruct
  AA.Any lbl cs -> do
    rnBoolStruct <- traverse renameBoolStruct cs
    pure $ AA.Any lbl rnBoolStruct
  AA.Not cs -> AA.Not <$> renameBoolStruct cs

renameMultiTerm :: (MonadState Scope m, MonadError String m) => LS.MultiTerm -> m RnMultiTerm
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

renameMultiTermExpression :: (MonadState Scope m, MonadError String m) => MultiTermContext -> LS.MTExpr -> m (RnExpr, MultiTermContext)
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
          | isL4BuiltIn name -> do
              rnName <- RnExprName <$> rnL4Builtin name
              pure (rnName, notInSelectorContext ctx)
          | ctx ^. multiTermContextInSelector -> do
              rnName <- RnExprName <$> insertName (mkSimpleOccName name) RnSelector
              pure (rnName, notInSelectorContext ctx)
          | otherwise -> do
              -- If this is not a selector, or a known variable, we infer it is a string type.
              -- TODO: only accept this, if the @name@ is enclosed in `"`.
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

-- ----------------------------------------------------------------------------
-- Builtins
-- ----------------------------------------------------------------------------

isL4BuiltIn :: Text -> Bool
isL4BuiltIn name = Set.member name (Set.fromList l4Builtins)

rnL4Builtin :: (MonadState Scope m, MonadError String m) => Text -> m RnName
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
assertMultiExprIsOnlyText :: (MonadError String m) => NonEmpty LS.MTExpr -> m Text
assertMultiExprIsOnlyText mtt = do
  xs <- traverse assertExprIsText mtt
  pure $ Text.unwords $ NE.toList xs

assertSingletonMultiTerm :: (MonadError String m) => NonEmpty LS.MTExpr -> m LS.MTExpr
assertSingletonMultiTerm (x NE.:| []) = pure x
assertSingletonMultiTerm xs = throwError $ "Expected singleton but got: " <> show xs

assertMultiExprIsText :: (MonadError String m) => NonEmpty LS.MTExpr -> m Text
assertMultiExprIsText mts = do
  mt <- assertSingletonMultiTerm mts
  assertExprIsText mt

assertExprIsText :: (MonadError String m) => LS.MTExpr -> m Text
assertExprIsText (LS.MTT t) = pure t
assertExprIsText mt = throwError $ "Expected MTT but got: " <> show mt

assertNoTypeSignature :: (MonadError String m) => LS.TypedMulti -> m (NonEmpty LS.MTExpr)
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
-- Nothing
toObjectPath :: LS.MultiTerm -> Maybe (Text, [Text])
toObjectPath [] = Nothing
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

-- ----------------------------------------------------------------------------
-- Example data for debugging.
-- TODO: don't merge this
-- ----------------------------------------------------------------------------

run :: String -> Either String Rule
run = fmap Parser.transRule . Parser.pRule . Parser.myLexer

runList :: String -> Either String [Rule]
runList = fmap (fmap Parser.transRule) . Parser.pListRule . Parser.myLexer
