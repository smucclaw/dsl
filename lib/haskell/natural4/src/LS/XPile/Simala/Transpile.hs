{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module LS.XPile.Simala.Transpile (
  -- * Main entry point to the transpiler
  transpile,

  -- * Transpiler monad
  Transpiler (..),
  runSimalaTranspiler,

  -- * Utilities to work with simala terms in naturalL4
  render,

  -- * Internal types that sometimes may be helpful
  SimalaTerm (..),

  -- * Typed errors and renderers
  TranspilerError (..),
  AssertionError (..),
  renderTranspilerError,

  -- * Debugging utilities
  debugTranspileRule,
) where

import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Trans.Except
import Data.Foldable qualified as Foldable
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy.IO qualified as TL
import Data.Tuple (Solo (..))
import Optics
import Text.Pretty.Simple qualified as Pretty

import LS.Renamer hiding (AssertionError (..), RenamerError (..), assertEmptyList)
import LS.Rule qualified as LS
import LS.Types qualified as LS
import TextuaL4.ParTextuaL qualified as Parser
import TextuaL4.Transform qualified as Parser

import AnyAll.BoolStruct qualified as AA

import LS.Log qualified as Log
import LS.Renamer.Rules
import Simala.Expr.Render qualified as Simala
import Simala.Expr.Type qualified as Simala

-- ----------------------------------------------------------------------------
-- Top Level transpilation functions and test helpers
-- ----------------------------------------------------------------------------

newtype Transpiler a = Transpiler {runTranspiler :: Except TranspilerError a}
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadError TranspilerError)

runSimalaTranspiler :: [RnRule] -> Either TranspilerError [Simala.Decl]
runSimalaTranspiler = runExcept . runTranspiler . transpile

transpile :: [RnRule] -> Transpiler [Simala.Decl]
transpile rules = do
  simalaTerms <- Maybe.catMaybes <$> traverse ruleToSimala rules
  traverse toSimalaDecl simalaTerms

-- ----------------------------------------------------------------------------
-- Transpiler specific intermediate representations (called IR)
-- ----------------------------------------------------------------------------

-- | A @'SimalaTerm'@ is like a 'Simala.Expr' but in an unsaturated form.
-- By "unsaturated", we mean that there might be holes in the expression that
-- we need to fill in later during further translation.
--
-- Thus, we pull out some of the 'Simala.Expr' constructors that we intend
-- process further.
-- Anything wrapped in 'TermExpr' is supposed to be opaque to the transpiler.
data SimalaTerm
  = -- | The head of a simala function.
    -- For example @f(x, y)@, where @f@ is a function and @x,y@ are its parameters.
    --
    -- This constructor is used to model function application of any supported
    -- form, and declaration of function definitions, e.g. `f x IS ...`, then
    -- 'TermApp' could be @'TermApp' "f" ["x"]@. Such a definition needs to be
    -- translated to 'TermFunction' once the right hand side is translated.
    TermApp Simala.Name [Simala.Name]
  | -- | Assign the given name with some expression.
    -- May contain intermediate selectors.
    --
    -- @x's y's z IS 5@
    --
    -- is translates to
    --
    -- @x = { y = { z = 5 }}@
    --
    -- However, if no intermediate selectors are present, this is a simple assignment:
    --
    -- @x IS 4@
    --
    -- is translated to
    --
    -- @x = 4@
    TermAttribute Simala.Name [Simala.Name] Simala.Expr
  | -- | A full fledged function definition. For example:
    --
    -- @f = fun(x, y) => x+y@
    --
    -- This aims to make the head of a function rule transparent for the transpiler.
    -- Especially important, when a function has a boolean constraint that we need to
    -- weave into the function definition.
    --
    -- Example:
    --
    -- @
    -- DECIDE f x IS x IF x > 0;
    --        f x IS 0 OTHERWISE
    -- @
    --
    -- Is supposed to be translated to:
    --
    -- @
    -- f = fun(x) => if x > 0 then x else 0
    -- @
    --
    -- Note, in practice, we might not remove 'OTHERWISE' and define a constant for it.
    TermFunction Simala.Transparency Simala.Name [Simala.Name] Simala.Expr
  | -- | A Let-In construct without an 'in' part.
    -- This is supposed to be used in simple variable assignment.
    TermLetIn Simala.Transparency Simala.Name Simala.Expr
  | -- | A simala expression that is supposed to be opaquely handled
    -- as it is a fully transpiled expression with no kind of hole.
    TermExpr Simala.Expr
  deriving (Show)

-- ----------------------------------------------------------------------------
-- Main translation helpers
-- ----------------------------------------------------------------------------

ruleToSimala :: RnRule -> Transpiler (Maybe SimalaTerm)
ruleToSimala (TypeDecl _typedecl) =
  -- Simala doesn't need to declare types, we can use them anonymously.
  -- We assume that each rule has been typechecked already, so we don't need to
  -- re-check anything.
  --
  pure Nothing
ruleToSimala (Hornlike hornlike) = do
  terms <- hornClausesToSimala hornlike.clauses
  -- TODO: handle multiple GIVETH's.
  -- Actually, handle GIVETHs at all.
  mainDefinition <- assertSingletonList "ruleToSimala" terms
  localDefinitions <- traverse ruleToSimala hornlike.wwhere
  Just <$> addLocalDefinitions mainDefinition (Maybe.catMaybes localDefinitions)

toSimalaDecl :: SimalaTerm -> Transpiler Simala.Decl
toSimalaDecl (TermLetIn t name expr) = do
  pure $ Simala.NonRec t name expr
toSimalaDecl (TermFunction t name params expr) = do
  pure $ Simala.NonRec t name $ mkFunctionDecl t params expr
toSimalaDecl term = do
  throwError $ TermToDeclUnsupported term

render :: [Simala.Decl] -> Text
render = Text.unlines . fmap Simala.render

-- ----------------------------------------------------------------------------
-- Post Processing of rule translation.
-- These steps include:
--
-- 1. Group terms for by their respective head clauses
--    This aims to create a single function / definition for multiple decide clauses.
--    These clauses can have bodies to express conditionals.
--    For example, functions can be defined as:
--
--    @
--      f x IS x IF x > 0;
--      f x IS 0 OTHERWISE
--    @
--
--    And we want to translate this to the Simala function:
--    @let f = fun(x) => if x > 0 then x else 0@
--
--    Thus, we group by the clause head (e.g. @f x@) and "fold" the clause bodies
--    into a chain of @if-then-else@ in Simala.
--
-- 2. Merge clause bodies referring to the same clause head.
--    E.g. @[(f x, x, x > 0), (f x, 0, "OTHERWISE")]@ is translated
--    to @(f x, if x > 0 then x else 0)@
--
--    A similar idea is applied for variable assignments and attribute decisions.
--
-- 3. At last, a hornlike rule has a 'WHERE' clause which contains arbitrary
--    rules. For now, we assume these are hornlike rules themselves, which define
--    functions and computations.
--    These functions and computations need to be included in the definition of
--    @f@, by "folding" them into the body of the function via nested @let-in@s.
--
-- ----------------------------------------------------------------------------

hornClausesToSimala :: [RnHornClause] -> Transpiler [SimalaTerm]
hornClausesToSimala clauses = do
  simalaTerms <- traverse processClause clauses
  let
    groupedSimalaTerms = groupClauses simalaTerms
  simplifiedSimalaTerms <- mergeGroups groupedSimalaTerms
  pure simplifiedSimalaTerms
 where
  processClause :: RnHornClause -> Transpiler (SimalaTerm, Maybe Simala.Expr)
  processClause clause = do
    hornHead <- relationalPredicateToSimala clause.rnHcHead
    hornBody <- traverse boolStructToSimala clause.rnHcBody
    pure (hornHead, hornBody)

-- | Group clauses and their respective bodies based on the similarity of the
-- clause head.
groupClauses :: (Foldable f) => f (SimalaTerm, Maybe Simala.Expr) -> [NonEmpty (SimalaTerm, Maybe Simala.Expr)]
groupClauses simalaTerms = do
  NE.groupBy (compareClauseHeads `on` fst) simalaTerms
 where
  compareClauseHeads :: SimalaTerm -> SimalaTerm -> Bool
  compareClauseHeads (TermLetIn _ name1 _) (TermLetIn _ name2 _) = name1 == name2
  compareClauseHeads (TermFunction _ fnName1 _ _) (TermFunction _ fnName2 _ _) = fnName1 == fnName2
  compareClauseHeads (TermAttribute name1 _ _) (TermAttribute name2 _ _) = name1 == name2
  compareClauseHeads (TermExpr _) (TermExpr _) = True
  compareClauseHeads _ _ = False

-- | Takes the translation of local variables in where clauses and turns
-- them into a Simala-let underneath potential lambdas or variable definitions.
--
-- Local definitions are, by definition, local to the encompassing 'SimalaTerm'
-- and may depend on parameters of said encompassing 'SimalaTerm'.
-- As such, the local definitions need to be added to the 'SimalaTerm', such
-- that it has access to said local definitions.
--
-- We do this, by moving local definition inside of any lambdas or let-ins
-- for variables.
--
-- For example
--
-- @GIVEN x DECIDE f x IS y WHERE y IS SUM(x,x)@
--
-- is supposed to be translated to:
--
-- @let f = fun(x) => let y = x + x in y@
addLocalDefinitions :: SimalaTerm -> [SimalaTerm] -> Transpiler SimalaTerm
addLocalDefinitions top [] = pure top
addLocalDefinitions top (x : xs) = case top of
  TermExpr{} -> throwError $ UnsupportedLocalTerm "addLocalDefinitions" top
  TermApp{} -> throwError $ UnsupportedLocalTerm "addLocalDefinitions" top
  TermLetIn t name expr -> do
    exprWithLocals <- linearLetIns expr (x :| xs)
    pure $ TermLetIn t name exprWithLocals
  TermAttribute name selectors expr -> do
    exprWithLocals <- linearLetIns expr (x :| xs)
    pure $ TermAttribute name selectors exprWithLocals
  TermFunction t fnName fnParams fnExpr -> do
    fnExprWithLocals <- linearLetIns fnExpr (x :| xs)
    pure $ TermFunction t fnName fnParams fnExprWithLocals
 where
  linearLetIns :: Simala.Expr -> NonEmpty SimalaTerm -> Transpiler Simala.Expr
  linearLetIns finalExpr terms = do
    inExpr <- case NE.tail terms of
      [] -> pure finalExpr
      (a : as) -> linearLetIns finalExpr (a :| as)
    case NE.head terms of
      TermExpr{} -> throwError $ UnsupportedLocalTerm "linearLetIns" (NE.head terms)
      TermApp{} -> throwError $ UnsupportedLocalTerm "linearLetIns" (NE.head terms)
      TermLetIn t name expr -> do
        pure $ mkLetIn t name expr inExpr
      TermAttribute name [] expr -> do
        pure $ mkLetIn Simala.Transparent name expr inExpr
      TermAttribute name (a : as) expr -> do
        pure $ mkLetIn Simala.Transparent name (buildRecordUpdate (a :| as) expr) inExpr
      TermFunction t fnName fnParams fnExpr -> do
        pure $ mkFunction t fnName fnParams fnExpr inExpr

-- | Given a collection of groups, merge each group into a single expression.
mergeGroups :: (Traversable t) => t (NonEmpty (SimalaTerm, Maybe Simala.Expr)) -> Transpiler (t SimalaTerm)
mergeGroups simalaTermGroups = do
  traverse mergeGroups' simalaTermGroups

-- | Do the heavy lifting of how to actually merge multiple clauses into a single term.
mergeGroups' :: NonEmpty (SimalaTerm, Maybe Simala.Expr) -> Transpiler SimalaTerm
mergeGroups' terms@((TermAttribute name _ _, _) :| _) = do
  attributeTerms <-
    traverse
      ( \(term, guard) -> do
          (_, attrs, expr) <- assertIsTermAttribute term
          pure (attrs, expr, guard)
      )
      terms
  mergeAttributes name attributeTerms
mergeGroups' ((term, Nothing) :| _) =
  pure term
mergeGroups' ((term, Just g) :| []) = do
  ifThenElseTerm <- mkIfThenElseTerm g term mkUndefinedTerm
  pure ifThenElseTerm
mergeGroups' ((term, Just g) :| (n : ns)) = do
  elseBranch <- mergeGroups' (n :| ns)
  mkIfThenElseTerm g term elseBranch

-- | Tries to merge multiple assignments for fields of a single record
-- into a single record construction.
mergeAttributes :: Simala.Name -> NonEmpty ([Simala.Name], Simala.Expr, Maybe Simala.Expr) -> Transpiler SimalaTerm
mergeAttributes name terms = do
  let
    initSelectors = NE.head terms ^. _1

  case initSelectors of
    [] -> do
      simpleTerms <-
        traverse
          ( \(selector, expr, guard) -> do
              _ <- assertEmptyList selector
              pure (expr, guard)
          )
          terms
      pure $ TermLetIn Simala.Transparent name $ toIfThenElseChain simpleTerms
    (_ : _) -> do
      rowTerms <-
        traverse
          ( \(selector, expr, guard) -> do
              nonEmptySelectors <- assertNonEmpty selector
              pure (nonEmptySelectors, expr, guard)
          )
          terms
      let
        rowGroups = NE.groupWith (^. _1) rowTerms
        rowGroups' = fmap reduceAttrPaths rowGroups
        rowExprs = fmap (\(attrName, expr) -> buildRecordUpdate attrName expr) rowGroups'
      recordRows <- traverse assertIsRecord rowExprs
      treeRows <- mergeRecordUpdates recordRows
      pure $ TermLetIn Simala.Transparent name treeRows
 where
  reduceAttrPaths :: NonEmpty (NonEmpty Simala.Name, Simala.Expr, Maybe Simala.Expr) -> (NonEmpty Simala.Name, Simala.Expr)
  reduceAttrPaths attrs =
    let
      attrPath = NE.head attrs ^. _1
      exprs = fmap (\(_, selectors, guards) -> (selectors, guards)) attrs
    in
      (attrPath, toIfThenElseChain exprs)

toIfThenElseChain :: NonEmpty (Simala.Expr, Maybe Simala.Expr) -> Simala.Expr
toIfThenElseChain ((expr, Nothing) :| []) = expr
toIfThenElseChain ((expr, Just guard) :| []) =
  Simala.mkIfThenElse guard expr Simala.Undefined
toIfThenElseChain ((expr, guard) :| terms) =
  let
    elseExpr = case terms of
      (x : xs) -> toIfThenElseChain (x :| xs)
  in
    case guard of
      Nothing -> expr
      Just g -> Simala.mkIfThenElse g expr elseExpr

-- ----------------------------------------------------------------------------
-- Transpilation
-- ----------------------------------------------------------------------------

relationalPredicateToSimala :: RnRelationalPredicate -> Transpiler SimalaTerm
relationalPredicateToSimala = \case
  RnRelationalTerm lhs -> lhsMultiTermToSimala lhs
  RnConstraint lhs LS.RPis rhs -> case lhs of
    RnFunDecl fnName fnParams -> do
      rhsExpr <- rhsMultiTermToSimala rhs
      mkFunctionTerm (toSimalaName fnName) (fmap toSimalaName fnParams) (TermExpr rhsExpr)
    RnProjection name selectors -> do
      rhsExpr <- rhsMultiTermToSimala rhs
      mkAssignmentTerm (toSimalaName name) (fmap toSimalaName selectors) rhsExpr
    term@(RnExprName _)
      | Just name <- isVariableName term -> do
          rhsExpr <- rhsMultiTermToSimala rhs
          mkVariableTerm (toSimalaName name) rhsExpr
    term -> throwError $ UnsupportedExpression term
  RnConstraint lhs predicate rhs -> do
    lhsSimalaExpr' <- lhsMultiTermToSimala lhs
    lhsSimalaExpr <- toSimalaExpression lhsSimalaExpr'
    rhsSimalaExpr <- rhsMultiTermToSimala rhs
    predRelToBuiltIn predicate [lhsSimalaExpr, rhsSimalaExpr]
  RnNary LS.RPis (lhs : rhs) -> do
    multiTerm <- assertPredicateIsMultiTerm "relationalPredicateToSimala" lhs
    lhsSimalaTerm <- lhsMultiTermToSimala multiTerm
    rhsExprs <- traverse relationalPredicateToSimala rhs
    case lhsSimalaTerm of
      TermApp fnName fnParams -> do
        fnExpr <- assertSingletonList "RnNary.TermApp" rhsExprs
        rhsExpr <- toSimalaExpression fnExpr
        mkFunctionTerm fnName fnParams (TermExpr rhsExpr)
      TermLetIn{} -> throwError $ ImpossibleLeftSide lhsSimalaTerm
      TermAttribute name selectors Simala.Undefined -> do
        someRhs <- assertSingletonList "RnNary.TermAttribute" rhsExprs
        rhsExpr <- toSimalaExpression someRhs
        pure $ TermAttribute name selectors rhsExpr
      TermAttribute{} -> throwError $ ImpossibleLeftSide lhsSimalaTerm
      TermFunction{} -> throwError $ NotImplemented "RpNary RPis TermFunction"
      TermExpr{} -> throwError $ ImpossibleLeftSide lhsSimalaTerm
  RnNary predicate mt ->
    predicateToSimala predicate mt
  RnBoolStructR lhs predicate rhs -> do
    lhsTerm <- lhsMultiTermToSimala lhs
    lhsExpr <- toSimalaExpression lhsTerm
    rhsSimalaExpr <- boolStructToSimala rhs
    predRelToBuiltIn predicate [lhsExpr, rhsSimalaExpr]

predicateToSimala :: LS.RPRel -> [RnRelationalPredicate] -> Transpiler SimalaTerm
predicateToSimala rp params' = do
  params <- traverse relationalPredicateToSimala params'
  exprs <- traverse toSimalaExpression params
  predRelToBuiltIn rp exprs

predRelToBuiltIn :: LS.RPRel -> [Simala.Expr] -> Transpiler SimalaTerm
predRelToBuiltIn rp exprs = case rp of
  LS.RPis -> throwError $ UnsupportedPredicate rp
  LS.RPhas -> throwError $ UnsupportedPredicate rp
  LS.RPeq -> fixedArity Simala.Eq 2 exprs
  LS.RPlt -> fixedArity Simala.Lt 2 exprs
  LS.RPlte -> fixedArity Simala.Le 2 exprs
  LS.RPgt -> fixedArity Simala.Gt 2 exprs
  LS.RPgte -> fixedArity Simala.Ge 2 exprs
  LS.RPelem -> throwError $ UnsupportedPredicate rp
  LS.RPnotElem -> throwError $ UnsupportedPredicate rp
  LS.RPnot -> fixedArity Simala.Not 1 exprs
  LS.RPand -> flexibleArity Simala.And exprs
  LS.RPor -> flexibleArity Simala.Or exprs
  LS.RPsum -> flexibleArity Simala.Sum exprs
  LS.RPproduct -> flexibleArity Simala.Product exprs
  LS.RPminus -> fixedArity Simala.Minus 2 exprs
  LS.RPdivide -> fixedArity Simala.Divide 2 exprs
  LS.RPmodulo -> fixedArity Simala.Modulo 2 exprs
  LS.RPsubjectTo -> throwError $ UnsupportedPredicate rp
  LS.RPmin -> atLeastArity Simala.Maximum 1 exprs
  LS.RPmax -> atLeastArity Simala.Minimum 1 exprs
  LS.RPmap -> throwError $ UnsupportedPredicate rp
  LS.RPTC _temporal -> throwError $ UnsupportedPredicate rp

flexibleArity :: Simala.Builtin -> [Simala.Expr] -> Transpiler SimalaTerm
flexibleArity b params = do
  pure $ TermExpr $ Simala.Builtin b params

atLeastArity :: Simala.Builtin -> Int -> [Simala.Expr] -> Transpiler SimalaTerm
atLeastArity b arity params' = do
  params <- assertLengthAtLeast arity params'
  pure $ TermExpr $ Simala.Builtin b params

fixedArity :: Simala.Builtin -> Int -> [Simala.Expr] -> Transpiler SimalaTerm
fixedArity b arity params' = do
  params <- assertLength arity params'
  pure $ TermExpr $ Simala.Builtin b params

lhsMultiTermToSimala :: RnExpr -> Transpiler SimalaTerm
lhsMultiTermToSimala rnExpr = case rnExpr of
  RnExprName name -> mkVariableTerm (toSimalaName name) Simala.Undefined
  RnExprBuiltin builtin -> pure $ TermExpr $ builtinToSimala builtin
  RnExprLit lit -> pure $ TermExpr $ litToSimala lit
  RnFunDecl fnName fnParams ->
    mkFunctionHead (toSimalaName fnName) (fmap toSimalaName fnParams)
  RnProjection varName selectors ->
    mkAssignmentTerm (toSimalaName varName) (fmap toSimalaName selectors) Simala.Undefined
  expr@RnFunApp{} -> throwError $ UnsupportedLeftSide expr

rhsMultiTermToSimala :: RnExpr -> Transpiler Simala.Expr
rhsMultiTermToSimala = \case
  RnExprName name -> pure $ Simala.Var $ toSimalaName name
  RnExprBuiltin builtin -> pure $ builtinToSimala builtin
  RnExprLit lit -> pure $ litToSimala lit
  RnFunApp fnName fnArgs -> do
    args <- traverse rhsMultiTermToSimala fnArgs
    pure $ Simala.App (Simala.Var $ toSimalaName fnName) args
  RnProjection varName [] -> pure $ Simala.Var $ toSimalaName varName
  RnProjection varName (sel : ssel) -> pure $ applySelectors (toSimalaName varName) (fmap toSimalaName (sel :| ssel))
  expr@RnFunDecl{} -> throwError $ UnsupportedRightSide expr

boolStructToSimala :: RnBoolStructR -> Transpiler Simala.Expr
boolStructToSimala = \case
  AA.Leaf relationalPredicate -> do
    simalaTerm <- relationalPredicateToSimala relationalPredicate
    toSimalaExpression simalaTerm
  AA.Any _lbl structs -> do
    simalaExprs <- traverse boolStructToSimala structs
    simalaAny <- flexibleArity Simala.Or simalaExprs
    toSimalaExpression simalaAny
  AA.All _lbl structs -> do
    simalaExprs <- traverse boolStructToSimala structs
    simalaAll <- flexibleArity Simala.And simalaExprs
    toSimalaExpression simalaAll
  AA.Not struct -> do
    simalaExpr <- boolStructToSimala struct
    simalaNot <- fixedArity Simala.Not 1 [simalaExpr]
    toSimalaExpression simalaNot

-- ----------------------------------------------------------------------------
-- Renamed Names utilities
-- ----------------------------------------------------------------------------

litToSimala :: RnLit -> Simala.Expr
litToSimala = \case
  RnInt int -> Simala.Lit $ Simala.IntLit $ fromIntegral int -- TODO: why does simala only support 'Int'?
  RnDouble _double -> error "Floating point numbers are unsupported in simala"
  RnBool boolean -> Simala.Lit $ Simala.BoolLit boolean
  RnString text -> Simala.Atom text

-- ----------------------------------------------------------------------------
-- Name translations
-- ----------------------------------------------------------------------------

toSimalaName :: RnName -> Simala.Name
toSimalaName name =
  Text.intercalate
    "_"
    [ rnNameTypePrefix name.rnNameType
    , slugifiedOccName
    , Text.pack (show name.rnUniqueId)
    ]
 where
  slugifiedOccName =
    name.rnOccName
      & NE.toList
      & fmap LS.mtexpr2text
      & Text.intercalate "_"
      & Text.replace " " "_"

rnNameTypePrefix :: RnNameType -> Text
rnNameTypePrefix = \case
  RnSelector -> "s"
  RnFunction -> "f"
  RnVariable -> "v"
  RnType -> "t"
  RnEnum -> "e"

builtinToSimala :: RnBuiltin -> Simala.Expr
builtinToSimala RnOtherwise = Simala.Var "otherwise"

-- ----------------------------------------------------------------------------
-- Assertion helpers
-- ----------------------------------------------------------------------------

assertIsTermAttribute :: SimalaTerm -> Transpiler (Simala.Name, [Simala.Name], Simala.Expr)
assertIsTermAttribute (TermAttribute name selectors expr) = pure (name, selectors, expr)
assertIsTermAttribute term = throwAssertion $ NotTermAttribute term

assertSingletonList :: Text -> [a] -> Transpiler a
assertSingletonList _errMsg [a] = pure a
assertSingletonList errMsg as =
  throwAssertion $ NotSingletonList errMsg (length as)

assertLengthAtLeast :: Int -> [a] -> Transpiler [a]
assertLengthAtLeast l as =
  let
    len = length as
  in
    if len < l
      then
        throwAssertion $ UnexpectedListSize (SizeAtLeast l) len
      else pure as

assertLength :: Int -> [a] -> Transpiler [a]
assertLength l as =
  let
    len = length as
  in
    if len /= l
      then
        throwAssertion $ UnexpectedListSize (SizeExact l) len
      else pure as

assertNonEmpty :: [a] -> Transpiler (NonEmpty a)
assertNonEmpty [] = throwAssertion UnexpectedEmptyList
assertNonEmpty (x : xs) = pure $ x :| xs

assertPredicateIsMultiTerm :: Text -> RnRelationalPredicate -> Transpiler RnExpr
assertPredicateIsMultiTerm _errMsg (RnRelationalTerm mt) = pure mt
assertPredicateIsMultiTerm errMsg predicate =
  throwAssertion $ NotMultiTerm errMsg predicate

assertEquals :: (Eq a, Show a) => a -> a -> Transpiler ()
assertEquals a b
  | a == b = pure ()
  | otherwise =
      throwAssertion $ NotEquals a b

assertIsRecord :: Simala.Expr -> Transpiler (Simala.Row Simala.Expr)
assertIsRecord (Simala.Record row) = pure row
assertIsRecord simalaExpr = throwAssertion $ NotRecord simalaExpr

-- | If we can't handle transpiling certain list of things, we just hope that
-- the parser doesn't give us a list with any elements.
-- We throwError if the list is not @'null'@.
assertEmptyList :: (Show a) => [a] -> Transpiler [b]
assertEmptyList [] = pure []
assertEmptyList xs = throwError $ AssertErr $ UnexpectedNonEmptyList xs

-- ----------------------------------------------------------------------------
-- Construction helpers for simala terms
-- ----------------------------------------------------------------------------

toSimalaExpression :: SimalaTerm -> Transpiler Simala.Expr
toSimalaExpression = \case
  TermExpr expr -> pure expr
  TermAttribute name [] Simala.Undefined -> pure $ Simala.Var name
  TermAttribute name (sel : ssels) Simala.Undefined -> pure $ applySelectors name (sel :| ssels)
  t@TermAttribute{} -> throwError $ UnexpectedSimalaTerm "TermExpr or TermAttribute" t
  t@TermApp{} -> throwError $ UnexpectedSimalaTerm "TermExpr or TermAttribute" t
  t@TermFunction{} -> throwError $ UnexpectedSimalaTerm "TermExpr or TermAttribute" t
  t@TermLetIn{} -> throwError $ UnexpectedSimalaTerm "TermExpr or TermAttribute" t

mkUndefinedTerm :: SimalaTerm
mkUndefinedTerm = TermExpr Simala.Undefined

mkAssignmentTerm :: Simala.Name -> [Simala.Name] -> Simala.Expr -> Transpiler SimalaTerm
mkAssignmentTerm name selectors expr = pure $ TermAttribute name selectors expr

mkVariableTerm :: Simala.Name -> Simala.Expr -> Transpiler SimalaTerm
mkVariableTerm name expr = pure $ TermAttribute name [] expr

mkFunctionHead :: Simala.Name -> [Simala.Name] -> Transpiler SimalaTerm
mkFunctionHead funcName funcParams = pure $ TermApp funcName funcParams

mkFunctionTerm :: Simala.Name -> [Simala.Name] -> SimalaTerm -> Transpiler SimalaTerm
mkFunctionTerm fnName fnParams term = do
  body <- toSimalaExpression term
  pure $ TermFunction Simala.Transparent fnName fnParams body

-- | Combine two 'SimalaTerm's via a Simala 'if-then-else' expression.
-- 'SimalaTerm's can not be generally combined. For example, combining
-- a function term and a let-in term cannot be done meaningfully, since the
-- result needs to be a single 'SimalaTerm' itself.
-- However, 'SimalaTerm's of the same constructor can sometimes be meaningfully combined!
-- For example, for two function terms with the same head (e.g., same name and same parameters),
-- we may weave the 'if-then-else' into the function body.
-- The same holds for 'TermLetIn' and 'TermAttribute' terms. Further, most terms can be combined
-- with arbitrary 'TermExpr's.
mkIfThenElseTerm :: Simala.Expr -> SimalaTerm -> SimalaTerm -> Transpiler SimalaTerm
mkIfThenElseTerm b (TermLetIn t1 name1 expr1) (TermLetIn t2 name2 expr2) = do
  assertEquals t1 t2
  assertEquals name1 name2
  let
    ifThenElse = Simala.mkIfThenElse b expr1 expr2
  pure $ TermLetIn t1 name1 ifThenElse
mkIfThenElseTerm b (TermLetIn t1 name1 body1) (TermExpr expr) = do
  let
    ifThenElse = Simala.mkIfThenElse b body1 expr
  pure $ TermLetIn t1 name1 ifThenElse
mkIfThenElseTerm b (TermAttribute name1 selectors1 expr1) (TermAttribute name2 selectors2 expr2) = do
  assertEquals name1 name2
  assertEquals selectors1 selectors2
  let
    ifThenElse = Simala.mkIfThenElse b expr1 expr2
  pure $ TermAttribute name1 selectors1 ifThenElse
mkIfThenElseTerm b (TermAttribute name1 selectors1 expr1) (TermExpr expr2) = do
  let
    ifThenElse = Simala.mkIfThenElse b expr1 expr2
  pure $ TermAttribute name1 selectors1 ifThenElse
mkIfThenElseTerm b (TermFunction t1 fnName1 fnParams1 expr1) (TermFunction t2 fnName2 fnParams2 expr2) = do
  assertEquals t1 t2
  assertEquals fnName1 fnName2
  assertEquals fnParams1 fnParams2
  let
    ifThenElse = Simala.mkIfThenElse b expr1 expr2
  pure $ TermFunction t1 fnName1 fnParams1 ifThenElse
mkIfThenElseTerm b (TermFunction t fnName1 fnParams1 expr1) (TermExpr expr) = do
  let
    ifThenElse = Simala.mkIfThenElse b expr1 expr
  pure $ TermFunction t fnName1 fnParams1 ifThenElse
mkIfThenElseTerm b (TermExpr expr1) (TermExpr expr2) = do
  let
    ifThenElse = Simala.mkIfThenElse b expr1 expr2
  pure $ TermExpr ifThenElse
mkIfThenElseTerm _b term1 term2 =
  throwError $ FailedToCombineTerms term1 term2

-- ----------------------------------------------------------------------------
-- Construction helpers for simala expressions
-- ----------------------------------------------------------------------------

applySelectors :: Simala.Name -> NonEmpty Simala.Name -> Simala.Expr
applySelectors name selectors =
  Foldable.foldl' applySelector (Simala.Var name) selectors

-- | Apply a selector to the given expression.
applySelector :: Simala.Expr -> Simala.Name -> Simala.Expr
applySelector expr proj = Simala.Project expr proj

mkLetIn :: Simala.Transparency -> Simala.Name -> Simala.Expr -> Simala.Expr -> Simala.Expr
mkLetIn transparency name rhs nextExpr =
  Simala.Let (Simala.NonRec transparency name rhs) nextExpr

mkFunction :: Simala.Transparency -> Simala.Name -> [Simala.Name] -> Simala.Expr -> Simala.Expr -> Simala.Expr
mkFunction transparency name params rhs nextExpr =
  mkLetIn transparency name (mkFunctionDecl transparency params rhs) nextExpr

mkFunctionDecl :: Simala.Transparency -> [Simala.Name] -> Simala.Expr -> Simala.Expr
mkFunctionDecl transparency params rhs =
  Simala.Fun transparency params rhs

buildRecordUpdate :: NonEmpty Simala.Name -> Simala.Expr -> Simala.Expr
buildRecordUpdate names expr = go $ NE.toList names
 where
  go [] = expr
  go (x : xs) = Simala.Record [(x, go xs)]

-- | Given a list of record updates, merge them into a singular record update.
-- Assumption: All record updates are unique, for example (in pseudo code):
--
-- @
--  { x = { y = 5 } }
--  { x = { z = { a = 0 } } }
--  { x = { z = { b = 1 } } }
--  { f = 2 }
-- @
--
-- is merged into:
--
-- @
--   { x = { y = 5, z = { a = 0, b = 1 } }, f = 2 }
-- @
mergeRecordUpdates :: [Simala.Row Simala.Expr] -> Transpiler Simala.Expr
mergeRecordUpdates rows = do
  let
    vars = NE.groupAllWith fst $ concat rows
  simpleRows <- traverse simplifyRow vars
  pure $ Simala.Record simpleRows
 where
  simplifyRow ::
    NonEmpty (Simala.Name, Simala.Expr) ->
    Transpiler (Simala.Name, Simala.Expr)
  simplifyRow ((n, expr) :| []) = pure (n, expr)
  simplifyRow assignment = do
    let
      name = fst $ NE.head assignment
      rowRhsExprs = fmap snd $ NE.toList assignment
    recordRows <- traverse assertIsRecord rowRhsExprs
    mergedRows <- mergeRecordUpdates recordRows
    pure (name, mergedRows)

-- ----------------------------------------------------------------------------
-- Typed Error
-- ----------------------------------------------------------------------------

data TranspilerError
  = TermToDeclUnsupported SimalaTerm
  | UnsupportedLocalTerm Text SimalaTerm
  | UnsupportedExpression RnExpr
  | ImpossibleLeftSide SimalaTerm
  | UnsupportedLeftSide RnExpr
  | UnsupportedRightSide RnExpr
  | UnexpectedSimalaTerm !Text SimalaTerm
  | NotImplemented Text
  | UnsupportedPredicate LS.RPRel
  | FailedToCombineTerms SimalaTerm SimalaTerm
  | AssertErr AssertionError

data ExpectedSize
  = SizeExact !Int
  | SizeAtLeast !Int

data AssertionError
  = forall a. (Show a) => UnexpectedNonEmptyList [a]
  | NotTermAttribute !SimalaTerm
  | NotSingletonList !Text !Int
  | NotMultiTerm !Text !RnRelationalPredicate
  | NotRecord !Simala.Expr
  | NotSelectorChain !SimalaTerm
  | forall a. (Show a) => NotEquals !a !a
  | UnexpectedEmptyList
  | UnexpectedListSize !ExpectedSize !Int

throwAssertion :: AssertionError -> Transpiler a
throwAssertion = throwError . AssertErr

renderTranspilerError :: TranspilerError -> Text
renderTranspilerError = \case
  TermToDeclUnsupported term ->
    "Cannot convert SimalaTerm to Decl: " <> tshow term
  UnsupportedLocalTerm herald term ->
    herald <> ": Unexpected local term: " <> tshow term
  UnsupportedExpression multiTerm ->
    "Unsupported RnExprs: " <> tshow multiTerm
  ImpossibleLeftSide term ->
    "The following SimalaTerm cannot occur on the left hand side of an assignment: " <> tshow term
  UnsupportedLeftSide multiTerm ->
    "Unsupported on the left side of an assignment: " <> tshow multiTerm
  UnsupportedRightSide multiTerm ->
    "Unsupported on the right side of an assignment: " <> tshow multiTerm
  UnexpectedSimalaTerm herald term ->
    "Expected " <> herald <> ", but got: " <> tshow term
  NotImplemented herald ->
    herald <> ": unsupported"
  UnsupportedPredicate relPred ->
    "Unsupported RelationalPredicate: " <> tshow relPred
  FailedToCombineTerms term1 term2 ->
    "Can't wrap terms in an if-then-else.\nFirst term: "
      <> tshow term1
      <> "\nSecond term: "
      <> tshow term2
  AssertErr assertionErr -> case assertionErr of
    UnexpectedNonEmptyList list ->
      "Expected empty list, but got: " <> tshow list
    NotTermAttribute term ->
      "Expected TermAttribute, but got: " <> tshow term
    NotSingletonList herald size ->
      herald <> ": Expected singleton list, but got: " <> tshow size
    NotMultiTerm herald rnPred ->
      herald <> ": Expected MultiTerm, but got: " <> tshow rnPred
    NotRecord expr ->
      "Expected Record, but got: " <> tshow expr
    NotSelectorChain term ->
      "Expected TermAttribute with non-empty selectors, but got: " <> tshow term
    NotEquals a b ->
      "Not equal: " <> tshow a <> ", " <> tshow b
    UnexpectedEmptyList ->
      "Expected non-empty list"
    UnexpectedListSize len n ->
      "Expected list of "
        <> ( case len of
              SizeExact i -> "size " <> tshow i
              SizeAtLeast i -> "at least size " <> tshow i
           )
        <> ", but got "
        <> tshow n

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

-- ----------------------------------------------------------------------------
-- Debugger helpers
-- ----------------------------------------------------------------------------

debugTranspileRule :: String -> IO ()
debugTranspileRule ruleSrc = do
  rule <- case run ruleSrc of
    Left err -> do
      putStrLn err
      fail "translation failed"
    Right r -> pure r
  TL.putStrLn $ Pretty.pShow rule
  renamerResult <- runRenamerFor (liftRenamerTracer Log.prettyTracer) $ MkSolo rule
  TL.putStrLn $ Pretty.pShow $ rnResultScope renamerResult
  case renamerResult of
    RenamerFail err _ -> Text.putStrLn $ renderRenamerError err
    RenamerSuccess (MkSolo rnRule) _ -> do
      TL.putStrLn $ Pretty.pShow rnRule
      case runExcept $ runTranspiler $ transpile [rnRule] of
        Left err -> Text.putStrLn $ renderTranspilerError err
        Right decls -> flip Foldable.traverse_ decls $ \decl -> do
          Text.putStrLn $ "Decl: " <> Simala.render decl

run :: String -> Either String LS.Rule
run = fmap Parser.transRule . Parser.pRule . Parser.myLexer

-- runList :: String -> Either String [LS.Rule]
-- runList = fmap (fmap Parser.transRule) . Parser.pListRule . Parser.myLexer
