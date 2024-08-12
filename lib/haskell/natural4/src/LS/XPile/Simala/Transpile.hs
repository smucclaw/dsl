{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall #-}

module LS.XPile.Simala.Transpile where

import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Trans.Except
import Data.Foldable qualified as Foldable
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe qualified as Maybe
import Data.String.Interpolate
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy.IO qualified as TL
import Data.Tuple (Solo (..))
import Optics
import Text.Pretty.Simple qualified as Pretty

import LS.Renamer
import LS.Rule qualified as LS
import LS.Types qualified as LS
import TextuaL4.ParTextuaL qualified as Parser
import TextuaL4.Transform qualified as Parser

import AnyAll.BoolStruct qualified as AA

import Simala.Expr.Parser (mkIfThenElse)
import Simala.Expr.Render qualified as Simala
import Simala.Expr.Type qualified as Simala

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
-- Top Level transpilation functions and test helpers
-- ----------------------------------------------------------------------------

newtype Transpiler a = Transpiler {runTranspiler :: Except String a}
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadError String)

transpile :: [RnRule] -> Transpiler Simala.Expr
transpile rules = do
  simalaTerms <- Maybe.catMaybes <$> traverse ruleToSimala rules
  combineSimalaTerms Simala.Undefined simalaTerms

combineSimalaTerms :: Simala.Expr -> [SimalaTerm] -> Transpiler Simala.Expr
combineSimalaTerms inExpr [] = pure inExpr
combineSimalaTerms inExpr (TermLetIn t name expr : terms) = do
  restOfInExpr <- combineSimalaTerms inExpr terms
  pure $ mkLetIn t name expr restOfInExpr
combineSimalaTerms inExpr (TermFunction t name params expr : terms) = do
  restOfInExpr <- combineSimalaTerms inExpr terms
  pure $ mkFunction t name params expr restOfInExpr
combineSimalaTerms _inExpr _terms = do
  throwError $ "combineSimalaTerms: Cannot combine SimalaTerms: " <> show _terms

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
  TermExpr{} -> throwError $ "addLocalDefinitions: Unexpected SimalaTerm: " <> show top
  TermApp{} -> throwError $ "addLocalDefinitions: Unexpected SimalaTerm: " <> show top
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
      TermApp{} -> throwError $ "linearLetIns: Unexpected SimalaTerm: " <> show (NE.head terms)
      TermLetIn t name expr -> do
        pure $ mkLetIn t name expr inExpr
      TermExpr{} -> throwError $ "linearLetIns: Unexpected SimalaTerm: " <> show (NE.head terms)
      TermAttribute name [] expr -> do
        pure $ mkLetIn Simala.Transparent name expr inExpr
      TermAttribute name (a : as) expr -> do
        pure $ mkLetIn Simala.Transparent name (buildRecordUpdate (a :| as) expr) inExpr
      TermFunction t fnName fnParams fnExpr -> do
        pure $ mkFunction t fnName fnParams fnExpr inExpr

-- | Given a collection of groups, merge each group into a single expression.
--
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
  Simala.Builtin Simala.IfThenElse [guard, expr, Simala.Undefined]
toIfThenElseChain ((expr, guard) :| terms) =
  let
    elseExpr = case terms of
      (x : xs) -> toIfThenElseChain (x :| xs)
  in
    case guard of
      Nothing -> expr
      Just g -> Simala.Builtin Simala.IfThenElse [g, expr, elseExpr]

-- ----------------------------------------------------------------------------
-- Transpilation
-- ----------------------------------------------------------------------------

relationalPredicateToSimala :: RnRelationalPredicate -> Transpiler SimalaTerm
relationalPredicateToSimala = \case
  RnRelationalTerm lhs -> lhsMultiTermToSimala lhs
  RnConstraint lhs LS.RPis rhs -> case lhs of
    (mtHead : args)
      | Just (fnName, fnParams) <- isFunctionDeclaration mtHead args -> do
          rhsExpr <- rhsMultiTermToSimala rhs
          mkFunctionTerm (toSimalaName fnName) (fmap toSimalaName fnParams) (TermExpr rhsExpr)
      | Just (var, selectors) <- isAssignment mtHead args -> do
          rhsExpr <- rhsMultiTermToSimala rhs
          mkAssignmentTerm (toSimalaName var) (fmap toSimalaName selectors) rhsExpr
      | otherwise -> throwError $ "relationalPredicateToSimala: Unsupported " <> show lhs
    [] -> throwError "empty lhs"
  RnConstraint lhs predicate rhs -> do
    lhsSimalaExpr' <- lhsMultiTermToSimala lhs
    lhsSimalaExpr <- assertTermExpr lhsSimalaExpr'
    rhsSimalaExpr <- rhsMultiTermToSimala rhs
    predRelToBuiltIn predicate [lhsSimalaExpr, rhsSimalaExpr]
  RnNary LS.RPis (lhs : rhs) -> do
    multiTerm <- assertPredicateIsMultiTerm "relationalPredicateToSimala" lhs
    lhsSimalaTerm <- lhsMultiTermToSimala multiTerm
    rhsExprs <- traverse relationalPredicateToSimala rhs
    case lhsSimalaTerm of
      TermApp fnName fnParams -> do
        fnExpr <- assertSingletonList "RnNary.TermApp" rhsExprs
        rhsExpr <- assertTermExpr fnExpr
        mkFunctionTerm fnName fnParams (TermExpr rhsExpr)
      TermLetIn{} -> throwError "Not implemented yet"
      TermAttribute name selectors Simala.Undefined -> do
        someRhs <- assertSingletonList "RnNary.TermAttribute" rhsExprs
        rhsExpr <- assertTermExpr someRhs
        pure $ TermAttribute name selectors rhsExpr
      TermAttribute _name _selectors _expr -> throwError "Not implemented yet"
      TermFunction{} -> throwError "Not implemented yet"
      TermExpr expr -> throwError $ "A saturated expression can't be left hand side: " <> show expr
  -- TODO: this is wrong, what about Var and Project?
  RnNary predicate mt ->
    predicateToSimala predicate mt
  RnBoolStructR lhs predicate rhs -> do
    lhsTerm <- lhsMultiTermToSimala lhs
    lhsExpr <- assertTermExpr lhsTerm
    rhsSimalaExpr <- boolStructToSimala rhs
    predRelToBuiltIn predicate [lhsExpr, rhsSimalaExpr]

predicateToSimala :: LS.RPRel -> [RnRelationalPredicate] -> Transpiler SimalaTerm
predicateToSimala rp params' = do
  params <- traverse relationalPredicateToSimala params'
  exprs <- traverse assertTermExpr params
  predRelToBuiltIn rp exprs

predRelToBuiltIn :: LS.RPRel -> [Simala.Expr] -> Transpiler SimalaTerm
predRelToBuiltIn rp exprs = case rp of
  LS.RPis -> throwError $ "Unsupported relational predicate: " <> show rp
  LS.RPhas -> throwError $ "Unsupported relational predicate: " <> show rp
  LS.RPeq -> fixedArity Simala.Eq 2 exprs
  LS.RPlt -> fixedArity Simala.Lt 2 exprs
  LS.RPlte -> fixedArity Simala.Le 2 exprs
  LS.RPgt -> fixedArity Simala.Gt 2 exprs
  LS.RPgte -> fixedArity Simala.Ge 2 exprs
  LS.RPelem -> throwError $ "Unsupported relational predicate: " <> show rp
  LS.RPnotElem -> throwError $ "Unsupported relational predicate: " <> show rp
  LS.RPnot -> fixedArity Simala.Not 1 exprs
  LS.RPand -> flexibleArity Simala.And exprs
  LS.RPor -> flexibleArity Simala.Or exprs
  LS.RPsum -> flexibleArity Simala.Sum exprs
  LS.RPproduct -> flexibleArity Simala.Product exprs
  LS.RPminus -> fixedArity Simala.Minus 2 exprs
  LS.RPdivide -> fixedArity Simala.Divide 2 exprs
  LS.RPmodulo -> fixedArity Simala.Modulo 2 exprs
  LS.RPsubjectTo -> throwError $ "Unsupported relational predicate: " <> show rp
  LS.RPmin -> atLeastArity Simala.Maximum 1 exprs
  LS.RPmax -> atLeastArity Simala.Minimum 1 exprs
  LS.RPmap -> throwError $ "Unsupported relational predicate: " <> show rp
  LS.RPTC _temporal -> throwError $ "Unsupported relational predicate: " <> show rp

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

lhsMultiTermToSimala :: RnMultiTerm -> Transpiler SimalaTerm
lhsMultiTermToSimala [rnExpr] = pure $ TermExpr $ exprToSimala rnExpr
lhsMultiTermToSimala (mtHead : rest)
  | Just (fnName, fnParams) <- isFunctionDeclaration mtHead rest =
      mkFunctionHead (toSimalaName fnName) (fmap toSimalaName fnParams)
  | Just (varName, selectors) <- isProjection mtHead rest =
      mkRecordAssignmentTerm (toSimalaName varName) (fmap toSimalaName selectors)
lhsMultiTermToSimala xs = throwError $ "lhsMultiTermToSimala: unsupported pattern: " <> show xs

rhsMultiTermToSimala :: RnMultiTerm -> Transpiler Simala.Expr
rhsMultiTermToSimala [rnExpr] = pure $ exprToSimala rnExpr
rhsMultiTermToSimala (mtHead : rest)
  | Just _fnName <- isFunction mtHead = pure $ Simala.App (exprToSimala mtHead) $ fmap exprToSimala rest
  | Just (varName, selectors) <- isProjection mtHead rest = pure $ applySelectors (toSimalaName varName) (fmap toSimalaName selectors)
rhsMultiTermToSimala exprs = throwError $ "Unhandled rhs: " <> show exprs

boolStructToSimala :: RnBoolStructR -> Transpiler Simala.Expr
boolStructToSimala = \case
  AA.Leaf relationalPredicate -> do
    simalaTerm <- relationalPredicateToSimala relationalPredicate
    assertTermExpr simalaTerm
  AA.Any _lbl structs -> do
    simalaExprs <- traverse boolStructToSimala structs
    simalaAny <- flexibleArity Simala.Or simalaExprs
    assertTermExpr simalaAny
  AA.All _lbl structs -> do
    simalaExprs <- traverse boolStructToSimala structs
    simalaAll <- flexibleArity Simala.And simalaExprs
    assertTermExpr simalaAll
  AA.Not struct -> do
    simalaExpr <- boolStructToSimala struct
    simalaNot <- fixedArity Simala.Not 1 [simalaExpr]
    assertTermExpr simalaNot

-- ----------------------------------------------------------------------------
-- Rule pattern recognition
-- ----------------------------------------------------------------------------

isAssignment :: RnExpr -> [RnExpr] -> Maybe (RnName, [RnName])
isAssignment name selectors = do
  rnName <- isVariable name
  rnSelectors <- traverse isSelector selectors
  pure (rnName, rnSelectors)

isFunctionDeclaration :: (Traversable t) => RnExpr -> t RnExpr -> Maybe (RnName, t RnName)
isFunctionDeclaration mtHead args = do
  fnName <- isFunction mtHead
  argNames <- traverse isVariable args
  pure (fnName, argNames)

isProjection :: RnExpr -> [RnExpr] -> Maybe (RnName, NE.NonEmpty RnName)
isProjection mtHead args = do
  varName <- isVariable mtHead
  nonEmptyRest <- NE.nonEmpty args
  selectors <- traverse isSelector nonEmptyRest
  pure (varName, selectors)

-- ----------------------------------------------------------------------------
-- Renamed Names utilities
-- ----------------------------------------------------------------------------

exprToSimala :: RnExpr -> Simala.Expr
exprToSimala (RnExprName name) = Simala.Var $ toSimalaName name
exprToSimala (RnExprLit lit) = litToSimala lit

litToSimala :: RnLit -> Simala.Expr
litToSimala = \case
  RnInt int -> Simala.Lit $ Simala.IntLit $ fromIntegral int -- TODO: why does simala only support 'Int'?
  RnDouble _double -> error "Floating point numbers are unsupported in simala"
  RnBool boolean -> Simala.Lit $ Simala.BoolLit boolean
  RnString text -> Simala.Atom text

isFunction :: RnExpr -> Maybe RnName
isFunction expr = isExprOfType expr (RnFunction ==)

isVariable :: RnExpr -> Maybe RnName
isVariable expr = isExprOfType expr (RnVariable ==)

isSelector :: RnExpr -> Maybe RnName
isSelector expr = isExprOfType expr (RnSelector ==)

isExprOfType :: RnExpr -> (RnNameType -> Bool) -> Maybe RnName
isExprOfType (RnExprName name) hasTy
  | hasTy name.rnNameType = Just name
  | otherwise = Nothing
isExprOfType (RnExprLit _) _ = Nothing

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
  RnBuiltin -> "b"

-- ----------------------------------------------------------------------------
-- Assertion helpers
-- ----------------------------------------------------------------------------

assertIsTermAttribute :: SimalaTerm -> Transpiler (Simala.Name, [Simala.Name], Simala.Expr)
assertIsTermAttribute (TermAttribute name selectors expr) = pure (name, selectors, expr)
assertIsTermAttribute term = throwError $ "Expected TermAttribute but got: " <> show term

assertSingletonList :: String -> [a] -> Transpiler a
assertSingletonList _errMsg [a] = pure a
assertSingletonList errMsg as =
  throwError $
    errMsg
      <> "\nExpected singleton list but got: "
      <> show (length as)
      <> " elements"

assertLengthAtLeast :: Int -> [a] -> Transpiler [a]
assertLengthAtLeast l as =
  let
    len = length as
  in
    if len < l
      then
        throwError $
          "Unexpected list size, expected at least: "
            <> show l
            <> " but got: "
            <> show (length as)
      else pure as

assertLength :: Int -> [a] -> Transpiler [a]
assertLength l as =
  let
    len = length as
  in
    if len /= l
      then
        throwError $
          "Expected list size, expected: "
            <> show l
            <> " but got: "
            <> show (length as)
      else pure as

assertNonEmpty :: [a] -> Transpiler (NonEmpty a)
assertNonEmpty [] = throwError "Expected non-empty list"
assertNonEmpty (x : xs) = pure $ x :| xs

assertPredicateIsMultiTerm :: String -> RnRelationalPredicate -> Transpiler RnMultiTerm
assertPredicateIsMultiTerm _errMsg (RnRelationalTerm mt) = pure mt
assertPredicateIsMultiTerm errMsg predicate = throwError $ errMsg <> "\nExpected RnRelationalTerm but got: " <> show predicate

assertTermExpr :: SimalaTerm -> Transpiler Simala.Expr
assertTermExpr (TermExpr expr) = pure expr
assertTermExpr term = throwError $ "Expected TermExpr but got: " <> show term

assertEquals :: (Eq a, Show a) => a -> a -> Transpiler ()
assertEquals a b
  | a == b = pure ()
  | otherwise = throwError $ "Provided args are not equal: " <> show a <> " /= " <> show b

assertIsRecord :: Simala.Expr -> Transpiler (Simala.Row Simala.Expr)
assertIsRecord (Simala.Record row) = pure row
assertIsRecord simalaExpr = throwError $ "Unexpected simala expression, expected Record but got: " <> show simalaExpr

assertAttributeHasSelectors :: SimalaTerm -> Transpiler (NonEmpty Simala.Name, Simala.Expr)
assertAttributeHasSelectors (TermAttribute _ (x : xs) expr) = pure (x :| xs, expr)
assertAttributeHasSelectors expr@(TermAttribute _ [] _) = throwError $ "Unexpected term, expected non-empty TermAttribute but got: " <> show expr
assertAttributeHasSelectors expr = throwError $ "Unexpected term, expected non-empty TermAttribute but got: " <> show expr

-- ----------------------------------------------------------------------------
-- Construction helpers for simala terms
-- ----------------------------------------------------------------------------

mkUndefinedTerm :: SimalaTerm
mkUndefinedTerm = TermExpr Simala.Undefined

mkAssignmentTerm :: Simala.Name -> [Simala.Name] -> Simala.Expr -> Transpiler SimalaTerm
mkAssignmentTerm name selectors expr = pure $ TermAttribute name selectors expr

mkFunctionHead :: Simala.Name -> [Simala.Name] -> Transpiler SimalaTerm
mkFunctionHead funcName funcParams = pure $ TermApp funcName funcParams

mkRecordAssignmentTerm :: Simala.Name -> NE.NonEmpty Simala.Name -> Transpiler SimalaTerm
mkRecordAssignmentTerm varName selectors =
  pure $
    TermAttribute
      varName
      (NE.toList selectors)
      Simala.Undefined

mkLetInTerm :: Simala.Name -> SimalaTerm -> Transpiler SimalaTerm
mkLetInTerm var term = do
  body <- assertTermExpr term
  pure $ TermLetIn Simala.Transparent var body

mkFunctionTerm :: Simala.Name -> [Simala.Name] -> SimalaTerm -> Transpiler SimalaTerm
mkFunctionTerm fnName fnParams term = do
  body <- assertTermExpr term
  pure $ TermFunction Simala.Transparent fnName fnParams body

-- Andres: needs documentation
mkIfThenElseTerm :: Simala.Expr -> SimalaTerm -> SimalaTerm -> Transpiler SimalaTerm
mkIfThenElseTerm b (TermLetIn t1 name1 expr1) (TermLetIn t2 name2 expr2) = do
  assertEquals t1 t2
  assertEquals name1 name2
  let
    ifThenElse = mkIfThenElse b expr1 expr2
  pure $ TermLetIn t1 name1 ifThenElse
mkIfThenElseTerm b (TermLetIn t1 name1 body1) (TermExpr expr) = do
  let
    ifThenElse = mkIfThenElse b body1 expr
  pure $ TermLetIn t1 name1 ifThenElse
mkIfThenElseTerm b (TermAttribute name1 selectors1 expr1) (TermAttribute name2 selectors2 expr2) = do
  assertEquals name1 name2
  assertEquals selectors1 selectors2
  let
    ifThenElse = mkIfThenElse b expr1 expr2
  pure $ TermAttribute name1 selectors1 ifThenElse
mkIfThenElseTerm b (TermAttribute name1 selectors1 expr1) (TermExpr expr2) = do
  let
    ifThenElse = mkIfThenElse b expr1 expr2
  pure $ TermAttribute name1 selectors1 ifThenElse
mkIfThenElseTerm b (TermFunction t1 fnName1 fnParams1 expr1) (TermFunction t2 fnName2 fnParams2 expr2) = do
  assertEquals t1 t2
  assertEquals fnName1 fnName2
  assertEquals fnParams1 fnParams2
  let
    ifThenElse = mkIfThenElse b expr1 expr2
  pure $ TermFunction t1 fnName1 fnParams1 ifThenElse
mkIfThenElseTerm b (TermFunction t fnName1 fnParams1 expr1) (TermExpr expr) = do
  let
    ifThenElse = mkIfThenElse b expr1 expr
  pure $ TermFunction t fnName1 fnParams1 ifThenElse
mkIfThenElseTerm b (TermExpr expr1) (TermExpr expr2) = do
  let
    ifThenElse = mkIfThenElse b expr1 expr2
  pure $ TermExpr ifThenElse
mkIfThenElseTerm _b term1 term2 =
  throwError $
    "Can't wrap terms in an if-then-else.\nFirst term: "
      <> show term1
      <> "\nSecond term: "
      <> show term2

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
  Simala.Let transparency name rhs nextExpr

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

-- TODO: what was I thinking?
mergeRecordUpdates :: [Simala.Row Simala.Expr] -> Transpiler Simala.Expr
mergeRecordUpdates xs = worker xs
 where
  worker rows = do
    let
      vars = NE.groupAllWith fst $ concat rows
    simpleRows <-
      traverse
        simplifyRow
        vars
    pure $ Simala.Record simpleRows

  simplifyRow ::
    NonEmpty (Simala.Name, Simala.Expr) ->
    Transpiler (Simala.Name, Simala.Expr)
  simplifyRow ((n, expr) :| []) = pure (n, expr)
  simplifyRow rows@((n, _) :| _) = do
    let
      rowExprs = fmap snd $ NE.toList rows
    recordRows <- traverse assertIsRecord rowExprs
    mergedRows <- worker recordRows
    pure $ (n, mergedRows)

-- ----------------------------------------------------------------------------
-- Test cases
-- ----------------------------------------------------------------------------

-- >>> transpileRulePure outputWithIndirection
-- "let v_x_0 = let v_y_1 = 5 in v_y_1"

outputWithIndirection :: String
outputWithIndirection =
  [i|
GIVETH x
DECIDE x IS y
WHERE
    y IS 5
|]

-- >>> transpileRulePure exampleWithOneOf
-- "let f_g_4 = fun (v_d_0) => let v_y_1 = if v_d_0 > 0 then e_green_2 else if b_OTHERWISE_5 then e_red_3 else undefined in v_y_1 in undefined"

exampleWithOneOf :: String
exampleWithOneOf =
  [i|
GIVEN d
GIVETH y IS ONE OF green, red
DECIDE g d IS y
WHERE
    y IS green IF d > 0;
    y IS red OTHERWISE
|]

-- >>> transpileRulePure bookWithAttributes
-- "let f_g_1 = fun(v_d_0) => let v_y_2 = {s_book_3 = if v_d_0 > 0 then 'green else if b_OTHERWISE_4 then 'red else undefined} in v_y_2"

bookWithAttributes :: String
bookWithAttributes =
  [i|
GIVEN d
DECIDE g d IS y
WHERE
    y's book IS green IF d > 0;
    y's book IS red OTHERWISE
|]

-- >>> transpileRulePure idFunction
-- "let f_id_1 = fun(v_x_0) => v_x_0"

idFunction :: String
idFunction =
  [i|
GIVEN x
DECIDE id x IS x
|]

-- >>> transpileRulePure sumFunction
-- "let f_sum3_1 = fun(v_x_0) => sum(v_x_0,v_x_0,v_x_0)"

sumFunction :: String
sumFunction =
  [i|
GIVEN x
DECIDE sum3 x IS SUM(x, x, x)
|]

-- >>> transpileRulePure simpleSelector
-- "let f_f_1 = fun(v_x_0) => v_x_0.s_z_2"

simpleSelector :: String
simpleSelector =
  [i|
GIVEN x
DECIDE f x IS x's z
|]

-- >>> transpileRulePure nestedSelector
-- "let f_f_1 = fun(v_x_0) => v_x_0.s_y_2.s_z_3"

nestedSelector :: String
nestedSelector =
  [i|
GIVEN x
DECIDE f x IS x's y's z
|]

-- >>> transpileRulePure decideWithIfs
-- "let f_f_1 = fun(v_x_0) => if v_x_0 > 0 then 1 else if b_OTHERWISE_2 then 0 else undefined"

decideWithIfs :: String
decideWithIfs =
  [i|
GIVEN x
DECIDE f x IS 1 IF x > 0;
       f x IS 0 OTHERWISE
|]

-- >>> transpileRulePure decideWithIfsNoOtherwise
-- "let f_f_1 = fun(v_x_0) => if v_x_0 > 0 then 1 else 0"

decideWithIfsNoOtherwise :: String
decideWithIfsNoOtherwise =
  [i|
GIVEN x
DECIDE f x IS 1 IF x > 0;
       f x IS 0
|]

-- f = fun(x) => if y > 0 then 1 else 0
-- let otherwise = true
-- in
--  let f = fun(x) => if y > 0 then 1 else 0

-- >>> transpileRulePure decideWithIfs2
-- "let f_f_1 = fun(v_x_0) => if v_x_0 > 0 then 1 else if b_OTHERWISE_2 then 0 else if v_x_0 < 0 then 2 else undefined"

decideWithIfs2 :: String
decideWithIfs2 =
  [i|
GIVEN x
DECIDE f x IS 1 IF x > 0;
       f x IS 0 OTHERWISE;
       f x IS 2 IF x < 0
|]

-- >>> transpileRulePure decideWithAttributes
-- "let f_f_1 = fun(v_x_0) => let v_y_2 = {s_p_4 = v_x_0 + v_x_0,s_z_3 = 0} in v_y_2"

decideWithAttributes :: String
decideWithAttributes =
  [i|
GIVEN x
DECIDE f x IS y
WHERE
  y's z IS 0;
  y's p IS SUM(x, x)
|]

-- >>> transpileRulePure decideWithSimpleConditionalAttributes
-- "let f_f_1 = fun(v_x_0) => let v_y_2 = {s_z_3 = if v_x_0 > 3 then 5 else undefined} in v_y_2"

decideWithSimpleConditionalAttributes :: String
decideWithSimpleConditionalAttributes =
  [i|
GIVEN x
DECIDE f x IS y
WHERE
  y's z IS 5 IF x > 3
|]

-- >>> transpileRulePure decideWithConditionalAttributes
-- "let f_f_1 = fun(v_x_0) => let v_y_2 = {s_p_4 = if v_x_0 > 5 then v_x_0 else if b_OTHERWISE_5 then v_x_0 + v_x_0 else undefined,s_z_3 = if v_x_0 > 3 then 5 else if b_OTHERWISE_5 then 0 else undefined} in v_y_2"

decideWithConditionalAttributes :: String
decideWithConditionalAttributes =
  [i|
GIVEN x
DECIDE f x IS y
WHERE
  y's z IS 5 IF x > 3;
  y's z IS 0 OTHERWISE;

  y's p IS x IF x > 5;
  y's p IS SUM(x, x) OTHERWISE
|]

-- let f = fun(x) =>
--    let y_z = if x > 5 then 5 else 0 in
--    let y_p = if x > 5 then x else sum(x, x) in
--    let y = { z = y_z, p = y_p } in
--    y

-- >>> transpileRulePure givethDefinition
-- "let v_y_0 = {s_z_1 = 5}"

givethDefinition :: String
givethDefinition =
  [i|
GIVETH y
DECIDE y's z IS 5
|]

-- >>> transpileRulePure givethNestedDefinition
-- "let v_y_0 = {s_a_1 = {s_b_2 = {s_c_3 = {s_z_4 = 5}}}}"

givethNestedDefinition :: String
givethNestedDefinition =
  [i|
GIVETH y
DECIDE y's a's b's c's z IS 5
|]

-- >>> transpileRulePure eragonBookDescription
-- "let v_eragon_0 = {s_character_3 = {s_friend_6 = 'Ork,s_main_4 = 'Eragon,s_villain_5 = 'Galbatorix},s_size_2 = 512,s_title_1 = 'Eragon}"

eragonBookDescription :: String
eragonBookDescription =
  [i|
GIVETH eragon
DECIDE
  eragon's title IS Eragon;
  eragon's size IS 512;
  eragon's character's main IS "Eragon";
  eragon's character's villain IS "Galbatorix";
  eragon's character's friend IS "Ork"
|]

-- >>> transpileRulePure eragonBookDescriptionWithWhere
-- "let v_eragon_0 = let v_localVar_1 = {s_character_4 = {s_friend_7 = 'Ork,s_main_5 = 'Eragon,s_villain_6 = 'Galbatorix},s_size_3 = 512,s_title_2 = 'Eragon} in v_localVar_1"

eragonBookDescriptionWithWhere :: String
eragonBookDescriptionWithWhere =
  [i|
GIVETH eragon
DECIDE
  eragon IS localVar
WHERE
  localVar's title IS "Eragon";
  localVar's size IS 512;
  localVar's character's main IS "Eragon";
  localVar's character's villain IS "Galbatorix";
  localVar's character's friend IS "Ork"
|]

-- >>> transpileRulePure noGivethDefinitionShouldFail
-- "let v_y_0 = {s_z_1 = 5}"
--
-- TODO: renamer fail, y is unknown, needs to fail!
noGivethDefinitionShouldFail :: String
noGivethDefinitionShouldFail =
  [i|
DECIDE y's z IS 5
|]

-- >>> transpileRulePure noGivethSimpleDefinitionShouldFail
-- "let v_y_0 = 5"
--
-- TODO: renamer fail, y is unknown, needs to fail!
noGivethSimpleDefinitionShouldFail :: String
noGivethSimpleDefinitionShouldFail =
  [i|
DECIDE y IS 5
|]

-- >>> transpileRulePure rodentsAndVermin
-- "Unsupported relational predicate: RPis"
--
rodentsAndVermin :: String
rodentsAndVermin =
  [i|
§ "Rodents and vermin"
DECIDE "Not Covered"
IF
   UNLESS ( "Loss or Damage" IS ANY ( "caused by rodents"
                                    , "caused by insects"
                                    , "caused by vermin"
                                    , "caused by birds"
                                    )

          , ANY ( ALL ( "Loss or Damage" IS "to Contents"
                      , "Loss or Damage" IS "caused by birds"
                      )

                , UNLESS ( "Loss or Damage" IS "ensuing covered loss"

                         , ANY ( "any other exclusion applies"
                               , "an animal caused water to escape from"
                                    ANY ( "a household appliance"
                                        , "a swimming pool"
                                        , "a plumbing, heating, or air conditioning system" )
                               )
                         )
                )
        )
|]

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
  let
    (res, s) = runRenamerFor $ MkSolo rule
  TL.putStrLn $ Pretty.pShow s
  case res of
    Left err -> putStrLn err
    Right (MkSolo rnRule) -> do
      TL.putStrLn $ Pretty.pShow rnRule
      case runExcept $ runTranspiler $ transpile [rnRule] of
        Left err -> putStrLn err
        Right expr -> do
          Text.putStrLn $ "Expr: " <> Simala.render expr

transpileRulePure :: String -> Text
transpileRulePure ruleSrc =
  case run ruleSrc of
    Left err -> Text.pack err
    Right rule -> case fst $ runRenamerFor (MkSolo rule) of
      Left err -> Text.pack err
      Right (MkSolo rnRule) -> do
        case runExcept $ runTranspiler $ transpile [rnRule] of
          Left err -> Text.pack err
          Right expr ->
            Simala.render expr

run :: String -> Either String LS.Rule
run = fmap Parser.transRule . Parser.pRule . Parser.myLexer

runList :: String -> Either String [LS.Rule]
runList = fmap (fmap Parser.transRule) . Parser.pListRule . Parser.myLexer
