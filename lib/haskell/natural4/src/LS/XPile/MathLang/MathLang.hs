{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}

module LS.XPile.MathLang.MathLang
  ( toMathLangMw,
    toMathLang,
    toMathLangExpand,
    gml2ml,
    runToMathLang,
  )
where

import Prelude hiding (exp)

import Control.Arrow ((>>>))
import Data.HashMap.Strict qualified as Map
import Data.List (groupBy, nub, (\\))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.String.Interpolate (i, __i)
import Data.Text qualified as T
import Debug.Trace (trace)
import Effectful (Eff, runPureEff)
import Effectful.Fail (Fail, runFail)
import Effectful.Reader.Static (Reader, ask, runReader)
import Effectful.Writer.Dynamic (Writer, runWriterLocal, tell)
import Explainable.MathLang hiding ((|>))
import Flow ((|>))

import LS.Rule (Interpreted (..))
import LS.XPile.IntroReader (MyEnv)
import LS.XPile.MathLang.GenericMathLang.GenericMathLangAST (BaseExp (..), ExplnAnnot (l4RuleName))
import LS.XPile.MathLang.GenericMathLang.GenericMathLangAST qualified as GML
import LS.XPile.MathLang.GenericMathLang.TranslateL4 qualified as GML
import LS.XPile.MathLang.GenericMathLang.ToGenericMathLang qualified as GML
import Optics
  ( Iso',
    coerced,
    gplate,
    over,
    view,
  )
import Prettyprinter (Doc, braces, vcat, pretty)
import qualified Prettyprinter as PP
import Text.Regex.PCRE.Heavy qualified as PCRE
{-
YM: This is currently more like a NOTES file,
with comments from MEng. Will integrate these later.
-}

-- ----------------------------------------------------------------------------
-- Main entry points for MathLang interpretations and translations (e.g., transpilers)
-- ----------------------------------------------------------------------------

toMathLang :: Interpreted -> ([Expr Double], MyState)
toMathLang = toMathLang' False

toMathLangExpand :: Interpreted -> ([Expr Double], MyState)
toMathLangExpand = toMathLang' True

toMathLang' :: Bool -> Interpreted -> ([Expr Double], MyState)
toMathLang' expand l4i = case GML.runToLC $ GML.l4ToLCProgram l4Hornlikes of
  Left errors -> trace [i|\ntoMathLang: failed when turning into GML, #{errors}\n|] ([], emptyState) -- GML.makeErrorOut errors
  Right prog -> lcProgToMathLang prog
  where
    l4Hornlikes = GML.getHornlikes l4i
      |> (if expand then GML.expandHornlikes l4i else id)
      |> GML.insertTypeDecls l4i

lcProgToMathLang :: GML.LCProgram -> ([Expr Double], MyState)
lcProgToMathLang lamCalcProgram = (toplevels, st)
  where
    userfuns = getUserFuns 0 Map.empty lamCalcProgram.userFuns
    st = gmls2ml userfuns lamCalcProgram.lcProgram
    giveth = (\x -> [i|#{x}|]) <$> lamCalcProgram.giveths

    toplevels = case Map.lookup "Top-Level" st.symtabF of
      Just exp -> [exp]
      Nothing -> case giveth of
        [] -> trace [i|\ntoMathLang: no giveth, returning all in symTab\n|] relevantSymtab st
        (mapMaybe go -> exprs@(_:_)) -> exprs
        ks -> trace [i|\ntoMathLang: no set variable given in #{ks}\n     st = #{st}\n     userfuns = #{userfuns}|] $ relevantSymtab st

    go k = MathSet k <$> Map.lookup k st.symtabF

-- If all values in symtabF are Pred or Undefined, use symtabP
relevantSymtab :: MyState -> [Expr Double]
relevantSymtab st
  | all predOrUndefined st.symtabF = MathPred <$> Map.elems st.symtabP
  | otherwise                      = Map.elems st.symtabF
  where
    predOrUndefined (Undefined _) = True
    predOrUndefined (MathPred _) = True
    predOrUndefined _ = False

-- | calling the output "MyState" is misleading, but this is the most general way to cover the idea that
-- a ruleset consists of more than one rule, similar to how the Vue interface gives more than one
-- element in the left nav; each of the different rules will have its own entry in the SymTab dictionary.
--
-- so, for example, if we have a single MustSing input, we would expect the output MyState dictionary symtab
-- to contain an @Expr Double@ called "must sing"
toMathLangMw :: Interpreted -> MyEnv -> (String, [String])
toMathLangMw l4i _myenv = (rendered, [])
 where
  (exprs, stRaw) = toMathLangExpand l4i
  state = stRaw {symtabF = Map.mapWithKey reintroduceSetVar $ symtabF stRaw}
  rendered = [__i|
                #{vcat $ fmap renderExp exprs}
                export const AllExprs = #{PP.list $ fmap (pretty.renderName) exprs}
                #{vcat $ fmap renderExp $ stateNotInExprs exprs state}
             |]

  stateNotInExprs _es st = relevantSymtab st \\ exprs

  replaceSpaces :: String -> String
  replaceSpaces = PCRE.gsub [PCRE.re| |] ("_" :: String)

  renderName :: (Show a) => Expr a -> String
  renderName expr = maybe "unnamedExpr" replaceSpaces $ getExprLabel expr

  renderExp :: (Show a) => Expr a -> Doc ann
  renderExp expr = [i|export const #{name} = () => #{ret expr}|]
    where
      name = renderName expr
      ret doc = braces [i|return #{pp doc}|]


-- ----------------------------------------------------------------------------
-- Generic MathLang to explainable MathLang translation.
-- Note, not all constructs are supported in explainable MathLang.
-- ----------------------------------------------------------------------------

-- | Translate the given generic mathlang expressions to explainable mathlang.
--
-- All of the results are in MyState, so we can ignore the actual res
gmls2ml :: SymTab VarsAndBody -> [GML.Exp] -> MyState
gmls2ml _userfuns [] = emptyState
gmls2ml userfuns (e:es) = st <> gmls2ml userfuns es
  where -- TODO: temporary hack, probably reconsider when exactly stuff is put into MyState
    seqE = case e.exp of
      ESeq _ -> e
      _ -> GML.MkExp (ESeq (GML.SeqExp [e])) []
    st = execToMathLang userfuns $ gml2ml seqE -- NB. returns emptyState if gml2ml fails

-- | Translate a generic mathlang expression to an explainable mathlang expression if possible.
-- If it isn't possible, we try our best to avoid crashes. If an expression can't be translated,
-- we insert a variable placeholder. This may allow the run-time to insert expressions later.
-- However, in the future, we should make sure explainable mathlang and generic mathlang have the
-- same feature set.
gml2ml :: GML.Exp -> ToMathLang (Expr Double)
gml2ml exp = case expExp of
  EEmpty -> fail "gml2ml: unexpected EEmpty"
  ESeq expSeq -> case getList exp of
    Just exps -> do
      let varname = "FIXME: unlabeled list found outside a context, probably an error"
      _ <- mkList varname exps -- puts list in MyState
      pure $ MathVar varname -- this function needs to return an Expr, not an ExprList so just return the MathVar
    Nothing -> mkVarSet $ GML.seqExpToExprs expSeq

  ELit lit -> pure $ mkVal lit

  EVar (GML.MkVar var) -> pure $ MathVar [i|#{var}|]

  ENumOp GML.OpMinOf e1 e2 -> do
    -- TODO: make it into a fold
    MathMin Nothing <$> gml2ml e1 <*> gml2ml e2

  ENumOp GML.OpMaxOf e1 e2 -> do
    MathMax Nothing <$> gml2ml e1 <*> gml2ml e2

  ENumOp op e1 e2 -> do
    mathOp <- numOptoMl op
    ex1 <- gml2mlWithListCoercion mathOp e1
    ex2 <- gml2mlWithListCoercion mathOp e2
    pure $ MathBin Nothing mathOp ex1 ex2

  EVarSet var (getList -> Just exps) ->
    trace [i|!!! found list #{var} = #{exps}|] do
      MathVar varName <- gml2ml var
      _ <- mkList varName exps -- puts list in MyState
      pure $ MathVar varName -- this function needs to return an Expr, not an ExprList so just return the MathVar

  EVarSet var val -> do
    MathVar varEx <- gml2ml var
    valEx <- gml2ml val
    let valExWithLabel = varEx @|= valEx
    --    if we get rid of the assumption that everything is a SeqExp, should do this
    --    ToMathLang $ tell $ emptyState { symtabF = Map.singleton varEx valExWithLabel }
    pure $ MathSet varEx valExWithLabel

  EPredSet _ _ -> do
    predSet@(PredSet name pr) <- exp2pred exp
    ToMathLang $ tell emptyState {symtabP = Map.singleton name pr}
    pure $ MathPred predSet -- this is just dummy to not have it crash, this value won't be present in the final result

  EIfThen condE thenE -> do
    condP <- exp2pred condE
    thenEx <- gml2ml thenE
    pure $ MathITE Nothing condP thenEx placeholderITE

  ERec fieldname recname -> do
    fnEx <- gml2ml fieldname
    rnEx <- gml2ml recname
    case (fnEx, rnEx) of
      (MathVar fname, MathVar rname) -> pure $ MathVar [i|#{rname}.#{fname}|]
      _x -> pure $ MathVar [i|gml2ml: unsupported record #{expExp}|]

  ELam (GML.MkVar v) body -> trace [i|\ngml2ml: found ELam #{exp}\n|] do
    bodyEx <- gml2ml body
    let varEx = [i|#{v}|]
    trace [i|     arg = #{v}\n      body = #{bodyEx}\n|] pure $
      MathSet varEx bodyEx

  EApp {} -> mkApp exp []

  EIs left (getPred -> Just right) -> do
    MathVar var <- gml2ml left
    gml2ml (exp {GML.exp = EPredSet (GML.MkVar [i|#{var}|]) right})

  EIs left right -> gml2ml exp {GML.exp = EVarSet left right}

  ENot body -> do
    mlBody <- exp2pred body
    pure $ MathPred (PredNot Nothing mlBody)

  ECompOp _ _ _ -> unhandledExpressionType

  EIfTE _ _ _ -> unhandledExpressionType

  ELet _ _ _ -> unhandledExpressionType

  EAnd _ _ -> unhandledExpressionType

  EOr _ _ -> unhandledExpressionType

  where
    expExp = exp.exp
    unhandledExpressionType = trace [i|\ngml2ml: not supported #{exp}\n|]
      pure $ MathVar [i|gml2ml: not implemented yet #{expExp}|]
    {- In order to keep the information of application but also expand it, we do two things:
        i) put the expanded function application into the state, as follows
            ( "Step 3 discounted by accident.risk percentage"
            , … Step 3 * (1 - accident.risk percentage … ) -- the actual definition as Expr Double

        ii) return name of the fun applied to args as a MathVar (whose value is found in state)
             MathVar "Step 3 discounted by accident.risk percentage"
    -}
    mkApp :: GML.Exp -> [Expr Double] -> ToMathLang (Expr Double)
    mkApp (GML.exp -> EApp f arg) args = do
      arg' <- gml2ml arg
      mkApp f (arg' : args)

    mkApp (GML.exp -> EVar (GML.MkVar f)) args = do
      userFuns :: SymTab VarsAndBody <- ToMathLang ask -- HashMap String ([Var], Expr Double)
      case Map.lookup [i|#{f}|] userFuns of
        Nothing -> fail [i|mkApp: trying to apply undefined function #{f}|]
        Just (boundVars, expr) -> do
          let funAppliedToArgsName = case getExprLabel <$> args of
                [Just arg] -> [i|#{f} #{arg}|]
                [Just arg1, Just arg2] -> [i|#{arg1} #{f} #{arg2}|]
                _ -> [i|TODO: #{f} applied to 3 or more arguments, or the arguments don't have labels|]
              expandedExpr = replaceVars (Map.fromList $ zip boundVars args) expr
              namedExpr = funAppliedToArgsName @|= expandedExpr
          ToMathLang $ tell emptyState {symtabF = Map.singleton funAppliedToArgsName namedExpr}
          pure $ MathVar funAppliedToArgsName

    mkApp e _ = trace [i|\ngml2ml.mkApp, exp=#{e}\n|] fail "mkApp: unexpected thing happened"

    gml2mlWithListCoercion :: MathBinOp -> GML.Exp -> ToMathLang (Expr Double)
    gml2mlWithListCoercion op (getList -> Just exps) =
      trace [i|gml2mlWithListCoercion: is a list #{exps}|] do
        list <- mkList "inline" exps -- this is only called from ENumOp to its arguments: if it's a list, then it's inline. (I think as of 20240503, this is impossible, but maybe it should be possible: get a better type inference and in the future this works.)
        fold <- op2somefold op
        pure $ ListFold Nothing fold list

    gml2mlWithListCoercion _ gmlExp = gml2ml gmlExp -- not a list

    getList :: GML.Exp -> Maybe [GML.Exp]
    getList gmlExp = case (gmlExp.exp, GML.typeLabel <$> gmlExp.md) of
      (ESeq seqExp, Just (GML.FromUser (GML.L4List _)):_)
        -> Just (GML.seqExpToExprs seqExp)
      _ -> Nothing

    getPred :: GML.Exp -> Maybe GML.Exp
    getPred gmlExp = case gmlExp.exp of
      EAnd {} -> Just gmlExp
      EOr {} -> Just gmlExp
      ENot {} -> Just gmlExp
      _ -> Nothing

    mkList :: String -> [GML.Exp] -> ToMathLang (ExprList Double)
    mkList varname exps = do
      seqs <- traverse gml2ml exps
      let list = MathList Nothing seqs
      ToMathLang $ tell emptyState {symtabL = Map.singleton varname list}
      -- pure $ MathVar varname
      pure list

    op2somefold :: MathBinOp -> ToMathLang SomeFold
    op2somefold Plus = pure FoldSum
    op2somefold Times = pure FoldProduct
    op2somefold op = fail [i|gml2ml: not allowed to apply #{op} to a list|]

    mkVarSet :: [GML.Exp] -> ToMathLang (Expr Double)
    mkVarSet exps = do
      seqs <- traverse gml2ml exps
      let newSeqs =
            -- trace [i|\ngml2ml: seqs #{seqs}\n|] $
            chainITEs seqs
          newF =
            -- trace [i|\ngml2ml: newSeqs #{newSeqs}\n|] $
            Map.fromList [(var, MathSet var val) | MathSet var val <- newSeqs]

      ToMathLang $ tell emptyState {symtabF = newF}

      let seqHeadName = case newSeqs of
            MathSet headName _ : _ -> headName
            MathVar headName : _ -> headName
            _ -> fail [i|\nUnexpected thing: #{newSeqs}\n\nFrom #{seqs}\n\nFrom #{exps}|]

      pure $ MathVar seqHeadName

-- | Translate an expression into an explainable predicate.
exp2pred :: GML.Exp -> ToMathLang (Pred Double)
exp2pred exp = case exp.exp of
  EEmpty -> fail "exp2pred: Unexpected EEmpty"
  EVar (GML.MkVar var) -> pure $ PredVar [i|#{var}|]
  ECompOp op e1 e2 ->
    PredComp Nothing (compOptoMl op) <$> gml2ml e1 <*> gml2ml e2
  EIs e1 e2 -> do
    ex1 <- gml2ml e1
    ex2 <- gml2ml e2
    case (ex1, ex2) of
      (MathVar var, val) -> do
        let ex2withLabel = var @|= val
        pure $ PredComp (Just [i|#{var} == #{showExpr val}|]) CEQ ex1 ex2withLabel
      _ -> fail [i|\nexp2pred: expected Var, got #{ex1}\n|]
  ELit GML.EBoolTrue -> pure $ PredVal Nothing True
  ELit GML.EBoolFalse -> pure $ PredVal Nothing False
  ELit (GML.EString lit) -> pure $ PredVar [i|#{lit}|]
  EOr {} -> PredFold (getLabel exp) PLOr <$> foldPredOr exp
  EAnd {} -> PredFold (getLabel exp) PLAnd <$> foldPredAnd exp
  EPredSet (GML.MkVar var) val -> do
    let varStr = [i|#{var}|]
    valEx <- exp2pred val
    let valExWithLabel = case valEx of
          PredVar _ -> valEx
          PredSet _ _ -> valEx
          _ -> varStr @|= valEx
    pure $ PredSet varStr valExWithLabel
  ENot body -> PredNot Nothing <$> exp2pred body
  EIfThen _ _ -> unhandledExpressionType
  EIfTE _ _ _ -> unhandledExpressionType
  ELam _ _ -> unhandledExpressionType
  EApp _ _ -> unhandledExpressionType
  EVarSet _ _ -> unhandledExpressionType
  ELet _ _ _ -> unhandledExpressionType
  ERec fieldname recname -> do
    fnEx <- gml2ml fieldname
    rnEx <- gml2ml recname
    case (fnEx, rnEx) of
      -- If both expressions can be resolved to a variable,
      -- we infer that this a record access of the form "var1.var2".
      (MathVar fname, MathVar rname) -> pure $ PredVar [i|#{rname}.#{fname}|]
      _x -> pure $ PredVar [i|exp2pred: unsupported record #{expExp}|]
  ENumOp _ _ _ -> unhandledExpressionType
  ESeq _ -> unhandledExpressionType
  ELit _ -> unhandledExpressionType
  where
    expExp = exp.exp
    unhandledExpressionType = trace ("exp2pred: not yet implemented\n    " <> show exp.exp) $ do
      pure $ PredVar [i|Not implemented yet: #{expExp}|]

numOptoMl :: GML.NumOp -> ToMathLang MathBinOp
numOptoMl = \case
  GML.OpPlus -> pure Plus
  GML.OpMinus -> pure Minus
  GML.OpMul -> pure Times
  GML.OpDiv -> pure Divide
  GML.OpModulo -> pure Modulo
  GML.OpSum -> pure Plus
  GML.OpProduct -> pure Times
  GML.OpMaxOf -> fail [i|numOptoMl: encountered OpMaxOf|]
  GML.OpMinOf -> fail [i|numOptoMl: encountered OpMinOf|]

-- | Translate generic mathlang comparisons to explainable mathlang.
-- Note, we don't differentiate between String, Bool or Number equality.
-- Further, explainable mathlang doesn't support Strings, so we throw an
-- error if a string equality is encountered.
compOptoMl :: GML.CompOp -> Comp
compOptoMl = \case
  GML.OpNumEq -> CEQ
  GML.OpLt -> CLT
  GML.OpLte -> CLTE
  GML.OpGt -> CGT
  GML.OpGte -> CGTE
  GML.OpBoolEq -> CEQ
  GML.OpStringEq -> error "MathLang doesn't support strings"

-- | Constant representing 'True'.
trueVariableName :: String
trueVariableName = "True"

-- | Constant representing 'False'.
falseVariableName :: String
falseVariableName = "False"

-- | Turn a literal into an explainable mathlang expression.
-- As explainable mathlang only supports 'Double's, this conversion is lossy.
-- For example, we lose date information and create variables for boolean literals.
-- However, we can provide variable assignment for
mkVal :: GML.Lit -> Expr Double
mkVal = \case
  GML.EInteger int -> Val Nothing $ fromInteger int
  GML.EFloat float -> Val Nothing float
  GML.EString lit -> MathVar [i|#{lit}|]
  GML.ECurrency curr double -> Val (Just [i|#{curr} #{double}|]) double
  -- These should probably be handled in a different way?
  -- Booleans are handled in Pred, not Expr. There is currently nowhere that Dates are handled in MathLang.
  GML.EBoolTrue -> MathVar trueVariableName
  GML.EBoolFalse -> MathVar falseVariableName
  GML.EDate day -> MathVar [i|#{day}|]
  GML.EENum val -> MathVar [i|#{val}|]

getLabel :: GML.Exp -> Maybe String
getLabel e = case e.md of
  m:_ms -> T.unpack . l4RuleName <$> m.explnAnnot
  []   -> Nothing

foldPredOr :: GML.Exp -> ToMathLang (PredList Double)
foldPredOr e = case e.exp of
  EOr l r -> do
    predL <- exp2pred l
    predR <- foldPredOr r
    pure $ predL : predR
  EEmpty -> pure []
  _x -> (:[]) <$> exp2pred e

foldPredAnd :: GML.Exp -> ToMathLang (PredList Double)
foldPredAnd e = case e.exp of
  EAnd l r -> do
    predL <- exp2pred l
    predR <- foldPredOr r
    pure $ predL : predR
  EEmpty -> pure []
  _ -> (:[]) <$> exp2pred e

chainITEs :: [Expr Double] -> [Expr Double]
chainITEs es = [moveVarsetToTop x xs | (x:xs) <- groupBy sameVarSet es]
  where
    moveVarsetToTop :: Expr Double -> [Expr Double] -> Expr Double
    moveVarsetToTop
      (MathITE lbl condP (MathSet var1 val1) (Undefined _)) exprs =
        MathSet var1 $ MathITE lbl condP val1 $ go exprs
      where
        go [MathITE _ (PredVal _ True) (MathSet _var2 val2) (Undefined _)] = val2
        go ((MathITE lbl2 condP2 (MathSet var2 val2) (Undefined _)):xs)
          | var1 == var2 = MathITE lbl2 condP2 val2 (go xs)
        go [] = Undefined (Just "No otherwise case")
        go xs = error $ "chainITEs.go: unexpected arguments: " <> show xs

    moveVarsetToTop x [] = x

    moveVarsetToTop x _ =
      trace "moveVarsetToTop: groupBy didn't do what's expected" x

    sameVarSet :: Expr Double -> Expr Double -> Bool
    sameVarSet (MathITE _ _ (MathSet var1 _) _ )
               (MathITE _ _ (MathSet var2 _) _ ) = var1 == var2
    sameVarSet _ _ = False

placeholderITE :: Expr Double
placeholderITE = Undefined (Just "placeholder for ITE")

-- | Add all user defined functions to the symbol table that maps function names to
-- a list of parameters and its respective implementation.
getUserFuns :: Int -> SymTab VarsAndBody -> SymTab ([GML.Var], GML.Exp) -> SymTab VarsAndBody
getUserFuns initialIx initialFirstPass initialFuns =
  NE.last $ NE.unfoldr go (initialIx, initialFirstPass, initialFuns)
  where
    go (ix, firstPass, funs) =
      let
        newFuns = funToVarAndBody firstPass <$> funs
      in
      trace [i|#{ix}: firstPass = #{firstPass}|]
        let fixPoint = if firstPass == newFuns
              then Nothing
              else Just (ix + 1, newFuns, funs)
        in
          (newFuns, fixPoint)

    funToVarAndBody :: SymTab VarsAndBody -> ([GML.Var], GML.Exp) -> VarsAndBody
    funToVarAndBody firstPass (boundVars, exp) =
      case runToMathLang firstPass $ mkAppForUF exp [] of
        Right mlExp -> ([[i|#{v}|] | GML.MkVar v <- nub boundVars], mlExp)
        Left errorMessage ->
          trace
            (if null firstPass then "" else [i|getUserFuns: #{errorMessage}|])
            ([], Undefined Nothing)

    mkAppForUF :: GML.Exp -> [String] -> ToMathLang (Expr Double)
    mkAppForUF (isApp -> Just (f, arg)) args = mkAppForUF f (arg : args)
    mkAppForUF (GML.exp -> EVar (GML.MkVar f)) args = do
      userFuns <- ToMathLang ask -- HashMap String ([Var], Expr Double)
      case Map.lookup f' userFuns of
        Nothing -> fail [i|mkAppForUF: this really shouldn't happen, but #{f} is not found in userFuns|]
        Just (boundVars, expr) -> do
          let newVars = map MathVar args
              replacedDef = replaceVars (Map.fromList $ zip boundVars newVars) expr
          pure $ MathApp Nothing f' args replacedDef -- still only replaced with the new set of arguments, not with more complex expressions
      where
        f' = [i|#{f}|]

    mkAppForUF exp _ = gml2ml exp

    isApp :: GML.Exp -> Maybe (GML.Exp, String)
    isApp (GML.exp -> EApp f (GML.exp -> GML.EVar (GML.MkVar arg))) =
      Just (f, [i|#{arg}|])
    isApp _ = Nothing

-- | Replace already resolved variables in an expression with their respective value.
-- If a variable isn't known yet, we leave it as is.
--
-- Example, assume the variables that are bound are:
--
-- * @[("x", a), ("y", b)]@
--
-- and the expressoin is roughly @"Var x" + Var "y"@. Then we can safely
-- rewrite this expression to @a + b@.
--
-- +----------------------+-------------------+--------+
-- |     Symbol Table     |    Expression     | Result |
-- +----------------------+-------------------+--------+
-- | [("x", a), ("y", b)] | Var "x" + Var "y" | a + b  |
-- +----------------------+-------------------+--------+
--
replaceVars :: Map.HashMap String (Expr Double) -> Expr Double -> Expr Double
replaceVars table = replace >>> returnBody
  where
    replace = \case
      MathVar k@(_:_) -> case Map.lookup k table of
                      Just v -> v
                      Nothing -> MathVar k
      x -> over (gplate @(Expr Double)) replace x

    returnBody = \case
      MathApp _lbl _name _vars body -> body
      expr -> expr

isSetOrPred :: Expr a -> Bool
isSetOrPred (MathSet _ _) = True
isSetOrPred (MathPred _) = True
isSetOrPred _ = False

-- MathSet "varName" expr --> expr
reintroduceSetVar :: String -> Expr Double -> Expr Double
reintroduceSetVar _var expr@(isSetOrPred -> True) = expr
reintroduceSetVar var  expr                       = MathSet var expr

-- ----------------------------------------------------------------------------
-- Translation types and effect monad types.
-- ----------------------------------------------------------------------------

type ToMathLangError = String
type VarsAndBody = ([String], Expr Double)

newtype ToMathLang a =
  ToMathLang (Eff [ Writer MyState
                  , Reader (SymTab VarsAndBody)
                  , Fail ] a)
  deriving newtype (Functor, Applicative, Monad, MonadFail)

_ToMathLang ::
  Iso' (ToMathLang a) (Eff [Writer MyState, Reader (SymTab VarsAndBody), Fail] a)
_ToMathLang = coerced

unToMathLang ::
  ToMathLang a -> Eff [Writer MyState, Reader (SymTab VarsAndBody), Fail] a
unToMathLang = view _ToMathLang

-- | Run the 'ToMathLang' action with the symbol table as a read-only variable.
runToMathLang :: SymTab VarsAndBody -> ToMathLang a -> Either String a
runToMathLang r m = case runToMathLang' r m of
              Right (res,_state) -> Right res
              Left err           -> Left err

execToMathLang :: SymTab VarsAndBody -> ToMathLang a -> MyState
execToMathLang r m = case runToMathLang' r m of
                    Right (_res,state) -> state
                    Left _             -> emptyState

runToMathLang' :: SymTab VarsAndBody -> ToMathLang a -> Either ToMathLangError (a, MyState)
runToMathLang' r =
  unToMathLang
    >>> runWriterLocal
    >>> runReader r
    >>> runFail
    >>> runPureEff

{-
  let topLevelExprs = qaHorns l4i

  -- what would the L4 of this be?
  -- § Two Plus Two is Four A       // version A, done as a list fold
  -- DECIDE four IS SUM two
  --                    two
  -- DECIDE two IS 2

      mathLangExpr1a = "Two Plus Two is Four A" @|= ("four" @|= sumOf [ "two" @|. 2
                                                                      , "two" @|. 2])
  -- § Two Plus Two is Four B      // version B, done as a binOp +
  -- DECIDE fourB IS  two  +  two
      mathLangExpr1b = "Two Plus Two is Four B" @|= ("fourB" @|= ("two" @|. 2)    |+   ("two" @|. 2))

  -- § Must Sing A
  -- EVERY person
  --   WHO qualifies
  --        MEANS  walks
  --          AND  eats
  --           OR  drinks
  --  MUST sing

      mathLangExpr2a = "qualifies" @|= (getVar "walks" |&& ( getVar "eats" ||| getVar "drinks") )

  -- §  amount you get
  -- DECIDE  amount  you get  IS  700  WHEN  qualifies
  -- "       "                IS  100  OTHERWISE

  -- mathlang doesn't do deontics, so we focus on the qualifying part -- see the comment below for an exhaustive dump of what the L4 AST actually looks like, and the qaHornsT version that we want to tackle.

      pau6amount = "amount you get" @|= (mathLangExpr2a
                                         @|? (Val Nothing 700)
                                         @|: (Val Nothing 100))



      debuggingOutput = pShowNoColorS $ mathLangExpr2a

   in (debuggingOutput, ["here we will pretend that the dumpTypescript function returned a well-behaved pure String and not an IO argh"])
-}

-- the Qualifies bit gets broken out into its own Hornlike rule, BUT you should access it via qaHornsT, which exposes it
-- for use by transpilers.

-- This is what's in the [Rule] AST, but read below
-- Hornlike
--     { name =
--         [ MTT "Qualifies" ]
--     , super = Nothing
--     , keyword = Means
--     , given = Nothing
--     , giveth = Nothing
--     , upon = Nothing
--     , clauses =
--         [ HC
--             { hHead = RPBoolStructR
--                 [ MTT "Qualifies" ] RPis
--                 ( All Nothing
--                     [ Leaf
--                         ( RPMT
--                             [ MTT "walks" ]
--                         )
--                     , Any Nothing
--                         [ Leaf
--                             ( RPMT
--                                 [ MTT "Drinks" ]
--                             )
--                         , Leaf
--                             ( RPMT
--                                 [ MTT "eats" ]
--                             )
--                         ]
--                     ]
--                 )
--             , hBody = Nothing
--             }
--         ]
--     , rlabel = Nothing
--     , lsource = Nothing
--     , wwhere = []
--     , srcref = Just
--         ( SrcRef
--             { url = "/Users/mengwong/Downloads/LegalSS v0.9.4.3 - Must Sing 5.csv"
--             , short = "/Users/mengwong/Downloads/LegalSS v0.9.4.3 - Must Sing 5.csv"
--             , srcrow = 6
--             , srccol = 7
--             , version = Nothing
--             }
--         )
--     , defaults = []
--     , symtab = []
--     }
-- ]


-- if you call qaHornsT you get just the BoolStruct on the inside
--
-- All Nothing
--     [ Leaf
--         ( RPMT
--             [ MTT "walks" ]
--         )
--     , Any Nothing
--         [ Leaf
--             ( RPMT
--                 [ MTT "drinks" ]
--             )
--         , Leaf
--             ( RPMT
--                 [ MTT "eats" ]
--             )
--         ]
--     ]




-- we want to read the rules from Interpreted and transform them into MathLang expressions.
-- so for example, if the L4 says
-- DECIDE foo
--     IF bar
--    AND baz
--    AND quux
--
-- we would want the MathLang output to be
-- output :: Pred Double
-- output = PredBin Nothing PredAnd (PredVar "foo")
--                                  (PredBin Nothing PredAnd (PredVar "bar")
--                                                           (PredBin Nothing PredAnd (PredVar "baz")
--                                                                                    (PredVar "quux")))
--
-- DECIDE foo
--     IF      ALL   bar
--                   baz
--                   quux
--
-- output :: Pred Double
-- output = PredFold Nothing PLAnd [PredVar "foo"
--                                 ,PredVar "bar"
--                                 ,PredVar "baz"
--                                 ,PredVar "quux"]
