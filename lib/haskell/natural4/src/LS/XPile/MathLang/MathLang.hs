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

module LS.XPile.MathLang.MathLang
  (toMathLangMw, toMathLang, gml2ml, runToMathLang)
where

import Control.Monad.Except (MonadError, throwError)
import Data.ByteString.Builder (generic)
import Data.Coerce (coerce)
import Data.Either (rights)
import Data.List (groupBy, nub)
import Data.Generics.Sum.Constructors (AsConstructor (_Ctor))
import Data.HashMap.Strict qualified as Map
import Data.String.Interpolate (i,__i)
import Data.Text qualified as T
import Effectful (Eff, (:>), runPureEff)
import Effectful.Fail (Fail, runFail)
import Effectful.Reader.Static (Reader, runReader, local, asks, ask)
import Effectful.Writer.Dynamic (Writer, runWriterLocal, tell)
import Explainable.MathLang hiding ((|>))
import LS.Interpreter
import LS.Rule (Rule, Interpreted (..))
import LS.XPile.IntroReader (MyEnv)
import LS.XPile.MathLang.GenericMathLang.GenericMathLangAST (BaseExp (..))
import LS.XPile.MathLang.GenericMathLang.GenericMathLangAST qualified as GML
import LS.XPile.MathLang.GenericMathLang.TranslateL4 qualified as GML
import Optics (Fold,Iso', view, re, coerced, cosmosOf, filteredBy, folded, gplate, over, (%), (%~), (^..))
import Flow ((|>))
import Debug.Trace (trace)
import Data.Maybe (maybeToList, mapMaybe)
{-
YM: This is currently more like a NOTES file,
with comments from MEng. Will integrate these later.
-}

toMathLang :: Interpreted -> ([Expr Double], MyState)
toMathLang l4i =
  let l4Hornlikes =
       l4i.origrules ^.. folded % cosmosOf (gplate @Rule) % filteredBy (_Ctor @"Hornlike")
  in case GML.runToLC $ GML.l4ToLCProgram l4Hornlikes of
        Left errors -> trace [i|\ntoMathLang: failed when turning into GML, #{errors}\n|] ([], emptyState) -- GML.makeErrorOut errors
        Right prog -> lcProgToMathLang prog

lcProgToMathLang :: GML.LCProgram -> ([Expr Double], MyState)
lcProgToMathLang lamCalcProgram = (toplevels, st)
  where
    userfuns = getUserFuns 0 Map.empty lamCalcProgram.userFuns
    st = gmls2ml userfuns lamCalcProgram.lcProgram
    giveth = T.unpack <$> lamCalcProgram.giveths
    toplevels = case Map.lookup "Top-Level" st.symtabF of
      Just exp -> [exp]
      Nothing ->
        case giveth of
          [] -> trace [i|\ntoMathLang: no giveth, returning all in symTab\n|] $ Map.elems st.symtabF
          ks -> case ks |> mapMaybe \k -> MathSet k <$> Map.lookup k st.symtabF of
                [] -> trace [i|\ntoMathLang: no set variable given in #{ks}\n     st = #{st}\n     userfuns = #{userfuns}|] $ Map.elems st.symtabF
                exprs -> exprs

--numOptoMl :: MonadError T.Text m => GML.NumOp -> m MathBinOp
numOptoMl :: GML.NumOp -> ToMathLang MathBinOp
numOptoMl = \case
  GML.OpPlus -> pure Plus
  GML.OpSum -> pure Plus
  GML.OpMinus -> pure Minus
  GML.OpMul -> pure Times
  GML.OpProduct -> pure Times
  GML.OpDiv -> pure Divide
  GML.OpModulo -> pure Modulo
  op -> fail [i|numOptoMl: encountered #{op}|]

compOptoMl :: GML.CompOp -> Comp
compOptoMl = \case
  GML.OpNumEq -> CEQ
  GML.OpLt -> CLT
  GML.OpLte -> CLTE
  GML.OpGt -> CGT
  GML.OpGte -> CGTE
  _ -> CNEQ -----

--mkVal :: MonadError T.Text m => GML.Lit -> m (Expr Double)
mkVal :: GML.Lit -> Expr Double
mkVal = \case
  GML.EInteger int -> Val Nothing $ fromInteger int
  GML.EFloat float -> Val Nothing float
  GML.EString lit -> MathVar $ T.unpack lit
  GML.ECurrency curr double -> Val (Just [i|#{curr} #{double}|]) double
  -- These should probably be handled in a different way? Booleans are handled in Pred, not Expr. There is currently nowhere that Dates are handled in MathLang.
  GML.EBoolTrue -> MathVar "True"
  GML.EBoolFalse -> MathVar "False"
  GML.EDate day -> MathVar $ show day
  GML.EENum val -> MathVar $ T.unpack val
--  lit -> throwError [i|mkVal: encountered #{lit}|]

exp2pred :: GML.Exp -> ToMathLang (Pred Double)
exp2pred exp = case exp.exp of
  EEmpty -> fail "exp2pred: Unexpected EEmpty"
  EVar (GML.MkVar var) -> pure $ PredVar $ T.unpack var
  ECompOp op e1 e2 ->
    PredComp Nothing (compOptoMl op) <$> gml2ml e1 <*> gml2ml e2
  EIs e1 e2 -> do
    ex1 <- gml2ml e1
    ex2 <- gml2ml e2
    case (ex1, ex2) of
      (MathVar var, MathVar val) -> -- phaseOfMoon IS gibbous, for now treat as a record. Should work nicely for enums, but do we want freeform text?
        pure $ PredVar [i|#{var}.#{val}|]
      (MathVar var, val) -> do
        let ex2withLabel = var @|= val
        pure $ PredComp Nothing CEQ ex1 ex2withLabel
      _ -> fail [i|\nexp2pred: expected Var, got #{ex1}\n|]
  ELit GML.EBoolTrue -> pure $ PredVal Nothing True
  ELit GML.EBoolFalse -> pure $ PredVal Nothing False
  ELit (GML.EString lit) -> pure $ PredVar [i|#{lit}|]
  EOr {} -> PredFold Nothing PLOr <$> foldPredOr exp
    --PredBin Nothing PredOr <$> exp2pred l <*> exp2pred r
  EAnd {} -> PredFold Nothing PLAnd <$> foldPredAnd exp
  EPredSet (GML.MkVar var) val -> do
    let varStr = T.unpack var
    valEx <- exp2pred val
    let valExWithLabel = case valEx of
          PredVar _ -> valEx
          PredSet _ _ -> valEx
          _ -> varStr @|= valEx
    pure $ PredSet varStr valExWithLabel
--  e -> trace ("exp2pred: not yet implemented\n    " <> show e) $ do
  e -> do
    mlEx <- gml2ml exp
    --trace ("but it is implemented in gml2ml\n    " <> show mlEx) $
    pure case mlEx of
      MathVar x -> PredVar x
      x -> PredVar $ "Not implemented yet: " <> show x

foldPredOr :: GML.Exp -> ToMathLang (PredList Double)
foldPredOr e = case e.exp of
  EOr l r -> do
    predL <- exp2pred l
    predR <- foldPredOr r
    pure $ predL : predR
  EEmpty -> pure []
  x -> trace [i|foldPredOr: encountered #{x}|] $ (:[]) <$> exp2pred e

foldPredAnd :: GML.Exp -> ToMathLang (PredList Double)
foldPredAnd e = case e.exp of
  EAnd l r -> do
    predL <- exp2pred l
    predR <- foldPredOr r
    pure $ predL : predR
  EEmpty -> pure []
  x -> trace [i|foldPredAnd: encountered #{x}|] $ (:[]) <$> exp2pred e

chainITEs :: [Expr Double] -> [Expr Double]
chainITEs es = [moveVarsetToTop x xs | (x:xs) <- groupBy sameVarSet es]
  where
    moveVarsetToTop :: Expr Double -> [Expr Double] -> Expr Double
    moveVarsetToTop
      (MathITE lbl condP (MathSet var1 val1) (Undefined _)) xs =
        MathSet var1 (MathITE lbl condP val1 (go xs))
      where
        go [MathITE _ (PredVal _ True) (MathSet var2 val2) (Undefined _)] = val2
        go ((MathITE lbl2 condP2 (MathSet var2 val2) (Undefined _)):xs)
          | var1 == var2 = MathITE lbl2 condP2 val2 (go xs)
        go [] = Undefined (Just "No otherwise case")

    moveVarsetToTop x [] = x

    moveVarsetToTop x _ =
      trace "moveVarsetToTop: groupBy didn't do what's expected" x

    sameVarSet :: Expr Double -> Expr Double -> Bool
    sameVarSet (MathITE _ _ (MathSet var1 _) _ )
               (MathITE _ _ (MathSet var2 _) _ ) = var1 == var2
    sameVarSet _ _ = False

placeholderITE :: Expr Double
placeholderITE = Undefined (Just "placeholder for ITE")

type ToMathLangError = String
type VarsAndBody = ([String], Expr Double)

newtype ToMathLang a =
  ToMathLang (Eff '[ Writer MyState
                   , Reader (SymTab VarsAndBody)
                   , Fail ] a)
  deriving newtype (Functor, Applicative, Monad, MonadFail)

_ToMathLang :: Iso' (ToMathLang a) (Eff '[Writer MyState, Reader (SymTab VarsAndBody), Fail] a)
_ToMathLang = coerced

mkToMathLang :: Eff '[Writer MyState, Reader (SymTab VarsAndBody), Fail] a -> ToMathLang a
mkToMathLang = view (re _ToMathLang)

unToMathLang :: ToMathLang a -> Eff '[Writer MyState, Reader (SymTab VarsAndBody), Fail] a
unToMathLang = view _ToMathLang

runToMathLang :: SymTab VarsAndBody -> ToMathLang a -> Either String a
runToMathLang r m = case runToMathLang' r m of
              Right (res,_state) -> Right res
              Left err           -> Left err

execToMathLang :: SymTab VarsAndBody -> ToMathLang a -> MyState
execToMathLang r m = case runToMathLang' r m of
                    Right (_res,state) -> state
                    Left _             -> emptyState

runToMathLang' :: SymTab VarsAndBody -> ToMathLang a -> Either ToMathLangError (a, MyState)
runToMathLang' r (unToMathLang -> m) =
  m
    |> runWriterLocal
    |> runReader r
    |> runFail
    |> runPureEff

-- all of the results are in MyState, so we can ignore the actual res
gmls2ml :: SymTab VarsAndBody -> [GML.Exp] -> MyState
gmls2ml _userfuns [] = emptyState
gmls2ml userfuns (e:es) = st <> gmls2ml userfuns es
  where -- TODO: temporary hack, probably reconsider when exactly stuff is put into MyState
    seqE = case e.exp of
      ESeq _ -> e
      _ -> GML.MkExp (ESeq (GML.SeqExp [e])) []
    st = execToMathLang userfuns $ gml2ml seqE -- NB. returns emptyState if gml2ml fails

getUserFuns :: Int -> SymTab VarsAndBody -> SymTab ([GML.Var], GML.Exp) -> SymTab VarsAndBody
getUserFuns ix firstPass funs = trace [i|#{ix}: firstPass = #{firstPass}|] $
  case firstPass == newFuns of
    True -> newFuns
    False -> getUserFuns (ix+1) newFuns funs
  where
    newFuns = Map.map f funs
    f :: ([GML.Var], GML.Exp) -> VarsAndBody
    f (boundVars, exp) =
      case runToMathLang firstPass $ mkAppForUF exp [] of
        Right mlExp -> ([T.unpack v | GML.MkVar v <- nub boundVars], mlExp)
        Left error -> trace (if firstPass /= Map.empty then [i|getUserFuns: #{error}|] else "") ([], Undefined Nothing)

    mkAppForUF :: GML.Exp -> [String] -> ToMathLang (Expr Double)
    mkAppForUF (isApp -> Just (f, arg)) args = mkAppForUF f (arg : args)
    mkAppForUF (GML.exp -> EVar (GML.MkVar f)) args = do
      userFuns :: SymTab VarsAndBody <- ToMathLang ask -- HashMap String ([Var], Expr Double)
      case Map.lookup (T.unpack f) userFuns of
        Nothing -> fail [i|mkAppForUF: this really shouldn't happen, but #{f} is not found in userFuns|]
        Just (boundVars, expr) -> do
          let newVars = map MathVar args
              replacedDef = replaceVars (zip boundVars newVars) expr
          pure $ MathApp Nothing (T.unpack f) args replacedDef -- still only replaced with the new set of arguments, not with more complex expressions
    mkAppForUF exp _ = gml2ml exp

    isApp :: GML.Exp -> Maybe (GML.Exp, String)
    isApp (GML.exp -> EApp f (GML.exp -> GML.EVar (GML.MkVar arg))) = Just (f, T.unpack arg)
    isApp _ = Nothing

gml2ml :: GML.Exp -> ToMathLang (Expr Double)
gml2ml exp =
  let expExp = exp.exp
  in case expExp of
  EEmpty -> fail "gml2ml: unexpected EEmpty"
  ESeq seq -> do
    seqs <- traverse gml2ml $ GML.seqExpToExprs seq
    let newSeqs =
          -- trace [i|\ngml2ml: seqs #{seqs}\n|] $
          chainITEs seqs
        newF =
          -- trace [i|\ngml2ml: newSeqs #{newSeqs}\n|] $
          Map.fromList [(var, var @|= val) | MathSet var val <- newSeqs]

    ToMathLang $ tell $ emptyState {symtabF = newF}

    let !headName = case newSeqs of
          MathSet headName _ : _ -> headName
          MathVar headName : _ -> headName
          _ -> fail [i|\nUnexpected thing: #{newSeqs}\n\nFrom #{seqs}\n\nFrom #{seq}|]

    pure $ MathVar headName

  ELit lit -> pure $ mkVal lit

  EVar (GML.MkVar var) -> pure $ MathVar [i|#{var}|]

  ENumOp GML.OpMinOf e1 e2 -> do
    -- TODO: make it into a fold
    MathMin Nothing <$> gml2ml e1 <*> gml2ml e2

  ENumOp GML.OpMaxOf e1 e2 -> do
    MathMax Nothing <$> gml2ml e1 <*> gml2ml e2

  ENumOp op e1 e2 -> do
    ex1 <- gml2ml e1
    ex2 <- gml2ml e2
    op <- numOptoMl op
    pure $ MathBin Nothing op ex1 ex2

  EVarSet var val -> do
    MathVar varEx <- gml2ml var
    valEx <- gml2ml val
    let valExWithLabel = varEx @|= valEx
    --    if we get rid of the assumption that everything is a SeqExp, should do this
    --    ToMathLang $ tell $ emptyState { symtabF = Map.singleton varEx valExWithLabel }
    pure $ MathSet varEx valExWithLabel

  EPredSet _ _ -> do
    PredSet name pr <- exp2pred exp
    ToMathLang $ tell $ emptyState {symtabP = Map.singleton name pr}
    pure $ Undefined Nothing -- this is just dummy to not have it crash, this value won't be present in the final result

  EIfThen condE thenE -> do
    condP <- exp2pred condE
    thenEx <- gml2ml thenE
    pure $ MathITE Nothing condP thenEx placeholderITE

  ERec fieldname recname -> do
    fnEx <- gml2ml fieldname
    rnEx <- gml2ml recname
    case (fnEx, rnEx) of
      (MathVar fname, MathVar rname) -> pure $ MathVar [i|#{rname}.#{fname}|]
      x -> pure $ MathVar [i|gml2ml: unsupported record #{expExp}|]

  ELam (GML.MkVar v) body -> trace [i|\ngml2ml: found ELam #{exp}\n|] $ do
    bodyEx <- gml2ml body
    let varEx = T.unpack v
    trace [i|     arg = #{v}\n      body = #{bodyEx}\n|] $ pure $ MathSet varEx bodyEx
  -- exp.exp :: BaseExp

  EApp f arg -> mkApp exp []
  _ ->
    trace [i|\ngml2ml: not supported #{exp}\n|] $
      pure $ MathVar [i|gml2ml: not implemented yet #{expExp}|]

  where
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
      case Map.lookup (T.unpack f) userFuns of
        Nothing -> fail "mkApp: trying to apply undefined function"
        Just (boundVars, expr) -> do
          let funAppliedToArgsName = case getExprLabel <$> args of
                [Just arg] -> [i|#{f} #{arg}|]
                [Just arg1, Just arg2] -> [i|#{arg1} #{f} #{arg2}|]
                _ -> [i|TODO: #{f} applied to 3 or more arguments, or the arguments don't have labels|]
              expandedExpr = replaceVars (zip boundVars args) expr
              namedExpr = funAppliedToArgsName @|= expandedExpr
          ToMathLang $ tell $ emptyState {symtabF = Map.singleton funAppliedToArgsName namedExpr}
          pure $ MathVar funAppliedToArgsName
    mkApp e _ = trace [i|\ngml2ml.mkApp, exp=#{e}\n|] $ fail "mkApp: unexpected thing happened"

--             [(x, a),  (y, b)]          x + y          a + b
replaceVars :: [(String, Expr Double)] -> Expr Double -> Expr Double
replaceVars table = returnBody . replace
  where
    replace = \case
      MathVar k@(_:_) -> case lookup k table of
                      Just v -> v
                      Nothing -> MathVar k
      x -> over (gplate @(Expr Double)) replace x
    returnBody = \case
      MathApp lbl _name _vars body -> body
      expr -> expr
{-  ECompOp
    ELet
    EIs
    ERec
    ENot
    EAnd
    EOr -}
-- | calling the output "MyState" is misleading, but this is the most general way to cover the idea that
-- a ruleset consists of more than one rule, similar to how the Vue interface gives more than one
-- element in the left nav; each of the different rules will have its own entry in the SymTab dictionary.
--
-- so, for example, if we have a single MustSing input, we would expect the output MyState dictionary symtab
-- to contain an @Expr Double@ called "must sing"
toMathLangMw :: Interpreted -> MyEnv -> (String, [String])
toMathLangMw l4i myenv = (rendered, [])
 where
  (exprs, state) = toMathLang l4i
  rendered = [__i|
                #{exprs}
                #{state}
              |]

--   intermediate l4i myenv
    -- the desired output of this function should be something consistent with what app/Main.hs is expecting.
    -- the most important transformations are:
    -- starting with the rules in l4i, or the slightly transformed versions available in qaHorns,
    -- we want to output a MathLang version of the input rules,
    -- named after the top-level rule entrypoints already given in qaHorns.
    -- The target output is Typescript such as is shown in sect10-typescript/src/pau.ts, which is produced by sect10-typescript/Makefile

-- intermediate generates a MyState containing something like this:
--
--  MyState { symtabF = Map.fromList [("maxClaim", ... -- snd element dumps to { return new tsm.Bool3 )] }

intermediate :: Interpreted -> MyEnv -> (String, [String])
intermediate l4i myenv = ("", [])
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