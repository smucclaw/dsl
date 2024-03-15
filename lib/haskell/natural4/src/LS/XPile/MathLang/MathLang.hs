{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.MathLang.MathLang
  (toMathLangMw, toMathLang, gml2ml)
where

import Control.Monad.Except (MonadError, throwError)
import Data.ByteString.Builder (generic)
import Data.Coerce (coerce)
import Data.Either (rights)
import Data.List (groupBy, nub)
import Data.Generics.Sum.Constructors (AsConstructor (_Ctor))
import Data.HashMap.Strict qualified as Map
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Explainable.MathLang
import LS.Interpreter
import LS.Rule (Rule, Interpreted (..))
import LS.XPile.IntroReader (MyEnv)
import LS.XPile.MathLang.GenericMathLang.GenericMathLangAST (BaseExp (..))
import LS.XPile.MathLang.GenericMathLang.GenericMathLangAST qualified as GML
import LS.XPile.MathLang.GenericMathLang.TranslateL4 qualified as GML
import Optics (cosmosOf, filteredBy, folded, gplate, (%), (^..))
import Debug.Trace (trace)
import Control.Monad (MonadPlus(mzero))
import Control.Monad.RWS.Class (MonadWriter(tell))
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Monad.Trans.Writer (Writer, runWriter)
import Data.Maybe (maybeToList, catMaybes)
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
    Right lamCalcProgram ->
      let exprs = gmls2ml lamCalcProgram.lcProgram
          userfuns = getUserFuns lamCalcProgram.userFuns
          st = exprs <> userfuns
          giveth = T.unpack  <$> lamCalcProgram.givethVar
          toplevels = case giveth of
            [] -> trace [i|\ntoMathLang: no giveth, returning all in symTab\n|] $ Map.elems st.symtabF
            ks -> case catMaybes [ MathSet k <$> Map.lookup k st.symtabF | k <- ks ] of
                   [] -> trace [i|\ntoMathLang: no set variable given in #{ks}\n     st = #{st}\n|] $ Map.elems st.symtabF
                   exprs -> exprs
        in (toplevels, st)


--numOptoMl :: MonadError T.Text m => GML.NumOp -> m MathBinOp
numOptoMl :: GML.NumOp -> MyStack MathBinOp
numOptoMl = \case
  GML.OpPlus -> pure Plus
  GML.OpSum -> pure Plus
  GML.OpMinus -> pure Minus
  GML.OpMul -> pure Times
  GML.OpProduct -> pure Times
  GML.OpDiv -> pure Divide
  op -> trace [i|numOptoMl: encountered #{op}|] mzero
--  op -> throwError [i|numOptoMl: encountered #{op}|]

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
  GML.EBoolTrue -> MathVar "True" -- TODO: this is from GenericMathLang `SetVar var True`. Should do deeper tree transformations so we don't end up here at all.
  GML.EBoolFalse -> MathVar "False" -- Just a placeholder, see comment above. Should represent "if COND then foo=True" in another way in GML AST.
--  lit -> throwError [i|mkVal: encountered #{lit}|]

exp2pred :: GML.Exp -> MyStack (Pred Double)
exp2pred exp = case exp.exp of
  EEmpty -> mzero
  EVar (GML.MkVar var) -> pure $ PredVar $ T.unpack var
  ECompOp op e1 e2 ->
    PredComp Nothing (compOptoMl op) <$> gml2ml e1 <*> gml2ml e2
  EIs e1 e2 -> do
    ex1 <- gml2ml e1
    ex2 <- gml2ml e2
    let ex2withLabel = case (ex1, ex2) of
          (MathVar var, Val Nothing val) -> Val (Just var) val
--          (MathVar var, MathVar val) -> TODO: what if they are both strings? like phaseOfMoon IS gibbous
          _ -> ex2
    pure $ PredComp Nothing CEQ ex1 ex2withLabel
  ELit GML.EBoolTrue -> pure $ PredVal Nothing True
  ELit GML.EBoolFalse -> pure $ PredVal Nothing False
  ELit (GML.EString lit) -> pure $ PredVar $ T.unpack lit
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
    pure $ case mlEx of
      MathVar x -> PredVar x
      x -> PredVar $ "Not implemented yet: " <> show x

foldPredOr :: GML.Exp -> MyStack (PredList Double)
foldPredOr e = case e.exp of
  EOr l r -> do
    predL <- exp2pred l
    predR <- foldPredOr r
    pure $ predL : predR
  EEmpty -> pure []
  x -> trace [i|foldPredOr: encountered #{x}|] $ (:[]) <$> exp2pred e

foldPredAnd :: GML.Exp -> MyStack (PredList Double)
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
    moveVarsetToTop _ _ = error "moveVarsetToTop: groupBy didn't do what's expected"

    sameVarSet :: Expr Double -> Expr Double -> Bool
    sameVarSet (MathITE _ _ (MathSet var1 _) _ )
               (MathITE _ _ (MathSet var2 _) _ ) = var1 == var2
    sameVarSet _ _ = False

placeholderITE :: Expr Double
placeholderITE = Undefined (Just "placeholder for ITE")

-- TODO: add Reader for userfuns
type MyStack = MaybeT (Writer MyState)

-- all of the results are in MyState, so we can ignore the actual res
gmls2ml :: [GML.Exp] -> MyState
gmls2ml [] = emptyState
gmls2ml (e:es) = trace [i|\ngmls2ml: #{res}\n|] $ st <> gmls2ml es
  where -- TODO: temporary hack, probably reconsider when exactly stuff is put into MyState
    seqE = case e.exp of
      ESeq _ -> e
      _ -> GML.MkExp (ESeq (GML.SeqExp [e])) []
    res@(_exp, st) = runWriter $ runMaybeT $ gml2ml seqE

getUserFuns :: Map.HashMap String GML.Exp -> MyState
getUserFuns hm = emptyState {symtabFun = Map.map f hm}
  where
    f :: GML.Exp -> ([String], Expr Double)
    f exp = case res of
            Nothing -> ([], Undefined Nothing)
            Just mlExp -> ([T.unpack v | GML.MkVar v <- nub vars], mlExp)
      where
        (res,_) = runWriter $ runMaybeT $ gml2ml exp
        -- TODO: check that only bound variables are there, there may be free variables in body
        vars = exp ^.. cosmosOf (gplate @GML.Exp) % gplate @GML.Var

gml2ml :: GML.Exp -> MyStack (Expr Double)
gml2ml exp = case exp.exp of
  EEmpty -> mzero
  ESeq seq -> do
    seqs <- mapM gml2ml $ GML.seqExpToExprs seq
    let newSeqs = --trace [i|\ngml2ml: seqs #{seqs}\n|] $
                  chainITEs seqs
        newF = -- trace [i|\ngml2ml: newSeqs #{newSeqs}\n|] $
               Map.fromList [(var, val) | MathSet var val <- newSeqs]

    tell $ emptyState { symtabF = newF}
    let !headName = case newSeqs of
         MathSet headName _ : _ -> headName
         MathVar headName : _ -> headName
         _ -> error [i|\nUnexpected thing: #{newSeqs}\n\nFrom #{seqs}\n\nFrom #{seq}|]
    pure $ MathVar headName
  ELit lit -> pure $ mkVal lit
  EVar (GML.MkVar var) -> pure $ MathVar $ T.unpack var
  ENumOp GML.OpMinOf e1 e2 -> do -- TODO: make it into a fold
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
    let valExWithLabel = case valEx of
          MathVar _ -> valEx
          MathSet _ _ -> valEx
          _ -> varEx @|= valEx
--    if we get rid of the assumption that everything is a SeqExp, should do this
--    tell $ emptyState { symtabF = Map.singleton varEx valExWithLabel }
    pure $ MathSet varEx valExWithLabel
  EPredSet _ _ -> do
    PredSet name pr <- exp2pred exp
    tell $ emptyState { symtabP = Map.singleton name pr }
    pure (Undefined Nothing) -- this is just dummy to not have it crash, this value won't be present in the final result
  EIfThen condE thenE -> do
    condP <- exp2pred condE
    thenEx <- gml2ml thenE
    pure $ MathITE Nothing condP thenEx placeholderITE
  ERec fieldname recname -> do
    fnEx <- gml2ml fieldname
    rnEx <- gml2ml recname
    case (fnEx, rnEx) of
      (MathVar fname, MathVar rname) -> pure $ MathVar (rname <> "." <> fname)
      x -> pure $ MathVar ("gml2ml: unsupported record " <> show exp.exp)
  EApp f arg -> mkApp exp
  -- TODO: store ELam with the function name in GML
  -- then store the body and vars into MyState, and replace the Var "x" / Var "y" in the expr
  ELam (GML.MkVar v)  body -> trace [i|\ngml2ml: found ELam #{exp}\n|] $ do
    bodyEx <- gml2ml body
    let varEx = T.unpack v
    trace [i|     arg = #{v}\n      body = #{bodyEx}\n|] $ pure $ MathSet varEx bodyEx
  _ -> trace [i|\ngml2ml: not supported #{exp}\n|] $
        pure $ MathVar ("gml2ml: not implemented yet " <> show exp.exp) --Undefined Nothing

  where
    mkApp :: GML.Exp -> MyStack (Expr Double)
    mkApp (GML.exp -> EApp f arg) = do
      -- This is just a placeholder, we need to replace the function application by its value.
      -- TODO:
      -- 1) change MyStack into Eff '[Reader UserFuns, Writer MyState, Maybe] so we get access to user-defined functions
      -- 2) Get the ([String], Expr Double) pair and replace all the "MathVar x" from the [String] argument with actual arguments of the EApp.
      argEx <- gml2ml arg
      fEx <- trace [i|\ngml2ml: argEx = #{argEx}\n|] $ gml2ml f
      pure $ MathVar [i|#{fEx}(#{argEx})|]
    mkApp _ = mzero
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
toMathLangMw l4i myenv = ("NotYetImplemented", [])

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