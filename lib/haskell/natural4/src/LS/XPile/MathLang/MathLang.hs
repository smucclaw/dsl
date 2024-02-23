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
  (toMathLangMw, toMathLang)
where
-- TODO: Rename `toMathLang` to something like `toMengMathLang`, and add a `toGenericMathLang` as well

import Control.Monad.Except (MonadError, throwError)
import Data.ByteString.Builder (generic)
import Data.Coerce (coerce)
import Data.Either (rights)
import Data.List (groupBy)
import Data.Generics.Sum.Constructors (AsConstructor (_Ctor))
import Data.HashMap.Strict qualified as Map
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Explainable.MathLang
import LS.Interpreter
import LS.Rule (Interpreted (..))
import LS.Utils (eitherToList)
import LS.XPile.IntroReader (MyEnv)
import LS.XPile.MathLang.GenericMathLang.GenericMathLangAST (BaseExp (..))
import LS.XPile.MathLang.GenericMathLang.GenericMathLangAST qualified as GML
import LS.XPile.MathLang.GenericMathLang.TranslateL4 qualified as GML
import Optics (filteredBy, folded, (%), (^..))

{-
YM: This is currently more like a NOTES file,
with comments from MEng. Will integrate these later.
-}

toMathLang :: Interpreted -> [Expr Double]
toMathLang l4i =
  let l4Hornlikes = l4i.origrules ^.. folded % filteredBy (_Ctor @"Hornlike")
  in case GML.runToLC $ GML.l4ToLCProgram l4Hornlikes of
    Left errors -> [] -- GML.makeErrorOut errors
    Right lamCalcProgram -> genericMLtoML lamCalcProgram.lcProgram

numOptoMl :: MonadError T.Text m => GML.NumOp -> m MathBinOp
numOptoMl = \case
  GML.OpPlus -> pure Plus
  GML.OpSum -> pure Plus
  GML.OpMinus -> pure Minus
  GML.OpMul -> pure Times
  GML.OpProduct -> pure Times
  GML.OpDiv -> pure Divide
  op -> throwError [i|numOptoMl: encountered #{op}|]

compOptoMl :: GML.CompOp -> Comp
compOptoMl = \case
  GML.OpNumEq -> CEQ
  GML.OpLt -> CLT
  GML.OpLte -> CLTE
  GML.OpGt -> CGT
  GML.OpGte -> CGTE
  _ -> CNEQ -----

-- TODO: needs an env to retrieve values for variables
mkVal :: MonadError T.Text m => GML.Lit -> m (Expr Double)
mkVal = \case
  GML.EInteger int -> pure $ Val Nothing $ fromInteger int
  GML.EFloat float -> pure $ Val Nothing float
  GML.EString lit -> pure $ MathVar $ T.unpack lit
  lit -> throwError [i|mkVal: encountered #{lit}|]

exp2pred :: GML.Exp -> [Pred Double]
exp2pred exp = case exp.exp of
  EVar (GML.MkVar var) -> pure $ PredVar $ T.unpack var
  ECompOp op e1 e2 -> do
    ex1 <- genericMLtoML e1
    ex2 <- genericMLtoML e2
    pure $ PredComp Nothing (compOptoMl op) ex1 ex2
  EIs e1 e2 -> PredComp Nothing CEQ <$> genericMLtoML e1 <*> genericMLtoML e2
  ELit GML.EBoolTrue -> pure $ PredVal Nothing True
  ELit GML.EBoolFalse -> pure $ PredVal Nothing False
  _ -> pure $ PredVar "TODO: not implemented yet"

chainITEs :: [Expr Double] -> [Expr Double]
chainITEs es = concat [chain x xs | (x:xs) <- groupBy sameVarSet es]
  where
    chain :: Expr Double -> [Expr Double] -> [Expr Double]
    chain x@(MathITE lbl condP thenEx (Undefined _)) xs = case xs of

      -- the list of expressions ends in OTHERWISE, so we found the final ELSE
      [MathITE _ (PredVal _ True) otherwiseEx (Undefined _)]
        -> pure $ MathITE lbl condP thenEx otherwiseEx

      -- the list of expressions continues, keep going deeper into ELSE
      (MathITE lbl' condP' thenEx' (Undefined _):elseEx:rest) -- the new else
        -> MathITE lbl condP thenEx     -- original x
                <$> MathITE lbl' condP' thenEx' -- first item of the list here
                        <$> chain elseEx rest      -- recursive call to chain

      -- singleton list = there was never an else branch in the original
      [] -> [x]

      -- first item is MathITE but isn't followed by more, unclear what to do—leave as is
      -- TODO: later add some logging
      xs -> (x:xs)

    -- if the first item is not a MathITE, just return the list as is
    chain x xs = x:xs

    sameVarSet :: Expr Double -> Expr Double -> Bool
    sameVarSet (MathITE _ _ (MathSet var1 _) _ )
               (MathITE _ _ (MathSet var2 _) _ ) = var1 == var2
    sameVarSet _ _ = False


genericMLtoML :: GML.Exp -> [Expr Double]
genericMLtoML exp = case exp.exp of
  EEmpty -> []
  ESeq seq -> chainITEs $ foldMap genericMLtoML $ GML.seqExpToExprs seq
  ELit lit -> eitherToList $ mkVal lit
  EVar (GML.MkVar var) -> [MathVar $ T.unpack var]
  ENumOp op e1 e2 -> do
    ex1 <- genericMLtoML e1
    ex2 <- genericMLtoML e2
    op <- eitherToList $ numOptoMl op
    pure $ MathBin Nothing op ex1 ex2
  EVarSet var val -> do
    MathVar varEx <- genericMLtoML var
    valEx <- genericMLtoML val
    pure $ MathSet varEx valEx

  EIfThen condE thenE -> do
    condP <- exp2pred condE
    thenEx <- genericMLtoML thenE
    pure $ MathITE Nothing condP thenEx $ Undefined Nothing
  _ -> pure $ Undefined Nothing
{-  ECompOp
    EApp
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