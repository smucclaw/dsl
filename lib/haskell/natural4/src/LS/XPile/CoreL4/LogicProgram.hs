{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.CoreL4.LogicProgram
  ( babyL4ToLogicProgram,
    LPLang (..),
    LogicProgram,
  )
where

import Control.Monad (join)
import Control.Monad.Validate (MonadValidate (refute))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable qualified as Fold
import Data.Hashable (Hashable)
import Data.List (nub, partition, sort)
import Data.Maybe (mapMaybe)
import Data.String.Interpolate (i)
import Flow ((|>))
import L4.KeyValueMap (ValueKVM)
import L4.Syntax
  ( BBoolOp (BBand),
    BinOp (BBool),
    Expr
      ( AppE,
        UnaOpE,
        ValE,
        VarE,
        subEOfExprUnaOpE,
        unaOpOfExprUnaOpE
      ),
    Program,
    QVarName (..),
    Rule (..),
    Tp (OkT),
    UBoolOp (UBnot),
    UnaOp (UBool),
    Val (BoolV),
    Var (GlobalVar, LocalVar, nameOfVar),
    VarDecl (VarDecl),
    rulesOfProgram,
  )
import L4.SyntaxManipulation
  ( appToFunArgs,
    applyVars,
    applyVarsNoType,
    decomposeBinop,
    funArgsToApp,
    funArgsToAppNoType,
    fv,
    isLocalVar,
  )
import LS.Utils (MonoidValidate, mapThenSwallowErrs, maybe2validate, (|$>))
import LS.XPile.CoreL4.LogicProgram.Common
  ( LPLang (..),
    LPRule (..),
    LogicProgram (..),
    OpposesClause (..),
  )
import LS.XPile.CoreL4.LogicProgram.Pretty ()
import LS.XPile.CoreL4.LogicProgram.Skolemize (skolemizeLPRule)
import Prettyprinter (Doc, Pretty (pretty), viaShow)
import Prettyprinter.Interpolate (__di)

-- TODO: type of function has been abstracted, is not Program t and not Program (Tp())
-- The price to pay: No more preprocessing of rules (simplification with clarify and ruleDisjL)
-- This could possibly be remedied with NoType versions of these tactics
babyL4ToLogicProgram ::
  forall (lpLang :: LPLang) ann t.
  (Show t, Ord t, Hashable t) =>
  Program t ->
  LogicProgram lpLang t
babyL4ToLogicProgram program = LogicProgram {..}
  where
    -- let rules = foldMap ruleDisjL (clarify (rulesOfProgram prg))
    -- putStrLn "Simplified L4 rules:"
    -- putDoc $ vsep (map (showL4 []) rules) <> line
    lpRulesWithNegs :: [(LPRule lpLang t, [(Var t, Var t, Int)])] =
      program |> rulesOfProgram |> mapThenSwallowErrs ruleToLPRule

    (lpRulesFact :: [LPRule lpLang t], lpRulesNoFact :: [LPRule lpLang t]) =
      lpRulesWithNegs
        |$> fst -- Grab all the lpRules
        |> partition isHeadOfPrecondFact -- Split into Fact and NoFact

    -- skolemizedLPRules :: [LPRule lpLang t]
    -- skolemizedLPRules = map skolemizeLPRule lpRulesNoFact  -- TODO: not used ??

    oppClauses :: [OpposesClause t] =
      lpRulesWithNegs
        -- Get all the oppClausePredNames, ie [(Var t, Var t, Int)], removing duplicates.
        |> foldMap snd
        |> nub
        -- Turn them into OpposesClauses
        |$> genOppClauseNoType

genOppClauseNoType :: (Var t, Var t, Int) -> OpposesClause t
genOppClauseNoType (posvar, negvar, n) = OpposesClause {..}
  where
    (posLit, negLit) = join bimap (`applyVarsNoType` args) (posvar, negvar)
    args =
      [LocalVar (QVarName vart [i|V#{index}|]) index
      | index <- [0 .. n - 1]]
    vart = annotOfQVarName $ nameOfVar posvar

-- TODO: details to be filled in
-- proveAssertionASP :: Show t => Program t -> ValueKVM  -> Assertion t -> IO ()
-- proveAssertionASP p v asrt = putStrLn "ASP solver implemented"

-- isFact :: Expr t -> Bool
-- isFact (ValE _ (BoolV True)) = True
-- isFact _ = False

isHeadOfPrecondFact :: LPRule lpLang t -> Bool
isHeadOfPrecondFact LPRule {preconds = ValE _ (BoolV True) : _} = True
isHeadOfPrecondFact _ = False

ruleToLPRule ::
  forall (lpLang :: LPLang) ann t.
  (Show t, Ord t, Hashable t) =>
  Rule t ->
  MonoidValidate (Doc ann) (LPRule lpLang t, [(Var t, Var t, Int)])
ruleToLPRule rule@Rule {..} = do
  precondsNeg :: [(Expr t, Maybe (Var t, Var t, Int))] <-
    precondOfRule                 -- (precond_1 && ... && precond_n) 
      |> decomposeBinop (BBool BBand)  -- [precond_1, ..., precond_n] 
      |> traverse negationPredicate

  postcondNeg@(postcond, _) :: (Expr t, Maybe (Var t, Var t, Int)) <-
    negationPredicate postcondOfRule

  ruleName :: String <-
    nameOfRule
      |> maybe2validate
          [__di|
            Error in logic program transpiler: ruleToLPRule: nameOfRule is a Nothing.
            #{viaShow rule}
            To exclude the ASP (resp Epilog) transpiler from a --workdir run, run natural4-exe with --toasp (resp --toepilog)
          |]

  let preconds :: [Expr t] = fst <$> precondsNeg

      negPreds :: [(Var t, Var t, Int)] =
        mapMaybe snd $ postcondNeg : precondsNeg

      (localVarDecls :: [VarDecl t], globalVarDecls :: [VarDecl t]) =
        postcond : preconds                 -- [pre and post conds]
          |> foldMap fv                     -- {free variables}
          |> Fold.toList                    -- [free varaibles]
          |> sort
          |> partition isLocalVar           -- (local vars, global vars)
          |> join bimap (varTovarDecl <$>)  -- (local var decls, global var decls)

  pure (LPRule {..}, negPreds)

varTovarDecl :: Var t -> VarDecl t
varTovarDecl (GlobalVar QVarName {..}) =
  VarDecl annotOfQVarName nameOfQVarName OkT
varTovarDecl (LocalVar {nameOfVar = QVarName {..}}) =
  VarDecl annotOfQVarName nameOfQVarName OkT

negationVarname :: QVarName t -> QVarName t
negationVarname qVarName@QVarName {nameOfQVarName} =
  qVarName {nameOfQVarName = [i|not#{nameOfQVarName}|]}

negationPredicate ::
  Expr t -> MonoidValidate (Doc ann) (Expr t, Maybe (Var t, Var t, Int))
negationPredicate
  UnaOpE
    { unaOpOfExprUnaOpE = UBool UBnot,
      subEOfExprUnaOpE = e@AppE {}
    } =
    case appToFunArgs [] e of
      (VarE t posvar@(GlobalVar vn), args) ->
        pure (appExpr, Just (posvar, negvar, length args))
        where
          appExpr = funArgsToAppNoType (VarE t negvar) args
          negvar = GlobalVar $ negationVarname vn
      _ -> refute "negationPredicate: ill-formed negation"
negationPredicate e = pure (e, Nothing)

-- TODO: type of function has been abstracted, is not Program t and not Program (Tp())
-- The price to pay: No more preprocessing of rules (simplification with clarify and ruleDisjL)
-- This could possibly be remedied with NoType versions of these tactics
-- astToASP :: (Eq t, Ord t, Show t) => Program t -> IO ()
-- astToASP prg = do
--     -- let rules = foldMap ruleDisjL (clarify (rulesOfProgram prg))
--     let rules = rulesOfProgram prg
--     -- putStrLn "Simplified L4 rules:"
--     -- putDoc $ vsep (map (showL4 []) rules) <> line
--     let aspRulesWithNegs = map (ruleToLPRule ) rules
--     let aspRules = map fst aspRulesWithNegs
--     let aspRulesNoFact = removeFacts aspRules
--     let aspRulesFact = keepFacts aspRules
--     let skolemizedLPRules = map skolemizeLPRule aspRulesNoFact  -- TODO: not used ??
--     let oppClausePrednames = nub (foldMap snd aspRulesWithNegs)
--     let oppClauses = map genOppClauseNoType oppClausePrednames

--     -- putStrLn "ASP rules:"
--     putDoc $ vsep (map (showASP AccordingToR) aspRulesNoFact) <> line <> line
--     putDoc $ vsep (map (showASP VarSubs1R) aspRulesNoFact) <> line <> line
--     putDoc $ vsep (map (showASP AddFacts) aspRulesFact) <> line <> line
--     putDoc $ vsep (map (showASP VarSubs3R) aspRulesNoFact) <> line <> line
--     putDoc $ vsep (map (showASP VarSubs2R) aspRulesNoFact) <> line <> line
--     putDoc $ vsep (map (showASP VarSubs4R) aspRulesNoFact) <> line <> line
--     -- putDoc $ vsep (map (showASP VarSubs2R) aspRules) <> line <> line
--     -- putDoc $ vsep (map (showASP ExplainsR) aspRules) <> line <> line
--     -- putDoc $ vsep (map (showASP ExplainsR) skolemizedLPRules) <> line <> line
--     putDoc $ vsep (map (showASP CausedByR) aspRulesNoFact) <> line <> line
--     putDoc $ vsep (map showOppClause oppClauses) <> line


-- removeFacts :: [ASPRule t] -> [ASPRule t]
-- removeFacts [] = []
-- removeFacts (r : rs)
--       | isFact (head (precondOfASPRule r)) = removeFacts rs
--       | otherwise = r : removeFacts rs


-- keepFacts :: [ASPRule t] -> [ASPRule t]
-- keepFacts [] =[]
-- keepFacts (r : rs)
--       | isFact (head (precondOfASPRule r)) = r : keepFacts rs
--       | otherwise = keepFacts rs

-- filt :: Eq t1 => [VarDecl t1] -> [VarDecl t1] -> [VarDecl t1]
-- filt [] _ = []
-- filt (x:xs) ys =
--   if x `elem` ys
--      then filt xs ys
--   else x : filt xs ys