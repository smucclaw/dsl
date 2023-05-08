{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

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
import Data.List (nub, partition)
import Data.Maybe (mapMaybe)
import Data.Tuple.All (Curry (uncurryN), SequenceT (sequenceT))
import Flow ((|>))
import L4.KeyValueMap (ValueKVM)
import L4.Syntax
import L4.SyntaxManipulation
  ( appToFunArgs,
    applyVars,
    applyVarsNoType,
    decomposeBinop,
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
import Data.String.Interpolate (i)

-- TODO: type of function has been abstracted, is not Program t and not Program (Tp())
-- The price to pay: No more preprocessing of rules (simplification with clarify and ruleDisjL)
-- This could possibly be remedied with NoType versions of these tactics
babyL4ToLogicProgram ::
  forall (lpLang :: LPLang) ann t.
  (Show t, Ord t, Eq t) =>
  Program t ->
  LogicProgram lpLang t
babyL4ToLogicProgram program =
  -- let rules = concatMap ruleDisjL (clarify (rulesOfProgram prg))
  -- putStrLn "Simplified L4 rules:"
  -- putDoc $ vsep (map (showL4 []) rules) <> line
  let lpRulesWithNegs :: [(LPRule lpLang t, [(Var t, Var t, Int)])] =
        program |> rulesOfProgram |> mapThenSwallowErrs ruleToLPRule

      (lpRulesFact, lpRulesNoFact) =
        lpRulesWithNegs
          |> map fst                        -- Grab all the lpRules
          |> partition isHeadOfPrecondFact  -- Split into Fact and NoFact

      -- skolemizedLPRules :: [LPRule lpLang t]
      -- skolemizedLPRules = map skolemizeLPRule lpRulesNoFact  -- TODO: not used ??

      oppClauses :: [OpposesClause t] =
        lpRulesWithNegs
          -- Get all the oppClausePredNames, ie [(Var t, Var t, Int)], removing duplicates.
          |> foldMap snd |> nub
          -- Turn them into OpposesClauses
          |> map genOppClauseNoType
  in LogicProgram {..}

-- genOppClause :: (Var (Tp ()), Var (Tp ()), Int) -> OpposesClause (Tp ())
-- genOppClause (posvar, negvar, n) = OpposesClause {..}
--   where
--     (posLit, negLit) = join bimap (`applyVars` args) (posvar, negvar)
--     args =
--       [LocalVar (QVarName IntegerT [i|V#{index}|]) index | index <- [0 .. n - 1]]

-- let args = zipWith (\ vn i -> LocalVar (QVarName IntegerT (vn ++ show i)) i) (replicate n "V") [0 .. n-1]
-- in OpposesClause (applyVars posvar args) (applyVars negvar args)

genOppClauseNoType :: (Var t, Var t, Int) -> OpposesClause t
genOppClauseNoType (posvar, negvar, n) = OpposesClause {..}
  where
    (posLit, negLit) = join bimap (`applyVarsNoType` args) (posvar, negvar)
    args =
      [LocalVar (QVarName vart [i|V#{index}|]) index | index <- [0 .. n - 1]]
    vart = annotOfQVarName $ nameOfVar posvar

  -- let vart = annotOfQVarName (nameOfVar posvar) in
  -- let args = zipWith (\ vn i -> LocalVar (QVarName vart (vn ++ show i)) i) (replicate n "V") [0 .. n-1]
  -- in OpposesClause (applyVarsNoType posvar args) (applyVarsNoType negvar args)

-- TODO: details to be filled in
-- proveAssertionASP :: Show t => Program t -> ValueKVM  -> Assertion t -> IO ()
-- proveAssertionASP p v asrt = putStrLn "ASP solver implemented"

-- isFact :: Expr t -> Bool
-- isFact (ValE _ (BoolV True)) = True
-- isFact _ = False

isHeadOfPrecondFact :: LPRule lpLang t -> Bool
isHeadOfPrecondFact (LPRule {preconds = (ValE _ (BoolV True)) : _}) = True
isHeadOfPrecondFact _ = False

ruleToLPRule ::
  forall (lpLang :: LPLang) ann t.
  (Show t, Ord t) =>
  Rule t ->
  MonoidValidate (Doc ann) (LPRule lpLang t, [(Var t, Var t, Int)])
ruleToLPRule rule = do
  precondsNeg :: [(Expr t, Maybe (Var t, Var t, Int))] <-
    rule                               -- (precond_1 && ... && precond_n) => postcond
      |> precondOfRule                 -- (precond_1 && ... && precond_n) 
      |> decomposeBinop (BBool BBand)  -- [precond_1, ..., precond_n] 
      |> traverse negationPredicate

  postcondNeg@(postcond, _) :: (Expr t, Maybe (Var t, Var t, Int)) <-
    negationPredicate $ postcondOfRule rule

  ruleName :: String <-
    rule
      |> nameOfRule
      |> maybe2validate
          [__di|
            Error in logic program transpiler: ruleToLPRule: nameOfRule is a Nothing.
            #{viaShow rule}
            To exclude the ASP (resp Epilog) transpiler from a --workdir run, run natural4-exe with --toasp (resp --toepilog)
          |]
            -- ("ToASP: ruleToLPRule: nameOfRule is a Nothing :-(\n" <>
            -- viaShow rule <> "\n" <>
            -- "To exclude the ToASP transpiler from a --workdir run, run natural4-exe with the --toasp option.")

  let preconds :: [Expr t] =
        map fst precondsNeg

      negPreds :: [(Var t, Var t, Int)] =
        mapMaybe snd $ postcondNeg : precondsNeg

      (localVarDecls :: [VarDecl t], globalVarDecls :: [VarDecl t]) =
        postcond : preconds                 -- [pre and post conds]
          |> foldMap (Fold.toList . fv)     -- [free variables] (We convert to a list to enable list fusion)
          |> partition isLocalVar           -- (local vars, global vars)
          |> join bimap (map varTovarDecl)  -- (local var decls, global var decls)

  pure (LPRule {..}, negPreds)

--varTovarDecl :: Var (Tp()) -> VarDecl (Tp())
--varTovarDecl :: Var t -> VarDecl t
varTovarDecl :: Var t -> VarDecl t
varTovarDecl (GlobalVar (QVarName a vn)) = VarDecl a vn OkT
varTovarDecl (LocalVar (QVarName a vn) _ind) = VarDecl a vn OkT

negationVarname :: QVarName t -> QVarName t
negationVarname (QVarName t vn) = QVarName t [i|not#{vn}|] -- ("not"++vn)

negationPredicate :: Expr t -> MonoidValidate (Doc ann) (Expr t, Maybe (Var t, Var t, Int))
negationPredicate (UnaOpE _ (UBool UBnot) e@AppE{}) =
  let (f, args) = appToFunArgs [] e in
      case f of
          VarE t posvar@(GlobalVar vn) ->
              let negvar = GlobalVar (negationVarname vn)
              in pure (funArgsToAppNoType (VarE t negvar) args, Just (posvar, negvar, length args))
          _ -> refute "negationPredicate: ill-formed negation"
negationPredicate e = pure (e, Nothing)

-- TODO: type of function has been abstracted, is not Program t and not Program (Tp())
-- The price to pay: No more preprocessing of rules (simplification with clarify and ruleDisjL)
-- This could possibly be remedied with NoType versions of these tactics
-- astToASP :: (Eq t, Ord t, Show t) => Program t -> IO ()
-- astToASP prg = do
--     -- let rules = concatMap ruleDisjL (clarify (rulesOfProgram prg))
--     let rules = rulesOfProgram prg
--     -- putStrLn "Simplified L4 rules:"
--     -- putDoc $ vsep (map (showL4 []) rules) <> line
--     let aspRulesWithNegs = map (ruleToLPRule ) rules
--     let aspRules = map fst aspRulesWithNegs
--     let aspRulesNoFact = removeFacts aspRules
--     let aspRulesFact = keepFacts aspRules
--     let skolemizedLPRules = map skolemizeLPRule aspRulesNoFact  -- TODO: not used ??
--     let oppClausePrednames = nub (concatMap snd aspRulesWithNegs)
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