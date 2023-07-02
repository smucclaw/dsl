{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.CoreL4.LogicProgram.Skolemize
  ( convertVarExprToDecl,
    skolemizeLPRule,
  )
where

import Control.Monad (join)
import Control.Monad.Validate (MonadValidate (refute))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable qualified as Fold
import Data.Maybe (fromMaybe)
import Flow ((.>), (|>))
import L4.PrintProg (capitalise)
import L4.Syntax
  ( Expr (VarE, annotOfExpr, varOfExprVarE),
    QVarName (QVarName, annotOfQVarName, nameOfQVarName),
    Var (..),
    VarDecl (..),
    VarName,
  )
import L4.SyntaxManipulation (appToFunArgs, funArgsToAppNoType)
import LS.Utils
  ( MonoidValidate,
    mapThenSwallowErrs,
    maybe2validate,
    swallowErrs,
    (|$>),
  )
import LS.XPile.CoreL4.LogicProgram.Common (LPRule (..))
import Prettyprinter (Doc, viaShow)
import Prettyprinter.Interpolate (__di)

-- Skolemized LP rules code
--Additional function when starting at natural4. 
--addinVarDecls :: LPRule -> LPRule

-- skolemizeLPRuleGlobals :: LPRule lpLang t -> [VarDecl t]
-- skolemizeLPRuleGlobals (LPRule {globalVarDecls}) = globalVarDecls
-- skolemizedLPRuleName :: LPRule lpLang t -> String
-- skolemizedLPRuleName (LPRule {ruleName})= ruleName
-- skolemizedLPRulePostcond :: LPRule lpLang t -> Expr t
-- skolemizedLPRulePostcond (LPRule {postcond}) = postcond

skolemizedLPRulePrecond :: Eq t => LPRule lpLang t -> [Expr t]
skolemizedLPRulePrecond LPRule {..} = do
  precond <- preconds
  [transformPrecond precond postcond varDecls globalVarDecls ruleName]
  where
    varDecls = localVarDecls <> globalVarDecls

  -- transformprecond precon (postcond r)
-- [transformPrecond precon (postcond r) (localVarDecls r ++ globalVarDecls r) (globalVarDecls r) (ruleName r) | precon <- preconds r]

--skolemizedLPRuleVardecls r = genSkolemList (localVarDeclsOfLPRule r) ([varExprToDecl expr (localVarDeclsOfLPRule r) | expr <- snd (appToFunArgs [] (postcondOfLPRule r))]) (nameOfLPRule r)

skolemizedLPRuleVardecls :: Eq t => LPRule lpLang t -> [VarDecl t]
skolemizedLPRuleVardecls
  LPRule {postcond = appToFunArgs [] -> (_, postcondArgs), ..} =
    postcondArgs
      |> mapThenSwallowErrs (convertVarExprToDecl localVarDecls)
      |> \x -> genSkolemList localVarDecls x globalVarDecls ruleName

  -- genSkolemList (localVarDecls rule) (map (convertVarExprToDecl (localVarDecls rule)) (snd (appToFunArgs [] (postcond rule)))) (globalVarDecls rule) (ruleName rule)

-- skolemizeLPRule :: LPRule t -> LPRule t

skolemizeLPRule :: Eq t => LPRule lpLang t -> LPRule lpLang t
skolemizeLPRule lpRule = lpRule {preconds = skolemizedLPRulePrecond lpRule}
  -- ruleName = skolemizedLPRuleName r
  -- (skolemizeLPRuleGlobals r) (skolemizedLPRuleVardecls r) (skolemizedLPRulePrecond r) (skolemizedLPRulePostcond r)

convertVarExprToDecl :: [VarDecl t2] -> Expr t -> MonoidValidate (Doc ann) (VarDecl t2)
convertVarExprToDecl decls (VarE _ (nameOfVar .> nameOfQVarName -> varName)) =
  decls
    |> Fold.find ((varName ==) . nameOfVarDecl)
    |> maybe2validate [__di|convertVarExprToDecl: couldn't find #{varName}|]

convertVarExprToDecl _decls _ =
  refute "trying to convert a non-variable expression to a declaration"

-- fromMaybe (error $ "convertVarExprToDecl: couldn't find " ++ nameOfQVarName (nameOfVar v)) $
-- findVarDecl (nameOfQVarName (nameOfVar v)) decls

--transformPrecond :: Expr t -> Expr t ->[VarDecl t] -> String -> Expr t
-- Takes in precon, lifts to var decl, transforms var decl, pushes back down to var expr, doesn't typecheck
-- because transformed predicates (ie function applications) have type Expr (Tp()) rather than Expr t
-- Need to change this !!! First : Check if precon occurs among vardecls, then check if postcon occurs among vardecls

transformPrecond ::
  Eq t => Expr t -> Expr t -> [VarDecl t] -> [VarDecl t] -> [Char] -> Expr t
transformPrecond
  (appToFunArgs [] -> (precondFun, precondArgs))
  (appToFunArgs [] -> (_, postcondArgs))
  varDecls
  varDeclsGlobal
  ruleid =
    ruleid
      |> genSkolemList precondVarDecls postcondVarDecls varDeclsGlobal
      |$> varDeclToExpr -- new_preconvar_dec
      |> funArgsToAppNoType precondFun -- new_precond
    where
      (precondVarDecls, postcondVarDecls) =
        (precondArgs, postcondArgs)
          |> join bimap (mapThenSwallowErrs $ convertVarExprToDecl varDecls)

--genSkolem ::  VarDecl t -> [VarDecl t] -> [VarDecl t] -> String -> VarDecl t
-- Takes in an existing precondition var_decl, list of postcon var_decls, list of global varDecls and returns skolemized precon var_decl
genSkolem ::
  Eq t => VarDecl t -> [VarDecl t] -> [VarDecl t] -> String -> VarDecl t
genSkolem varDecl _ ((varDecl `elem`) -> True) _ = varDecl

genSkolem varDecl@VarDecl {nameOfVarDecl} ((varDecl `elem`) -> True) _ _ =
  varDecl {nameOfVarDecl = capitalise nameOfVarDecl}

genSkolem varDecl _ _ _ = varDecl {nameOfVarDecl = "extVar"}

-- List version of genSkolem
genSkolemList :: Eq t => [VarDecl t] -> [VarDecl t] -> [VarDecl t] -> String -> [VarDecl t]
genSkolemList xs y w z = [genSkolem x y w z | x <- xs]


-- var to var expr
-- mkVarE :: Var t -> Expr t
-- mkVarE v = VarE {annotOfExpr = annotOfQVarName (nameOfVar v), varOfExprVarE = v}

-- Takes in a var_decl and turns it into a var_expr
varDeclToExpr :: VarDecl t -> Expr t
varDeclToExpr VarDecl {annotOfVarDecl, nameOfVarDecl} =
  VarE {..}
  where
    annotOfExpr = annotOfVarDecl
    varOfExprVarE =
      GlobalVar $
        QVarName
          { annotOfQVarName = annotOfVarDecl,
            nameOfQVarName = nameOfVarDecl
          }

-- varDeclToExpr (VarDecl x y _z) = mkVarE (GlobalVar (QVarName x y))

-- varDeclToExprTup :: VarDecl t -> (Expr t, VarDecl t)
-- varDeclToExprTup (VarDecl x y z) = (varDeclToExpr (VarDecl x y z), VarDecl x y z)

-- Matching function

-- match :: (Eq t, Show t) => t -> [(t, p)] -> p
-- match x (y:ys) = if x == fst y
--      then snd y
--      else match x ys
-- match x [] = error ("looking for " ++ show x)

-- var expr to var decl, takes in a var expr and returns its corresponding var decl
-- varExprToDecl :: (Eq t, Show t) => Expr t -> [VarDecl t] -> VarDecl t
-- varExprToDecl expr listdec = match expr (map varDeclToExprTup listdec)

-- Expression data structure in Syntax.hs
-- many additional functions in SyntaxManipuation.hs

-- End of Skoemized ASP rules code