{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

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
import Flow ((|>))
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
skolemizedLPRulePrecond (LPRule {..}) = do
  let varDecls = localVarDecls <> globalVarDecls
  precond <- preconds
  pure $ transformPrecond precond postcond varDecls globalVarDecls ruleName

  -- transformprecond precon (postcond r)
-- [transformPrecond precon (postcond r) (localVarDecls r ++ globalVarDecls r) (globalVarDecls r) (ruleName r) | precon <- preconds r]

--skolemizedLPRuleVardecls r = genSkolemList (localVarDeclsOfLPRule r) ([varExprToDecl expr (localVarDeclsOfLPRule r) | expr <- snd (appToFunArgs [] (postcondOfLPRule r))]) (nameOfLPRule r)

skolemizedLPRuleVardecls :: Eq t => LPRule lpLang t -> [VarDecl t]
skolemizedLPRuleVardecls (LPRule {..}) =
  postcond
    |> appToFunArgs []
    |> snd
    |> mapThenSwallowErrs (convertVarExprToDecl localVarDecls)
    |> (\x -> genSkolemList localVarDecls globalVarDecls x ruleName)

  -- genSkolemList (localVarDecls rule) (map (convertVarExprToDecl (localVarDecls rule)) (snd (appToFunArgs [] (postcond rule)))) (globalVarDecls rule) (ruleName rule)

-- skolemizeLPRule :: LPRule t -> LPRule t

skolemizeLPRule :: Eq t => LPRule lpLang t -> LPRule lpLang t
skolemizeLPRule
  lpRule@( LPRule
             { ruleName,
               localVarDecls,
               globalVarDecls,
               postcond
             }
           ) =
    LPRule {preconds = skolemizedLPRulePrecond lpRule, ..}
    -- ruleName = skolemizedLPRuleName r
    -- (skolemizeLPRuleGlobals r) (skolemizedLPRuleVardecls r) (skolemizedLPRulePrecond r) (skolemizedLPRulePostcond r)


-- findVarDecl :: VarName -> [VarDecl t2] -> Maybe (VarDecl t2)
-- findVarDecl varname = find (\d -> varname == nameOfVarDecl d)

convertVarExprToDecl :: [VarDecl t2] -> Expr t -> MonoidValidate (Doc ann) (VarDecl t2)
convertVarExprToDecl decls (VarE _ var) =
  decls
    |> Fold.find ((varName ==) . nameOfVarDecl)
    |> maybe2validate [__di|convertVarExprToDecl: couldn't find #{varName}|]
  where
    varName = nameOfQVarName $ nameOfVar var

convertVarExprToDecl _decls _ =
  refute "trying to convert a non-variable expression to a declaration"

-- fromMaybe (error $ "convertVarExprToDecl: couldn't find " ++ nameOfQVarName (nameOfVar v)) $
-- findVarDecl (nameOfQVarName (nameOfVar v)) decls

--transformPrecond :: Expr t -> Expr t ->[VarDecl t] -> String -> Expr t
-- Takes in precon, lifts to var decl, transforms var decl, pushes back down to var expr, doesn't typecheck
-- because transformed predicates (ie function applications) have type Expr (Tp()) rather than Expr t
-- Need to change this !!! First : Check if precon occurs among vardecls, then check if postcon occurs among vardecls

transformPrecond :: Eq t => Expr t -> Expr t -> [VarDecl t] -> [VarDecl t] -> [Char] -> Expr t
transformPrecond precon postcon vardecls vardeclsGlobal ruleid =
  ruleid
    |> genSkolemList preconvar_dec postconvar_dec vardeclsGlobal
    |> map varDeclToExpr -- new_preconvar_dec
    |> funArgsToAppNoType (fst $ appToFunArgs [] precon) -- new_precond
  where
    (preconvar_dec, postconvar_dec) =
      (precon, postcon) |> join bimap exprToVarDecls
    exprToVarDecls expr =
      mapThenSwallowErrs
        (convertVarExprToDecl vardecls)
        (snd $ appToFunArgs [] expr)

  -- -- [varExprToDecl expr vardecls | expr <- snd (appToFunArgs [] precon)]
  -- let preconvar_dec = mapThenSwallowErrs (convertVarExprToDecl vardecls) (snd (appToFunArgs [] precon))
  --     -- [varExprToDecl expr vardecls | expr <- snd (appToFunArgs [] postcon)]
  --     postconvar_dec = mapThenSwallowErrs (convertVarExprToDecl vardecls) (snd (appToFunArgs [] postcon))
  --     new_preconvar_dec = genSkolemList preconvar_dec postconvar_dec vardeclsGlobal ruleid
  --     new_precond = funArgsToAppNoType (fst (appToFunArgs [] precon)) (map varDeclToExpr new_preconvar_dec)
  --  in new_precond

-- transformPrecond :: Eq t => Expr t -> Expr t -> [VarDecl t] -> [VarDecl t] -> [Char] -> Expr t
-- transformPrecond precon postcon vardecls vardeclsGlobal ruleid =
--   -- [varExprToDecl expr vardecls | expr <- snd (appToFunArgs [] precon)]
--   let preconvar_dec = map (convertVarExprToDecl vardecls) (snd (appToFunArgs [] precon))
--       -- [varExprToDecl expr vardecls | expr <- snd (appToFunArgs [] postcon)]
--       postconvar_dec = map (convertVarExprToDecl vardecls) (snd (appToFunArgs [] postcon))
--       new_preconvar_dec = genSkolemList preconvar_dec postconvar_dec vardeclsGlobal ruleid
--       new_precond = funArgsToAppNoType (fst (appToFunArgs [] precon)) (map varDeclToExpr new_preconvar_dec)
--    in new_precond

--genSkolem ::  VarDecl t -> [VarDecl t] -> [VarDecl t] -> String -> VarDecl t
-- Takes in an existing precondition var_decl, list of postcon var_decls, list of global varDecls and returns skolemized precon var_decl
genSkolem :: Eq t => VarDecl t -> [VarDecl t] -> [VarDecl t] -> String -> VarDecl t
genSkolem varDecl@(VarDecl t vn u) y w _ =
  VarDecl t newVarName u
  where
    newVarName
      | varDecl `elem` w = vn
      | varDecl `elem` y = capitalise vn
      | otherwise = "extVar"

  -- | varDecl `elem` w = VarDecl t vn u
  -- | varDecl `elem` y = VarDecl t (capitalise vn) u
  -- | otherwise = VarDecl t "extVar" u

-- List version of genSkolem
genSkolemList :: Eq t => [VarDecl t] -> [VarDecl t] -> [VarDecl t] -> String -> [VarDecl t]
genSkolemList xs y w z = [genSkolem x y w z | x <- xs]


-- var to var expr
-- mkVarE :: Var t -> Expr t
-- mkVarE v = VarE {annotOfExpr = annotOfQVarName (nameOfVar v), varOfExprVarE = v}

-- Takes in a var_decl and turns it into a var_expr
varDeclToExpr :: VarDecl t -> Expr t
varDeclToExpr VarDecl {annotOfVarDecl, nameOfVarDecl} =
  QVarName {annotOfQVarName = annotOfVarDecl, nameOfQVarName = nameOfVarDecl}
    |> GlobalVar
    |> mkVarE
  where
    mkVarE var@GlobalVar {nameOfVar} =
      VarE
        { annotOfExpr = annotOfQVarName nameOfVar,
          varOfExprVarE = var
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