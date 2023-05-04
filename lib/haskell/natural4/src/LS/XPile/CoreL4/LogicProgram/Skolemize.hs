module LS.XPile.CoreL4.LogicProgram.Skolemize
where

import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import L4.PrintProg (capitalise)
import L4.Syntax
    ( VarDecl(VarDecl, nameOfVarDecl),
      Expr(VarE, varOfExprVarE, annotOfExpr),
      Var(GlobalVar, nameOfVar),
      QVarName(QVarName, nameOfQVarName, annotOfQVarName),
      VarName )
import L4.SyntaxManipulation (appToFunArgs, funArgsToAppNoType)
import LS.XPile.CoreL4.LogicProgram.Common ( LPRule(..) )

-- Skolemized LP rules code
--Additional function when starting at natural4. 
--addinVarDecls :: LPRule -> LPRule

skolemizeLPRuleGlobals :: LPRule lpType t -> [VarDecl t]
skolemizeLPRuleGlobals = globalVarDeclsOfLPRule
skolemizedLPRuleName :: LPRule lpType t -> String
skolemizedLPRuleName = nameOfLPRule
skolemizedLPRulePostcond :: LPRule lpType t -> Expr t
skolemizedLPRulePostcond = postcondOfLPRule
skolemizedLPRulePrecond :: Eq t => LPRule lpType t -> [Expr t]
skolemizedLPRulePrecond r = [transformPrecond precon (postcondOfLPRule r) (localVarDeclsOfLPRule r ++ globalVarDeclsOfLPRule r) (globalVarDeclsOfLPRule r) (nameOfLPRule r) | precon <- precondOfLPRule r]
--skolemizedLPRuleVardecls r = genSkolemList (localVarDeclsOfLPRule r) ([varExprToDecl expr (localVarDeclsOfLPRule r) | expr <- snd (appToFunArgs [] (postcondOfLPRule r))]) (nameOfLPRule r)

skolemizedLPRuleVardecls :: Eq t => LPRule lpType t -> [VarDecl t]
skolemizedLPRuleVardecls r =
  genSkolemList (localVarDeclsOfLPRule r) (map (convertVarExprToDecl (localVarDeclsOfLPRule r)) (snd (appToFunArgs [] (postcondOfLPRule r)))) (globalVarDeclsOfLPRule r) (nameOfLPRule r)

-- skolemizeLPRule :: LPRule t -> LPRule t

skolemizeLPRule :: Eq t => LPRule lpType t -> LPRule lpType t
skolemizeLPRule r = LPRule (skolemizedLPRuleName r)  (skolemizeLPRuleGlobals r) (skolemizedLPRuleVardecls r) (skolemizedLPRulePrecond r) (skolemizedLPRulePostcond r)


findVarDecl :: VarName -> [VarDecl t2] -> Maybe (VarDecl t2)
findVarDecl varname decls = find (\d -> varname == nameOfVarDecl d) decls

-- Jo Hsi: when exactly is this error triggered?
convertVarExprToDecl :: [VarDecl t2] -> Expr t ->VarDecl t2
convertVarExprToDecl decls (VarE _ v) = fromMaybe (error $ "convertVarExprToDecl: couldn't find " ++ nameOfQVarName (nameOfVar v)) $
                                        findVarDecl (nameOfQVarName (nameOfVar v)) decls
convertVarExprToDecl _decls _ = error "trying to convert a non-variable expression to a declaration"

--transformPrecond :: Expr t -> Expr t ->[VarDecl t] -> String -> Expr t
-- Takes in precon, lifts to var decl, transforms var decl, pushes back down to var expr, doesn't typecheck
-- because transformed predicates (ie function applications) have type Expr (Tp()) rather than Expr t
-- Need to change this !!! First : Check if precon occurs among vardecls, then check if postcon occurs among vardecls

transformPrecond :: Eq t => Expr t -> Expr t -> [VarDecl t] -> [VarDecl t] -> [Char] -> Expr t
transformPrecond precon postcon vardecls vardeclsGlobal ruleid =
                    -- [varExprToDecl expr vardecls | expr <- snd (appToFunArgs [] precon)]
                let preconvar_dec = map (convertVarExprToDecl vardecls) (snd (appToFunArgs [] precon))
                    -- [varExprToDecl expr vardecls | expr <- snd (appToFunArgs [] postcon)]
                    postconvar_dec = map (convertVarExprToDecl vardecls) (snd (appToFunArgs [] postcon))
                    new_preconvar_dec = genSkolemList preconvar_dec postconvar_dec vardeclsGlobal ruleid
                    new_precond = funArgsToAppNoType (fst (appToFunArgs [] precon)) (map varDeclToExpr new_preconvar_dec)

                in new_precond


--genSkolem ::  VarDecl t -> [VarDecl t] -> [VarDecl t] -> String -> VarDecl t
-- Takes in an existing precondition var_decl, list of postcon var_decls, list of global varDecls and returns skolemized precon var_decl
genSkolem :: Eq t => VarDecl t -> [VarDecl t] -> [VarDecl t] -> String -> VarDecl t
genSkolem (VarDecl t vn u) y w z
  | VarDecl t vn u `elem` w = VarDecl t  vn u
  | VarDecl t vn u `elem` y = VarDecl t (capitalise vn) u
  | otherwise = VarDecl t "extVar" u

-- List version of genSkolem
genSkolemList :: Eq t => [VarDecl t] -> [VarDecl t] -> [VarDecl t] -> String -> [VarDecl t]
genSkolemList x y w z = [genSkolem xs y w z | xs <- x]

toBrackets :: [VarDecl t] -> String
toBrackets [] = "()"
toBrackets [VarDecl _t vn _u] = "(" ++ capitalise vn ++ ")"
toBrackets ((VarDecl _t vn _u):xs) = "(" ++ capitalise vn ++ "," ++ tail (toBrackets xs)

-- var to var expr
mkVarE :: Var t -> Expr t
mkVarE v = VarE {annotOfExpr =  annotOfQVarName (nameOfVar v), varOfExprVarE = v}
--Takes in a var_decl and turns it into a var_expr
varDeclToExpr :: VarDecl t -> Expr t
varDeclToExpr (VarDecl x y _z) = mkVarE (GlobalVar (QVarName x y))

varDeclToExprTup :: VarDecl t -> (Expr t, VarDecl t)
varDeclToExprTup (VarDecl x y z) = (varDeclToExpr (VarDecl x y z), VarDecl x y z)

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