module LS.XPile.CoreL4.LogicProgram.Pretty.Utils
  ( my_str_trans_list,
    preconToVarStrList,
    skolemize2,
    toBrackets2,
    varDeclToVarStrList,
  )
where

import L4.Syntax ( VarDecl(VarDecl), Expr )
import LS.XPile.CoreL4.LogicProgram.Skolemize
    ( convertVarExprToDecl )
import L4.PrintProg ( capitalise )
import L4.SyntaxManipulation ( appToFunArgs )

-- Additional functions to write var substitution code

preconToVarStrList :: Expr t ->[VarDecl t] -> [String]
preconToVarStrList precon vardecls = varDeclToVarStrList(map (convertVarExprToDecl vardecls) (snd (appToFunArgs [] precon)))

varDeclToVarStrList :: [VarDecl t] -> [String]
varDeclToVarStrList [] = []
varDeclToVarStrList ((VarDecl t vn u) : xs) = capitalise vn : varDeclToVarStrList xs

my_str_trans :: [String] -> String -> String
my_str_trans s t = if elem t s
      then t
else "V" ++ "_" ++ t

my_str_trans_list :: [String] -> [String] -> [String]
my_str_trans_list s ts = [my_str_trans s t | t <- ts]

my_str_trans2 :: String -> [String] -> String -> String
my_str_trans2 v postc rulen  =
    if v `elem` postc
    then v
    else "extVar"

my_str_trans_list2 :: [String] -> [String] -> String -> [String]
my_str_trans_list2 s t u = [my_str_trans2 r t u | r <- s]

toBrackets2 :: [String] -> String
toBrackets2 [] = "()"
toBrackets2 [x] = "(" ++ x ++ ")"
toBrackets2 (x:xs) = "(" ++ x ++ "," ++ tail (toBrackets2 xs)

-- toBrackets3 :: [VarDecl t] -> String
-- toBrackets3 [] = "()"
-- toBrackets3 [VarDecl t vn u] = "(" ++ vn ++ ")"
-- toBrackets3 ((VarDecl t vn u):xs) = "(" ++ vn ++ "," ++ tail (toBrackets xs)

--skolemize2 :: Eq t1 => [VarDecl t1] -> [VarDecl t1] -> Expr t2 -> String -> String
skolemize2 :: [VarDecl t1] -> [VarDecl t2] -> Expr t3 -> String -> String
skolemize2 vardecs localvar postc rulename =  toBrackets2 (my_str_trans_list2 (varDeclToVarStrList localvar) (varDeclToVarStrList ((map (convertVarExprToDecl vardecs) (snd (appToFunArgs [] postc))))) rulename)