{-# LANGUAGE OverloadedStrings #-}

module LS.XPile.CoreL4.LogicProgram.Pretty.Utils
  ( my_str_trans_list,
    preconToVarStrList,
    skolemize2,
    toBrackets,
    toBrackets2,
    varDeclToVarStrList,
  )
where

import Data.List (intersperse)
import Flow ((|>))
import L4.PrintProg (capitalise)
import L4.Syntax (Expr, VarDecl (VarDecl))
import L4.SyntaxManipulation (appToFunArgs)
import LS.Utils (mapThenSwallowErrs, (|$>))
import LS.XPile.CoreL4.LogicProgram.Skolemize
  ( convertVarExprToDecl,
  )
import Prettyprinter (Doc, Pretty (pretty), concatWith)

-- Additional functions to write var substitution code

toBrackets :: [VarDecl t] -> Doc ann
toBrackets varDecls = varDecls |> varDeclToVarStrList |> toBrackets2

-- toBrackets [] = "()"
-- toBrackets [VarDecl _t vn _u] = "(" ++ capitalise vn ++ ")"
-- toBrackets ((VarDecl _t vn _u):xs) = "(" ++ capitalise vn ++ "," ++ tail (toBrackets xs)

preconToVarStrList :: Expr t -> [VarDecl t] -> [String]
preconToVarStrList precon vardecls = varDeclToVarStrList (mapThenSwallowErrs (convertVarExprToDecl vardecls) (snd (appToFunArgs [] precon)))

varDeclToVarStrList :: [VarDecl t] -> [String]
varDeclToVarStrList = map $ \ (VarDecl _t vn _u) -> capitalise vn

-- varDeclToVarStrList [] = []
-- varDeclToVarStrList ((VarDecl t vn u) : xs) = capitalise vn : varDeclToVarStrList xs

my_str_trans :: [String] -> String -> String
my_str_trans s t = if elem t s
      then t
else "V" ++ "_" ++ t

my_str_trans_list :: [String] -> [String] -> [String]
my_str_trans_list s = map $ my_str_trans s

my_str_trans2 :: String -> [String] -> String -> String
my_str_trans2 v postc rulen  =
    if v `elem` postc
    then v
    else "extVar"

my_str_trans_list2 :: [String] -> [String] -> String -> [String]
my_str_trans_list2 s t u = [my_str_trans2 r t u | r <- s]

toBrackets2 :: Pretty a => [a] -> Doc ann
toBrackets2 xs =
  xs
    |$> pretty          -- ["x1", "x2", ..., "xn"]
    |> concatWith (<.>) -- "x1, x2, ..., xn"
    |> parenthesize     -- "(x1, x2, ..., xn)"
  where
    x <.> y = x <> ", " <> y
    parenthesize x = "(" <> x <> ")"

-- toBrackets2 [] = "()"
-- toBrackets2 [x] = "(" ++ x ++ ")"
-- toBrackets2 (x:xs) = "(" ++ x ++ "," ++ tail (toBrackets2 xs)

-- toBrackets3 :: [VarDecl t] -> String
-- toBrackets3 [] = "()"
-- toBrackets3 [VarDecl t vn u] = "(" ++ vn ++ ")"
-- toBrackets3 ((VarDecl t vn u):xs) = "(" ++ vn ++ "," ++ tail (toBrackets xs)

--skolemize2 :: Eq t1 => [VarDecl t1] -> [VarDecl t1] -> Expr t2 -> String -> String
skolemize2 :: [VarDecl t1] -> [VarDecl t2] -> Expr t3 -> String -> Doc ann
skolemize2 vardecs localvar postc rulename =  toBrackets2 (my_str_trans_list2 (varDeclToVarStrList localvar) (varDeclToVarStrList ((mapThenSwallowErrs (convertVarExprToDecl vardecs) (snd (appToFunArgs [] postc))))) rulename)