-- Execution / evaluation of expressions

module Exec where

import Data.List
import Syntax
import Typing

lift_uarith_op :: UArithOp -> Val -> Val
lift_uarith_op u c = case (u, c) of
  (UAminus, IntV i) -> IntV (- i)
  _ -> ErrV

lift_ubool_op :: UBoolOp -> Val -> Val
lift_ubool_op u c = case (u, c) of
  (UBneg, BoolV b) -> BoolV (not b)
  _ -> ErrV

lift_unaop_expr :: UnaOp -> Expr Tp -> Expr Tp
lift_unaop_expr uop e = case (uop, e) of
  (UArith u, ValE t c) -> ValE t (lift_uarith_op u c)
  (UBool u, ValE t c) -> ValE t (lift_ubool_op u c)
  _ -> ValE ErrT ErrV

barith_fun :: BArithOp -> Integer -> Integer -> Integer
barith_fun ba = case ba of
  BAadd -> (+)
  BAsub -> (-)
  BAmul -> (*)
  BAdiv -> (div)
  BAmod -> (mod)

lift_barith_op :: BArithOp -> Val -> Val -> Val
lift_barith_op ba c1 c2 = case (c1, c2) of
  (IntV i1, IntV i2) -> IntV ((barith_fun ba) i1 i2)
  _ -> ErrV


lift_binop_expr :: BinOp -> Expr Tp -> Expr Tp -> Expr Tp
lift_binop_expr bop e1 e2 = case (bop, e1, e2) of
    (BArith ba, ValE t1 c1, ValE t2 c2) -> ValE (tp_barith t1 t2 ba) (lift_barith_op ba c1 c2)
    
constr_clos :: Tp -> Expr Tp -> Expr Tp -> Expr Tp
constr_clos rtp f a = case f of
  ClosE t cbd (FunE _ v _ e) -> ClosE rtp ((v, a):cbd) e
  _ -> error "application of non-function"

-- Takes an expression and returns an evaluated expression.
-- An expression is evaluated if it is of the form
-- ValE t c : explicit value
-- ClosE t1 bd (FunE ...): closure
-- All closure expressions are supposed to be closure-evaluated,
-- which means that for every closure of the form (ClosE t1 cbd e),
-- the expressions in cbd are evaluated and the closures in cbd are closure-evaluated
eval_expr :: [(VarName, Expr Tp)] -> Expr Tp -> Expr Tp
eval_expr bd x = case x of
  ValE t c -> ValE t c
  VarE t v ->
    case lookup v bd of
      Nothing -> ValE ErrT ErrV
      Just e -> e
  UnaOpE t uop e -> lift_unaop_expr uop (eval_expr bd e)
  BinOpE t bop e1 e2 -> lift_binop_expr bop (eval_expr bd e1) (eval_expr bd e2)
  AppE t f a -> eval_expr bd (constr_clos t (eval_expr bd f) (eval_expr bd a))
  fe@(FunE t v tparam e) -> ClosE t [] fe
  ClosE t cbd e -> case eval_expr (cbd ++ bd) e of
    ve@(ValE _ _) -> ve
    (ClosE t2 cbd2 e2) -> ClosE t (cbd2 ++ cbd) e2

