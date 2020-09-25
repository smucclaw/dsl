-- Typing of expressions

module Typing where

-- import qualified Prelude as C (Eq, Ord, Show, Read)
import AbsSyntax

data Tp = BoolT | IntT | ErrT
  deriving (Eq, Ord, Show, Read)


tp_constval :: ConstVal -> Tp
tp_constval x = case x of
  BoolV _ -> BoolT
  IntV _ -> IntT

tp_var :: [(String,Tp)] -> String -> Tp
tp_var env v =
  case lookup v env of
    Nothing -> ErrT
    Just t -> t  

tp_of_expr :: Exp t -> t
tp_of_expr x = case x of
  ConstE t _      -> t
  VarE t _        -> t
  UnaOpE t _ _    -> t
  BinOpE t _ _ _  -> t
  ListE t _ _     -> t

tp_barith :: Tp -> Tp -> BArithOp -> Tp
tp_barith t1 t2 ba = if (t1 == t2) && t1 == IntT then IntT else ErrT

tp_bcompar :: Tp -> Tp -> BComparOp -> Tp
tp_bcompar t1 t2 bc = if (t1 == t2) then BoolT else ErrT

tp_bbool :: Tp -> Tp -> BBoolOp -> Tp
tp_bbool t1 t2 bc = if (t1 == t2) && t1 == BoolT then BoolT else ErrT

tp_binop :: Tp -> Tp -> BinOp -> Tp
tp_binop t1 t2 bop = case bop of
  BArith ba  -> tp_barith t1 t2 ba
  BCompar bc -> tp_bcompar t1 t2 bc
  BBool bb   -> tp_bbool t1 t2 bb

tp_expr :: [(String,Tp)] -> Exp () -> Exp Tp
tp_expr env x = case x of
  ConstE () c -> ConstE (tp_constval c) c
  VarE () v -> VarE (tp_var env v) v
  BinOpE () bop e1 e2 ->
    let te1 = (tp_expr env e1)
        te2 = (tp_expr env e2)
        t   = tp_binop (tp_of_expr te1) (tp_of_expr te2) bop in
        BinOpE t bop te1 te2
        

-- $> tp_expr [] (BinOpE () (BCompar BClte) (BinOpE () (BArith BAadd) (ConstE () (IntV 50)) (BinOpE () (BArith BAmul) (BinOpE () (BArith BAadd) (ConstE () (IntV 90)) (ConstE () (IntV 4))) (VarE () "foo"))) (BinOpE () (BArith BAmul) (ConstE () (IntV 2)) (ConstE () (IntV 5))))

-- $> tp_expr [("foo", IntT)] (BinOpE () (BCompar BClte) (BinOpE () (BArith BAadd) (ConstE () (IntV 50)) (BinOpE () (BArith BAmul) (BinOpE () (BArith BAadd) (ConstE () (IntV 90)) (ConstE () (IntV 4))) (VarE () "foo"))) (BinOpE () (BArith BAmul) (ConstE () (IntV 2)) (ConstE () (IntV 5))))
