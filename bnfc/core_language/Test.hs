

module Test where

import Data.List
import AbsSyntax
import Typing
import Exec
import TaToUppaal

----------------------------------------------------------------------
-- Exec.hs
----------------------------------------------------------------------

-- $> eval_expr [] (AppE BoolT (FunE (FunT BoolT BoolT) (VarNm "x") BoolT  (UnaOpE BoolT (UBool UBneg) (VarE BoolT (VarNm "x")))) (ValE BoolT (BoolV True)))


-- $> eval_expr [] (AppE IntT (FunE (FunT IntT IntT) (VarNm "x") IntT  (BinOpE IntT (BArith BAadd) (ValE IntT (IntV 3)) (VarE IntT (VarNm "x")))) (ValE IntT (IntV 5)))

-- $> eval_expr [] (tp_expr (Env preambleMdl (LVD [])) (AppE () (AppE () (FunE () (VarNm "x") IntT (FunE () (VarNm "y") IntT (BinOpE () (BArith BAadd) (VarE () (VarNm "x")) (VarE () (VarNm "x"))))) (ValE () (IntV 5))) (ValE () (IntV 2))))

----------------------------------------------------------------------
-- Typing.hs
----------------------------------------------------------------------

-- $> super_classes_decls customCs

-- $> elaborate_fields_in_class_decls (elaborate_supers_in_class_decls customCs)

-- Cyclic superclass hierarchy:
-- $> super_classes_decls [ClsDecl (ClsNm "Foo") (ClsDef (Just (ClsNm "Bar")) []), ClsDecl (ClsNm "Bar") (ClsDef (Just (ClsNm "Foo")) [])]


-- $> elaborate_module (Mdl customCs [])

-- Expressions

-- $> tp_expr (Env preambleMdl (LVD [])) (AppE () (FunE () (VarNm "x") IntT (VarE () (VarNm "x"))) (ValE () (IntV 3)))

-- $> tp_expr (Env (elaborate_module preambleMdl) (LVD [])) (ValE () (RecordV (ClsNm "SGD") [(FldNm "val", IntV 50)]))

-- $> tp_expr (Env preambleMdl (LVD [])) (BinOpE () (BCompar BClte) (BinOpE () (BArith BAadd) (ValE () (IntV 50)) (BinOpE () (BArith BAmul) (BinOpE () (BArith BAadd) (ValE () (IntV 90)) (ValE () (IntV 4))) (VarE () (VarNm "foo")))) (BinOpE () (BArith BAmul) (ValE () (IntV 2)) (ValE () (IntV 5))))

-- $> tp_expr (Env preambleMdl (LVD [(VarNm "foo", IntT)])) (BinOpE () (BCompar BClte) (BinOpE () (BArith BAadd) (ValE () (IntV 50)) (BinOpE () (BArith BAmul) (BinOpE () (BArith BAadd) (ValE () (IntV 90)) (ValE () (IntV 4))) (VarE () (VarNm "foo")))) (BinOpE () (BArith BAmul) (ValE () (IntV 2)) (ValE () (IntV 5))))



-- $> tp_expr (Env preambleMdl (LVD [])) (UnaOpE () (UBool UBneg) (BinOpE () (BCompar BClte) (ValE () (IntV 2)) (BinOpE () (BArith BAmul) (ValE () (IntV 2)) (ValE () (IntV 5)))))


-- $> tp_expr (Env preambleMdl (LVD [])) (ValE () (IntV 5))




----------------------------------------------------------------------
-- Example: Timed Automata
----------------------------------------------------------------------

tra1 :: Transition
tra1 = Trans (Lc "l1") [ClCn (Cl "cl1") BClte 3] Internal [(Cl "cl1")] (Lc "l2")
tra2 :: Transition
tra2 = Trans (Lc "l1") [ClCn (Cl "cl2") BCgte 5] (Act (ClsNm "A1") Snd) [(Cl "cl1")] (Lc "l3")
trb2 :: Transition
trb2 = Trans (Lc "l1") [ClCn (Cl "cl1") BCgte 7] (Act (ClsNm "A1") Rec) [(Cl "cl1")] (Lc "l2")

autA :: TA Tp
autA = TmdAut "AutA"
       [(Lc "l1"), (Lc "l2"), (Lc "l3")] [(ClsNm "A1")] [(Cl "cl1"), (Cl "cl2")] [tra1, tra2] [(Lc "l1")] [] []
autB :: TA Tp
autB = TmdAut "AutB"
       [(Lc "l1"), (Lc "l2")] [(ClsNm "A1")] [(Cl "cl1"), (Cl "cl2")] [trb2] [(Lc "l1")] [] []


-- $> writeFile "test_haskell_uppaal.xta"  (ta_sys_to_uppaal (TmdAutSys [autA, autB]))

