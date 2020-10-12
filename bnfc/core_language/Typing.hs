-- Typing of expressions

module Typing where

-- import qualified Prelude as C (Eq, Ord, Show, Read)
import AbsSyntax
import Data.List

----------------------------------------------------------------------
-- Environment
----------------------------------------------------------------------

-- Typing is done in an environment, composed of
-- the global decls of a module and
-- local variable declarations
data LocalVarDecls = LVD [(VarName,Tp)]
  deriving (Eq, Ord, Show, Read)
data Environment = Env Module LocalVarDecls
  deriving (Eq, Ord, Show, Read)

module_of_env :: Environment -> Module
module_of_env (Env m _) = m

locals_of_env :: Environment -> [(VarName,Tp)]
locals_of_env (Env _ (LVD ls)) = ls


----------------------------------------------------------------------
-- Class manipulation
----------------------------------------------------------------------

class_def_assoc :: [ClassDecl] -> [(ClassName, ClassDef)]
class_def_assoc cds = map (\(ClsDecl cn cdf) -> (cn, cdf)) cds

-- For a class name 'cn', returns the list of the names of the superclasses of 'cn'
-- Here, 'cdf_assoc' is an association of class names and class defs as contained in a module.
-- 'visited' is the list of class names already visited on the way up the class hierarchy
super_classes :: [(ClassName, ClassDef)] -> [ClassName] -> ClassName -> [ClassName]
super_classes cdf_assoc visited cn =
  case lookup cn cdf_assoc of
    Nothing -> error "in super_classes: cn not in cdf_assoc (internal error)"
    Just (ClsDef Nothing _) -> reverse (cn : visited)
    Just (ClsDef (Just scn) _) -> 
      if elem scn visited
      then error ("cyclic superclass hierarchy for class " ++ (case cn of (ClsNm n) -> n))
      else super_classes cdf_assoc (cn : visited) scn 

-- For each of a list of class declarations, returns its list of superclass names
super_classes_decls :: [ClassDecl] -> [[ClassName]]
super_classes_decls cds =
  let cdf_assoc = class_def_assoc cds
  in map (super_classes cdf_assoc []) (map fst cdf_assoc)

-- $> customCs

-- $> super_classes_decls customCs


-- Cyclic superclass hierarchy:
-- $> super_classes_decls [ClsDecl (ClsNm "Foo") (ClsDef (Just (ClsNm "Bar")) []), ClsDecl (ClsNm "Bar") (ClsDef (Just (ClsNm "Foo")) [])]


-- TODO: several checks for well-formedness of class definitions:
-- - no ref to undefined superclass
-- - no cyclic graph hierarchy (implemented in super_classes above)
-- - no duplicate field declarations (local and inherited)

-- the class decl does not reference an undefined superclass
defined_superclass :: [ClassName] -> ClassDecl -> Bool
defined_superclass cns cdc =
  case cdc of
    (ClsDecl cn (ClsDef Nothing _)) -> True
    (ClsDecl cn (ClsDef (Just scn) _)) ->
      if elem scn cns
      then True
      else error ("undefined superclass for class " ++ (case cn of (ClsNm n) -> n))


wellformed_class_decls_in_module :: Module -> Bool
wellformed_class_decls_in_module md =
  case md of
    (Mdl cds rls) ->
      let class_names = map name_of_class_decl cds
      in all (defined_superclass class_names) cds
  

----------------------------------------------------------------------
-- Typing functions
----------------------------------------------------------------------


tp_constval :: Val -> Tp
tp_constval x = case x of
  BoolV _ -> BoolT
  IntV _ -> IntT

tp_var :: Environment -> VarName -> Tp
tp_var env v =
  case lookup v (locals_of_env env) of
    Nothing -> ErrT
    Just t -> t  

tp_of_expr :: Exp t -> t
tp_of_expr x = case x of
  ValE t _      -> t
  VarE t _        -> t
  UnaOpE t _ _    -> t
  BinOpE t _ _ _  -> t
  CastE t _ _     -> t
  ListE t _ _     -> t


tp_uarith :: Tp -> UArithOp -> Tp
tp_uarith t ua = if t == IntT then IntT else ErrT

tp_ubool :: Tp -> UBoolOp -> Tp
tp_ubool t ub = if t == BoolT then BoolT else ErrT

tp_unaop :: Tp -> UnaOp -> Tp
tp_unaop t uop = case uop of
  UArith ua  -> tp_uarith t ua
  UBool ub   -> tp_ubool t ub


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


-- the first type can be cast to the second type
cast_compatible :: Tp -> Tp -> Bool
cast_compatible te ctp = True

tp_expr :: Environment -> Exp () -> Exp Tp
tp_expr env x = case x of
  ValE () c -> ValE (tp_constval c) c
  VarE () v -> VarE (tp_var env v) v
  UnaOpE () uop e -> 
    let te = (tp_expr env e)
        t   = tp_unaop (tp_of_expr te) uop
    in  UnaOpE t uop te
  BinOpE () bop e1 e2 ->
    let te1 = (tp_expr env e1)
        te2 = (tp_expr env e2)
        t   = tp_binop (tp_of_expr te1) (tp_of_expr te2) bop
    in  BinOpE t bop te1 te2
  CastE () ctp e ->        
    let te = (tp_expr env e)
    in if cast_compatible (tp_of_expr te) ctp
       then CastE ctp ctp te
       else CastE ErrT ctp te



----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

-- $> tp_expr (Env (Mdl [] []) (LVD [])) (BinOpE () (BCompar BClte) (BinOpE () (BArith BAadd) (ValE () (IntV 50)) (BinOpE () (BArith BAmul) (BinOpE () (BArith BAadd) (ValE () (IntV 90)) (ValE () (IntV 4))) (VarE () (VarNm "foo")))) (BinOpE () (BArith BAmul) (ValE () (IntV 2)) (ValE () (IntV 5))))

-- $> tp_expr (Env (Mdl [] []) (LVD [(VarNm "foo", IntT)])) (BinOpE () (BCompar BClte) (BinOpE () (BArith BAadd) (ValE () (IntV 50)) (BinOpE () (BArith BAmul) (BinOpE () (BArith BAadd) (ValE () (IntV 90)) (ValE () (IntV 4))) (VarE () (VarNm "foo")))) (BinOpE () (BArith BAmul) (ValE () (IntV 2)) (ValE () (IntV 5))))

-- $> tp_expr (Env (Mdl [] []) (LVD [])) (ValE () (IntV 5))

-- $> tp_expr (Env (Mdl [] []) (LVD [])) (UnaOpE () (UBool UBneg) (BinOpE () (BCompar BClte) (ValE () (IntV 2)) (BinOpE () (BArith BAmul) (ValE () (IntV 2)) (ValE () (IntV 5)))))

