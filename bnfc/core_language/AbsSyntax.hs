
module AbsSyntax where

import Prelude (Bool(True,False), Char, Double, Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified AbsL

----------------------------------------------------------------------
-- Definition of expressions
----------------------------------------------------------------------

data ConstVal
    = BoolV Bool
    | IntV Integer
  deriving (C.Eq, C.Ord, C.Show, C.Read)

-- unary arithmetic operators
data UArithOp = UAminus
  deriving (C.Eq, C.Ord, C.Show, C.Read)

-- unary boolean operators
data UBoolOp = UBneg
  deriving (C.Eq, C.Ord, C.Show, C.Read)

-- unary operators (union of the above)
data UnaOp
    = UArith UArithOp
    | UBool UBoolOp
  deriving (C.Eq, C.Ord, C.Show, C.Read)

-- binary arithmetic operators
data BArithOp = BAadd | BAsub | BAmul | BAdiv | BAmod
  deriving (C.Eq, C.Ord, C.Show, C.Read)

-- binary comparison operators
data BComparOp = BCeq | BClt | BClte | BCgt | BCgte | BCne
  deriving (C.Eq, C.Ord, C.Show, C.Read)

-- binary boolean operators
data BBoolOp = BBand | BBor
  deriving (C.Eq, C.Ord, C.Show, C.Read)

-- binary operators (union of the above)
data BinOp
    = BArith BArithOp
    | BCompar BComparOp
    | BBool BBoolOp
  deriving (C.Eq, C.Ord, C.Show, C.Read)

-- operators for combining list elements
data ListOp = AndList | OrList | XorList | CommaList
    deriving (C.Eq, C.Ord, C.Show, C.Read)

-- Exp t is an expression of type t (to be determined during type checking / inference)
data Exp t
    = ConstE t ConstVal
    | VarE t String
    | UnaOpE t UnaOp (Exp t)
    | BinOpE t BinOp (Exp t) (Exp t)
    | ListE t ListOp [Exp t]
    deriving (C.Eq, C.Ord, C.Show, C.Read)
    
 
