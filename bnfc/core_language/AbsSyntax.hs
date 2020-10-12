
module AbsSyntax where

-- import Prelude (Bool(True,False), Char, Double, Integer, String)
-- import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified AbsL

----------------------------------------------------------------------
-- Definition of expressions
----------------------------------------------------------------------

----- Names 
newtype VarName = VarNm String
  deriving (Eq, Ord, Show, Read)
newtype ClassName = ClsNm String
  deriving (Eq, Ord, Show, Read)
newtype FieldName = FldNm String
  deriving (Eq, Ord, Show, Read)


----- Types 
data Tp
  = BoolT
  | IntT
  | ClassT ClassName
  | ErrT
  deriving (Eq, Ord, Show, Read)

-- Field attributes: for example cardinality restrictions
-- data FieldAttribs = FldAtt
data FieldDecl = FldDecl FieldName Tp -- FieldAttribs
  deriving (Eq, Ord, Show, Read)

-- superclass, list of field declarations
data ClassDef t = ClsDef t [FieldDecl] 
  deriving (Eq, Ord, Show, Read)

data ClassDecl t = ClsDecl ClassName (ClassDef t)
  deriving (Eq, Ord, Show, Read)

name_of_class_decl :: ClassDecl t -> ClassName
name_of_class_decl (ClsDecl cn _) = cn

fields_of_class_def :: ClassDef t -> [FieldDecl]
fields_of_class_def (ClsDef scn fds) = fds

-- some custom classes - should eventually go into a prelude and not be hard-wired
objectC = ClsDecl (ClsNm "Object") (ClsDef Nothing [])

-- QualifiedNumeric class with val field
-- TODO: should its type be IntT or a FloatT?
qualifNumC = ClsDecl (ClsNm "QualifiedNumeric")
                    (ClsDef (Just (ClsNm "Object"))
                            [FldDecl (FldNm "val") IntT])

-- Currency as QualifiedNumeric, with specific currencies (SGD, USD) as subclasses
currencyC = ClsDecl (ClsNm "Currency")
                    (ClsDef (Just (ClsNm "QualifiedNumeric")) [])
currencyCs = [ClsDecl (ClsNm "SGD") (ClsDef (Just (ClsNm "Currency")) []),
              ClsDecl (ClsNm "USD") (ClsDef (Just (ClsNm "Currency")) [])]
  
-- Time as QualifiedNumeric, with Year, Month, Day etc. as subclasses
-- TODO: treatment of time needs a second thought
--       (so far no distinction between time point and duration)
timeC = ClsDecl (ClsNm "Time")
                    (ClsDef (Just (ClsNm "QualifiedNumeric")) [])
timeCs = [ClsDecl (ClsNm "Year") (ClsDef (Just (ClsNm "Time")) []),
          ClsDecl (ClsNm "Day") (ClsDef (Just (ClsNm "Time")) [])]

customCs = [objectC, qualifNumC, currencyC] ++ currencyCs ++ [timeC] ++ timeCs
data Rule = TBD
  deriving (Eq, Ord, Show, Read)
data Module t = Mdl [ClassDecl t] [Rule]
  deriving (Eq, Ord, Show, Read)


----- Expressions 
data Val
    = BoolV Bool
    | IntV Integer
  deriving (Eq, Ord, Show, Read)

-- unary arithmetic operators
data UArithOp = UAminus
  deriving (Eq, Ord, Show, Read)

-- unary boolean operators
data UBoolOp = UBneg
  deriving (Eq, Ord, Show, Read)

-- unary operators (union of the above)
data UnaOp
    = UArith UArithOp
    | UBool UBoolOp
  deriving (Eq, Ord, Show, Read)

-- binary arithmetic operators
data BArithOp = BAadd | BAsub | BAmul | BAdiv | BAmod
  deriving (Eq, Ord, Show, Read)

-- binary comparison operators
data BComparOp = BCeq | BClt | BClte | BCgt | BCgte | BCne
  deriving (Eq, Ord, Show, Read)

-- binary boolean operators
data BBoolOp = BBand | BBor
  deriving (Eq, Ord, Show, Read)

-- binary operators (union of the above)
data BinOp
    = BArith BArithOp
    | BCompar BComparOp
    | BBool BBoolOp
  deriving (Eq, Ord, Show, Read)

-- operators for combining list elements
data ListOp = AndList | OrList | XorList | CommaList
    deriving (Eq, Ord, Show, Read)

-- Exp t is an expression of type t (to be determined during type checking / inference)
data Exp t
    = ValE t Val
    | VarE t VarName
    | UnaOpE t UnaOp (Exp t)
    | BinOpE t BinOp (Exp t) (Exp t)
    | FldAccE t (Exp t) FieldName
    | CastE t Tp (Exp t)
    | ListE t ListOp [Exp t]
    deriving (Eq, Ord, Show, Read)

 
