{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module LS.XPile.CoreL4.LogicProgram.Common
  ( LPLang (..),
    LPRule (..),
    ASPRule,
    EpilogRule,
    LogicProgram (..),
    ASPProgram,
    EpilogProgram,
    OpposesClause (..),
  )
where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import L4.PrintProg
  ( PrintConfig (..),
    PrintCurried (..),
    PrintVarCase (..),
  )
import L4.Syntax

data LPLang
  = ASP
  | Epilog
  deriving (Eq, Generic, Ord, Read, Show)

instance Hashable LPLang

{-
  Logic program rules and logic programs are types indexed by:
  - a type in the (closed) type universe lpLang, ie ASP or Epilog
  - a babyl4 type annotation t 
-}
data LPRule (lpLang :: LPLang) t = LPRule
  { ruleName :: String,
    globalVarDecls :: [VarDecl t],
    localVarDecls :: [VarDecl t],
    preconds :: [Expr t],
    postcond :: Expr t
  }
  deriving (Eq, Generic, Ord, Read, Show)

instance Hashable t => Hashable (LPRule lpLang t)

type ASPRule = LPRule ASP
type EpilogRule = LPRule Epilog

data LogicProgram (lpLang :: LPLang) t = LogicProgram
  { lpRulesNoFact :: [LPRule lpLang t],
    lpRulesFact :: [LPRule lpLang t],
    oppClauses :: [OpposesClause t]
  }
  deriving (Eq, Generic, Ord, Read, Show)

instance Hashable t => Hashable (LogicProgram lpLang t)

type ASPProgram = LogicProgram ASP
type EpilogProgram = LogicProgram Epilog

data OpposesClause t = OpposesClause
  { posLit :: Expr t,
    negLit :: Expr t
  }
  deriving (Eq, Generic, Ord, Read, Show)

instance Hashable t => Hashable (OpposesClause t)

deriving instance Generic (QVarName t)
deriving instance Generic (Var t)
deriving instance Generic Val
deriving instance Generic ClassName
deriving instance Generic (Tp t)
deriving instance Generic (VarDecl t)

deriving instance Generic UArithOp
deriving instance Generic UBoolOp
deriving instance Generic UTemporalOp
deriving instance Generic UnaOp

deriving instance Generic BArithOp
deriving instance Generic BBoolOp
deriving instance Generic BComparOp
deriving instance Generic BinOp

deriving instance Generic Quantif
deriving instance Generic FieldName
deriving instance Generic ListOp

deriving instance Generic (Expr t)

instance Hashable t => Hashable (QVarName t)
instance Hashable t => Hashable (Var t)
instance Hashable Val
instance Hashable ClassName
instance Hashable t => Hashable (Tp t)
instance Hashable t => Hashable (VarDecl t)

instance Hashable UArithOp
instance Hashable UBoolOp
instance Hashable UTemporalOp
instance Hashable UnaOp

instance Hashable BArithOp
instance Hashable BBoolOp
instance Hashable BComparOp
instance Hashable BinOp

instance Hashable Quantif
instance Hashable FieldName
instance Hashable ListOp

instance Hashable t => Hashable (Expr t)