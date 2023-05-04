{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}

module LS.XPile.CoreL4.LogicProgram.Common
  ( LPType (..),
    LPRule (..),
    ASPRule,
    EpilogRule,
    LogicProgram (..),
    ASPProgram,
    EpilogProgram,
    OpposesClause (..),
  )
where

import L4.PrintProg
  ( PrintConfig (..),
    PrintCurried (..),
    PrintVarCase (..),
  )
import L4.Syntax (Expr, VarDecl)

data LPType
  = ASP
  | Epilog
  deriving (Eq, Ord, Read, Show)

{-
  Logic program rules and logic programs are type families indexed by:
  - a type in the (closed) type universe LPType, ie ASP or Epilog
  - a babyl4 type annotation t 
-}
data LPRule (lpType :: LPType) t = LPRule
  { nameOfLPRule :: String,
    globalVarDeclsOfLPRule :: [VarDecl t],
    localVarDeclsOfLPRule :: [VarDecl t],
    precondOfLPRule :: [Expr t],
    postcondOfLPRule :: Expr t
  }
  deriving (Eq, Ord, Read, Show)

type ASPRule = LPRule ASP
type EpilogRule = LPRule Epilog

data LogicProgram (lpType :: LPType) t = LogicProgram
  { lpRulesNoFact :: [LPRule lpType t],
    lpRulesFact :: [LPRule lpType t],
    oppClauses :: [OpposesClause t]
  }
  deriving (Eq, Ord, Read, Show)

type ASPProgram = LogicProgram ASP
type EpilogProgram = LogicProgram Epilog

data OpposesClause t = OpposesClause
  { posLit :: Expr t,
    negLit :: Expr t
  }
  deriving (Eq, Ord, Read, Show)