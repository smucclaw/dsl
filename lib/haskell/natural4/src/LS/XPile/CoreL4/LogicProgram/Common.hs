{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}

module LS.XPile.CoreL4.LogicProgram.Common
where

import L4.PrintProg
  ( PrintConfig (..),
    PrintCurried (..),
    PrintVarCase (..),
  )
import L4.Syntax ( Expr, VarDecl )

data LPType
  = ASP
  | Epilog
  deriving (Eq, Ord, Read, Show)

{-
  Family of types for logic program rules, indexed by:
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
  deriving (Eq, Ord, Show, Read)

type ASPRule = LPRule ASP
type EpilogRule = LPRule Epilog

data OpposesClause t = OpposesClause
  { posLit :: Expr t,
    negLit :: Expr t
  }
  deriving (Eq, Ord, Show, Read)

data LogicProgram (lpType :: LPType) t = LogicProgram
  { lpRulesNoFact :: [LPRule lpType t],
    lpRulesFact :: [LPRule lpType t],
    oppClauses :: [OpposesClause t]
  }
  deriving (Eq, Ord, Read, Show)

type ASPProgram = LogicProgram ASP
type EpilogProgram = LogicProgram Epilog