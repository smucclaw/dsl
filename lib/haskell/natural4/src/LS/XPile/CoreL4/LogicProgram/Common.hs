{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
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
import L4.Syntax
  ( Expr,
    VarDecl
  )

data LPLang
  = ASP
  | Epilog
  deriving (Eq, Generic, Hashable, Ord, Read, Show)

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
  deriving (Eq, Generic, Hashable, Ord, Read, Show)

type ASPRule = LPRule ASP
type EpilogRule = LPRule Epilog

data LogicProgram (lpLang :: LPLang) t = LogicProgram
  { lpRulesNoFact :: [LPRule lpLang t],
    lpRulesFact :: [LPRule lpLang t],
    oppClauses :: [OpposesClause t]
  }
  deriving (Eq, Generic, Hashable, Ord, Read, Show)

type ASPProgram = LogicProgram ASP
type EpilogProgram = LogicProgram Epilog

data OpposesClause t = OpposesClause
  { posLit :: Expr t,
    negLit :: Expr t
  }
  deriving (Eq, Generic, Hashable, Ord, Read, Show)
