{-# OPTIONS_GHC -W #-}
{-# OPTIONS_GHC -foptimal-applicative-do #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds, KindSignatures, AllowAmbiguousTypes, ApplicativeDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns, DataKinds, GADTs #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}



{-
Parse L4 into [? tentatively] labelled exprs 
-}
module LS.XPile.MathLang.ParseL4 where
-- TODO: Add export list

import Data.Text qualified as T
import LS.Utils.TextUtils (int2Text, float2Text)

import Control.Monad.Validate
  ( MonadValidate (..)
  , Validate
  , refute
  )
import Optics
-- import Data.Generics.Product.Types (types)
import Data.String (IsString)
import Data.String.Interpolate (i)

import AnyAll qualified as AA
import LS.Types qualified as L4
import LS.Types (RelationalPredicate(..), RPRel(..), MTExpr(..))
import LS.Rule as L4 (Rule(..), extractMTExprs)

import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

import Data.String (IsString)
import Prettyprinter (Pretty)

type VarName = String
type TLabel = String

data Stage = Prelim | Desugared
  deriving stock (Eq, Ord, Show)

data ExpF md (stage :: Stage) where 
  ELit :: { md :: md, lit :: Lit } -> ExpF md stage
  EOp ::
    { md :: md,
      binOp :: Op,
      leftArg :: ExpF md stage, -- ^ left
      rightArg :: ExpF md stage -- ^ right
    } -> ExpF md stage
  EUnOp :: { md :: md, unOp :: UnOp, arg :: ExpF md stage } -> ExpF md stage
  EVar :: { md :: md, var :: VarName } -> ExpF md stage
  EIf ::
    { md :: md,
      condExp :: ExpF md stage,
      thenExp :: ExpF md stage
    } -> ExpF md stage
  ELam ::
    { md :: md,             -- ^ lam metadata
      paramMd :: md,        -- ^ param metadata
      param :: VarName,     -- ^ param
      body :: ExpF md stage -- ^ body
    } -> ExpF md stage
  EApp ::
    { md :: md,
      func :: ExpF md stage, -- ^ func 
      arg :: ExpF md stage   -- ^ arg
    } -> ExpF md stage

  -- | variable mutation; prob treat as eval-ing to assigned value
  ESet ::
    { md :: md,
      var :: VarName, 
      arg :: ExpF md stage -- ^ arg 
    } -> ExpF md stage

  -- | sequence of statements; returns expression of the last
  ESeq :: { md :: md, stmts :: [ExpF md stage] } -> ExpF md stage    

  ELet ::
    -- Need to prefix field names with `let` because of https://gitlab.haskell.org/ghc/ghc/-/issues/12159
    { letMd :: md,
      letVar :: VarName,
      val :: ExpF md stage, -- ^ value 
      letBody :: ExpF md stage -- ^ body
    } -> ExpF md 'Prelim
  EAnd ::
    { andMd :: md,
      andLeftArg :: ExpF md stage,  -- ^ left
      andRightArg :: ExpF md stage  -- ^ right
    } -> ExpF md 'Prelim
  EOr ::
    { orMd :: md,
      orLeftArg :: ExpF md stage,
      orRightArg :: ExpF md stage
    } -> ExpF md 'Prelim
  EEmpty :: { md :: md } -> ExpF md stage

-- NOTE: will want to be able to tally the desugared nodes with the prelim nodes too, and port type info from the former to the latter
-- since will prob need to translate one of the prelim ASTs to Meng eval ast

deriving instance Show md => Show (ExpF md stage)

data Lit = ENumber | EBool | EString
  deriving stock (Eq, Ord, Show)

data Op = OpPlus | OpNumEq | OpStrEq | OpMaxOf | OpSum | OpProduct
  deriving stock (Eq, Ord, Show)

data UnOp 
  deriving stock (Eq, Ord, Show)
-- TODO: may not need this


data SrcPositn = MkPositn
  { row :: Int
  , col :: Int
  , filename :: String
  } deriving stock (Eq, Ord, Show)

data TypeMetadata = MkTMdata
  { tlabel :: TLabel
  } deriving stock (Eq, Ord, Show)

data ExpMetadata = MkEMdata
  { srcPos :: SrcPositn
  , typeMd :: TypeMetadata
  } deriving stock (Eq, Ord, Show)
makePrisms ''ExpMetadata

newtype Exp stage = ExpF ExpMetadata
  deriving stock (Eq, Ord, Show)
