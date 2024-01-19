{-# OPTIONS_GHC -W #-}
{-# OPTIONS_GHC -foptimal-applicative-do #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
import Prettyprinter(Pretty)

type VarName = String
type TLabel = String

data Stage = Prelim | Desugared

data ExpF md stage where 
  LitE :: Lit -> md -> ExpF md stage
  OpE :: md 
      -> Op 
      -> ExpF md stage -- ^ left
      -> ExpF md stage -- ^ right
      -> ExpF md stage
  UnOpE :: md -> UnOp -> ExpF md stage -> ExpF md stage
  VarE :: md -> VarName -> ExpF md stage
  IfE :: md -> ExpF md stage -> ExpF md stage -> ExpF md stage
  LamE :: md        -- ^ lam metadata
       -> md        -- ^ param metadata
       -> VarName      -- ^ param
       -> ExpF md stage -- ^ body
       -> ExpF md stage
  AppE :: md 
       -> ExpF md stage -- ^ func 
       -> ExpF md stage -- ^ arg
       -> ExpF md stage

  -- variable mutation
  SetE :: md         
       -> VarName 
       -> ExpF md stage -- ^ arg 
       -> ExpF md stage
  SeqE :: md -> [ExpF md stage] -> ExpF md stage     
  LetE :: md 
       -> VarName 
       -> ExpF md stage -- ^ value 
       -> ExpF md stage -- ^ body
       -> ExpF md 'Prelim
  And :: md 
      -> ExpF md stage  -- ^ left
      -> ExpF md stage  -- ^ right
      -> (ExpF md 'Prelim)
  Or :: md 
     -> ExpF md stage 
     -> ExpF md stage
     -> (ExpF md 'Prelim) 
  EmptyE :: md -> ExpF md stage

data Lit = NumberE | BoolE | StringE
    deriving stock (Eq, Ord, Show)

data Op = OpPlus | OpNumEq | OpStrEq | OpMaxOf | OpSum | OpProduct
  deriving stock (Show, Eq, Ord)

data UnOp 
-- TODO: may not need this


data ExpMetadata = MkMdata
  { row :: Int
  , col :: Int
  , filename :: String
  , tlabel :: TLabel
  } deriving stock (Eq, Ord, Show)
makePrisms ''ExpMetadata

newtype Exp stage = ExpF ExpMetadata
  deriving stock (Eq, Ord, Show)
