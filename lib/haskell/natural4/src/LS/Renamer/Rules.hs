{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module LS.Renamer.Rules (
  -- * Renamed Rule types
  RnRule (..),
  RnHornlike (..),
  RnTypeDecl (..),
  RnHornClause (..),
  RnTypedMulti (..),
  RnParamText (..),
  RnRuleName,
  RnExpr (..),
  RnName (..),
  RnNameType (..),
  RnTypeSig (..),
  RnEntityType,
  RnLit (..),
  RnRelationalPredicate (..),
  RnBoolStructR,
  Unique,
  OccName,
  mkSimpleOccName,

  -- * Builtins
  RnBuiltin (..),
  isL4BuiltIn,
  l4Builtins,

  -- * Utilities
  isFunctionName,
  isVariableName,
  isSelectorName,
  isExprOfNameType,
  isNameOrLitOrBuiltin,

  -- * Pretty functions for types that do not have a canonical 'Pretty' unique
  prettyMT,
) where

import AnyAll.BoolStruct qualified as AA
import LS.Rule (RuleLabel)
import LS.Types (MyToken, SrcRef)
import LS.Types qualified as LS

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Prettyprinter

-- ----------------------------------------------------------------------------
-- Types specific to the renamer phase
-- ----------------------------------------------------------------------------

-- | A rename rule is the same as a 'Rule' but names that occur in the rule
-- are resolved and renamed.
-- This aims to provide common ground for transpilers, s.t. a transpiler can
-- assume a name is already defined, and language ambiguities are resolved.
-- Further, this representation aims to be usable for typechecking.
data RnRule
  = Hornlike RnHornlike
  | TypeDecl RnTypeDecl
  deriving (Eq, Ord, Show, Generic)

type RnBoolStructR = AA.OptionallyLabeledBoolStruct RnRelationalPredicate

-- | Corresponds to 'HornClause2', which is equivalent to @HornClause BoolStructR@.
--
-- We don't seem to require any parameterization.
data RnHornClause = RnHornClause
  { rnHcHead :: RnRelationalPredicate
  , rnHcBody :: Maybe RnBoolStructR
  }
  deriving (Eq, Ord, Show, Generic)

type RnRuleName = RnExpr
type RnEntityType = RnName

data RnHornlike = RnHornlike
  { name :: RnRuleName -- MyInstance
  , super :: Maybe RnTypeSig -- IS A Superclass
  , keyword :: MyToken -- decide / define / means
  , given :: Maybe RnParamText -- a:Applicant, p:Person, l:Lender       -- the type signature of the input
  , giveth :: Maybe RnParamText -- m:Amount,   mp:Principal, mi:Interest -- the type signature of the output
  , upon :: Maybe RnParamText -- second request occurs
  , clauses :: [RnHornClause] -- colour IS blue WHEN fee > $10 ; colour IS green WHEN fee > $20 AND approver IS happy
  , rlabel :: Maybe RuleLabel
  , lsource :: Maybe Text
  , wwhere :: [RnRule]
  , srcref :: Maybe SrcRef
  , defaults :: [RnRelationalPredicate] -- SomeConstant IS 500 ; MentalCapacity TYPICALLY True
  , symtab :: [RnRelationalPredicate] -- SomeConstant IS 500 ; MentalCapacity TYPICALLY True
  }
  deriving (Eq, Ord, Show, Generic)

data RnTypeDecl = RnTypeDecl
  { name :: RnRuleName -- MyInstance
  , super :: Maybe RnTypeSig -- IS A Superclass
  , has :: [RnRule] -- HAS foo :: List Hand, bar :: Optional Restaurant
  , enums :: Maybe RnParamText -- ONE OF rock, paper, scissors (basically, disjoint subtypes)
  , given :: Maybe RnParamText
  , upon :: Maybe RnParamText
  , rlabel :: Maybe RuleLabel
  , lsource :: Maybe Text.Text
  , srcref :: Maybe SrcRef
  , defaults :: [RnRelationalPredicate] -- SomeConstant IS 500 ; MentalCapacity TYPICALLY True
  , symtab :: [RnRelationalPredicate] -- SomeConstant IS 500 ; MentalCapacity TYPICALLY True
  }
  deriving (Eq, Ord, Show, Generic)

data RnTypeSig
  = RnSimpleType LS.ParamType RnEntityType
  | RnInlineEnum LS.ParamType RnParamText
  deriving (Eq, Ord, Show, Generic)

newtype RnParamText = RnParamText
  { mkParamText :: NonEmpty RnTypedMulti
  }
  deriving (Eq, Ord, Show, Generic)

data RnTypedMulti = RnTypedMulti
  { rnTypedMultiExpr :: NonEmpty RnExpr
  , rnTypedMultiTypeSig :: Maybe RnTypeSig
  }
  deriving (Eq, Ord, Show, Generic)

-- | A name is something that can be resolved as either a variable, function, or enum.
data RnName = RnName
  { rnOccName :: OccName
  , rnUniqueId :: Unique
  , rnNameType :: RnNameType
  -- TODO: add the binding scope for scope checking
  -- , rnBindingScope :: BindingScope
  }
  deriving (Eq, Ord, Show, Generic)

data RnNameType
  = RnSelector
  | RnFunction
  | RnVariable
  | RnType
  | RnEnum
  deriving (Eq, Ord, Show, Generic)

data RnExpr
  = RnExprName RnName
  | RnExprBuiltin RnBuiltin
  | RnExprLit RnLit
  | RnFunDecl RnName [RnName]
  | RnFunApp RnName [RnExpr]
  | RnProjection RnName [RnName]
  deriving (Eq, Ord, Show, Generic)

data RnLit
  = RnInt Integer
  | RnDouble Double
  | RnBool Bool
  | RnString Text
  deriving (Eq, Ord, Show, Generic)

data RnRelationalPredicate
  = -- | Might be something like a record access.
    RnRelationalTerm RnExpr
  | RnConstraint RnExpr LS.RPRel RnExpr
  | RnBoolStructR RnExpr LS.RPRel RnBoolStructR
  | RnNary LS.RPRel [RnRelationalPredicate]
  deriving (Eq, Ord, Show, Generic)

type Unique = Int

-- | An unresolved name as it occurs in the original source.
type OccName = NonEmpty LS.MTExpr

-- | A simple 'OccName' which contains a single text fragment.
mkSimpleOccName :: Text -> OccName
mkSimpleOccName = NE.singleton . LS.MTT

-- ----------------------------------------------------------------------------
-- Builtins
-- ----------------------------------------------------------------------------

data RnBuiltin
  = RnOtherwise
  deriving (Eq, Ord, Show, Generic)

isL4BuiltIn :: OccName -> Maybe RnBuiltin
isL4BuiltIn name = Map.lookup name l4Builtins

l4Builtins :: Map OccName RnBuiltin
l4Builtins =
  Map.fromList
    [ (oTHERWISE, RnOtherwise)
    ]

oTHERWISE :: OccName
oTHERWISE = mkSimpleOccName "OTHERWISE"

-- ----------------------------------------------------------------------------
-- Renamer utilities
-- ----------------------------------------------------------------------------

isFunctionName :: RnExpr -> Maybe RnName
isFunctionName expr = isExprOfNameType expr (RnFunction ==)

isVariableName :: RnExpr -> Maybe RnName
isVariableName expr = isExprOfNameType expr (RnVariable ==)

isSelectorName :: RnExpr -> Maybe RnName
isSelectorName expr = isExprOfNameType expr (RnSelector ==)

isExprOfNameType :: RnExpr -> (RnNameType -> Bool) -> Maybe RnName
isExprOfNameType (RnExprName name) hasTy
  | hasTy name.rnNameType = Just name
  | otherwise = Nothing
isExprOfNameType _ _ = Nothing

isNameOrLitOrBuiltin :: RnExpr -> Maybe RnExpr
isNameOrLitOrBuiltin = \case
  rn@RnExprName{} -> Just rn
  rn@RnExprBuiltin{} -> Just rn
  rn@RnExprLit{} -> Just rn
  _ -> Nothing

-- ----------------------------------------------------------------------------
-- Pretty instances
-- ----------------------------------------------------------------------------

prettyMT :: LS.MTExpr -> Doc ann
prettyMT (LS.MTT t) = pretty t
prettyMT (LS.MTI int) = pretty int
prettyMT (LS.MTF float) = pretty float
prettyMT (LS.MTB boolean) = pretty boolean

instance Pretty RnNameType where
  pretty = \case
    RnSelector -> "Selector"
    RnFunction -> "Function"
    RnVariable -> "Variable"
    RnType -> "Type"
    RnEnum -> "Enum"
