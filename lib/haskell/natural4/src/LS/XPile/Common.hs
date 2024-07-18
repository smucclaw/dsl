{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module LS.XPile.Common where

import Data.Hashable (Hashable)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LS.Types (ParamType (..), TypeSig (..), enumLabels)

-- Functions and utilities that are not specific to individual transpilers.

-- ----------------------------------------------------------------------------
-- L4 declared entity types
-- ----------------------------------------------------------------------------

-- | GML types that are declared in L4 by the user, e.g. 'Person' or 'Singaporean citizen'.
data L4EntityType
  = -- | a user-defined named type
    L4EntityType T.Text
  | -- | a user-defined enumeration type
    L4Enum [T.Text]
  | -- | a user-defined list type
    L4List L4EntityType
  deriving stock (Eq, Generic, Show, Ord)
  deriving anyclass (Hashable)

{- | Turn a surface-L4 type signature into a GML type.

NOTE / TODO: This translation seems lossy.
-}
mkEntityType :: TypeSig -> L4EntityType
mkEntityType = \case
  SimpleType TOne tn -> L4EntityType tn
  SimpleType TOptional tn -> L4EntityType tn -- no optional
  SimpleType _ tn -> L4List $ mkEntityType $ SimpleType TOne tn -- lists, sets, no difference
  InlineEnum _ pt -> L4Enum $ enumLabels pt -- assuming no lists here (is there an example of a list of enum values?)

