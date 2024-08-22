{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module LS.Renamer.Scope (
  -- * Types for implementing the Renamer scope checking
  Scope (..),
  emptyScope,
  scScopeTable,
  scUniqueSupply,
  ScopeTable (..),
  stVariables,
  stFunction,
  unionScopeTable,
  differenceScopeTable,
  emptyScopeTable,
  FuncInfo (..),
  funcArity,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Optics hiding (has)

import LS.Renamer.Rules

data FuncInfo = FuncInfo
  { _funcArity :: (Int, Int)
  -- ^ Arity of a function. The first component means how many parameters
  -- are allowed before the function, the second component how many parameters
  -- are allowed afterwards.
  -- For example @(1, 1)@ is a simple infix function of the form @x f y@ where @f@
  -- is the name of the function.
  }
  deriving (Eq, Ord, Show)

data Scope = Scope
  { _scScopeTable :: ScopeTable
  , _scUniqueSupply :: Unique
  -- ^ next unique value that we can use
  }
  deriving (Eq, Ord, Show)

-- | A 'ScopeTable' keeps tab on the variables and functions that occur in a
-- program.
--
-- Invariant:
--
-- Every name that gets resolved to an 'RnName' with 'RnNameType' being
-- 'RnFunction' should have additional 'FuncInfo' in '_stFunction'.
data ScopeTable = ScopeTable
  { _stVariables :: Map OccName RnName
  -- ^ all names currently in scope
  , _stFunction :: Map RnName FuncInfo
  -- ^ additional information for resolved functions
  }
  deriving (Eq, Ord, Show)

unionScopeTable :: ScopeTable -> ScopeTable -> ScopeTable
unionScopeTable tbl1 tbl2 =
  ScopeTable
    { _stVariables = Map.union tbl1._stVariables tbl2._stVariables
    , _stFunction = Map.union tbl1._stFunction tbl2._stFunction
    }

differenceScopeTable :: ScopeTable -> ScopeTable -> ScopeTable
differenceScopeTable tbl1 tbl2 =
  ScopeTable
    { _stVariables = Map.difference tbl1._stVariables tbl2._stVariables
    , _stFunction = Map.difference tbl1._stFunction tbl2._stFunction
    }

emptyScopeTable :: ScopeTable
emptyScopeTable =
  ScopeTable
    { _stVariables = Map.empty
    , _stFunction = Map.empty
    }

makeFieldsNoPrefix 'Scope
makeFieldsNoPrefix 'ScopeTable
makeFieldsNoPrefix 'FuncInfo

emptyScope :: Scope
emptyScope =
  Scope
    { _scScopeTable = emptyScopeTable
    , _scUniqueSupply = 0
    }
