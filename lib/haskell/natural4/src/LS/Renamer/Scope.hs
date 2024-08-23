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
  -- * The ScopeTable
  ScopeTable (..),
  stVariables,
  stFunction,
  -- * Utilities to work with 'ScopeTable'
  unionScopeTable,
  differenceScopeTable,
  emptyScopeTable,
  filterScopeTable,
  -- * Renamer information we keep track of for each function.
  FuncInfo (..),
  funcArity,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Optics hiding (has)

import LS.Renamer.Rules
import qualified Data.Set as Set

data Scope = Scope
  { _scScopeTable :: ScopeTable
  , _scUniqueSupply :: Unique
  -- ^ next unique value that we can use
  }
  deriving (Eq, Ord, Show)

-- | Initialise an empty environment for Scope checking.
emptyScope :: Scope
emptyScope =
  Scope
    { _scScopeTable = emptyScopeTable
    , _scUniqueSupply = 0
    }

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

-- | Union names of a 'ScopeTable' with another 'ScopeTable'.
-- On conflict, prefer names from the second 'ScopeTable'.
unionScopeTable :: ScopeTable -> ScopeTable -> ScopeTable
unionScopeTable tbl1 tbl2 =
  ScopeTable
    { _stVariables = Map.union tbl2._stVariables tbl1._stVariables
    , _stFunction = Map.union tbl2._stFunction tbl1._stFunction
    }

-- | Filter the scope table, propagating changes to the '_stFunction' Map as well.
-- Keeps all elements that satisfy the predicate.
--
filterScopeTable :: (OccName -> RnName -> Bool) -> ScopeTable -> ScopeTable
filterScopeTable p tbl =
  let
    newVars = Map.filterWithKey p tbl._stVariables
    newFunctions = Map.restrictKeys tbl._stFunction (Set.fromList $ Map.elems newVars)
  in
    ScopeTable
      { _stVariables = newVars
      , _stFunction = newFunctions
      }

-- | Remove all names from the first 'ScopeTable' that occur in the second
-- 'ScopeTable'.
differenceScopeTable :: ScopeTable -> ScopeTable -> ScopeTable
differenceScopeTable tbl1 tbl2 =
  ScopeTable
    { _stVariables = Map.difference tbl1._stVariables tbl2._stVariables
    , _stFunction = Map.difference tbl1._stFunction tbl2._stFunction
    }

-- | Create an Empty ScopeTable
emptyScopeTable :: ScopeTable
emptyScopeTable =
  ScopeTable
    { _stVariables = Map.empty
    , _stFunction = Map.empty
    }

-- | 'FuncInfo' summarises meta information of one particular function.
-- For now, this tracks merely the arity of each function, but might be
-- extended in the futre.
data FuncInfo = FuncInfo
  { _funcArity :: (Int, Int)
  -- ^ Arity of a function. The first component means how many parameters
  -- are allowed before the function, the second component how many parameters
  -- are allowed afterwards.
  -- For example @(1, 1)@ is a simple infix function of the form @x f y@ where @f@
  -- is the name of the function.
  }
  deriving (Eq, Ord, Show)


makeFieldsNoPrefix 'Scope
makeFieldsNoPrefix 'ScopeTable
makeFieldsNoPrefix 'FuncInfo
