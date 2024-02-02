-- {-# OPTIONS_GHC -W #-}
module LS.XPile.LogicalEnglish.ValidateL4Input
  (   L4Rules -- opaque
    , ValidHornls
    , Unvalidated

    , check
    , refine
    , loadRawL4AsUnvalid

    , isHornlike -- TODO: TEMP export; will remove this after implementing the prevalidation
  )
where

import Control.Monad.Validate (MonadValidate (refute), Validate, runValidate)

import Data.Text qualified as T

import Data.Maybe (fromMaybe, listToMaybe)
import Data.Coerce (coerce)
-- import Optics

import LS.Rule qualified as L4 (Rule(..))

import Debug.Trace (trace)

{-------------------------------------------------------------------------------
   Validation related
-------------------------------------------------------------------------------}

data Unvalidated
data ValidatedNotRefined
data ValidHornls

newtype L4Rules validStatus = MkL4Rules [L4.Rule]
  deriving (Eq, Ord, Show)

loadRawL4AsUnvalid :: [L4.Rule] -> L4Rules Unvalidated
loadRawL4AsUnvalid = coerce

-- | TODO: Work on implementing this and adding the Monad Validate or Data.Validation stuff instead of Maybe (i.e., rly doing checks upfront and carrying along the error messages and potential warnings) after getting enoguh of the main transpiler out
check :: L4Rules Unvalidated -> Maybe (L4Rules ValidatedNotRefined)
check (MkL4Rules rulelist) = Just $ coerce rulelist

{- | L4 rules are refined when:
* All the rules are DECIDEs / HCs
* There is exactly one HC in the clauses field -- i.e., 
  the 'ditto' / decision table syntax will be desugared in the obvious way.
  But note that we will not support the 'OTHERWISE' keyword.
* The RelationalPredicate in the head of a HC will be a RPConstraint or RPMT -- never a RPnary or RPBoolstructR RPrel 
    (even tho the HC data type currently allows for tt)
-}
refine :: L4Rules ValidatedNotRefined -> L4Rules ValidHornls
refine (MkL4Rules rulelist) = coerce $ filter isHornlike rulelist

-- | I experimented with auto generating prisms with Optics, but there was an issue with where some of the other type defs were; didn't seem worth the time fixing for our purposes when this would be good enoguh 
isHornlike :: L4.Rule -> Bool
isHornlike     L4.Hornlike{} = True
isHornlike                 _ = False
