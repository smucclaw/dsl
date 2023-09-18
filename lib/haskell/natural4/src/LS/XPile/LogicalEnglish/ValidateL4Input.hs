-- {-# OPTIONS_GHC -W #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}

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
import Data.List.NonEmpty qualified as NE

import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.HashMap.Strict qualified as Map
import Control.Monad.Identity ( Identity )
import Data.Bifunctor       ( first )
import Data.String (IsString)
import Data.Coerce (coerce)
-- import Optics

import LS.Types as L4
import LS.Types (RelationalPredicate(..), RPRel(..))
import LS.Rule qualified as L4 (Rule(..)) 
import LS.XPile.LogicalEnglish.Types

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
loadRawL4AsUnvalid = MkL4Rules 


-- | TODO: Work on implementing this and adding the Monad Validate or Data.Validation stuff instead of Maybe (i.e., rly doing checks upfront and carrying along the error messages and potential warnings) after getting enoguh of the main transpiler out
check :: L4Rules Unvalidated -> Maybe (L4Rules ValidatedNotRefined)
check (MkL4Rules rulelist) = Just $ MkL4Rules rulelist


{- | L4 rules are refined when:
* All the rules are DECIDEs / HCs
* There is exactly one HC in the clauses field -- i.e., 
  the 'ditto' / decision table syntax will be desugared in the obvious way.
  But note that we will not support the 'OTHERWISE' keyword.
* The RelationalPredicate in the head of a HC will be a RPConstraint or RPMT -- never a RPnary or RPBoolstructR RPrel 
    (even tho the HC data type currently allows for tt)
-}
refine :: L4Rules ValidatedNotRefined -> L4Rules ValidHornls
refine (MkL4Rules rulelist) =  MkL4Rules (filter isHornlike rulelist)



-- | I experimented with auto generating prisms with Optics, but there was an issue with where some of the other type defs were; didn't seem worth the time fixing for our purposes when this would be good enoguh 
isHornlike :: L4.Rule -> Bool
isHornlike     L4.Hornlike{} = True
isHornlike             __ = False
