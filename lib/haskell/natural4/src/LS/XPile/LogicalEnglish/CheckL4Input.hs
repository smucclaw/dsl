{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}

module LS.XPile.LogicalEnglish.CheckL4Input 
  (
      checkAndRefine
    , L4Rules -- opaque
    , ValidHornls
  ) 
  where

import Control.Monad.Validate (MonadValidate (refute), Validate, runValidate)

import Data.Text qualified as T
import Data.Bifunctor       ( first )
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.HashMap.Strict qualified as Map
import Control.Monad.Identity ( Identity )
import Data.String (IsString)

import LS.Rule (Rule(..))
import LS.XPile.LogicalEnglish.Types
import LS.XPile.LogicalEnglish.Common (
    L4Prog,
    (|>)
    )

data Unvalidated
data ValidatedNotRefined
data ValidHornls

newtype L4Rules validStatus = MkL4Rules [Rule]
  deriving (Eq, Ord, Show)

-- | TODO: Work on implementing this and adding the Monad Validate or Data.Validation stuff instead of Maybe (i.e., rly doing checks upfront and carrying along the error messages and potential warnings) after getting enoguh of the main transpiler out
checkAndRefine :: L4Rules Unvalidated -> Maybe (L4Rules ValidHornls)
checkAndRefine rawrules = do
  validatedL4rules <- check rawrules
  return $ refine validatedL4rules


-- -- | TODO: Work on this after getting enoguh of the main transpiler out
check :: L4Rules Unvalidated -> Maybe (L4Rules ValidatedNotRefined)
check (MkL4Rules rulelist) = Just $ MkL4Rules rulelist

refine :: L4Rules ValidatedNotRefined -> L4Rules ValidHornls
refine (MkL4Rules rulelist) =  MkL4Rules (filter isHornlike rulelist)

------------ helpers -----------------

-- | I experimented with auto generating prisms with Optics, but there was an issue with where some of the other type defs were; didn't seem worth the time fixing for our purposes when this would be good enoguh 
isHornlike :: Rule -> Bool
isHornlike     Hornlike{} = True
isHornlike             __ = False
