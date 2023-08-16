{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}

module LS.XPile.LogicalEnglish.Internal
  (   L4Rules -- opaque
    , ValidHornls
    , Unvalidated

    , check
    , refine
    , loadRawL4AsUnvalid
    , gvarsFromL4Rule
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

import LS.Types
import LS.Rule qualified as L4 (Rule(..)) 
import LS.XPile.LogicalEnglish.Types
import LS.XPile.LogicalEnglish.Common (
    L4Prog,
    (|>)
    )

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

refine :: L4Rules ValidatedNotRefined -> L4Rules ValidHornls
refine (MkL4Rules rulelist) =  MkL4Rules (filter isHornlike rulelist)


-- | I experimented with auto generating prisms with Optics, but there was an issue with where some of the other type defs were; didn't seem worth the time fixing for our purposes when this would be good enoguh 
isHornlike :: L4.Rule -> Bool
isHornlike     L4.Hornlike{} = True
isHornlike             __ = False


{-------------------------------------------------------------------------------
   L4 rules -> SimpleL4HCs related
-------------------------------------------------------------------------------}

-- | Transforms a MTExpr tt appears in the GIVEN of a HC to a Gvar. This is importantly different from `mtexpr2text` in that it only converts the cases we use for LE and that we would encounter in the Givens on our LE conventions
gMTExpr2gvar :: MTExpr -> GVar
gMTExpr2gvar = \case 
  MTT var -> MkGVar var
  _       -> error "non-text mtexpr variable names in the GIVEN are not allowed on our LE spec :)"

extractGiven :: L4.Rule -> [MTExpr]
  -- [(NE.NonEmpty MTExpr, Maybe TypeSig)]
extractGiven L4.Hornlike {given=Nothing}        = [] 
-- won't need to worry abt this when we add checking upfront
extractGiven L4.Hornlike {given=Just paramtext} = concatMap (NE.toList . fst) (NE.toList paramtext)
extractGiven _                                  = trace "not a Hornlike rule, not extracting given" mempty
-- also won't need to worry abt this when we add checking + filtering upfront



-- wrapper :: L4Rules ValidHornls -> [(NE.NonEmpty MTExpr, Maybe TypeSig)]
-- wrapper = concat . map extractGiven . coerce

gvarsFromL4Rule :: L4.Rule -> GVarSet
gvarsFromL4Rule rule = let givenMTExprs = extractGiven rule
                       in HS.fromList $ map gMTExpr2gvar givenMTExprs