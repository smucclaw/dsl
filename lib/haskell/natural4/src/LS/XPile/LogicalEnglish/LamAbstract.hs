{-# OPTIONS_GHC -W #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields, RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds, KindSignatures, AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module LS.XPile.LogicalEnglish.LamAbstract (
    lamAbstract
  , lamabstractAP  
  , lamabstractBody
) where

import Data.Text qualified as T
import Data.HashSet qualified as HS
import GHC.Generics (Generic)
import Data.String (IsString)
import Data.List.NonEmpty qualified as NE
import Debug.Trace (trace)
import Data.Coerce (coerce)
import Data.Text
import Data.String.Interpolate ( i )

import LS.XPile.LogicalEnglish.Types
import LS.XPile.LogicalEnglish.ValidateL4Input
      (L4Rules, ValidHornls, Unvalidated,
      loadRawL4AsUnvalid)

lamAbstract :: SimpleL4HC -> LamAbsHC
lamAbstract = \case
  MkL4FactHc{..} -> MkLAFact { lafhead =  lamabstractAP fgiven fhead }
  MkL4RuleHc{..} -> MkLARule { larhead =  lamabstractAP rgiven rhead
                             , larbody = lamabstractBody rgiven rbody }

-- TODO: Refactor with a Reader when time permits to de-emphasize the gvars threading
{- | Lambda abstracts over a L4AtomicP, in the sense that
* non-vars (according to the spec) stay as text
* things that should be vars (according to the spec) get converted to TemplateVars
-}
lamabstractAP :: GVarSet -> L4AtomicP -> LamAbsAtomicP
lamabstractAP gvars = \case
  ABPatomic cells ->
    ABPatomic $ fmap mklabscell cells
  ABPIsDiffFr t1 t2 ->
    ABPIsDiffFr (cell2labscell gvars t1)
                (cell2labscell gvars t2)
  ABPIsOpOf t opOf termargs ->
    ABPIsOpOf (cell2labscell gvars t) opOf (fmap mklabscell termargs)
  ABPIsOpSuchTt t opST cells ->
    ABPIsOpSuchTt (cell2labscell gvars t) opST
                  (fmap mklabscell cells)
  where
    mklabscell = cell2labscell gvars

lamabstractBody :: GVarSet -> BoolPropn L4AtomicP -> BoolPropn LamAbsAtomicP
lamabstractBody gvars l4boolprop =
  let absAtomic = lamabstractAP gvars
  in fmap absAtomic l4boolprop


---- helpers

{- |
The code for simplifying L4 AST has established these invariants:  
  * every IS NUM has had the IS removed, with the number converted to T.Text and wrapped in a MkCellIsNum
  * every IS tt was NOT an IS NUM has been replaced with a `MkCellT "is"`.

So the only time we need to think about IS-es, going forward, is when we have a MkCellIsNum. 
In other words, we can convert an arbitrary Cell to a LamAbsCell as long as we know the set of given vars, without having to check what other cells are / are not around it.
-}
cell2labscell :: GVarSet -> Cell -> LamAbsCell
cell2labscell gvars = \case
  MkCellT celltxt ->
    if txtIsAGivenVar gvars celltxt
    then TempVar (MatchGVar celltxt)
    else if isAposVar gvars celltxt
         then TempVar (EndsInApos celltxt)
         else Pred celltxt
  MkCellIsNum numtxt -> TempVar (IsNum numtxt)


-- {- | Deprecating this and the next fn b/c the encoding suggests terms other than the args for op of might not just be either MatchGVar or EndsInApos --- they can also be atoms / non-variables
-- -}
-- term2tvar :: GVarSet -> Term -> TemplateVar
-- term2tvar gvars = \case
--   MkCellT trm -> whichTVar trm
--   MkCellIsNum trm -> whichTVar trm
--   where 
--     whichTVar :: T.Text -> TemplateVar
--     whichTVar trm
--       | txtIsAGivenVar gvars trm = MatchGVar trm
--       | isAposVar gvars trm = EndsInApos trm
--       | otherwise = error "shouldn't be anything else"
--         -- TODO: add a check upfront for this 
-- optOfArg :: Cell -> TemplateVar
-- optOfArg = \case
--   MkCellT t -> OpOfVarArg t
--   MkCellIsNum t -> OpOfVarArg t


txtIsAGivenVar :: GVarSet -> T.Text -> Bool
txtIsAGivenVar gvars txt = HS.member (coerce txt) gvars

isAposVar :: GVarSet -> T.Text -> Bool
isAposVar gvs ctxt = let (prefix, suffix) = T.splitAt 2 ctxt
                      in suffix == "'s" && txtIsAGivenVar gvs prefix