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
  MkL4FactHc{..} -> MkLAFact { lafgiven = fgiven
                             , lafhead =  lamabstractAP fgiven fhead }
  MkL4RuleHc{..} -> MkLARule { largiven = rgiven
                             , larhead =  lamabstractAP rgiven rhead
                             , larbody = lamabstractBody rgiven rbody }

-- TODO: Refactor with a Reader when time permits to de-emphasize the gvars threading
{- | Lambda abstracts over a L4AtomicP, in the sense that
* non-vars (according to the spec) stay as text
* things that should be vars (according to the spec) get converted to TemplateVars
-}
lamabstractAP :: GVarSet -> L4AtomicP -> LamAbsAtomicP
lamabstractAP gvars = \case
  ABPatomic cells ->
    ABPatomic $ fmap (cell2labscell gvars) cells
  ABPIsDiffFr t1 t2 ->
    ABPIsDiffFr (term2tvar gvars t1)
                (term2tvar gvars t2)
  ABPIsOpOf t opOf termargs ->
    ABPIsOpOf (term2tvar gvars t) opOf (fmap optOfArg termargs)
  ABPIsOpSuchTt t opST cells ->
    ABPIsOpSuchTt (term2tvar gvars t) opST
                  (fmap (cell2labscell gvars) cells)

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

-- TODO: Look into a better / more concise way of doing this
optOfArg :: Term -> TemplateVar
optOfArg = \case
  MkCellT t -> OpOfVarArg t
  MkCellIsNum t -> OpOfVarArg t

term2tvar :: GVarSet -> Term -> TemplateVar
term2tvar gvars = \case
    MkCellT trm -> tryOtherCasesFirst trm
    MkCellIsNum trm -> tryOtherCasesFirst trm
    where 
      tryOtherCasesFirst :: T.Text -> TemplateVar
      tryOtherCasesFirst trm
        | txtIsAGivenVar gvars trm = MatchGVar trm
        | isAposVar gvars trm = EndsInApos trm
        | otherwise = OtherVar trm
-- TODO: Look into trying to do away with the OtherVar case by leveraging the guarantees we have or don't have re the vars tt appear there. We basically only need OtherVar if it's possible to have a non-MatchGVar, non-EndsInApos term in that position (as it is for an OptOf term arg)


txtIsAGivenVar :: GVarSet -> T.Text -> Bool
txtIsAGivenVar gvars txt = HS.member (coerce txt) gvars

isAposVar :: GVarSet -> T.Text -> Bool
isAposVar gvs ctxt = let (prefix, suffix) = T.splitAt 2 ctxt
                      in suffix == "'s" && txtIsAGivenVar gvs prefix