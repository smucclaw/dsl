{-# OPTIONS_GHC -W #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields, RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module LS.XPile.LogicalEnglish.IdVars (
    idVarsInHC
  , idVarsInAP  
  , idVarsInBody
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

idVarsInHC :: SimpleL4HC -> VarsHC
idVarsInHC = \case
  MkL4FactHc{..} -> MkVarsFact { vfhead =  idVarsInAP fgiven fhead }
  MkL4RuleHc{..} -> MkVarsRule { vrhead =  idVarsInAP rgiven rhead
                             , vrbody = idVarsInBody rgiven rbody }

-- TODO: Refactor with a Reader when time permits to de-emphasize the gvars threading
{- | Identifies vars in L4AtomicP:
* non-vars (according to the spec) stay as text
* things that should be vars (according to the spec) get converted to TemplateVars
-}
idVarsInAP :: GVarSet -> L4AtomicP -> AtomicPWithVars
idVarsInAP gvars = \case
  ABPatomic cells ->
    ABPatomic $ fmap mklabscell cells
  ABPIsDiffFr t1 t2 ->
    ABPIsDiffFr (cell2vcell gvars t1)
                (cell2vcell gvars t2)
  ABPIsOpOf t opOf termargs ->
    ABPIsOpOf (cell2vcell gvars t) opOf (fmap mklabscell termargs)
  ABPIsOpSuchTt t opST cells ->
    ABPIsOpSuchTt (cell2vcell gvars t) opST
                  (fmap mklabscell cells)
  where
    mklabscell = cell2vcell gvars

idVarsInBody :: GVarSet -> BoolPropn L4AtomicP -> BoolPropn AtomicPWithVars
idVarsInBody gvars l4boolprop =
  let absAtomic = idVarsInAP gvars
  in fmap absAtomic l4boolprop


---- helpers

{- |
The code for simplifying L4 AST has established these invariants:  
  * every IS NUM has had the IS removed, with the number converted to T.Text and wrapped in a MkCellIsNum
  * every IS tt was NOT an IS NUM has been replaced with a `MkCellT "is"`.

So the only time we need to think about IS-es, going forward, is when we have a MkCellIsNum. 
In other words, we can convert an arbitrary Cell to a VCell as long as we know the set of given vars, without having to check what other cells are / are not around it.
-}
cell2vcell :: GVarSet -> Cell -> VCell
cell2vcell gvars = \case
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