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


module LS.XPile.LogicalEnglish.LamAbstract where
-- TODO: Make export list

import Data.Text qualified as T
import Data.Bifunctor       ( first )
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.HashMap.Strict qualified as Map
import Data.String (IsString)
import Data.List.NonEmpty qualified as NE
import Debug.Trace (trace)
import Data.Coerce (coerce)

import qualified AnyAll as AA
import LS.Types qualified as L4
import LS.Types (RelationalPredicate(..), RPRel(..), MTExpr(..), BoolStructR(..), BoolStructT)
import LS.Rule qualified as L4 (Rule(..))
import LS.XPile.LogicalEnglish.Types
import LS.XPile.LogicalEnglish.ValidateL4Input
      (L4Rules, ValidHornls, Unvalidated,
      loadRawL4AsUnvalid)

import LS.XPile.LogicalEnglish.UtilsLEReplDev -- for prototyping

lamAbstract :: SimpleL4HC -> LamAbsHC
lamAbstract = \case
  MkL4FactHc{..} -> MkLAFact { lafgiven = fgiven
                             , lafhead =  lamabsAtomicProp fgiven fhead }
  MkL4RuleHc{..} -> MkLARule { largiven = rgiven
                             , larhead =  lamabsAtomicProp rgiven rhead
                             , larbody = undefined }
{-
type L4AtomicP = AtomicBPropn Term [Cell]

data BoolPropn a = AtomicBP a
                 | And [BoolPropn a]
                 | Or  [BoolPropn a]
                 | Not (BoolPropn a)
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

-- | Atomic(ish) Boolean proposition
data AtomicBPropn var baseprop =
    ABPatomic baseprop
  | ABPIsDiffFr var var
  | ABPIsOpOf var OpOf [var]
  | ABPIsOpSuchTt var OpSuchTt baseprop


data L4Fact = L4Fact { givenVars :: GVarSet
                     , head      :: L4AtomicP
                     }
data L4Rule = L4Rule { givenVars :: GVarSet
                     , head      :: L4AtomicP
                     , body      :: BoolPropn L4AtomicP }


type LamAbsAtomicP = AtomicBPropn TemplateVar LamAbsBase

data LamAbsBase = TempVar TemplateVar
                | Pred    !T.Text
-}

-- TODO: Refactor with a Reader when time permits to de-emphasize the gvars threading
lamabsAtomicProp :: GVarSet -> L4AtomicP -> LamAbsAtomicP
lamabsAtomicProp gvars = \case
  ABPatomic cells -> 
    ABPatomic $ fmap (cell2labscell gvars) cells
  ABPIsDiffFr t1 t2 -> 
    ABPIsDiffFr (term2labTempVar gvars t1) 
                (term2labTempVar gvars t2)
  ABPIsOpOf t opOf termargs -> 
    ABPIsOpOf (term2labTempVar gvars t) opOf (fmap (term2labTempVar gvars) termargs)
  ABPIsOpSuchTt t opST cells -> 
    ABPIsOpSuchTt (term2labTempVar gvars t) opST 
                  (fmap (cell2labscell gvars) cells)


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

term2labTempVar :: GVarSet -> Term -> TemplateVar
term2labTempVar gvars = \case
  MkCellT celltxt ->
    if txtIsAGivenVar gvars celltxt
    then MatchGVar celltxt
    else if isAposVar gvars celltxt
         then EndsInApos celltxt
         else error "the input cell was not a term!"
          -- TODO: Would be better to make the L4 Term a type whose values are guaranteed to be either a MatchGVar or EndsInApos, and move the validation further upstream. Will do this when time permits.
  MkCellIsNum numtxt -> IsNum numtxt
    -- TODO: after we make the L4 Term more constrained, we shld be able to remove this case



txtIsAGivenVar :: GVarSet -> T.Text -> Bool
txtIsAGivenVar gvars txt = HS.member (coerce txt) gvars


isAposVar :: GVarSet -> T.Text -> Bool
isAposVar gvs ctxt = let (prefix, suffix) = T.splitAt 2 ctxt
                      in suffix == "'s" && txtIsAGivenVar gvs prefix


-- nlasFromLamAbsFact 
-- for now let's assume that the NLAs for facts are already in the fixed lib
-- TODO: When have more time, write smtg tt checks if it is indeed in fixed lib, and add it if not.

-- nlasFromLamAbsRule :: LamAbsRule -> HS.HashSet LENatLangAnnot
-- nlasFromLamAbsRule = undefined
-- {- for each base template (bt) in the LamAbsRule, across the head and body,
--   we take its sequence of original variable names <"v1", "v2", ..., "vn">,
--   make a new sequence <"*a v1", "a v2", ..., "a vn">,
--  and then instantiate the bt with that new sequence. 
-- -}
