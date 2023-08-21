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
                             , lafhead =  lamabstractAP fgiven fhead }
  MkL4RuleHc{..} -> MkLARule { largiven = rgiven
                             , larhead =  lamabstractAP rgiven rhead
                             , larbody = lamabstractBody rgiven rbody }
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
  MkCellT t -> OpOfTerm t
  MkCellIsNum t -> OpOfTerm t

term2tvar :: GVarSet -> Term -> TemplateVar
term2tvar gvars = \case
    MkCellT trm -> tryOtherCasesFirst trm
    MkCellIsNum trm -> tryOtherCasesFirst trm
    where 
      tryOtherCasesFirst :: T.Text -> TemplateVar
      tryOtherCasesFirst trm
        | txtIsAGivenVar gvars trm = MatchGVar trm
        | isAposVar gvars trm = EndsInApos trm
        | otherwise = OtherTerm trm
-- TODO: Look into trying to do away with the OtherTerm case by leveraging the guarantees we have or don't have re the vars tt appear there. We basically only need OtherTerm if it's possible to have a non-MatchGVar, non-EndsInApos term in that position (as it is for an OptOf term arg)


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
