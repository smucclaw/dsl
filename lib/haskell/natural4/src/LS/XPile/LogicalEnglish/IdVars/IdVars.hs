{-# OPTIONS_GHC -W #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields, RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.LogicalEnglish.IdVars.IdVars (
    idVarsInHC
  -- , idVarsInAP
  -- , idVarsInBody
) where

import Data.Coerce (coerce)
import Data.HashSet qualified as HS
import Data.Text qualified as T

import LS.XPile.LogicalEnglish.Types
  (
      BoolPropn(..)
    -- L4-related types
    , GVar(..)
    , GVarSet
    , Cell(..)
    
    , SimpleL4HC(MkL4FactHc, fgiven, fhead,
                 MkL4RuleHc, rgiven, rhead, rbody)

    , AtomicBPropn(..)
    , L4AtomicP

    -- Intermediate representation types, prisms, and constants
    , TemplateVar(..)
    -- , _TempVar
    --, _AposAtom, _NonVarOrNonAposAtom
    -- , aposSuffix
    , VarsHC(MkVarsFact,
             MkVarsRule, 
             vfhead,
             vrhead, vrbody)    
    , AtomicPWithVars
    , VCell(..)
  )
import LS.XPile.LogicalEnglish.IdVars.ReplaceTxtVCells (replaceTxtVCell)

-- $setup
-- >>> import Data.Text qualified as T
-- >>> :seti -XOverloadedStrings
-- >>> import Text.Replace (Replace (Replace), listToTrie, replaceWithTrie)
-- >>> import Data.Sequences (fromStrict, toStrict)

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
idVarsInAP gvars = postprocAP . fmap makeVCell
  where
    makeVCell = cell2vcell gvars

-- | Replace "." with "dot" and "," with "comma", in the Pred txts of ABPatomics
postprocAP :: AtomicPWithVars -> AtomicPWithVars
postprocAP = \case
  ABPatomic cells -> ABPatomic $ fmap replaceTxtVCell cells
  others          -> others

idVarsInBody :: GVarSet -> BoolPropn L4AtomicP -> BoolPropn AtomicPWithVars
idVarsInBody gvars = fmap $ idVarsInAP gvars


---- helpers

{- | Convert a SimplifiedL4 Cell to a VCell
The code for simplifying L4 AST has established these invariants:  
  * every IS NUM has had the IS removed, with the number converted to T.Text and wrapped in a MkCellIsNum
  * every IS tt was NOT an IS NUM has been marked as belonging to the ABPBaseIs variant of AtomicBP

So the only time we need to think about IS-es, going forward, is when we have a MkCellIsNum. 
In other words, we can convert an arbitrary Cell to a VCell as long as we know the set of given vars, without having to check what other cells are / are not around it.
-}
cell2vcell :: GVarSet -> Cell -> VCell
cell2vcell gvars = \case
  MkCellT celltxt    -> celltxt2vcell gvars celltxt
  MkCellIsNum numtxt -> TempVar (IsNum numtxt)

celltxt2vcell :: GVarSet -> T.Text -> VCell
celltxt2vcell gvars (T.stripSuffix "'s" -> Just prefix) = 
-- NOTE / TODO: this matching on "'s" is a bit brittle cos unicode
    if txtIsAGivenVar gvars prefix 
    then TempVar (EndsInApos prefix)
    else AposAtom prefix 
celltxt2vcell gvars celltxt
  | txtIsAGivenVar gvars celltxt = TempVar (MatchGVar celltxt)
  | otherwise = NonVarOrNonAposAtom celltxt

txtIsAGivenVar :: GVarSet -> T.Text -> Bool
txtIsAGivenVar gvars txt = HS.member (coerce txt) gvars


-- {-  Deprecating this and the next fn b/c the encoding suggests terms other than the args for op of might not just be either MatchGVar or EndsInApos --- they can also be atoms / non-variables
-- -}
-- term2tvar :: GVarSet -> Term -> TemplateVar
-- term2tvar gvars = \case
--   MkCellT trm -> whichTVar trm
--   MkCellIsNum trm -> whichTVar trm
--   where 
--     whichTVar :: T.Text -> TemplateVar
--     whichTVar trm
--       | txtIsAGivenVar gvars trm = MatchGVar trm
--       | checkApos gvars trm = EndsInApos trm
--       | otherwise = error "shouldn't be anything else"
--         -- TODO: add a check upfront for this 
-- optOfArg :: Cell -> TemplateVar
-- optOfArg = \case
--   MkCellT t -> OpOfVarArg t
--   MkCellIsNum t -> OpOfVarArg t
