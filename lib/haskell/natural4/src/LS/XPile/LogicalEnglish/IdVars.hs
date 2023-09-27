{-# OPTIONS_GHC -W #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields, RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.LogicalEnglish.IdVars (
    idVarsInHC
  -- , idVarsInAP
  -- , idVarsInBody
) where

import Data.Text qualified as T
import Text.Replace
import Data.HashSet qualified as HS
import Data.Coerce (coerce)

import LS.XPile.LogicalEnglish.Types
import Data.Sequences (toStrict, fromStrict)

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
idVarsInAP gvars = fmap makeVCell
  where
    makeVCell = cell2vcell gvars

-- | Replace "." with "dot" and "," with "comma", in the Pred txts of ABPatomics
postprocAP :: AtomicPWithVars -> AtomicPWithVars
postprocAP = \case
  ABPatomic cells -> ABPatomic $ fmap replaceTxtVCell cells
  others          -> others

idVarsInBody :: GVarSet -> BoolPropn L4AtomicP -> BoolPropn AtomicPWithVars
idVarsInBody gvars = fmap (postprocAP . idVarsInAP gvars)


---- helpers

-- | Replace text in VCells
replaceTxtVCell :: VCell -> VCell
replaceTxtVCell = \case
  tv@(TempVar _) -> tv
  Pred txt  -> Pred $ replaceTxt txt

{- | TODO: Would be better to read in a dictionary of what/how to replace from some config file,
a config file that is kept in sync with the downstream stuff 
(since have to do this kind of replacement in the converse direction when generating justification)
-}
replaceTxt :: T.Text -> T.Text
replaceTxt = toStrict . replaceWithList replacements . fromStrict
  where
    replacements =
      [ Replace "," " comma ",
        Replace "." " dot ",
        Replace "%" " percent "
      ]

{- >>> replaceTxt "" 
""
-}

-- replaceTxt txt =  if txt == T.empty then txt
--                   -- T.replace will error if input empty
--                   else replacePercent . replaceCommaDot $ txt
--                   where replaceCommaDot = T.replace "," " comma " .
--                                           T.replace "." " dot "
--                         replacePercent = T.replace "%" " percent "

{- | Convert a SimplifiedL4 Cell to a VCell
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
    else
      let (prefix, isAposV) = isAposVar gvars celltxt
      in if isAposV
        then TempVar (EndsInApos prefix)
        else Pred celltxt
  MkCellIsNum numtxt -> TempVar (IsNum numtxt)

txtIsAGivenVar :: GVarSet -> T.Text -> Bool
txtIsAGivenVar gvars txt = HS.member (coerce txt) gvars

type PrefixAposVar = T.Text
isAposVar :: GVarSet -> T.Text -> (PrefixAposVar, Bool)
isAposVar gvs (T.stripSuffix "'s" -> Just prefix) =
            if txtIsAGivenVar gvs prefix
            then (prefix, True)
            else ("", False)
isAposVar _ _                                     = ("", False)
-- TODO: this matching on "'s" is a bit brittle cos unicode



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
--       | isAposVar gvars trm = EndsInApos trm
--       | otherwise = error "shouldn't be anything else"
--         -- TODO: add a check upfront for this 
-- optOfArg :: Cell -> TemplateVar
-- optOfArg = \case
--   MkCellT t -> OpOfVarArg t
--   MkCellIsNum t -> OpOfVarArg t
