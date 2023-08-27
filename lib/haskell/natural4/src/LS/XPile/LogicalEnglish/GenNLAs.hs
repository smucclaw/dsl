{-# OPTIONS_GHC -W #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields, RecordWildCards #-}
-- {-# LANGUAGE OverloadedRecordDot #-}
-- {-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.LogicalEnglish.GenNLAs (
    nlasFromVarsHC
  )
where

-- TODO: Make export list

-- import Data.Text qualified as T
import Data.HashSet qualified as HS
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import qualified Data.List as L hiding (head, tail)
-- import Debug.Trace (trace)
import Data.Coerce (coerce)
-- import Data.String.Interpolate ( i )

import LS.XPile.LogicalEnglish.Types


nlasFromVarsHC :: VarsHC -> HS.HashSet LENatLangAnnot
nlasFromVarsHC = \case
  VhcF vfact ->
    case (nlaFromVFact vfact) of
      Nothing -> HS.empty
      Just nla -> HS.singleton nla
  VhcR vrule ->
    nlasFromVarsRule vrule

-- TODO: When have more time, write smtg tt checks if it is indeed in fixed lib, and add it if not.
nlaFromVFact :: VarsFact -> Maybe LENatLangAnnot
nlaFromVFact VFact{..} = nlaLoneFromVAtomicP varsfhead

nlasFromVarsRule :: VarsRule -> HS.HashSet LENatLangAnnot
nlasFromVarsRule MkBaseRule{..} =
  let bodyNLAs = nlasFromBody rbody
  in case (nlaLoneFromVAtomicP rhead) of
    Nothing -> bodyNLAs
    Just headNLA -> HS.insert headNLA bodyNLAs

nlasFromBody :: BoolPropn AtomicPWithVars -> HS.HashSet LENatLangAnnot
nlasFromBody varsABP =
  let lstNLAs = fmap nlaLoneFromVAtomicP varsABP
  in HS.fromList . catMaybes . toList $ lstNLAs

-- TODO: Check if this really does conform to the spec --- went a bit fast here
nlaLoneFromVAtomicP :: AtomicPWithVars -> Maybe LENatLangAnnot
nlaLoneFromVAtomicP =  \case
  ABPatomic vcells -> annotFromVCells vcells
  ABPIsOpSuchTt _ _ vcells -> annotFromVCells vcells
  ABPIsDiffFr{} -> Nothing
  ABPIsOpOf{}   -> Nothing
  where
    annotFromVCells :: [VCell] -> Maybe LENatLangAnnot
    annotFromVCells = annotFromNLAcells . nlacellsFromLacs

    nlacellsFromLacs :: [VCell] -> [NLACell]
    nlacellsFromLacs = fmap vcell2NLAcell

annotFromNLAcells :: [NLACell] -> Maybe LENatLangAnnot
annotFromNLAcells = \case
  (mconcat . intersperseWithSpace -> MkNonParam concatted) -> 
        Just $ coerce concatted
  _ ->  Nothing
  where 
    spaceDelimtr = MkNonParam " "
    intersperseWithSpace = L.intersperse spaceDelimtr 

vcell2NLAcell :: VCell -> NLACell
vcell2NLAcell = \case
  TempVar tvar -> tvar2NLAcell tvar
  Pred nonparamtxt -> MkNonParam nonparamtxt

{- | 
Invariant: all NLAParams take one of the following two forms:
  *a var*
  *a var*'s
-}
tvar2NLAcell :: TemplateVar -> NLACell
tvar2NLAcell = \case
  EndsInApos _   -> MkParam "*a var*'s"
  IsNum _numtxt  -> MkParam "is *a var*"
  -- handling this case explicitly to remind ourselves tt we've handled it, and cos we might want to use "*a number*" instead
  MatchGVar _    -> MkParam "*a var*"
  {- ^
  From the LE handbook:
    An instance of a template is obtained from the template by replacing every parameter of the template by a list of words separated by spaces. 
    **There need not be any relationship between the words in a parameter and the words in the instance of the parameter. Different parameters in the same template can be replaced by different or identical instances.** (emphasis mine)

  Right now I'm making all of them "a var" or "a var's", as opposed to "a <orig text>" / "a <orig text>'s", so tt it'll be easy to remove duplicates
  -}