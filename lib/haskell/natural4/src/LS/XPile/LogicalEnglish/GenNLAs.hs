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
    nlasFromLamAbsHC
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


nlasFromLamAbsHC :: LamAbsHC -> HS.HashSet LENatLangAnnot
nlasFromLamAbsHC = \case
  LAhcF labsfact ->
    case (nlaFromLamAbsFact labsfact) of
      Nothing -> HS.empty
      Just nla -> HS.singleton nla
  LAhcR labsrule ->
    nlasFromLamAbsRule labsrule

-- TODO: When have more time, write smtg tt checks if it is indeed in fixed lib, and add it if not.
nlaFromLamAbsFact :: LamAbsFact -> Maybe LENatLangAnnot
nlaFromLamAbsFact LAFact{..} = nlaLoneFromLAbsAtomicP lfhead

nlasFromLamAbsRule :: LamAbsRule -> HS.HashSet LENatLangAnnot
nlasFromLamAbsRule MkBaseRule{..} =
  let bodyNLAs = nlasFromBody rbody
  in case (nlaLoneFromLAbsAtomicP rhead) of
    Nothing -> bodyNLAs
    Just headNLA -> HS.insert headNLA bodyNLAs

nlasFromBody :: BoolPropn LamAbsAtomicP -> HS.HashSet LENatLangAnnot
nlasFromBody lamabsBP =
  let lstNLAs = fmap nlaLoneFromLAbsAtomicP lamabsBP
  in HS.fromList . catMaybes . toList $ lstNLAs

-- TODO: Check if this really does conform to the spec --- went a bit fast here
nlaLoneFromLAbsAtomicP :: LamAbsAtomicP -> Maybe LENatLangAnnot
nlaLoneFromLAbsAtomicP =  \case
  ABPatomic labscells -> annotFromLabscells labscells
  ABPIsOpSuchTt _ _ labscells -> annotFromLabscells labscells
  ABPIsDiffFr{} -> Nothing
  ABPIsOpOf{}   -> Nothing
  where
    annotFromLabscells :: [LamAbsCell] -> Maybe LENatLangAnnot
    annotFromLabscells = annotFromNLAcells . nlacellsFromLacs

    nlacellsFromLacs :: [LamAbsCell] -> [NLACell]
    nlacellsFromLacs = fmap labscell2NLAcell

annotFromNLAcells :: [NLACell] -> Maybe LENatLangAnnot
annotFromNLAcells = \case
  (mconcat . intersperseWithSpace -> MkNonParam concatted) -> 
        Just $ coerce concatted
  _ ->  Nothing
  where 
    spaceDelimtr = MkNonParam " "
    intersperseWithSpace = L.intersperse spaceDelimtr 

labscell2NLAcell :: LamAbsCell -> NLACell
labscell2NLAcell = \case
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