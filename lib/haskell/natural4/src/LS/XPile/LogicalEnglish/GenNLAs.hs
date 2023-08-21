{-# OPTIONS_GHC -W #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields, RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.LogicalEnglish.GenNLAs where
  
-- TODO: Make export list

import Data.Text qualified as T
import Data.HashSet qualified as HS
import Data.Foldable (toList)
import Data.Maybe (catMaybes)

import Debug.Trace (trace)
import Data.Coerce (coerce)
import Data.String.Interpolate ( i )

import LS.XPile.LogicalEnglish.Types

import LS.XPile.LogicalEnglish.UtilsLEReplDev 

-- nlasFromLamAbsFact 
-- for now let's assume that the NLAs for facts are already in the fixed lib
-- TODO: When have more time, write smtg tt checks if it is indeed in fixed lib, and add it if not.

nlasFromLamAbsRule :: LamAbsRule -> HS.HashSet LENatLangAnnot
nlasFromLamAbsRule LARule{..} = 
  let bodyNLAs = nlasFromBody body
  in case (nlaLoneFromLAbsAtomicP head) of
    Nothing -> bodyNLAs
    Just headNLA -> HS.insert headNLA bodyNLAs

nlasFromBody :: BoolPropn LamAbsAtomicP -> HS.HashSet LENatLangAnnot
nlasFromBody lamabsBP = 
  let lstNLAs = fmap nlaLoneFromLAbsAtomicP lamabsBP
  in HS.fromList . catMaybes . toList $ lstNLAs

-- TODO: Check if this really does conform to the spec --- went a bit fast here
nlaLoneFromLAbsAtomicP :: LamAbsAtomicP -> Maybe LENatLangAnnot
nlaLoneFromLAbsAtomicP =  \case
  ABPatomic labscells -> nlacs2annot labscells
  ABPIsOpSuchTt _ _ labscells -> nlacs2annot labscells
  ABPIsDiffFr{} -> Nothing 
  ABPIsOpOf{}   -> Nothing   
  where 
    nlacs2annot :: [LamAbsCell] -> Maybe LENatLangAnnot
    nlacs2annot = nlacellseq2annot . lacs2nlacs
        
    lacs2nlacs :: [LamAbsCell] -> [NLACell]
    lacs2nlacs = fmap labscell2NLAcell


nlacellseq2annot :: [NLACell] -> Maybe LENatLangAnnot
nlacellseq2annot = \case  
  (mconcat -> MkNonParam concatted) -> Just $ coerce concatted
  _ -> Nothing

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
  IsNum numtxt -> MkNonParam numtxt
  OpOfVarArg arg -> MkNonParam arg
  EndsInApos _ -> MkParam "*a var*'s"
  _            -> MkParam "*a var*"
  {- ^
  From the LE handbook:
    An instance of a template is obtained from the template by replacing every parameter of the template by a list of words separated by spaces. 
    **There need not be any relationship between the words in a parameter and the words in the instance of the parameter. Different parameters in the same template can be replaced by different or identical instances.** (emphasis mine)
  -}