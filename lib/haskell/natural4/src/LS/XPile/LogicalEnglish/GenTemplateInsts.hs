{-# OPTIONS_GHC -W #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields, RecordWildCards #-}
-- {-# LANGUAGE OverloadedRecordDot #-}
-- {-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.LogicalEnglish.GenTemplateInsts
where
-- TODO: Make export list


import Data.Text qualified as T
import Data.HashSet qualified as HS
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import qualified Data.List as L hiding (head, tail)
-- import Debug.Trace (trace)
import Data.Coerce (coerce)
-- import Data.String.Interpolate ( i )
import Control.Monad.State
import Data.Traversable
import Control.Monad.Identity (Identity)

import Data.Bifunctor

import LS.XPile.LogicalEnglish.Types



leHCFromLabsHC :: LamAbsHC -> LEhcPrint
leHCFromLabsHC = \case
  LAhcF labsfact -> 
    LEHcF . leFactPrintFromLabsFact $ labsfact
  LAhcR labsrule -> 
    LEHcR . textifyUnivMarkedRule . markUnivVarsInRule $ labsrule

textifyUnivMarkedRule :: RuleWithUnivsMarked -> LERuleForPrint
textifyUnivMarkedRule = fmap (bimap univst2tmpltetxt temptxtify)

{-|
Generates RuleWithUnivsMarked = BaseRule (AtomicBPropn UnivStatus [UnivStatus]) from LamAbsRule

Explaining the logic here
-------------------------
TODO: in the midst of changing names

Shorthands:
  LEhcCell := ptc
  BaseRule := br
  UnivStatus := tic

We know:

  type RuleWithUnivsMarked = br (AtomicBPropn tic [tic])
  prettrule :: br (AtomicBPropn ptc [ptc])

  type LEhcAtomicP =  AtomicBPropn ptc [ptc]
  markUnivVarsInAtomicPacc :: NormdVars-> LEhcAtomicP -> (NormdVars, AtomicBPropn UnivStatus [UnivStatus])
                       := pvset -> ptap -> (pvset, ticap)

We want: 
  to go from 
      BaseRule (AtomicBPropn ptc [ptc]) = BaseRule LEhcAtomicP := br ptap
  to 
      BaseRule (AtomicBPropn tic [tic]) := br ticap

We also know
  mapAccumL :: forall (t :: * -> *) s a b.
                Traversable t =>
                (s -> a -> (s, b)) -> s -> t a -> (s, t b)

Instantiating that with our desired concrete types, we get:     
     (pvset -> ptap -> (pvset, ticap)  )                   
     -> pvset -> br ptap -> (pvset, br ticap)
-}
markUnivVarsInRule :: LamAbsRule -> RuleWithUnivsMarked
markUnivVarsInRule larule = 
  let lerule :: BaseRule (AtomicBPropn LEhcCell [LEhcCell]) = simplifyLAtomicP <$> larule
  in snd (mapAccumL markUnivVarsInAtomicPacc HS.empty lerule)

-- type LEFactForPrint = AtomicBPropn LETemplateTxt LETemplateTxt
leFactPrintFromLabsFact :: LamAbsFact -> LEFactForPrint
leFactPrintFromLabsFact = (bimap univst2tmpltetxt temptxtify) . markUnivVarsInFact
  where 
    -- TODO: could prob do a bit more to make these helpers more readable and concise
    markUnivVarsInFact :: LamAbsFact -> AtomicBPropn UnivStatus [UnivStatus]
    markUnivVarsInFact LAFact{..} = 
      markUnivVarsInAtomicP . simplifyLAtomicP $ lfhead

    markUnivVarsInAtomicP :: LEhcAtomicP -> AtomicBPropn UnivStatus [UnivStatus]
    markUnivVarsInAtomicP leabp = 
      let getUnivStatuses = snd
      in getUnivStatuses (markUnivVarsInAtomicPacc HS.empty leabp)


-- TODO: Look into how to do this without this much plumbing
markUnivVarsInAtomicPacc :: NormdVars -> LEhcAtomicP -> (NormdVars, AtomicBPropn UnivStatus [UnivStatus])
markUnivVarsInAtomicPacc nvars = \case
  ABPatomic lecells -> 
    let (nvars', univStatuses) = markUnivVarsInLeCells nvars lecells
    in (nvars', ABPatomic univStatuses)
  ABPIsDiffFr v1 v2 -> 
    let (nvars', v1') = identifyUnivVar nvars v1
        (nvars'', v2') = identifyUnivVar nvars' v2
    in (nvars'', ABPIsDiffFr v1' v2')
  ABPIsOpOf var opof varlst ->
    let (nvars', var') = identifyUnivVar nvars var
        (nvars'', univStatuses) = markUnivVarsInLeCells nvars' varlst
    in (nvars'', ABPIsOpOf var' opof univStatuses)
  ABPIsOpSuchTt var ostt lecells ->
    let (nvars', var') = identifyUnivVar nvars var
        (nvars'', univStatuses) = markUnivVarsInLeCells nvars' lecells
    in (nvars'', ABPIsOpSuchTt var' ostt univStatuses)


--- start by doing it the EASIEST possible way 
markUnivVarsInLeCells :: NormdVars -> [LEhcCell] -> (NormdVars, [UnivStatus])
markUnivVarsInLeCells init lecells = 
  mapAccumL identifyUnivVar init lecells

identifyUnivVar :: NormdVars -> LEhcCell -> (NormdVars, UnivStatus)
identifyUnivVar normdvars = \case
  NotVar txt     -> (normdvars, NoPrefix txt)
  VarNonApos vtxt -> checkSeen normdvars vtxt vtxt
  VarApos origprefix -> checkSeen normdvars origprefix (origprefix <> "'s")
  where
    checkSeen :: NormdVars -> T.Text -> T.Text -> (NormdVars, UnivStatus)
    checkSeen nvset vartxt finalvartxt = 
      let nvar =  MkNormVar vartxt
      in 
        if HS.member nvar nvset 
        then (nvset, NoPrefix vartxt)
        else 
          let nvset' = HS.insert nvar nvset
          in (nvset', PrefixWithA finalvartxt)

-------------

normalizeVar :: LEhcCell -> Maybe NormalizedVar
normalizeVar = \case
  NotVar _ -> Nothing
  VarApos prefix -> Just $ coerce prefix
  VarNonApos var -> Just $ coerce var

simplifyLAtomicP :: LamAbsAtomicP -> AtomicBPropn LEhcCell [LEhcCell]
simplifyLAtomicP = bimap tvar2lecell (map simplifyLabscs)
    
simplifyLabscs :: LamAbsCell -> LEhcCell
simplifyLabscs = \case
  Pred txt    -> NotVar txt
  TempVar tv -> tvar2lecell tv

-- IMPT TODO: just realized this is prob not correct --- prob want to retain a variant for the 'ends in apos' case in LEhcCell so tt can check if the prefix is in `seen` when traversing the rule!
tvar2lecell :: TemplateVar -> LEhcCell
tvar2lecell = \case
    MatchGVar vtxt  -> VarNonApos vtxt
    EndsInApos prefix -> VarApos prefix
    IsNum txt       -> NotVar txt
    OpOfVarArg txt  -> NotVar txt
                       -- ^ I think we never want to put an 'a' in front of the args for that, but it's worth checking again


temptxtify :: [UnivStatus] -> LETemplateTxt
temptxtify univStatuses = 
  mconcat . map univst2tmpltetxt $ intersperseWithSpace univStatuses
  where
    spaceDelimtr = NoPrefix " "
    intersperseWithSpace = L.intersperse spaceDelimtr 

univst2tmpltetxt :: UnivStatus -> LETemplateTxt
univst2tmpltetxt = \case
  PrefixWithA txt -> coerce ("a " <> txt)
  NoPrefix    txt -> coerce txt

