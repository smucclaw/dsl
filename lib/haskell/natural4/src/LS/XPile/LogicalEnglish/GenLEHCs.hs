{-# OPTIONS_GHC -W #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields, RecordWildCards #-}
-- {-# LANGUAGE OverloadedRecordDot #-}
-- {-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE DerivingStrategies #-}

module LS.XPile.LogicalEnglish.GenLEHCs (leHCFromVarsHC) where

import Data.Text qualified as T
import Data.HashSet qualified as HS
-- import Data.Foldable (toList)
-- import Debug.Trace (trace)
import Data.Coerce (coerce)
-- import Data.String.Interpolate ( i )
import Data.Traversable

import LS.XPile.LogicalEnglish.Types
  ( -- L4-related types
      AtomicBPropn(..)
    
    -- Intermediate representation types, prisms, and consts
    , TemplateVar(..)
    , aposSuffix
    , VarsHC(VhcF, VhcR)
    , VarsFact(..)
    , BaseRule(..)
    , VarsRule
    , AtomicPWithVars
    , VCell(..)
  
    -- LE-related types
    , LEhcCell(..)
    , LEVar(..)
    , NormdVars
    , NormalizedVar(..)
    , LEhcAtomicP
    , LETemplateTxt(..)
    , UnivStatus(..)
    , RuleWithUnivsMarked
    , LERuleForPrint
    , LEhcPrint(..) 
  )


leHCFromVarsHC :: VarsHC -> LEhcPrint
leHCFromVarsHC = \case
  VhcF vfact ->
    LEHcF . leFactPrintFromVFact $ vfact
  VhcR vrule ->
    LEHcR . textifyUnivMarkedRule . markUnivVarsInRule $ vrule

-- type LEFactForPrint = AtomicBPropn LETemplateTxt 
leFactPrintFromVFact :: VarsFact ->  AtomicBPropn LETemplateTxt 
leFactPrintFromVFact = fmap univst2tmpltetxt . markUnivVarsInFact

markUnivVarsInFact :: VarsFact -> AtomicBPropn UnivStatus
markUnivVarsInFact VFact{..} =
  markUnivVarsInAtomicP . simplifyVAtomicP $ varsfhead
  where
    markUnivVarsInAtomicP :: LEhcAtomicP -> AtomicBPropn UnivStatus
    markUnivVarsInAtomicP leabp =
      let getUnivStatuses = snd
      in getUnivStatuses (markUnivVarsInAtomicPacc HS.empty leabp)


textifyUnivMarkedRule :: RuleWithUnivsMarked -> LERuleForPrint
textifyUnivMarkedRule = fmap . fmap $ univst2tmpltetxt

{-|
Generates RuleWithUnivsMarked := BaseRule (AtomicBPropn UnivStatus) from VarsRule

Explaining the logic here
-------------------------
At a high level:
  We're doing a traverse with an accumulator, 
  a traverse that exploits how BaseRule, which comprises the head and the boolean-proposition-tree body, is parametrized over the atomic (boolean) proposition type.

In more detail:

  Shorthands:
    LEhcCell := lec
    BaseRule := br
    UnivStatus := univst

  We know:

    type RuleWithUnivsMarked = br (AtomicBPropn univst [univst])
    prettrule :: br (AtomicBPropn lec [lec])

    type LEhcAtomicP =  AtomicBPropn lec [lec]
    markUnivVarsInAtomicPacc :: NormdVars-> LEhcAtomicP -> (NormdVars, AtomicBPropn UnivStatus [UnivStatus])
                                := nvars -> leap -> (nvars, uvsp)

  We want: 
    to go from 
        BaseRule (AtomicBPropn lec [lec]) = BaseRule LEhcAtomicP := br leap
    to 
        BaseRule (AtomicBPropn univst [univst]) := br uvsp

  We also know
    mapAccumL :: forall (t :: * -> *) s a b.
                  Traversable t =>
                  (s -> a -> (s, b)) -> s -> t a -> (s, t b)

  Instantiating that with our desired concrete types, we get:     
      (nvars -> leap -> (nvars, uvsp)  )                   
      -> nvars -> br leap -> (nvars, br uvsp)

  It's also worth studying `markUnivVarsInAtomicPacc` and `markUnivVarsInLeCells`
  since those functions are what implement the lower-level mechanics of threading 
  the accumulator argument through
-}
markUnivVarsInRule :: VarsRule -> RuleWithUnivsMarked
markUnivVarsInRule larule =
  let lerule :: BaseRule LEhcAtomicP = simplifyVAtomicP <$> larule
  in snd (mapAccumL markUnivVarsInAtomicPacc HS.empty lerule)

{- TODO: I've thought of a way to do this with less plumbing using optics, 
just not sure if streamlining the plumbing is worth the potential increased complexity for others -}
markUnivVarsInAtomicPacc :: NormdVars -> LEhcAtomicP -> (NormdVars, AtomicBPropn UnivStatus)
markUnivVarsInAtomicPacc nvars = \case
  ABPatomic lecells ->
    let (nvars', univStatuses) = markUnivVarsInLeCells nvars lecells
    in (nvars', ABPatomic univStatuses)

  ABPBaseIs lefts rights -> 
    let (nvars', leftsWithUnivStats) = markUnivVarsInLeCells nvars lefts
        (nvars'', rightsWithUnivStats) = markUnivVarsInLeCells nvars' rights
    in (nvars'', ABPBaseIs leftsWithUnivStats rightsWithUnivStats)

  ABPIsIn t1 t2     -> isSmtg ABPIsIn t1 t2
  ABPIsDiffFr t1 t2 -> isSmtg ABPIsDiffFr t1 t2

  ABPIsOpOf term opof termlst ->
    let (nvars', term') = identifyUnivVar nvars term
        (nvars'', univStatuses) = markUnivVarsInLeCells nvars' termlst
    in (nvars'', ABPIsOpOf term' opof univStatuses)

  ABPIsOpSuchTt term ostt lecells ->
    let (nvars', term') = identifyUnivVar nvars term
        (nvars'', univStatuses) = markUnivVarsInLeCells nvars' lecells
    in (nvars'', ABPIsOpSuchTt term' ostt univStatuses)

  where
    isSmtg op t1 t2 = 
      let (nvars', t1') = identifyUnivVar nvars t1
          (nvars'', t2') = identifyUnivVar nvars' t2
      in (nvars'', op t1' t2')

--- start by doing it the EASIEST possible way 
markUnivVarsInLeCells :: NormdVars -> [LEhcCell] -> (NormdVars, [UnivStatus])
markUnivVarsInLeCells init lecells =
  mapAccumL identifyUnivVar init lecells

identifyUnivVar :: NormdVars -> LEhcCell -> (NormdVars, UnivStatus)
identifyUnivVar normdvars = \case
  NotVar txt   -> (normdvars, NoPrefix txt)
  VarCell var  -> checkSeen normdvars var
  where
    checkSeen :: NormdVars -> LEVar -> (NormdVars, UnivStatus)
    checkSeen nvset levar =
      let nvar   =  normalizeVar levar
          vartxt = printlev levar
      in
        if HS.member nvar nvset
        then (nvset, NoPrefix vartxt)
        else
          let nvset' = HS.insert nvar nvset
          in (nvset', PrefixWithA vartxt)

------------- helpers

normalizeVar :: LEVar -> NormalizedVar
normalizeVar = \case
  VarNonApos vartxt -> coerce vartxt
  VarApos prefix -> coerce prefix

simplifyVAtomicP :: AtomicPWithVars -> LEhcAtomicP
simplifyVAtomicP = fmap simplifyVCells

simplifyVCells :: VCell -> LEhcCell
simplifyVCells = \case
  PredOrNonAposAtom txt -> NotVar txt
  AposAtom prefix       -> NotVar prefix <> aposSuffix
  TempVar tv            -> tvar2lecell tv

tvar2lecell :: TemplateVar -> LEhcCell
tvar2lecell = \case
    MatchGVar vtxt    -> VarCell $ VarNonApos vtxt
    EndsInApos prefix -> VarCell $ VarApos prefix
    IsNum txt         -> NotVar ("is " <> txt)

-- | Prints the intended text for a LEVar
printlev :: LEVar -> T.Text
printlev = \case
  VarApos origprefix -> origprefix <> "'s"
  VarNonApos vartxt  -> vartxt

-- | Converts a UnivStatus to a LETemplateTxt in the obvious way -- basically materializing the UnivStatus tag
univst2tmpltetxt :: UnivStatus -> LETemplateTxt
univst2tmpltetxt = \case
  PrefixWithA txt -> coerce ("a " <> txt)
  NoPrefix    txt -> coerce txt
