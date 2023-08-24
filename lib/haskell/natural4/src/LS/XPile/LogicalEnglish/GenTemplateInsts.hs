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
    LEHcR 
    . leRulePrintFromleRIntrmd 
    . leRIntrmdFromLAbsRule 
    $ labsrule

leRulePrintFromleRIntrmd :: LERuleIntrmd -> LERuleForPrint
leRulePrintFromleRIntrmd = fmap (bimap ticell2tmpltetxt temptxtify)

{-|
Generates LERuleIntrmd = BaseRule (AtomicBPropn TInstCell [TInstCell]) from LamAbsRule

Explaining the logic here
-------------------------
Shorthands:
  PreTTCell := ptc
  BaseRule := br
  TInstCell := tic

We know:

  type LERuleIntrmd = br (AtomicBPropn tic [tic])
  prettrule :: br (AtomicBPropn ptc [ptc])

  type PreTTAtomicP =  AtomicBPropn ptc [ptc]
  tiABPFromPrettABPacc :: NormdVars-> PreTTAtomicP -> (NormdVars, AtomicBPropn TInstCell [TInstCell])
                       := pvset -> ptap -> (pvset, ticap)

We want: 
  to go from 
      BaseRule (AtomicBPropn ptc [ptc]) = BaseRule PreTTAtomicP := br ptap
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
leRIntrmdFromLAbsRule :: LamAbsRule -> LERuleIntrmd
leRIntrmdFromLAbsRule larule = 
  let prettrule :: BaseRule (AtomicBPropn PreTTCell [PreTTCell]) = simplifyLAtomicP <$> larule
  in snd (mapAccumL tiABPFromPrettABPacc HS.empty prettrule)

-- type LEFactForPrint = AtomicBPropn LETemplateTxt LETemplateTxt
leFactPrintFromLabsFact :: LamAbsFact -> LEFactForPrint
leFactPrintFromLabsFact = bimap ticell2tmpltetxt temptxtify . leFactIntrmdFromLabsFact
  where 
    -- TODO: could prob do a bit more to make these helpers more readable and concise
    leFactIntrmdFromLabsFact :: LamAbsFact -> AtomicBPropn TInstCell [TInstCell]
    leFactIntrmdFromLabsFact LAFact{..} = 
      lefactIntrmdFromPrettABP . simplifyLAtomicP $ lfhead

    lefactIntrmdFromPrettABP :: PreTTAtomicP -> AtomicBPropn TInstCell [TInstCell]
    lefactIntrmdFromPrettABP prettabp = 
      let getTInstCells = snd
      in getTInstCells (tiABPFromPrettABPacc HS.empty prettabp)


-- type PreTTAtomicP =  AtomicBPropn PreTTCell [PreTTCell]
-- TODO: Look into how to do this without this much plumbing
tiABPFromPrettABPacc :: NormdVars-> PreTTAtomicP -> (NormdVars, AtomicBPropn TInstCell [TInstCell])
tiABPFromPrettABPacc pretvs = \case
  ABPatomic prettcells -> 
    let (pretvs', ticells) = ticellsFrPrettcellsAcc pretvs prettcells
    in (pretvs', ABPatomic ticells)
  ABPIsDiffFr v1 v2 -> 
    let (pretvs', v1') = makeTInstCell pretvs v1
        (pretvs'', v2') = makeTInstCell pretvs' v2
    in (pretvs'', ABPIsDiffFr v1' v2')
  ABPIsOpOf var opof varlst ->
    let (pretvs', var') = makeTInstCell pretvs var
        (pretvs'', ticells) = ticellsFrPrettcellsAcc pretvs' varlst
    in (pretvs'', ABPIsOpOf var' opof ticells)
  ABPIsOpSuchTt var ostt prettcells ->
    let (pretvs', var') = makeTInstCell pretvs var
        (pretvs'', ticells) = ticellsFrPrettcellsAcc pretvs' prettcells
    in (pretvs'', ABPIsOpSuchTt var' ostt ticells)


--- start by doing it the EASIEST possible way 
ticellsFrPrettcellsAcc :: NormdVars -> [PreTTCell] -> (NormdVars, [TInstCell])
ticellsFrPrettcellsAcc init prettcells = 
  mapAccumL makeTInstCell init prettcells

makeTInstCell :: NormdVars -> PreTTCell -> (NormdVars, TInstCell)
makeTInstCell normdvars = \case
  NotVar txt     -> (normdvars, NoPrefix txt)
  VarNonApos vtxt -> checkSeen normdvars vtxt vtxt
  VarApos origprefix -> checkSeen normdvars origprefix (origprefix <> "'s")
  where
    checkSeen :: NormdVars -> T.Text -> T.Text -> (NormdVars, TInstCell)
    checkSeen nvset vartxt finalvartxt = 
      let nvar =  MkNormVar vartxt
      in 
        if HS.member nvar nvset 
        then (nvset, NoPrefix vartxt)
        else 
          let nvset' = HS.insert nvar nvset
          in (nvset', PrefixWithA finalvartxt)

-------------

normalizeVar :: PreTTCell -> Maybe NormalizedVar
normalizeVar = \case
  NotVar _ -> Nothing
  VarApos prefix -> Just $ coerce prefix
  VarNonApos var -> Just $ coerce var

simplifyLAtomicP :: LamAbsAtomicP -> AtomicBPropn PreTTCell [PreTTCell]
simplifyLAtomicP = bimap tvar2prettcell (map simplifyLabscs)
    
simplifyLabscs :: LamAbsCell -> PreTTCell
simplifyLabscs = \case
  Pred txt    -> NotVar txt
  TempVar tv -> tvar2prettcell tv

-- IMPT TODO: just realized this is prob not correct --- prob want to retain a variant for the 'ends in apos' case in PreTTCell so tt can check if the prefix is in `seen` when traversing the rule!
tvar2prettcell :: TemplateVar -> PreTTCell
tvar2prettcell = \case
    MatchGVar vtxt  -> VarNonApos vtxt
    EndsInApos prefix -> VarApos prefix
    IsNum txt       -> NotVar txt
    OpOfVarArg txt  -> NotVar txt
                       -- ^ I think we never want to put an 'a' in front of the args for that, but it's worth checking again


temptxtify :: [TInstCell] -> LETemplateTxt
temptxtify ticells = 
  mconcat . map ticell2tmpltetxt $ intersperseWithSpace ticells
  where
    spaceDelimtr = NoPrefix " "
    intersperseWithSpace = L.intersperse spaceDelimtr 

ticell2tmpltetxt :: TInstCell -> LETemplateTxt
ticell2tmpltetxt = \case
  PrefixWithA txt -> coerce ("a " <> txt)
  NoPrefix    txt -> coerce txt

