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
  tiABPFromPrettABPacc :: PretVSet-> PreTTAtomicP -> (PretVSet, AtomicBPropn TInstCell [TInstCell])
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
tiABPFromPrettABPacc :: PretVSet-> PreTTAtomicP -> (PretVSet, AtomicBPropn TInstCell [TInstCell])
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
ticellsFrPrettcellsAcc :: PretVSet -> [PreTTCell] -> (PretVSet, [TInstCell])
ticellsFrPrettcellsAcc init prettcells = 
  mapAccumL makeTInstCell init prettcells

makeTInstCell :: PretVSet -> PreTTCell -> (PretVSet, TInstCell)
makeTInstCell prettvars = \case
  NotTTVar txt     -> (prettvars, NoPrefix txt)
  orig@(TTVar txt) -> checkSeen prettvars orig txt
  where
    checkSeen :: PretVSet -> PreTTCell -> T.Text -> (PretVSet, TInstCell)
    checkSeen tvars origPtic vartxt =  
      if HS.member origPtic tvars 
      then (tvars, NoPrefix vartxt)
      else 
        let tvars' = HS.insert origPtic tvars
        in (tvars', PrefixWithA vartxt)

-------------



simplifyLAtomicP :: LamAbsAtomicP -> AtomicBPropn PreTTCell [PreTTCell]
simplifyLAtomicP = bimap tvar2prettcell (map simplifyLabscs)
    
simplifyLabscs :: LamAbsCell -> PreTTCell
simplifyLabscs = \case
  Pred txt    -> NotTTVar txt
  TempVar tv -> tvar2prettcell tv

-- IMPT TODO: just realized this is prob not correct --- prob want to retain a variant for the 'ends in apos' case in PreTTCell so tt can check if the prefix is in `seen` when traversing the rule!
tvar2prettcell :: TemplateVar -> PreTTCell
tvar2prettcell = \case
    MatchGVar vtxt  -> TTVar vtxt
    EndsInApos vtxt -> TTVar (vtxt <> "'s")
    IsNum txt       -> NotTTVar txt
    OpOfVarArg txt  -> NotTTVar txt
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

