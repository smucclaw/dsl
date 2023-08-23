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

import LS.XPile.LogicalEnglish.Types



leHCFromLabsHC :: LamAbsHC -> LEhcPrint
leHCFromLabsHC = \case
  LAhcF labsfact -> undefined
  LAhcR labsrule -> undefined


leFactFromLabsFact :: LamAbsFact -> LEFactForPrint
leFactFromLabsFact = undefined


--TODO: Think abt whether to make this an inner fn in leFactFromLabsFact 
leFactIntrmdFromLabsFact :: LamAbsFact -> LEFactIntrmd
leFactIntrmdFromLabsFact LAFact{..} = undefined


--- start by doing it the EASIEST possible way 
ticellsFromLabscells :: [LamAbsCell] -> [TInstCell]
ticellsFromLabscells labscells = 
  let getTInstCells = snd
      preTIcells = map simplifyLabscell labscells
  in getTInstCells (mapAccumL makeTInstCell HS.empty preTIcells)

makeTInstCell :: PreTIVSet -> PreTICell -> (PreTIVSet, TInstCell)
makeTInstCell ptivars = \case
  NotTIVar txt     -> (ptivars, NoPrefix txt)
  orig@(TIVar txt) -> checkSeen ptivars orig txt
  where
    checkSeen :: PreTIVSet -> PreTICell -> T.Text -> (PreTIVSet, TInstCell)
    checkSeen tvars origPtic vartxt =  
      if HS.member origPtic tvars 
      then (tvars, NoPrefix vartxt)
      else 
        let tvars' = HS.insert origPtic tvars
        in (tvars', PrefixWithA vartxt)


{-
data LamAbsHC = LAhcF LamAbsFact | LAhcR LamAbsRule
      deriving stock (Eq, Ord, Show)

data LamAbsFact = LAFact { head      :: LamAbsAtomicP }
      deriving stock (Eq, Ord, Show)
data LamAbsRule = LARule { head      :: LamAbsAtomicP
                         , body      :: BoolPropn LamAbsAtomicP }
                         
type LamAbsAtomicP = AtomicBPropn TemplateVar [LamAbsCell]

data LamAbsCell = TempVar TemplateVar
                | Pred    !T.Text

newtype LEFact a = LEFact { fhead :: a }
type LEFactIntrmd = LEFact (AtomicBPropn TInstCell [TInstCell])
type LEFactForPrint = LEFact LETemplateInstance

data LERule a = 
    LERule { rhead :: AtomicBPropn TInstCell a
           , rbody :: BoolPropn (AtomicBPropn TInstCell a)
           }



data TInstCell = PrefixWithA !OrigVarName
               | NoPrefix !T.Text

-}



leRulePrintFromleRuleIntrmd :: LERuleIntrmd -> LERuleForPrint
leRulePrintFromleRuleIntrmd = undefined
--TODO: mconcat, intersperse. refactor helper function into a common util module

leRuleIntrmdFromLAbsRule :: LamAbsRule -> LERuleIntrmd
leRuleIntrmdFromLAbsRule = undefined

-------------


simplifyLabscell :: LamAbsCell -> PreTICell
simplifyLabscell = \case
  Pred txt    -> NotTIVar txt
  TempVar tv -> case tv of
    MatchGVar vtxt  -> TIVar vtxt
    EndsInApos vtxt -> TIVar (vtxt <> "'s")
    IsNum txt       -> NotTIVar txt
    OpOfVarArg txt  -> NotTIVar txt
                       -- ^ I think we never want to put an 'a' in front of the args for that, but it's worth checking again
