{-# OPTIONS_GHC -W #-}

-- {-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields, OverloadedRecordDot #-}
-- {-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DerivingStrategies #-}

{-# LANGUAGE DataKinds, KindSignatures, AllowAmbiguousTypes #-}
{-|

We're trying to work with the rules / AST instead, 
in part because we don't need most of the stuff Interpreter.hs provides,
and in part to avoid breaking if the spec / API for Interpreter.hs changes.
After all, the design intentions for this short-term LE transpiler aren't the same as those for the analzyer (which is part of what will hopefully become an effort in longer-term, more principled language development).
-}

module LS.XPile.LogicalEnglish.LogicalEnglish (toLE)  where

-- import Text.Pretty.Simple   ( pShowNoColor )
import Data.Text qualified as T
import Data.HashSet qualified as HS
-- import Data.List (sort)
-- import Data.Maybe (fromMaybe, listToMaybe)
import Control.Monad.Validate (runValidate)
import Data.Coerce (coerce)

-- import Optics
import Prettyprinter
  ( Doc,
    Pretty (pretty))
import LS.PrettyPrinter
    ( myrender)
import LS.XPile.LogicalEnglish.Pretty(LEProg(..), libTemplatesTxt)

-- import LS.Types qualified as L4
-- import LS.Types (RelationalPredicate(..), RPRel(..), MTExpr, BoolStructR, BoolStructT)
import LS.Rule qualified as L4 (Rule(..))
import LS.XPile.LogicalEnglish.Types
import LS.XPile.LogicalEnglish.ValidateL4Input
      (
        isHornlike
      --   L4Rules, ValidHornls, Unvalidated,
      -- check, refine, loadRawL4AsUnvalid, 
      )
import LS.XPile.LogicalEnglish.SimplifyL4 (SimpL4(..), SimL4Error(..), simplifyL4rule)
import LS.XPile.LogicalEnglish.IdVars (idVarsInHC)
import LS.XPile.LogicalEnglish.GenNLAs 
    ( nlasFromVarsHC
    , NLATxt(..)
    , NLA 
    , getNLAtxt
    , RegexTrav
    , FilterResult(..)
    , removeInternallySubsumed
    , regextravifyNLASection
    , removeRegexMatches
    , removeDisprefdInEquivUpToVarNames
    )

import LS.XPile.LogicalEnglish.GenLEHCs (leHCFromVarsHC)

-- import LS.XPile.LogicalEnglish.UtilsLEReplDev -- for prototyping

{- 

TODO: Add property based tests
  EG: 
    * If you add a new var anywhere in a randomly generated LamAbs HC, the new LE HC should have an 'a' in front of that var
    * If you take a rand generated LamAbs HC with a var v and add another occurrence of `v` before the old one, the ordering of variables with an `a` prefix in the corresponding new LE HC should differ from that in the old LE HC in the appropriate way (this should be made a bit more precise depending on what is easy to implement)
    * Take a randomly generated LamABs HC with vars that potentially have multiple occurrences and generate the LE HC from it. For every var in the HC, the 'a' prefix should only appear once.
    * There should be as many NLAs as leaves in the HC (modulo lib NLAs)


TODO: Think abt doing more on the pre-validation front, e.g. checking the L4 input rules __upfront__ for things like whether it's using unsupported keywords, whether the input is well-formed by the lights of the translation rules, and so forth. 
The thought is that if the upfront checks fail, we'll be able to exit gracefully and provide more helpful diagnostics / error messages. 
But it might be enough to just do what we are currently doing with the validation in the simplifyL4 step

But for now, we will help ourselves, undeservedly, to the assumption that the L4 input is wellformed. 


TODO: Add some prevalidation stuff in the future, if time permits: 
  * e.g., checking for typos (e.g., if there's something in a cell tt's very similar to but not exactly the same as a given var)

-}


{-------------------------------------------------------------------------------
   Orchestrating and pretty printing
-------------------------------------------------------------------------------}

toLE :: [L4.Rule] -> String
toLE l4rules =
  case runAndValidate . simplifyL4rules . filter isHornlike $ l4rules of
    Left errors -> errs2str errors
    Right hcs   -> xpileSimplifiedL4hcs hcs
  where
    errs2str = pure "ERRORS FOUND:\n" <> T.unpack . T.intercalate "\n" . coerce . HS.toList
    runAndValidate = runValidate . runSimpL4
{- ^ TODO: think abt whether to do more on the pre-simplifyL4rules front
-}

-- | Generate LE Nat Lang Annotations from VarsHCs  
getNLATxtResults :: Foldable g => g VarsHC -> FilterResult [NLATxt]
getNLATxtResults =  (fmap . fmap $ getNLAtxt) . removeSubsumedOrDisprefed . foldMap nlasFromVarsHC
  where
    removeSubsumedOrDisprefed :: HS.HashSet NLA -> FilterResult [NLA]
    removeSubsumedOrDisprefed = removeDisprefdInEquivUpToVarNames . removeSubsumedByLibTemplates . removeInternallySubsumed 

    libTemplatesRegTravs :: [RegexTrav]
    libTemplatesRegTravs = regextravifyNLASection libTemplatesTxt

    removeSubsumedByLibTemplates :: Foldable f => f NLA -> HS.HashSet NLA
    removeSubsumedByLibTemplates = removeRegexMatches libTemplatesRegTravs  

simplifyL4rules :: [L4.Rule] -> SimpL4 [SimpleL4HC]
simplifyL4rules = sequenceA . concatMap simplifyL4rule

xpileSimplifiedL4hcs :: [SimpleL4HC] -> String
xpileSimplifiedL4hcs simpL4HCs =
  let hcsVarsMarked :: [VarsHC] = map idVarsInHC simpL4HCs
      nlatresults               = getNLATxtResults hcsVarsMarked
      lehcs                     = map leHCFromVarsHC hcsVarsMarked
      leProgam                  = MkLEProg { keptnlats =  nlatresults.kept, subsumednlats = nlatresults.subsumed
                                           , leHCs = lehcs 
                                           , commentSym = "%"}
  in doc2str . pretty $ leProgam


doc2str :: Doc ann -> String
doc2str = T.unpack . myrender