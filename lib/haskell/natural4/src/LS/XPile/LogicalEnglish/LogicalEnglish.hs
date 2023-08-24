{-# OPTIONS_GHC -W #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

{-# LANGUAGE DataKinds, KindSignatures, AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-|

We're trying to work with the rules / AST instead, 
in part because we don't need most of the stuff Interpreter.hs provides,
and in part to avoid breaking if the spec / API for Interpreter.hs changes.
After all, the design intentions for this short-term LE transpiler aren't the same as those for the analzyer (which is part of what will hopefully become an effort in longer-term, more principled language development).
-}

module LS.XPile.LogicalEnglish.LogicalEnglish (toLE)  where

import LS.PrettyPrinter
    ( myrender, vvsep, (</>), tildes, (<//>), srchs )
import Prettyprinter
    ( vsep, viaShow, hsep, emptyDoc, (<+>), Pretty(pretty), Doc, indent, line )
import Text.Pretty.Simple   ( pShowNoColor )
import Data.Text qualified as T
import Data.Bifunctor       ( first )
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.HashMap.Strict qualified as Map
import Control.Monad.Identity ( Identity )
import Data.String (IsString)

import qualified AnyAll as AA
import LS.Types qualified as L4
import LS.Types (RelationalPredicate(..), RPRel(..), MTExpr, BoolStructR(..), BoolStructT)
import LS.Rule qualified as L4 (Rule(..))
import LS.XPile.LogicalEnglish.Types
import LS.XPile.LogicalEnglish.ValidateL4Input
      (L4Rules, ValidHornls, Unvalidated,
      check, refine, loadRawL4AsUnvalid)
import LS.XPile.LogicalEnglish.SimplifyL4 (simplifyL4ruleish) -- TODO: Add import list
import LS.XPile.LogicalEnglish.LamAbstract (lamAbstract)
import LS.XPile.LogicalEnglish.GenNLAs (nlasFromLamAbsHC)
import LS.XPile.LogicalEnglish.GenLEHCs (leHCFromLabsHC)


import LS.XPile.LogicalEnglish.UtilsLEReplDev -- for prototyping

{- TODO: After we get a v simple end-to-end prototype out, 
we'll add functionality for checking the L4 input rules __upfront__ for things like whether it's using unsupported keywords, whether the input is well-formed by the lights of the translation rules, and so forth. 
(This should be done with Monad.Validate or Data.Validation -- XPileLog isn't as good a fit for this.)
The thought is that if the upfront checks fail, we'll be able to exit gracefully and provide more helpful diagnostics / error messages. 

But for now, we will help ourselves, undeservedly, to the assumption that the L4 input is wellformed. -}


{-------------------------------------------------------------------------------
   L4 rules -> SimpleL4HCs -> LamAbsRules
-------------------------------------------------------------------------------}

-- | TODO: Work on implementing this and adding the Monad Validate or Data.Validation stuff instead of Maybe (i.e., rly doing checks upfront and carrying along the error messages and potential warnings) after getting enoguh of the main transpiler out
checkAndRefine :: L4Rules Unvalidated -> Maybe (L4Rules ValidHornls)
checkAndRefine rawrules = do
  validatedL4rules <- check rawrules
  return $ refine validatedL4rules


allLamAbsHCs :: [L4.Rule] -> [LamAbsHC] 
allLamAbsHCs = map (lamAbstract . simplifyL4ruleish)

------------ 

-- | Generate LE Nat Lang Annotations from LamAbsHCs  
allNLAs :: [LamAbsHC] -> HS.HashSet LENatLangAnnot
allNLAs lamabsHCs = HS.unions $ map nlasFromLamAbsHC lamabsHCs

-- | Generate LE HCs from LamAbsHCs
allLEhcs :: [LamAbsHC] -> [LEhcPrint]
allLEhcs = map leHCFromLabsHC

{-------------------------------------------------------------------------------
   Orchestrating and pretty printing
-------------------------------------------------------------------------------}

toLE :: [L4.Rule] -> String
toLE = const "some output"

{-
note
------

Key types from codebase:
  type ParamText = NonEmpty TypedMulti
  type TypedMulti = (NonEmpty MTExpr, Maybe TypeSig)

  data MTExpr = MTT Text.Text -- ^ Text string
              | MTI Integer   -- ^ Integer
              | MTF Float     -- ^ Float
              | MTB Bool      -- ^ Boolean
            deriving (Eq, Ord, Show, Generic, ToJSON)

    -- | the parser returns a list of MTExpr, to be parsed further at some later point
  type MultiTerm = [MTExpr] --- | apple | banana | 100 | $100 | 1 Feb 1970

  given    :: Maybe ParamText
  aka the stuff in the given field is a non-mt list of (NonEmpty MTExpr, Maybe TypeSig)

-}
