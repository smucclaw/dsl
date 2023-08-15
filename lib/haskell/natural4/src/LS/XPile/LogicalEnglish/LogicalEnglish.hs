{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import LS.Rule qualified as L4 (Rule(..))
import LS.XPile.LogicalEnglish.Types
import LS.XPile.LogicalEnglish.Common (
    L4Prog,
    (|>)
    )

import LS.XPile.LogicalEnglish.UtilsLEReplDev -- for prototyping

{- TODO: After we get a v simple end-to-end prototype out, 
we'll add functionality for checking the L4 input rules __upfront__ for things like whether it's using unsupported keywords, whether the input is well-formed by the lights of the translation rules, and so forth. 
(This should be done with Monad.Validate or Data.Validation -- XPileLog isn't as good a fit for this.)
The thought is that if the upfront checks fail, we'll be able to exit gracefully and provide more helpful diagnostics / error messages. 

But for now, we will help ourselves, undeservedly, to the assumption that the L4 input is wellformed. -}


{-------------------------------------------------------------------------------
   L4 rules -> SimpleL4HCs -> RuleIRs
-------------------------------------------------------------------------------}


simplifyL4rule :: L4.Rule -> SimpleL4HC
simplifyL4rule = undefined



makeIR :: SimpleL4HC -> RuleIR
makeIR = undefined


----- helper funcs -----------------

gvarsFromL4Rule :: L4.Rule -> GVarSet
gvarsFromL4Rule = undefined



{-------------------------------------------------------------------------------
   RuleIRs -> LE Nat Lang Annotations 
-------------------------------------------------------------------------------}


-- | Generate natural language annotations from a RuleIR
nlasFromRuleIR :: RuleIR -> HS.HashSet LENatLangAnnotatn
nlasFromRuleIR = undefined
{- for each base template (bt) in the RuleIR, across the head and body,
  we take its sequence of original variable names <"v1", "v2", ..., "vn">,
  make a new sequence <"*a v1", "a v2", ..., "a vn">,
 and then instantiate the bt with that new sequence. 
-}

allNLAs :: [RuleIR] -> HS.HashSet LENatLangAnnotatn
allNLAs ruleIRs = HS.unions $ map nlasFromRuleIR ruleIRs


{-------------------------------------------------------------------------------
    RuleIRs -> LE rules
-------------------------------------------------------------------------------}


leruleFromRuleIR :: RuleIR -> LERule
leruleFromRuleIR = undefined
{- `ruleLocalsIn` in Interpreter.hs may be worth looking at, though I suspect it'd be cleaner to do this with optics 
-}
allLERules :: [RuleIR] -> [LERule]
allLERules = map leruleFromRuleIR 

{-------------------------------------------------------------------------------
   Orchestrating and pretty printing
-------------------------------------------------------------------------------}

toLE :: L4Prog -> String
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