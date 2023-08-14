{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}


{-|

We're trying to work with the rules / AST instead, 
in part because we don't need most of the stuff Interpreter.hs provides,
and in part to avoid breaking if the spec / API for Interpreter.hs changes.
After all, the design intentions for this short-term LE transpiler aren't the same as those for the analzyer (which is part of what will hopefully become an effort in longer-term, more principled language development).
-}

module LS.XPile.LogicalEnglish.LogicalEnglish (toLE) where


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

import LS.Rule (Rule(..))
import LS.XPile.LogicalEnglish.Common (
    L4Prog,
    (|>)
    )

import LS.XPile.LogicalEnglish.UtilsLEReplDev -- for prototyping

{- 
TODO: After we get a v simple end-to-end prototype out, 
we'll add functionality for checking the L4 input rules __upfront__ for things like whether it's using unsupported keywords, whether the input is well-formed by the lights of the translation rules, and so forth. 
(This should be done with Monad.Validate or Data.Validation -- XPileLog isn't as good a fit for this.)
The thought is that if the upfront checks fail, we'll be able to exit gracefully and provide more helpful diagnostics / error messages. 

But for now, we will help ourselves, undeservedly, to the assumption that the L4 input is wellformed.
-}


data L4Var = NoApos T.Text 
           | WithApos T.Text
           deriving (Ord, Read, Show, Eq, Generic, Hashable)
-- TODO: this is a quick first pass; need to think more about how to model L4 var for our purposes
-- instance Hashable L4Var


type LEtemplate = T.Text -- aka 'natural language annotation'. 
-- TODO: Should this be a newtype?
type VarsFrRuleGiven = HS.HashSet L4Var

data IntermedRule
{- TODO: This will be a BoolStruct-esque intermediate representation that contains the HC, but not the given
-- a tree of T.Texts with constructors like All, Any, Not 
where each of the T.Text values is a LE condition that has an "a" for every first occurrence of a variable
and where we've done the main syntactic transformations we're interested in
i.e., an IntermedRule is the result of doing all the syntactic transformation and processing, so that pretty printing this to LE will just amount to pretty printing a tree of texts, 
using the structure of the tree to guide indentation

Supposing we already have the LE templates for the L4 program,
processing a L4 rule thus amounts to:

  L4 Rule -> Intermediate-Boolean-proposition-tree -> pretty printed LE string

TODO: Look into whether we want to re-use the L4 BoolStruct for this, or have our own newtype wrapper
-}

-------- L4 Program -> LE templates 
l4toLEtemplates :: L4Prog -> HS.HashSet LEtemplate
l4toLEtemplates = undefined

-- `ruleLocalsIn` in Interpreter.hs may be worth looking at, though I suspect it'd be cleaner to do this with optics 
-- TODO: think -- how best to model variable, given that we also want to be able to hash it?
varsFromHCgiven :: Rule -> HS.HashSet L4Var
varsFromHCgiven = undefined


-------- L4 Rule -> LE rule

l4rule2intermed :: Rule
        -> IntermedRule
l4rule2intermed = undefined
{-
  with a  helper function that knows which vars are in the given and that keeps track of the set of variables that we've already seen  
-} 


--------------



data TranspilerCfg = 
  TranspilerCfg { indentSpaces :: Int,
                  docHeader    :: T.Text,
                  templatesHeader :: T.Text,
                  ruleBodyHeader :: T.Text}

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