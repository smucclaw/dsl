{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

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

import Data.String (IsString)
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



{-------------------------------------------------------------------------------
  Common types 
-------------------------------------------------------------------------------}

type OrigVarName = T.Text

{-| This data structure is designed for easy pretty printing: 
    that's what dictates whether to keep or discard the original L4 structure. 
-}
data ComplexPropn a =
  Atomic a
    -- ^ the structure in SUM, PRODUCT etc would be flattened out so that it's just a list of Cells --- i.e., a list of strings 
  | And [ComplexPropn a]
  | Or  [ComplexPropn a]
  | Not [ComplexPropn a]
  | IsMax [ComplexPropn a]
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

{-------------------------------------------------------------------------------
  The L4-related data types
-------------------------------------------------------------------------------}
-- | vars in the GIVEN of an L4 HC 
newtype GVar = MkGVar T.Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, Hashable)
type GVarSet = HS.HashSet GVar

newtype Cell = MkCell T.Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

-- not sure right now how best to model the initial L4 side --- need to consult Meng's docs / inspect the AST more
data SimpleL4HC = MkSL4hc { givenVars :: GVarSet
                          , head      :: [Cell]
                          , body      :: L4ComplexPropn }
type L4ComplexPropn = ComplexPropn [Cell]
-- type IntermedComplexPropn = ComplexPropn TemplateBase

{-------------------------------------------------------------------------------
  Types for L4 -> LE / intermediate representation
-------------------------------------------------------------------------------}

type OrigVarPrefix = T.Text
data TemplateVar = MatchGVar OrigVarName
                 | IsNum OrigVarName
                 | EndsInApos OrigVarPrefix -- ^ so the orig var name, the thing that occupied the cell, would have been OrigVarPrefix <> "'s"
      deriving stock (Eq, Ord, Show)

type VarSeq = [TemplateVar] -- TODO: Look into replacing [] with a more general Sequence type?
newtype Substn = MkSubstn [TemplateVar]
  deriving stock (Show)
  deriving newtype (Eq, Ord)

data TemplateBase = MkTBase { varSeq :: VarSeq
                            , instTemplate :: Substn -> T.Text } 

{-| intermediate representation from which we can generate either LE natl lang annotations or LE rules -}
data IntermedRepn = MkIntermed { givenVars :: GVarSet
                               , head      :: TemplateBase
                               , body      :: ComplexPropn TemplateBase }

{-------------------------------------------------------------------------------
  LE data types
-------------------------------------------------------------------------------}
{-|

-}
newtype LENatLangAnnotatn = MkNLA T.Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

newtype LETemplateInstance = MkTInstance T.Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)
-- TODO: Think about using type-level machinery to capture wehther or not the template has been instantiated

data LERule = LERule
            { head :: LETemplateInstance
            , body :: LECondnTree
            }
    deriving stock (Eq, Ord, Show)

{-| This is really for *our* dialect of LE (with our in-house libs) rather than standard LE. 
See https://github.com/LogicalContracts/LogicalEnglish/blob/main/le_syntax.md for the 'condition' nomenclature.
 -}
type LECondnTree = ComplexPropn LETemplateInstance
-- ^ so the `sum of`, `product of` would just be atomic LETemplateInsts / texts, since they don't differ indentation-wise from normal atomic conditions 
-- TODO: Ask Joe if the condition in `the max suhc that...` must be atomic


-------- L4 Program -> LE Nat Lang Annotations 
{-|
Generating the templates / nat lang annotations from a set of L4 rules:

Terminology / concepts:
  * A `GivenVar` is a variable that appears in the GIVEN of the L4.

TODO: Add more docs
-}



l4toLENatLangAnnots :: L4Prog -> HS.HashSet LENatLangAnnotatn
l4toLENatLangAnnots = undefined

-- `ruleLocalsIn` in Interpreter.hs may be worth looking at, though I suspect it'd be cleaner to do this with optics 
-- TODO: think -- how best to model variable, given that we also want to be able to hash it?
varsFromHCgiven :: Rule -> GVarSet
varsFromHCgiven = undefined


-------- L4 Rule -> LE rule



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