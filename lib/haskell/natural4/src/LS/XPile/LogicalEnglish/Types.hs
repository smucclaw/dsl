{-# OPTIONS_GHC -W #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields#-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms, DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module LS.XPile.LogicalEnglish.Types (
    -- Common types 
      OrigVarName
    , BoolPropn(..)
    , SimpleNum(..)

    -- L4-related types
    , GVar(..)
    , GVarSet
    , Cell(..)
    , Term
    , SimpleL4HC(MkL4FactHc, fgiven, fhead, 
                 MkL4RuleHc, rgiven, rhead, rbody)
    -- , pattern MkL4RuleHc
    -- , pattern MkL4FactHc 
    -- , pattern 
    -- , pattern MkL4FactHc

    , OpOf(..)
    , OpSuchTt(..)
    , AtomicBPropn(..)
    , L4AtomicBP
    , pattern MkTrueAtomicBP
    , pattern MkIsOpSuchTtBP
    , pattern MkIsOpOf
    , pattern MkIsDiffFr

    -- Intermediate representation types
    , TemplateVar(..)
    , OrigVarPrefix
    , OrigVarSeq
    , Substn
    , LamAbsRule(..)
    , LamAbsBase(..)

    -- LE-related types
    , LENatLangAnnot
    , LETemplateInstance
    , TemplInstanceOrNLA(..)
    , LERule(..)
    , LECondnTree

    -- Configuration and LE-specific consts
    , LEProg
    -- , MkLEProg
) where


import Data.Text qualified as T
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Control.Monad.Identity ( Identity )

import Data.String (IsString)
import LS.Rule as L4 (Rule(..)) 

{-------------------------------------------------------------------------------
  Common types 
-------------------------------------------------------------------------------}

{-| The BoolPropn and AtomicBPropn data structures are designed for easy pretty printing to our dialect of LE: 
    that's what dictates whether to keep or discard the original L4 structure. 
-}
data BoolPropn a = AtomicBP a
                 | And [BoolPropn a]
                 | Or  [BoolPropn a]
                 | Not (BoolPropn a)
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

-- | Atomic(ish) Boolean proposition
data AtomicBPropn var baseprop = 
    ABPatomic baseprop
  | ABPIsDiffFr var var
  | ABPIsOpOf var OpOf [var]
    -- ^ 't IS MAX / MIN / SUM / PROD t_1, ..., t_n'  
  | ABPIsOpSuchTt var OpSuchTt baseprop
    {- |  t IS MAX / MIN / SUM / PROD x where φ(x) -- these require special indentation
        * the first Term would be, e.g., the "total savings" in "total savings is the max x such that"
        * the second propn would be the indented φ(x) condition
      Note: right now our LE dialect only accepts an atomic φ(x)
    -}
  deriving stock (Show, Eq, Ord)


data OpOf = MaxOf
          | MinOf
          | SumOf
          | ProductOf
  deriving stock (Show, Eq, Ord)

data OpSuchTt = MaxXSuchThat 
              | MinXSuchThat 
              | SumEachXSuchThat
  deriving stock (Show, Eq, Ord)


{-------------------------------------------------------------------------------
  The L4-related data types
-------------------------------------------------------------------------------}

-- | vars in the GIVEN of an L4 HC 
newtype GVar = MkGVar T.Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, Hashable)
type GVarSet = HS.HashSet GVar


-- | We only need to be able to represent texts and integers in our current encoding  
data Cell = MkCellT !T.Text
          | MkCellNum !SimpleNum 
          | MkCellIs
  deriving stock (Show, Eq, Ord)

data SimpleNum = MkInteger Integer | MkFloat Float
  deriving stock (Show, Eq, Ord)

type Term = Cell
type L4AtomicBP = AtomicBPropn Term [Cell] 

-- patterns to make it easier to program with L4AtomicBP and AtomicBPropn
pattern MkTrueAtomicBP :: [Cell] -> BoolPropn L4AtomicBP
pattern MkTrueAtomicBP cells = AtomicBP (ABPatomic cells)

pattern MkIsOpSuchTtBP :: Term -> OpSuchTt -> [Cell] -> BoolPropn L4AtomicBP
pattern MkIsOpSuchTtBP var ost bprop = AtomicBP (ABPIsOpSuchTt var ost bprop)

pattern MkIsDiffFr :: Term -> Term -> BoolPropn L4AtomicBP
pattern MkIsDiffFr t1 t2 = AtomicBP (ABPIsDiffFr t1 t2)

pattern MkIsOpOf :: Term -> OpOf -> [Term] -> BoolPropn L4AtomicBP
pattern MkIsOpOf term op args = AtomicBP (ABPIsOpOf term op args)

-- | Two varieties of SimpleL4HC
data SimpleL4HC = L4hcF L4Fact | L4hcR L4Rule

data L4Fact = L4Fact { givenVars :: GVarSet
                     , head      :: L4AtomicBP
                     }

data L4Rule = L4Rule { givenVars :: GVarSet
                     , head      :: L4AtomicBP
                     , body      :: BoolPropn L4AtomicBP }

pattern MkL4RuleHc :: GVarSet -> L4AtomicBP -> BoolPropn L4AtomicBP -> SimpleL4HC
pattern MkL4RuleHc{rgiven, rhead, rbody} = 
  L4hcR (L4Rule { givenVars = rgiven
                , head = rhead
                , body = rbody })

pattern MkL4FactHc :: GVarSet -> L4AtomicBP -> SimpleL4HC
pattern MkL4FactHc{fgiven, fhead} = 
  L4hcF  (L4Fact { givenVars = fgiven
                 , head = fhead})

{-------------------------------------------------------------------------------
  Types for L4 -> LE / intermediate representation
-------------------------------------------------------------------------------}
-- | Current thought is that we only need text / strs to capture what the original var 'names' were, because what we will eventually be printing out strings!
type OrigVarName = T.Text

type OrigVarPrefix = T.Text
{-| TemplateVars mark the places where we'd instantiate / substitute in the LamAbsBase / condition template to get either a natural language annotation or a LE rule. They store the original text / var name in the cell so that that text can be transformed as needed when instantiating the LamAbsBase. -}
data TemplateVar = MatchGVar !OrigVarName
                 | EndsInApos !OrigVarPrefix -- ^ so the orig var name, the thing that occupied the cell, would have been OrigVarPrefix <> "'s"
                 | IsNum !OrigVarName 
                 -- This case should be treated differently depending on whether trying to generate a NLA or LE rule
      deriving stock (Eq, Ord, Show)

type OrigVarSeq = [TemplateVar] -- TODO: Look into replacing [] with a more general Sequence type?

-- | Substn is a sequence of values that should be substituted for the variables
newtype Substn = MkSubstn [T.Text]
  deriving stock (Show)
  deriving newtype (Eq, Ord)

--TODO: Edit this / think thru it again when we get to this on Mon
{-| Intermediate representation from which we can generate either LE natl lang annotations or LE rules. -}
data LamAbsRule = MkLAbsRule { givenVars  :: GVarSet
                             , head      :: LamAbsBase
                             , body      :: BoolPropn LamAbsBase } -- this might need to be a Maybe (BoolPropn LamAbsBase)
{-| This is best understood in the context of LamAbsRule  -}
data LamAbsBase = MkTBase { getVarSeq :: OrigVarSeq
                          , instTemplate :: Substn -> TemplInstanceOrNLA } 

{-------------------------------------------------------------------------------
  LE data types
-------------------------------------------------------------------------------}
type LEVar = T.Text

newtype LENatLangAnnot = MkNLA T.Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, Hashable)

newtype LETemplateInstance = MkTInstance T.Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, Hashable)

data TemplInstanceOrNLA = TInst LETemplateInstance
                        | NLA LENatLangAnnot
  deriving stock (Eq, Ord, Show)


data LERule = MkLERule
            { head :: LETemplateInstance
            , body :: LECondnTree
            }
    deriving stock (Eq, Ord, Show)

{-| This is really for *our* dialect of LE (with our in-house libs) rather than standard LE. 
See https://github.com/LogicalContracts/LogicalEnglish/blob/main/le_syntax.md for the 'condition' nomenclature.
 -}
type LEAtomicBPropn = AtomicBPropn LEVar LETemplateInstance
type LECondnTree = BoolPropn LEAtomicBPropn
-- ^ TODO: This might be too much structure -- think more abt this when we get to pretty printing


-- TODO: maybe hide the real constructor and use a pattern to make a convenient constructor tt alr initializes with some consts like the doc header?
pattern MkLEProg <- undefined
data LEProg = MkLEProg_ {   docHeader    :: !T.Text
                          , nlasHeader :: !T.Text
                          , ruleBodyHeader :: !T.Text
                          , nlas :: [LENatLangAnnot]
                          , lerules :: [LERule] }
{-------------------------------------------------------------------------------
    Configs
-------------------------------------------------------------------------------}

data LECfg = LECfg { numIndentSpaces :: !Word }

