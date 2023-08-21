{-# OPTIONS_GHC -W #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields#-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms, DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module LS.XPile.LogicalEnglish.Types (
    -- Common types 
      OrigVarName
    , BoolPropn(..)
    -- L4-related types
    , GVar(..)
    , GVarSet
    , Cell(..)
    , Term
    , SimpleL4HC(MkL4FactHc, fgiven, fhead,
                 MkL4RuleHc, rgiven, rhead, rbody)

    , OpOf(..)
    , OpSuchTt(..)
    , AtomicBPropn(..)
    , L4AtomicP
    , pattern MkTrueAtomicBP
    , pattern MkIsOpSuchTtBP
    , pattern MkIsOpOf
    , pattern MkIsDiffFr

    -- Intermediate representation types
    , TemplateVar(..)
    , OrigVarPrefix
    , OrigVarSeq
    , Substn
    , LamAbsHC(MkLAFact, lafgiven, lafhead,
               MkLARule, largiven, larhead, larbody)
    , LamAbsFact(..)
    , LamAbsRule(..)
    , LamAbsAtomicP
    , LamAbsCell(..)

    -- LE-related types
    , NLACell(..)
    , LENatLangAnnot(..)
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
import qualified Data.Bits as Generically

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
  -- TODO: Look into what guarantees we have or don't have for the sorts of vars tt can appear here
  | ABPIsOpOf var OpOf [var]
    -- TODO: Look into what guarantees we have or don't have for the sorts of vars tt can appear in the leftmost position
    -- ^ 't IS MAX / MIN / SUM / PROD t_1, ..., t_n'  
  | ABPIsOpSuchTt var OpSuchTt baseprop
    -- TODO: Look into what guarantees we have or don't have for the sorts of vars tt can appear in the leftmost position
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
          | MkCellIsNum !T.Text
  deriving stock (Show, Eq, Ord)

-- data SimpleNum = MkInteger Integer | MkFloat Float
--   deriving stock (Show, Eq, Ord)

type Term = Cell
type L4AtomicP = AtomicBPropn Term [Cell]

-- patterns to make it easier to program with L4AtomicP and AtomicBPropn
pattern MkTrueAtomicBP :: [Cell] -> BoolPropn L4AtomicP
pattern MkTrueAtomicBP cells = AtomicBP (ABPatomic cells)

pattern MkIsOpSuchTtBP :: Term -> OpSuchTt -> [Cell] -> BoolPropn L4AtomicP
pattern MkIsOpSuchTtBP var ost bprop = AtomicBP (ABPIsOpSuchTt var ost bprop)

pattern MkIsDiffFr :: Term -> Term -> BoolPropn L4AtomicP
pattern MkIsDiffFr t1 t2 = AtomicBP (ABPIsDiffFr t1 t2)

pattern MkIsOpOf :: Term -> OpOf -> [Term] -> BoolPropn L4AtomicP
pattern MkIsOpOf term op args = AtomicBP (ABPIsOpOf term op args)

-- | Two varieties of SimpleL4HC
data SimpleL4HC = L4hcF L4Fact | L4hcR L4Rule

data L4Fact = L4Fact { givenVars :: GVarSet
                     , head      :: L4AtomicP
                     }

data L4Rule = L4Rule { givenVars :: GVarSet
                     , head      :: L4AtomicP
                     , body      :: BoolPropn L4AtomicP }

pattern MkL4RuleHc :: GVarSet -> L4AtomicP -> BoolPropn L4AtomicP -> SimpleL4HC
pattern MkL4RuleHc{rgiven, rhead, rbody} =
  L4hcR (L4Rule { givenVars = rgiven
                , head = rhead
                , body = rbody })

pattern MkL4FactHc :: GVarSet -> L4AtomicP -> SimpleL4HC
pattern MkL4FactHc{fgiven, fhead} =
  L4hcF (L4Fact { givenVars = fgiven
                 , head = fhead})

{-# COMPLETE MkL4FactHc, MkL4RuleHc #-}
{-------------------------------------------------------------------------------
  Types for L4 -> LE / intermediate representation
-------------------------------------------------------------------------------}
-- | Current thought is that we only need text / strs to capture what the original var 'names' were, because what we will eventually be printing out strings!
type OrigVarName = T.Text

type OrigVarPrefix = T.Text
{-| TemplateVars mark the places where we'd instantiate / substitute in the LamAbsCell / condition template to get either a natural language annotation or a LE rule. 
They store the original text / var name in the cell so that that text can be transformed as needed when instantiating the LamAbsCell. -}
data TemplateVar = MatchGVar !OrigVarName
                 | EndsInApos !OrigVarPrefix
                   {- ^ so the orig var name, the thing that occupied the cell, would have been OrigVarPrefix <> "'s"
                  `OrigVarPrefix` must have been a GVar
                    -}
                 | IsNum !OrigVarName
                   -- This case should be treated differently depending on whether trying to generate a NLA or LE rule
                 | OpOfVarArg !OrigVarName
                 | OtherVar !OrigVarName -- TODO: Look into whether can remove this case 
      deriving stock (Eq, Ord, Show)

type OrigVarSeq = [TemplateVar] -- TODO: Look into replacing [] with a more general Sequence type?

-- | Substn is a sequence of values that should be substituted for the variables
newtype Substn = MkSubstn [T.Text]
  deriving stock (Show)
  deriving newtype (Eq, Ord)

--TODO: Edit this / think thru it again when we get to this on Mon
{-| Intermediate representation from which we can generate either LE natl lang annotations or LE rules.

Things to note / think about:
* One difference between NLAs and making LE rules: 
  Not all L4AtomicBPs will need to be converted to NLAs --- e.g., t1 is different from t2 already has a NLA in the fixed lib. 
  By contrast, we do need to be able to convert every L4AtomicP to a LE condition.
* 

 -}
data LamAbsHC = LAhcF LamAbsFact | LAhcR LamAbsRule
      deriving stock (Eq, Ord, Show)

data LamAbsFact = LAFact { givenVars  :: GVarSet
                         , head      :: LamAbsAtomicP }
      deriving stock (Eq, Ord, Show)
data LamAbsRule = LARule { givenVars  :: GVarSet
                         , head      :: LamAbsAtomicP
                         , body      :: BoolPropn LamAbsAtomicP }
      deriving stock (Eq, Ord, Show)

pattern MkLAFact :: GVarSet -> LamAbsAtomicP -> LamAbsHC
pattern MkLAFact{lafgiven, lafhead}
  = LAhcF (LAFact { givenVars = lafgiven
                  , head      = lafhead })

pattern MkLARule :: GVarSet -> LamAbsAtomicP -> BoolPropn LamAbsAtomicP -> LamAbsHC
pattern MkLARule{largiven, larhead, larbody}
  = LAhcR (LARule { givenVars = largiven
                  , head      = larhead
                  , body      = larbody})
{-# COMPLETE MkLAFact, MkLARule #-}

{- | This might seem a bit confusing, because now there can be template variables both within a LamAbsCell and outside of it (e.g., if it's a ABPIsOpSuchTt). 
  But I wanted to retain information about what the original variant of AtomicBPropn was for p printing afterwards.
  Also, it's helpful to have tt info for generating NLAs, 
  since the only time we need to generate an NLA is when we have a `baseprop` / `LamAbsCell` --- we don't need to do tt for ABPIsDiffFr and ABPIsOpOf. 
  To put it another way: NLAs are generated *from*, and only from, LamAbsBases.
 -}
type LamAbsAtomicP = AtomicBPropn TemplateVar [LamAbsCell]

{-| This is best understood in the context of the other lam abs data types  -}
data LamAbsCell = TempVar TemplateVar
                | Pred    !T.Text
          deriving stock (Eq, Ord, Show)
  --  MkTBase { getVarSeq :: OrigVarSeq
  --                         , instTemplate :: Substn -> TemplInstanceOrNLA } 


{-------------------------------------------------------------------------------
  LE data types
-------------------------------------------------------------------------------}
-- | For generating template instances / non-NLAs. The main difference is tt we no longer have an IsNum variant
data LEtiVar = LEMatchGV !OrigVarName
             | LEApos !OrigVarPrefix
    deriving stock (Eq, Ord, Show)
    deriving (Generic, Hashable)


{-
Got this error 
    Module ‘Data.Hashable.Generic’ does not export ‘gHashWithSalt’
   |
63 | import Data.Hashable.Generic ( gHashWithSalt )
   |                                ^^^^^^^^^^^^^
on my mac when trying

instance Hashable LEtiVar where
  hashWithSalt = gHashWithSalt
  {-# INLINEABLE hashWithSalt #-}

from https://hackage.haskell.org/package/hashable-generics-1.1.7/docs/Data-Hashable-Generic.html
-}

data NLACell = MkParam !T.Text 
             | MkNonParam !T.Text
  deriving stock (Eq, Ord, Show)

instance Semigroup NLACell where
  MkParam l <> MkParam r = MkNonParam $ l <> r
  MkParam l <> MkNonParam r = MkNonParam $ l <> r
  MkNonParam l <> MkParam r = MkNonParam $ l <> r
  MkNonParam l <> MkNonParam r = MkNonParam $ l <> r
instance Monoid NLACell where
  mempty = MkNonParam ""

{- Another option:
deriving stock Generic
deriving (Semigroup, Monoid) via Generically NLACell 
Need a base that’s shipped with ghc 94 or newer and need to import Generically.
But sticking to handwritten instance b/c it's easy enough, and to make the behavior explicit-}
  
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
type LEAtomicBPropn = AtomicBPropn LEtiVar LETemplateInstance
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

