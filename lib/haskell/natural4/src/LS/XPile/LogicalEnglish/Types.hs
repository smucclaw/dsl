{-# OPTIONS_GHC -W #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms, DataKinds #-}

module LS.XPile.LogicalEnglish.Types (
    -- Common types 
      OrigVarName
    , Propn(..)
    , OpWhere
    , SimpleNum(..)

    -- L4-related types
    , GVar(..)
    , GVarSet
    , Cell(..)
    , SimpleL4HC(..)
    -- , L4ComplexPropn

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

{-| This data structure is designed for easy pretty printing: 
    that's what dictates whether to keep or discard the original L4 structure. 
-}
data Propn a =
  Atomic a
  -- ^ the structure in 'IS MAX / MIN / SUM / PROD t_1, ..., t_n' would be flattened out so that it's just a list of Cells --- i.e., a list of strings 
  | IsOpSuchThat OpWhere a
  -- ^ IS MAX / MIN / SUM / PROD where φ(x) -- these require special indentation, and right now our LE dialect only accepts an atomic propn as the arg to such an operator
  | And [Propn a]
  | Or  [Propn a]
  | Not (Propn a)
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

{-
Considered using phantom types, gadts, and datakinds to distinguish between the variants of Propn (esp. atomic vs non-atomic for the head vs body), but decided not worth the effort.

  data CPstatus = IsAtomic | IsOST | IsAnd | IsOr | IsNot 

  data Propn a b where
    Atomic :: a -> Propn a 'IsAtomic
    -- ^ the structure in 'IS MAX / MIN / SUM / PROD t_1, ..., t_n' would be flattened out so that it's just a list of Cells --- i.e., a list of strings 
    IsOpSuchThat :: OpWhere -> a -> Propn a 'IsOST
    -- ^ IS MAX / MIN / SUM / PROD where φ(x) -- these require special indentation, and right now our LE dialect only accepts an atomic propn as the arg to such an operator
    And :: [Propn a b] -> Propn a 'IsAnd
    Or  :: [Propn a b] -> Propn a 'IsOr
    Not :: [Propn a b] -> Propn a 'IsNot
    deriving (Eq, Ord, Show, Generic, Functor, Foldable)


-}

data OpWhere = MaxWhere | MinWhere | SumWhere
  deriving stock (Eq, Ord, Show)

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
          -- ^ TODO: see if it's possible to use a more generic number type tt includes floats, since Joe  uses floats in his encoding.
          | MkCellIs
          | MkCellIsDiffFr
          | MkCellIsMaxOf
          | MkCellIsMinOf
          | MkCellIsSumOf
          | MkCellIsProductOf
  deriving stock (Show, Eq, Ord)

data SimpleNum = MkInteger Integer | MkFloat Float
  deriving stock (Show, Eq, Ord)

-- not sure right now how best to model the initial L4 side --- need to consult Meng's docs / inspect the AST more
data SimpleL4HC = MkSL4hc { givenVars :: GVarSet
                          , head      :: Propn [Cell]
                            -- ^ tho really this shld be just the atomic variant
                          , body      :: Maybe (Propn [Cell]) }
-- type L4ComplexPropn = Propn [Cell]
-- type IRComplexPropn = Propn LamAbsBase

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

{-| Intermediate representation from which we can generate either LE natl lang annotations or LE rules. -}
data LamAbsRule = MkLAbsRule { givenVars  :: GVarSet
                             , head      :: LamAbsBase
                             , body      :: Propn LamAbsBase }
{-| This is best understood in the context of LamAbsRule  -}
data LamAbsBase = MkTBase { getVarSeq :: OrigVarSeq
                          , instTemplate :: Substn -> TemplInstanceOrNLA } 

{-------------------------------------------------------------------------------
  LE data types
-------------------------------------------------------------------------------}
newtype LENatLangAnnot = MkNLA T.Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, Hashable)

newtype LETemplateInstance = MkTInstance T.Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, Hashable)

data TemplInstanceOrNLA = TInst LETemplateInstance
                        | NLA LENatLangAnnot
-- TODO: try to derive instances somehow?

data LERule = LERule
            { head :: LETemplateInstance
            , body :: LECondnTree
            }
    deriving stock (Eq, Ord, Show)

{-| This is really for *our* dialect of LE (with our in-house libs) rather than standard LE. 
See https://github.com/LogicalContracts/LogicalEnglish/blob/main/le_syntax.md for the 'condition' nomenclature.
 -}
type LECondnTree = Propn LETemplateInstance
-- ^ so the `sum of`, `product of` would just be atomic LETemplateInsts / texts, since they don't differ indentation-wise from normal atomic conditions 

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

