{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LS.XPile.LogicalEnglish.Types (
    -- Common types 
      OrigVarName
    , ComplexPropn

    -- L4-related types
    , GVar(..)
    , GVarSet
    , Cell
    , SimpleL4HC(..)
    -- , L4ComplexPropn

    -- Intermediate representation types
    , TemplateVar
    , OrigVarPrefix
    , OrigVarSeq
    , Substn
    , LamAbsRule
    , BaseTemplate

    -- LE-related types
    , LENatLangAnnot
    , LETemplateInstance
    , TemplInstanceOrNLA
    , LERule
    , LECondnTree

    -- Configuration and LE-specific consts
    , TranspilerCfg
) where


import LS.PrettyPrinter
    ( myrender, vvsep, (</>), tildes, (<//>), srchs )
import Prettyprinter
    ( vsep, viaShow, hsep, emptyDoc, (<+>), Pretty(pretty), Doc, indent, line )
import Text.Pretty.Simple   ( pShowNoColor )
import Data.Text qualified as T
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Data.HashMap.Strict qualified as Map
import Control.Monad.Identity ( Identity )

import Data.String (IsString)
import LS.Rule qualified as L4 (Rule(..)) 
import LS.XPile.LogicalEnglish.Common (
    L4Prog,
    (|>)
    )

{-------------------------------------------------------------------------------
  Common types 
-------------------------------------------------------------------------------}

{-| This data structure is designed for easy pretty printing: 
    that's what dictates whether to keep or discard the original L4 structure. 
-}
data ComplexPropn a =
  Atomic a
  -- ^ the structure in 'IS MAX / MIN / SUM / PROD t_1, ..., t_n' would be flattened out so that it's just a list of Cells --- i.e., a list of strings 
  | IsOpSuchTt a
  -- ^ IS MAX / MIN / SUM / PROD -- these require special indentation, and right now our LE dialect only accepts an atomic propn as the arg to such an operator
  | And [ComplexPropn a]
  | Or  [ComplexPropn a]
  | Not [ComplexPropn a]
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

{-

-}

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
          | MkCellI Integer
  deriving stock (Show, Eq, Ord)

-- not sure right now how best to model the initial L4 side --- need to consult Meng's docs / inspect the AST more
data SimpleL4HC = MkSL4hc { givenVars :: GVarSet
                          , head      :: [Cell]
                          , body      :: ComplexPropn [Cell] }
-- type L4ComplexPropn = ComplexPropn [Cell]
-- type IRComplexPropn = ComplexPropn BaseTemplate

{-------------------------------------------------------------------------------
  Types for L4 -> LE / intermediate representation
-------------------------------------------------------------------------------}
-- | Current thought is that we only need text / strs to capture what the original var 'names' were, because what we will eventually be printing out strings!
type OrigVarName = T.Text


type OrigVarPrefix = T.Text
{-| TemplateVars are what can get instantiated / substituted to give us either a natural language annotation or a LE rule -}
data TemplateVar = MatchGVar !OrigVarName
                 | EndsInApos !OrigVarPrefix -- ^ so the orig var name, the thing that occupied the cell, would have been OrigVarPrefix <> "'s"
             --  | IsNum !OrigVarName -- TODO: Work on this case later
      deriving stock (Eq, Ord, Show)

type OrigVarSeq = [TemplateVar] -- TODO: Look into replacing [] with a more general Sequence type?

-- | Substn is a sequence of values that should be substituted for the variables
newtype Substn = MkSubstn [T.Text]
  deriving stock (Show)
  deriving newtype (Eq, Ord)

{-| Intermediate representation from which we can generate either LE natl lang annotations or LE rules. -}
data LamAbsRule = MkLAbsRule { givenVars  :: GVarSet
                                  , head      :: BaseTemplate
                                  , body      :: ComplexPropn BaseTemplate }
{-| This is best understood in the context of LamAbsRule  -}
data BaseTemplate = MkTBase { getVarSeq :: OrigVarSeq
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
type LECondnTree = ComplexPropn LETemplateInstance
-- ^ so the `sum of`, `product of` would just be atomic LETemplateInsts / texts, since they don't differ indentation-wise from normal atomic conditions 


{-------------------------------------------------------------------------------
    Configs
-------------------------------------------------------------------------------}

data TranspilerCfg =
  TranspilerCfg { numIndentSpaces :: !Word,
                  docHeader    :: !T.Text,
                  nlasHeader :: !T.Text,
                  ruleBodyHeader :: !T.Text}

