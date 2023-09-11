{-# OPTIONS_GHC -W #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields, NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns, DataKinds, GADTs #-}

module LS.XPile.LogicalEnglish.Types (
    -- Common types 
      OrigVarName
    , BoolPropn(..)
    -- L4-related types
    , InlineRPrel(..)
    , RPnonPropAnaph 
    , RParithComp
    , RPothers
    , GVar(..)
    , GVarSet
    , Cell(..)
    , L4Term
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
    , VarsHC(MkVarsFact,
             MkVarsRule, 
             vfhead,
             vrhead, vrbody,
             VhcF, VhcR)
    , VarsFact(..)
    , BaseRule(..)
    , VarsRule
    , AtomicPWithVars
    , VCell(..)

    -- LE-related types
    , LEhcCell(..)
    , LEVar(..)
    , NormdVars
    , NormalizedVar(..)

    , LEhcAtomicP
    , TxtAtomicBP

    , NLACell(..)
    , NLATxt(..)

    , NLA' (NLA) -- opaque; exporting only pattern for matching on the NLATxt
    , mkNLA      -- smart constructor
    , getNLAtxt

    , LERule
    , LETemplateTxt(..)
    , UnivStatus(..)

    , FactWithUnivsMarked
    , RuleWithUnivsMarked
    , LEFactForPrint
    , LERuleForPrint
    , LEhcPrint(..)

    -- Configuration and LE-specific consts
    , LEProg(..)
) where


import Data.Text qualified as T
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Data.Containers.NonEmpty (HasNonEmpty, onNonEmpty)

import Data.Foldable (toList)
import Data.Sequence.NonEmpty (NESeq)
-- import Data.Sequence.NonEmpty qualified as NESeq
import Data.Sequence qualified as Seq (fromList)
import Data.String (IsString)
-- import LS.Rule as L4 (Rule(..))
import Prettyprinter(Pretty)

{- |
Misc notes
-----------
When we say 'LE' here, we really mean *our* dialect of LE (with our in-house libs) rather than standard LE. 

* See https://github.com/LogicalContracts/LogicalEnglish/blob/main/le_syntax.md for the 'condition' nomenclature.

-}

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

{-| Atomic(ish) Boolean proposition
The notion of 'term' here is that which is employed in Prolog and logic. 
In particular, it includes not only variables but also atoms.
-}
data AtomicBPropn term =
    ABPatomic [term]
  | ABPIsDiffFr term term
    -- ^ Note: the encoding has a few rules that use an atom in the rightmost term
  | ABPIsOpOf term OpOf [term]
    -- ^ 't IS MAX / MIN / SUM / PROD t_1, ..., t_n'  
  | ABPIsOpSuchTt term OpSuchTt [term]
    {- |  t IS MAX / MIN / SUM / PROD x where φ(x) -- these require special indentation
        * the first L4Term would be, e.g., the "total savings" in "total savings is the max x such that"
        * the second propn would be the indented φ(x) condition
      Note: right now our LE dialect only accepts an atomic φ(x)
    -}
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)


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
data RPnonPropAnaph
data RParithComp
data RPothers

{- | 
  Some RPs are supported by converting them to cases in other data structures
  Some RPs are, by contrast, 'inlined'; the following are the 'inline' RPRels tt are supported by L4 -> LE transpiler

  Having a GADT like this is useful for various reasons.
  For example, it allows us to mark explicitly in the types which of the various RPRel types a function uses (because often, e.g., we only use a specific proper subset), 
  and to avoid incomplete-pattern-matching errors from the compiler (i.e., to actually get the sort of compile-time guarantees we'd like)
-}
data InlineRPrel a where 
  InlRPlt :: InlineRPrel RParithComp
  InlRPlte :: InlineRPrel RParithComp
  InlRPgt :: InlineRPrel RParithComp
  InlRPgte :: InlineRPrel RParithComp

  InlRPor :: InlineRPrel RPnonPropAnaph
  InlRPand :: InlineRPrel RPnonPropAnaph

  InlRPelem :: InlineRPrel RPothers

-- | vars in the GIVEN of an L4 HC 
newtype GVar = MkGVar T.Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, Hashable)
type GVarSet = HS.HashSet GVar


-- | We only need to be able to represent texts and integers in our current encoding  
data Cell = MkCellT !T.Text
          | MkCellIsNum !T.Text
  deriving stock (Show, Eq, Ord)

type L4Term = Cell
type L4AtomicP = AtomicBPropn Cell

-- patterns to make it easier to program with L4AtomicP and AtomicBPropn
pattern MkTrueAtomicBP :: [Cell] -> BoolPropn L4AtomicP
pattern MkTrueAtomicBP cells = AtomicBP (ABPatomic cells)

pattern MkIsOpSuchTtBP :: L4Term -> OpSuchTt -> [Cell] -> BoolPropn L4AtomicP
pattern MkIsOpSuchTtBP var ost bprop = AtomicBP (ABPIsOpSuchTt var ost bprop)

pattern MkIsDiffFr :: L4Term -> L4Term -> BoolPropn L4AtomicP
pattern MkIsDiffFr t1 t2 = AtomicBP (ABPIsDiffFr t1 t2)

pattern MkIsOpOf :: L4Term -> OpOf -> [L4Term] -> BoolPropn L4AtomicP
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
-- | we only need text / strs to capture what the original var 'names' were, because what we will eventually be printing out strings!
type OrigVarName = T.Text

type OrigVarPrefix = T.Text
{-| TemplateVars mark the places where we'd instantiate / substitute in the VCell / condition template to get either a natural language annotation or a LE rule. 
They store the original text / var name in the cell so that that text can be transformed as needed when instantiating the VCell. -}
data TemplateVar = MatchGVar !OrigVarName
                 | EndsInApos !OrigVarPrefix
                   {- ^ so the orig var name, the thing that occupied the cell, would have been OrigVarPrefix <> "'s"
                  `OrigVarPrefix` must have been a GVar
                    -}
                 | IsNum !OrigVarName
                   -- This case should be treated differently depending on whether trying to generate a NLA or LE rule
      deriving stock (Eq, Ord, Show)
      deriving (Generic, Hashable)
type TVarSet = HS.HashSet TemplateVar

{- Got this error 
    Module ‘Data.Hashable.Generic’ does not export ‘gHashWithSalt’
   |
63 | import Data.Hashable.Generic ( gHashWithSalt )
   |                                ^^^^^^^^^^^^^
on my mac when trying

instance Hashable TemplateVar where
  hashWithSalt = gHashWithSalt
  {-# INLINEABLE hashWithSalt #-}

from https://hackage.haskell.org/package/hashable-generics-1.1.7/docs/Data-Hashable-Generic.html -}

type OrigVarSeq = [TemplateVar] -- TODO: Look into replacing [] with a more general Sequence type?

{-| Intermediate representation from which we can generate either LE natl lang annotations or LE rules.

Things to note / think about:
* One difference between NLAs and making LE rules: 
  Not all L4AtomicBPs will need to be converted to NLAs --- e.g., t1 is different from t2 already has a NLA in the fixed lib. 
  By contrast, we do need to be able to convert every L4AtomicP to a LE condition.
* 

 -}
data VarsHC = VhcF VarsFact | VhcR VarsRule
      deriving stock (Eq, Ord, Show)

newtype VarsFact = VFact { varsfhead :: AtomicPWithVars }
      deriving stock (Show)
      deriving newtype (Eq, Ord)

data BaseRule a = MkBaseRule { rhead :: a
                             , rbody :: BoolPropn a }
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

type VarsRule = BaseRule AtomicPWithVars


pattern MkVarsFact :: AtomicPWithVars -> VarsHC
pattern MkVarsFact{vfhead}
  = VhcF (VFact { varsfhead = vfhead })

pattern MkVarsRule :: AtomicPWithVars -> BoolPropn AtomicPWithVars -> VarsHC
pattern MkVarsRule{vrhead, vrbody}
  = VhcR (MkBaseRule { rhead  = vrhead
                      , rbody  = vrbody})
{-# COMPLETE MkVarsFact, MkVarsRule #-}

{- | This might seem a bit confusing, because now there can be template variables both within a VCell and outside of it (e.g., if it's a ABPIsOpSuchTt). 
  But I wanted to retain information about what the original variant of AtomicBPropn was for p printing afterwards.
  Also, it's helpful to have tt info for generating NLAs, 
  since the only time we need to generate an NLA is when we have a `baseprop` / `VCell` --- we don't need to do tt for ABPIsDiffFr and ABPIsOpOf. 
  To put it another way: NLAs are generated *from*, and only from, LamAbsBases.
 -}
type AtomicPWithVars = AtomicBPropn VCell

{-| This is best understood in the context of the other VarsX data types  -}
data VCell = TempVar TemplateVar
           | Pred    !T.Text
          deriving stock (Eq, Ord, Show)

{-------------------------------------------------------------------------------
  LE data types
-------------------------------------------------------------------------------}

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

{- Another option, courtesy of `Mango IV.` from the Functional Programming discord:
  deriving stock Generic
  deriving (Semigroup, Monoid) via Generically NLACell 
This requires a base that's shipped with ghc 94 or newer and and import Generically.
But sticking to handwritten instance b/c it's easy enough, and to make the behavior explicit -}
  

{-
newtype NLA = MkNLA T.Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, Hashable, Pretty)
-}  

newtype NLATxt = MkNLATxt T.Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, Hashable, Pretty)

data Regex -- placeholder; to be removed later

data NLA' =  MkNLA' { getBase'   :: NESeq NLACell 
                    , getNLATxt' :: NLATxt
                    , getRegex'  :: Regex }

{-| public getter to view the NLAtxt
Don't need to export a lens for this field cos not going to change / set it -}
getNLAtxt :: NLA' -> NLATxt
getNLAtxt nla' = nla'.getNLATxt'

-- | public pattern to match on the NLAtxt
pattern NLA :: NLATxt -> NLA'
pattern NLA nlatxt <- (getNLAtxt -> nlatxt)

-- | Smart constructor for making NLA'
mkNLA :: forall f. (Foldable f, HasNonEmpty (f NLACell)) => f NLACell -> Maybe NLA'
mkNLA (Seq.fromList . toList -> nlacells) = 
  onNonEmpty make nlacells
    where 
      make :: NESeq NLACell -> NLA'
      make base = MkNLA' { getBase'   = base
                         , getNLATxt' = annotxtify base
                         , getRegex'  = regexify base }

-- | Private function for making NLATxt from NESeq NLACell (this knows that the underlying record uses NESeq NLACell for getBase')
annotxtify :: NESeq NLACell -> NLATxt              
annotxtify = undefined

regexify :: NESeq NLACell -> Regex
regexify = undefined


---------------- For generating template instances / non-NLAs

data LEVar = VarApos !OrigVarPrefix
           | VarNonApos !OrigVarName
    deriving stock (Eq, Ord, Show)

{-| The first prep step for generating TemplateTxts from LamAbs stuff involves simplifying LamAbsCells
-}
data LEhcCell = VarCell LEVar 
              | NotVar !T.Text 
                -- | i.e., not smtg tt we will ever need to check if we need to prefix with an 'a'
          deriving stock (Eq, Ord, Show)
          deriving (Generic)

newtype NormalizedVar = MkNormVar T.Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, Hashable)

type NormdVars = HS.HashSet NormalizedVar

-- | When generating template instances / non-NLAs, we transform PreTTCells to UnivStatuses, before basically concatenating them to get LETemplateTxts 
data UnivStatus = PrefixWithA !OrigVarName
                | NoPrefix !T.Text
    deriving stock (Eq, Ord, Show)
    deriving (Generic, Hashable)

newtype LETemplateTxt = MkTempTxt T.Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, Hashable, Pretty)
  deriving (Semigroup, Monoid) via T.Text


-- The LE HCs
data LEhcPrint = LEHcF LEFactForPrint | LEHcR LERuleForPrint
      deriving stock (Eq, Ord, Show)

-- The atomic bprops we'll use

type LEhcAtomicP = AtomicBPropn LEhcCell
type TxtAtomicBP = AtomicBPropn LETemplateTxt

type FactWithUnivsMarked = AtomicBPropn UnivStatus
type LEFactForPrint = AtomicBPropn LETemplateTxt 

type LERule = BaseRule (AtomicBPropn LEhcCell)
type RuleWithUnivsMarked = BaseRule (AtomicBPropn UnivStatus)
type LERuleForPrint = BaseRule TxtAtomicBP

----- for pretty printing -------------------------------------------------------


data LEProg = MkLEProg {  nlas :: [NLATxt]
                        , leHCs :: [LEhcPrint] 
                        }


--- to remove once we are sure we won't want to go back to this way of doing this: 
-- LE Rule
-- data LERule a b = 
--     LERule { rhead :: AtomicBPropn a b
--            , rbody :: BoolPropn (AtomicBPropn a b)
--            }
--     deriving stock (Eq, Ord, Show)
-- type RuleWithUnivsMarked = LERule UnivStatus [UnivStatus]
-- type LERuleForPrint = LERule LETemplateTxt LETemplateTxt

-- LE Fact
-- data LEFact a = LEFact { fhead :: AtomicBPropn a  }
--   deriving stock (Show, Eq, Ord)
-- TODO: Think about whether to bother with a wrapper for facts
