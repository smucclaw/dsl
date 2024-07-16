{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | Defines the GML abstract syntax tree.
module LS.XPile.MathLang.GenericMathLang.GenericMathLangAST where

import Data.Text qualified as T
import Data.Time (Day(..))
-- import Money (Dense)
import Optics (re, view)
import Optics.TH (makeFieldLabelsNoPrefix, makePrisms)
import GHC.Generics
-- import Data.List.NonEmpty (NonEmpty(..))
-- import Data.List.NonEmpty qualified as NE

-- import Data.Generics.Product.Types (types)
-- import Data.String ( IsString )
-- import Data.String.Interpolate (i)

import LS.Types (TypeSig(..), ParamType(..), enumLabels)

import Data.HashMap.Strict (HashMap, empty)
import Data.Hashable (Hashable)
import Data.Coerce (coerce)
import Language.Haskell.TH.Syntax (Lift)
-- import GHC.Generics (Generic)

------------ L4 declared entity types ----------------------

-- | GML types that are declared in L4 by the user, e.g. 'Person' or 'Singaporean citizen'.
--
data L4EntType =
    L4EntType T.Text  -- ^ a user-defined named type
  | L4Enum [T.Text]   -- ^ a user-defined enumeration type
  | L4List L4EntType  -- ^ a user-defined list type
  deriving stock (Eq, Generic, Show, Ord)
  deriving anyclass (Hashable)

-- | Turn a surface-L4 type signature into a GML type.
--
-- NOTE / TODO: This translation seems lossy.
--
mkEntType :: TypeSig -> L4EntType
mkEntType = \case
  SimpleType TOne      tn -> L4EntType tn
  SimpleType TOptional tn -> L4EntType tn -- no optional
  SimpleType _         tn -> L4List $ mkEntType $ SimpleType TOne tn -- lists, sets, no difference
  InlineEnum _         pt -> L4Enum $ enumLabels pt -- assuming no lists here (is there an example of a list of enum values?)

{-------------------------------------------------------
    AST
==========================================================-}

{- | Note: The first draft of this will ignore the complexities to do with variables and assume global scope, as Meng does
     We may not even bother with trying to translate function definitions and applications in the first draft
-}

type FieldLabel = T.Text
type Number = Double
-- ^ TODO: Will want to change this to something that can represent money in the future

-- | GML type information can either stem from annotations or from inference.
--
data TLabel = FromUser L4EntType
            | Inferred T.Text
  deriving stock (Eq, Ord, Show)
makePrisms ''TLabel


{-----------
TO THINK ABT
------------
  * Do we really want to allow for user annotations for *every* kind of expr?
-}

{----------------------------------------------------------
    Metadata related types
==========================================================-}

-- TODO. ExplnImptce is a reach goal
data ExplnImptce = HighEI | LowEI | DebugEI
  deriving stock (Eq, Ord, Show)

{- |
To think about:
  * Inari: Might it make sense to stick GF trees in here (or some other metadata type associated with each Exp)?
  YM's quick thought / reaction: Will want some way of toggling
  whether to parse to GF regardless, since can imagine that for
  some applications / users, they may not need the GF trees.
  May want to look into HKDs too.
-}
data ExplnAnnot = MkExplnAnnot
  { l4RuleName :: T.Text
  , overridAnnot :: Maybe T.Text
  -- ^ if L4 writer wants to override the default annotation
  , explnImptce :: Maybe ExplnImptce
  -- ^ how impt it is to log the relevant annotation when tracing the eval, to (optionally) be provided by L4 writer
  -- what the default shld be can be a configurable L4 setting (and can made configurable on the downstream side as well)

  -- don't need to add 'human readable' version of corresponding L4 snippet since can just use SrcPositn to get the correspondence
  } deriving stock (Eq, Ord, Show)
makeFieldLabelsNoPrefix ''ExplnAnnot

-- | Source position.
--
-- (Row and column, but not filename.)
--
data SrcPositn = MkPositn
  { row :: !Int
  , col :: !Int
  } deriving stock (Eq, Ord, Show, Generic)
    deriving Hashable
makeFieldLabelsNoPrefix ''SrcPositn

-- | Metadata stored with an expression.
--
data ExpMetadata =
  MkExpMetadata
  { srcPos     :: SrcPositn        -- ^ source position of annotated expression
  , typeLabel  :: Maybe TLabel     -- ^ (possibly) type information for annotated expression
  , explnAnnot :: Maybe ExplnAnnot -- ^ (possibly) additional annotations
  }
  deriving stock (Eq, Ord, Show)
makeFieldLabelsNoPrefix ''ExpMetadata


type MdGrp = [ExpMetadata]
-- Hacky, but: in the case of Lam, want to have md for param too
-- This should always have either 1 or 2 elts

------------------------------------------------------------
-- TODO: Look into whether the costs of using records for sum variants (eg partial functions) outweigh benefits

-- | Variable names in GML.
--
-- The internal representation is text.
--
newtype Var = MkVar T.Text
  deriving stock (Show, Ord)
  deriving newtype (Eq, Hashable)
makePrisms ''Var
-- Add metadata like what the original L4 string was?
-- Or do that only at the final pretty printing stage, when we normalize the formatting etc?

-- | Construct a variable from its name / text.
--
mkVar :: T.Text -> Var
mkVar = coerce

-- | Extract the text from a variable.
--
varAsTxt :: Var -> T.Text
varAsTxt = coerce

type Currency = T.Text

-- | Literals.
--
-- Currently supported:
--
-- - Booleans
-- - currencies (pair of a commodity / currency and a float)
-- - dates
-- - integers
-- - floats
-- - enumeration types
-- - strings
--
-- Remarks:
--
-- - Currencies are just strings.
-- - The numeric values in currencies should probably not be 'Double', but 'Fixed' or 'Rational' or perhaps 'Scientific'.
--
data Lit
  = EBoolTrue
  | EBoolFalse
  | ECurrency Currency Double
  | EDate Day -- 4 Jan 2023
  | EInteger Integer
  | EFloat Double
  | EENum T.Text  -- ^ actual value of enum
--         [T.Text] -- ^ possible values of enum (no use for now, but potential ease of printing error msgs?)
  | EString T.Text
  deriving stock (Eq, Ord, Show)

-- | Numeric binary operators.
--
-- (Consume two numbers, produce one number.)
--
--
data NumOp
  = OpPlus
  | OpMinus
  | OpMul
  | OpDiv
  | OpModulo
  | OpMaxOf
  | OpMinOf
  | OpSum
  | OpProduct
  deriving stock (Eq, Show, Ord, Lift)

-- | Comparison operators.
--
-- (Consume two args, produce a Boolean.)
--
data CompOp = OpBoolEq | OpStringEq | OpNumEq | OpLt | OpLte | OpGt | OpGte
  deriving stock (Eq, Show, Ord, Lift)

newtype SeqExp = SeqExp [Exp]
  deriving stock (Show, Generic, Eq, Ord)
  deriving (Semigroup, Monoid) via [Exp]

seqExpToExprs :: SeqExp -> [Exp]
seqExpToExprs = coerce

exprsToSeqExp :: [Exp] -> SeqExp
exprsToSeqExp = coerce

consSE :: Exp -> SeqExp -> SeqExp
consSE expr (SeqExp exprs) = exprsToSeqExp $ expr : exprs

-- | Un-annotated GML expression.
--
-- The recursive calls are all to 'Exp', which pairs a base expression with metadata.
-- So we have an "annotated fixed point" construction here, but with the annotation
-- being fixed to always be metadata.
--
-- Notes:
--
-- - It is unclear why the operators are separated from each other.
-- - It is unclear why And / Or have separate constructors instead of being operators.
--
-- Old comments:
--
-- - Removed GADTs because had been experimenting with `unbound-generics` and didn't know how to get them to work well tgt
-- - May want to put the `md`s back into BaseExp and collapse Exp and BaseExp back into one data structure. Not sure what's more ergo rn
--
data BaseExp =
    ELit { lit :: Lit }
  | ENumOp
    { numOp :: NumOp,
      nopLeft :: Exp, -- ^ left
      nopRight :: Exp -- ^ right
    }
  | ECompOp
    { compOp :: CompOp,
      compLeft :: Exp,
      compRight :: Exp
    }

  | EIfThen
    { condExp :: Exp,
      thenExp :: Exp
    }
  | EIfTE
    { condExp :: Exp,
      thenExp :: Exp,
      elseExp :: Exp
    }
  | EVar { var :: Var }
  | ELam
    { param :: Var
    , body :: Exp }
  | EApp
    { func :: Exp, -- ^ func
      appArg :: Exp   -- ^ arg
    }
  {- |
  My impression from Meng's examples had been that he wanted variable mutation,
  but he just told me on Wed Jan 31 that he actually prefers variables to be immutable.
  This does help to simplify things.

  TODO: Change AST and implmenetation in TranslateL4.hs accordingly if necessary
    Need to think more about exactly to model Meng's L4 examples if vars are immutable
  -}
  | EVarSet
    { vsetVar :: Exp, -- IF n==1 THEN f n = 0
      arg :: Exp
    }
  | EPredSet -- Inari experiment: in mathlang we have Pred and PredSet, easier translation
    { psetVar :: Var,
      parg :: Exp -- evaluates to Bool eventually, we assume, e.g. ITE, CompOp
    }
  | ELet
    { letVar :: Exp
    , letVal :: Exp
    , letBody :: Exp
    }
  | EIs
    { isLeft :: Exp
    , isRight :: Exp
    }
  | ERec
    { fieldName :: Exp -- this can become a predicate
    , recName :: Exp -- can be nested: ind's parent's (income)
    }
  -- TODO: mostly for V2
  -- | Block / sequence of expressions (likely to be variable assignments,
  -- where each expression can refer to previously bound variables
  | ESeq { seq :: SeqExp }

  | ENot { arg :: Exp }
  | EAnd
    { left :: Exp,  -- ^ left
      right :: Exp  -- ^ right
    }
  | EOr
    { left :: Exp,
      right :: Exp
    }
  | EEmpty
  deriving stock (Show, Generic, Eq, Ord)

-- | A GML expression is an unannotated 'BaseExp' together with metadata annotations.
--
data Exp = MkExp
  { exp :: BaseExp
  , md :: MdGrp }
  deriving stock (Show, Ord, Eq, Generic)
makeFieldLabelsNoPrefix ''Exp

-- Consider doing something like the following in the future (http://blog.vmchale.com/article/ir-instances)
-- instance Num Exp where
--     (+) = ENumOp OpPlus
--     (*) = ENumOp OpMul
--     (-) = ENumOp OpMinus

-- fromInteger = ...<TO ADD>... ELit . EInteger

{--------------------------------------------------
  LC / Generic MathLang Program
--------------------------------------------------}

{- | Keeps track of *global* var bindings, e.g. 'globally' declared GIVENs
This should be useful b/c most langs require more upfront declaration of global vars than Meng seems to want in L4
-}

--newtype GlobalVars = MkGlobalVars {mkGlobalVars :: HashMap Var (Maybe L4EntType)}
newtype GlobalVars = MkGlobalVars (HashMap Var (Maybe L4EntType))
  deriving stock (Show)
  deriving (Semigroup, Monoid, Eq) via (HashMap Var (Maybe L4EntType))
makePrisms ''GlobalVars

mkGlobalVars :: HashMap Var (Maybe L4EntType) -> GlobalVars
mkGlobalVars = view (re _MkGlobalVars)


-- | Datatype representing a GML program.
data LCProgram =
  MkLCProgram
  { lcProgram    :: [Exp]                       -- ^ actual program
  , globalVars   :: GlobalVars                  -- ^ names of global variables and their types
  , giveths      :: [T.Text]                    -- ^ if the L4 program specifies what it giveth, record it here
  , userFuns     :: HashMap String ([Var], Exp) -- ^ user-defined functions
  }
  deriving stock (Show, Eq)

emptyProgram :: LCProgram
emptyProgram = MkLCProgram {
    lcProgram = []
  , globalVars = MkGlobalVars empty
  , giveths = []
  , userFuns = empty
  }


{----------------------------------------------------------
 Misc notes to self
-----------------------------------------------------------
note re `if`:
  prob safe to assume for now that type of each branch has to be the same
  type of IfThenElse in Meng MathLang seems to be Number | Bool

NOTE: will want to be able to tally the desugared nodes with the prelim nodes too, and port type info from the former to the latter
since will prob need to translate one of the prelim ASTs to Meng eval ast
-}


{----------------------------------------------------------
 Reach goals / Future TODOs
==========================================================

* Var binding stuff; LetStar / Seq


Architecture / design
* Figure out how to do records and record accessors with `unbound-generics` --- or maybe don't even worry about
collisions if record accessors will always be on external data
* figure out how to use GADTs with Generic and Typeable

-}


{- | Allow L4 users to define a program-wide / global dict
   that can be used by downstream targets to, e.g., explain what certain bits of jargon mean
-}
-- newtype ProgramGlossary = ProgramGlossary { getProgramGlossary :: [(T.Text, T.Text)] }
