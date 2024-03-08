{-# OPTIONS_GHC -W #-}

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

module LS.XPile.MathLang.GenericMathLang.GenericMathLangAST where
-- TODO: Add export list

import Data.Text qualified as T
import Optics (re, view)
import Optics.TH (makeFieldLabelsNoPrefix, makePrisms)
import GHC.Generics

-- import Data.Generics.Product.Types (types)
import Data.String ( IsString )
-- import Data.String.Interpolate (i)

-- import AnyAll qualified as AA
-- import LS.Types qualified as L4
-- import LS.Types (RelationalPredicate(..), RPRel(..), MTExpr(..))
-- import LS.Rule as L4 (Rule(..), extractMTExprs)

-- import Data.HashSet qualified as HS
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Coerce (coerce)
-- import GHC.Generics (Generic)

------------ L4 declared entity types ----------------------

-- | Types that are declared in L4 by the user, e.g. 'Person' or 'Singaporean citizen'
newtype L4EntType = MkL4EntType T.Text
  deriving stock (Show)
  deriving newtype (Eq, IsString, Hashable)
makePrisms ''L4EntType

mkEntType :: T.Text -> L4EntType
mkEntType = view $ re _MkL4EntType

entTypeAsTxt :: L4EntType -> T.Text
entTypeAsTxt = view _MkL4EntType

{-------------------------------------------------------
    AST
==========================================================-}

{- | Note: The first draft of this will ignore the complexities to do with variables and assume global scope, as Meng does
     We may not even bother with trying to translate function definitions and applications in the first draft
-}

type FieldLabel = T.Text
type Number = Double
-- ^ TODO: Will want to change this to something that can represent money in the future

data TLabel = FromUser L4EntType
            | Inferred T.Text
  deriving stock (Eq, Show)
makePrisms ''TLabel

-- only used for debugging purposes, TODO remove later
instance Semigroup TLabel where
  FromUser (MkL4EntType x) <> FromUser (MkL4EntType y) = FromUser (MkL4EntType $ x <> y)
  FromUser (MkL4EntType x) <> Inferred y = FromUser (MkL4EntType $ x <> y)
  Inferred x <> FromUser (MkL4EntType y) = FromUser (MkL4EntType $ x <> y)
  Inferred x <> Inferred y = Inferred $ x <> y

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

data SrcPositn = MkPositn
  { row :: Int
  , col :: Int
  } deriving stock (Eq, Ord, Show, Generic)
    deriving Hashable
makeFieldLabelsNoPrefix ''SrcPositn

-- | Could use HKDs, but that'd make things more complex in other ways
data ExpMetadata =
  MkExpMetadata { srcPos :: SrcPositn
                , typeLabel :: Maybe TLabel
                , explnAnnot :: Maybe ExplnAnnot }
  deriving stock (Eq, Show)
makeFieldLabelsNoPrefix ''ExpMetadata


type MdGrp = [ExpMetadata]
-- Hacky, but: in the case of Lam, want to have md for param too
-- This should always have either 1 or 2 elts

------------------------------------------------------------
-- TODO: Look into whether the costs of using records for sum variants (eg partial functions) outweigh benefits

newtype Var = MkVar T.Text
  deriving stock (Show)
  deriving newtype (Eq, Hashable)
makePrisms ''Var
-- Add metadata like what the original L4 string was?
-- Or do that only at the final pretty printing stage, when we normalize the formatting etc?

mkVar :: T.Text -> Var
mkVar = view $ re _MkVar

varAsTxt :: Var -> T.Text
varAsTxt = view _MkVar


data Lit = EBoolTrue | EBoolFalse | EInteger Integer | EFloat Double | EString T.Text
  deriving stock (Eq, Ord, Show)


data NumOp = OpPlus | OpMinus | OpMul | OpDiv | OpMaxOf | OpMinOf | OpSum | OpProduct
  deriving stock (Eq, Show)

data CompOp = OpBoolEq | OpStringEq | OpNumEq | OpLt | OpLte | OpGt | OpGte
  deriving stock (Eq, Show)

newtype SeqExp = SeqExp [Exp]
  deriving stock (Show, Generic, Eq)
  deriving (Semigroup, Monoid) via [Exp]

seqExpToExprs :: SeqExp -> [Exp]
seqExpToExprs = coerce

exprsToSeqExp :: [Exp] -> SeqExp
exprsToSeqExp = coerce

consSE :: Exp -> SeqExp -> SeqExp
consSE expr (seqExpToExprs -> exprs) = exprsToSeqExp $ expr : exprs

-- removed GADTs because had been experimenting with `unbound-generics` and didn't know how to get them to work well tgt
-- | May want to put the `md`s back into BaseExp and collapse Exp and BaseExp back into one data structure. Not sure what's more ergo rn
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
    { param :: Var   -- ELam x
    , body :: Exp }  ---     (ELam (y  (ENumOp Mul x (Enum)))
  | EApp
    { func :: Exp, -- ^ func
      appArg :: Var   -- ^ arg
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
  deriving stock (Show, Generic, Eq)

data Exp = MkExp
  { exp :: BaseExp
  , md :: MdGrp }
  deriving stock (Show, Eq)
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
newtype GlobalVars = MkGlobalVars (HashMap Var (Maybe L4EntType))
  deriving stock (Show)
  deriving (Semigroup, Monoid) via (HashMap Var (Maybe L4EntType))
makePrisms ''GlobalVars

mkGlobalVars :: HashMap Var (Maybe L4EntType) -> GlobalVars
mkGlobalVars = view (re _MkGlobalVars)


{- | Metadata for programs in generic lam calc
[TODO] Things that could be added in the future:
    * the timestamp from Main.hs
    * feature flags, esp. the explanation-related ones
    * config flags that downstream targets need to know about
-}
data LCProgMetadata =
  MkLCProgMdata { filename :: T.Text }
  deriving stock (Eq, Show)

data LCProgram =
  MkLCProgram { progMetadata :: LCProgMetadata
              , lcProgram :: [Exp]
              , globalVars :: GlobalVars
              , givethVar :: [T.Text] -- if the L4 program specifies what it giveth, record it here
              }
  deriving stock (Show)


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
