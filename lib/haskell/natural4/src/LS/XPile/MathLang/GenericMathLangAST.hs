{-# OPTIONS_GHC -W #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns, AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes, UndecidableInstances, DataKinds, TypeFamilies, DeriveAnyClass #-}

module LS.XPile.MathLang.GenericMathLangAST where
-- TODO: Add export list

import Data.Text qualified as T

import Optics.TH (makeFieldLabelsNoPrefix)
import GHC.Generics

-- import Data.Generics.Product.Types (types)
-- import Data.String ( IsString )
-- import Data.String.Interpolate (i)

-- import AnyAll qualified as AA
-- import LS.Types qualified as L4
-- import LS.Types (RelationalPredicate(..), RPRel(..), MTExpr(..))
-- import LS.Rule as L4 (Rule(..), extractMTExprs)

-- import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
-- import GHC.Generics (Generic)

{-------------------------------------------------------
    AST
==========================================================-}

{- | Note: The first draft of this will ignore the complexities to do with variables and assume global scope, as Meng does
     We may not even bother with trying to translate function definitions and applications in the first draft
-}


type FieldLabel = T.Text
type TLabel = T.Text

type Number = Double
-- ^ TODO: Will want to change this to something that can represent money in the future

{-----------
TO THINK ABT
------------
  * Do we really want to allow for user annotations for *every* kind of expr?
  * Shld we factor 'statements' out?
-}

{----------------------------------------------------------
    Metadata related types
==========================================================-}

-- TODO. ExplnImptce is a reach goal
data ExplnImptce = HighEI | LowEI | DebugEI
  deriving stock (Eq, Ord, Show)

data ExplnAnnot = MkExplnAnnot
  { l4RuleName :: !T.Text
  , overridAnnot :: !(Maybe T.Text)
  -- ^ if L4 writer wants to override the default annotation 
  , explnImptce :: !(Maybe ExplnImptce)
  -- ^ how impt it is to log the relevant annotation when tracing the eval, to (optionally) be provided by L4 writer
  -- what the default shld be can be a configurable L4 setting (and can made configurable on the downstream side as well)

  -- don't need to add 'human readable' version of corresponding L4 snippet since can just use SrcPositn to get the correspondence
  } deriving stock (Eq, Ord, Show)
makeFieldLabelsNoPrefix ''ExplnAnnot

data SrcPositn = MkPositn
  { row :: !Int
  , col :: !Int
  } deriving stock (Eq, Ord, Show)
makeFieldLabelsNoPrefix ''SrcPositn

-- data TypeMetadata = MkTMdata
--   { tlabel :: !TLabel
--   } deriving stock (Eq, Ord, Show)
-- makePrisms ''TypeMetadata


data ExpMetadata = MkEMdata
  { srcPos :: !SrcPositn
  , explnAnnot :: !(Maybe ExplnAnnot)
  -- , typeMd :: !(Maybe TypeMetadata)
  } deriving stock (Eq, Ord, Show)
makeFieldLabelsNoPrefix ''ExpMetadata

type MdGrp = [ExpMetadata]
-- Hacky, but: in the case of Lam, want to have md for param too
-- This should always have either 1 or 2 elts

------------------------------------------------------------
-- TODO: Look into whether the costs of using records for sum variants (eg partial functions) outweigh benefits

newtype Var = MkVar T.Text
  deriving stock (Show)
  deriving newtype (Eq, Hashable)
-- Add metadata like what the original L4 string was?
-- Or do that only at the final pretty printing stage, when we normalize the formatting etc?

-- TODO: Need to figure out how best to deal with numbers, esp. wrt money. Shld not use floats for money.
data Lit = ENumber !Number | EBool !Bool | EString !T.Text
  deriving stock (Eq, Ord, Show)

data Op = OpPlus | OpNumEq | OpStrEq | OpMaxOf | OpSum | OpProduct
  deriving stock (Eq, Ord, Show)


-- removed GADTs because had been experimenting with `unbound-generics` and didn't know how to get them to work well tgt
data BaseExp =
    ELit { lit :: !Lit }
  | EOp
    { binOp :: !Op,
      opLeft :: !Exp, -- ^ left
      opRight :: !Exp -- ^ right
    }

  | EIf
    { condExp :: !Exp,
      thenExp :: !Exp,
      elseExp :: !Exp
    }
  | EVar { var :: Var }

  -----------------------
  -- TODO: For v2
  -----------------------
  -- | ELam
  --   { param :: Var
  --   , body :: !Exp }
  -- | EApp
  --   { func :: !Exp, -- ^ func 
  --     arg :: !Exp   -- ^ arg
  --   }
  -- | ERecdRef -- with fake records
  --   { rcdName :: T.Text
  --   , rcdField :: FieldLabel }
  {- For now assume record labels are unique and won't clash with non-record varnames
  -}

  -- | variable mutation; prob treat as also eval-ing to assigned value
  | EVarSet
    { var :: Var,
      arg :: !Exp
    }
  | ELet
    { var :: Var
    , val :: !Exp
    , body :: !Exp
    }

  -- TODO: For V2
  -- | Block / sequence of nested bindings,
  -- where each binding expression can refer to previously bound variables
  -- | ESeq { stmts :: ![Exp] }

  | EAnd
    { left :: !BaseExp,  -- ^ left
      right :: !BaseExp  -- ^ right
    }
  | EOr
    { left :: !BaseExp,
      right :: !BaseExp
    }
  | EEmpty
  deriving stock (Show, Generic)

data Exp = MkExp
  { exp :: !BaseExp
  , md :: !MdGrp }
  deriving stock (Show)
makeFieldLabelsNoPrefix ''Exp


{--------------------------------------------------
  LC / Generic MathLang Program 
--------------------------------------------------}

{- | Metadata for programs in generic lam calc
[TODO] Things that could be added in the future:
    * the timestamp from Main.hs
    * feature flags, esp. the explanation-related ones
    * config flags that downstream targets need to know about
-}
data LCProgMetadata =
  MkLCProgMdata { filename :: !T.Text }
  deriving stock (Eq, Show)

data LCProgram = 
  MkLCProgram { progMetadata :: LCProgMetadata
              , lcProgram :: [Exp]
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
