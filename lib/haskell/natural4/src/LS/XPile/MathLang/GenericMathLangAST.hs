{-# OPTIONS_GHC -W #-}
{-# OPTIONS_GHC -foptimal-applicative-do #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns, DataKinds, GADTs, KindSignatures, AllowAmbiguousTypes, ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module LS.XPile.MathLang.GenericMathLangAST where
-- TODO: Add export list

import Data.Text qualified as T

-- import Control.Monad.Validate
--   ( MonadValidate (..)
--     , Validate
--     , refute
--     )
import Optics.TH
-- import Data.Generics.Product.Types (types)
-- import Data.String ( IsString )
-- import Data.String.Interpolate (i)

-- import AnyAll qualified as AA
-- import LS.Types qualified as L4
-- import LS.Types (RelationalPredicate(..), RPRel(..), MTExpr(..))
-- import LS.Rule as L4 (Rule(..), extractMTExprs)

-- import Data.HashSet qualified as HS
-- import Data.Hashable (Hashable)
-- import GHC.Generics (Generic)

{----------------------------------------------------------
    AST
==========================================================-}

{- | Note: The first draft of this will ignore the complexities to do with variables and assume global scope, as Meng does
     We may not even bother with trying to translate function definitions and applications in the first draft
-}
type VarName = T.Text
type FieldLabel = T.Text
type TLabel = String

type Number = Float 
-- ^ TODO: Will want to change this to something that can represent money in the future

data Stage = Prelim | Desugared
  deriving stock (Eq, Ord, Show)


{-
TO THINK ABT:

  * Do we really want to allow for user annotations for *every* kind of expr?
  * Is it worth bothering with `stage`?
  * Shld we factor 'statements' out?
-}
data ExpF md (stage :: Stage) where
  ELit :: { litMd :: md, lit :: Lit } -> ExpF md stage
  EOp ::
    { opMd :: md,
      binOp :: Op,
      leftArg :: ExpF md stage, -- ^ left
      rightArg :: ExpF md stage -- ^ right
    } -> ExpF md stage
  EUnOp :: { md :: md, unOp :: UnOp, arg :: ExpF md stage } -> ExpF md stage
  EIf ::
    { ifMd :: md,
      condExp :: ExpF md stage,
      thenExp :: ExpF md stage,
      elseExp :: ExpF md stage
    } -> ExpF md stage
  ELam ::
    { lamMd :: md,          -- ^ lam metadata
      paramMd :: md,        -- ^ param metadata
      param :: VarName,     -- ^ param
      body :: ExpF md stage -- ^ body
    } -> ExpF md stage
  EApp ::
    { appMd :: md,
      appFunc :: ExpF md stage, -- ^ func 
      appArg :: ExpF md stage   -- ^ arg
    } -> ExpF md stage

  EVar :: { md :: md, var :: VarName } -> ExpF md stage
  ERecdRef :: { recrefMd :: md, recdExp :: ExpF md stage, recdField :: FieldLabel } -> ExpF md stage

  -- | variable mutation; prob treat as eval-ing to assigned value
  EVarSet ::
    { vsetMd :: md,
      vsetVar :: VarName,
      arg :: ExpF md stage -- ^ arg 
    } -> ExpF md stage

  -- | sequence of statements; returns last of the exprs
  ESeq :: { seqMd :: md, stmts :: [ExpF md stage] } -> ExpF md stage

  ELet ::
    -- Need to prefix field names with `let` because of https://gitlab.haskell.org/ghc/ghc/-/issues/12159
    { letMd :: md,
      letVar :: VarName,
      val :: ExpF md stage, -- ^ value 
      letBody :: ExpF md stage -- ^ body
    } -> ExpF md 'Prelim
  EAnd ::
    { andMd :: md,
      andLeftArg :: ExpF md stage,  -- ^ left
      andRightArg :: ExpF md stage  -- ^ right
    } -> ExpF md 'Prelim
  EOr ::
    { orMd :: md,
      orLeftArg :: ExpF md stage,
      orRightArg :: ExpF md stage
    } -> ExpF md 'Prelim
  EEmpty :: { md :: md } -> ExpF md stage

  -- note re if
  -- prob safe to assume for now that type of each branch has to be the same
  -- type of IfThenElse in Meng MathLang seems to be Number | Bool



-- NOTE: will want to be able to tally the desugared nodes with the prelim nodes too, and port type info from the former to the latter
-- since will prob need to translate one of the prelim ASTs to Meng eval ast

deriving instance Show md => Show (ExpF md stage)

-- TODO: Need to figure out how best to deal with numbers, esp. wrt money. Shld not use floats for money.
data Lit = ENumber Number | EBool Bool | EString !T.Text
  deriving stock (Eq, Ord, Show)

data Op = OpPlus | OpNumEq | OpStrEq | OpMaxOf | OpSum | OpProduct
  deriving stock (Eq, Ord, Show)

data UnOp
  deriving stock (Eq, Ord, Show)
-- TODO: may not need this

-- TODO; this would be a reach goal
data ExplnImptce = HighEI | LowEI | DebugEI
  deriving stock (Eq, Ord, Show)

data ExplnAnnot = MkExplnAnnot
  { l4RuleName :: !T.Text
  , overridAnnot :: Maybe T.Text 
  -- ^ if L4 writer wants to override the default annotation 
  , explnImptce :: Maybe ExplnImptce 
  -- ^ how impt it is to log the relevant annotation when tracing the eval, to (optionally) be provided by L4 writer
  -- what the default shld be can be a configurable L4 setting (and can made configurable on the downstream side as well)

  -- don't need to add 'human readable' version of corresponding L4 snippet since can just use SrcPositn to get the correspondence
  -- prob other fields too
  } deriving stock (Eq, Ord, Show)


data SrcPositn = MkPositn
  { row :: !Int
  , col :: !Int
  , filename :: !T.Text
  } deriving stock (Eq, Ord, Show)

data TypeMetadata = MkTMdata
  { tlabel :: TLabel
  } deriving stock (Eq, Ord, Show)


data ExpMetadata = MkEMdata
  { srcPos :: SrcPositn
  , explnAnnot :: Maybe ExplnAnnot
  , typeMd :: Maybe TypeMetadata
  } deriving stock (Eq, Ord, Show)
makePrisms ''ExpMetadata


newtype Exp stage = ExpF ExpMetadata
  deriving stock (Eq, Ord, Show)

--------------------------------------------------------------------------------

{----------------------------------------------------------
 Reach goals 
==========================================================-}

{- | Allow L4 users to define a program-wide / global dict 
   that can be used by downstream targets to, e.g., explain what certain bits of jargon mean
-}
-- newtype ProgramGlossary = ProgramGlossary { getProgramGlossary :: [(T.Text, T.Text)] }