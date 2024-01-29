{- | Experiments adapting Monad Validate to Effectful -}
{-# OPTIONS_GHC -W #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields, OverloadedLabels, UndecidableInstances #-}
{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE AllowAmbiguousTypes, TypeApplications, DataKinds, TypeFamilies, FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module LS.XPile.MathLang.EffValidate where

import LS.XPile.MathLang.GenericMathLang.GenericMathLangAST -- TODO: Add import list
import LS.XPile.MathLang.Logging (LogConfig, defaultLogConfig)

import Data.Kind (Type)

-- import AnyAll qualified as AA
-- import LS.Types qualified as L4
import LS.Types as L4 (RelationalPredicate(..), RPRel(..), MTExpr(..), EntityType, HornClause (..))
-- import LS.Interpreter (qaHornsT)
import LS.Rule (
                -- Interpreted(..), 
                extractMTExprs, getGivenWithSimpleType,
                defaultHorn)
import LS.Rule qualified as L4 (Rule(..))

import Control.Monad (foldM)
import Effectful (Effect, Eff, runPureEff)
import Effectful.TH (makeEffect)
import Effectful.Dispatch.Dynamic (send, interpret, localSeqUnlift)
-- import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Reader.Static (runReader, Reader)
import Effectful.State.Static.Shared (State, runState)
-- experimenting with Effectful.Error rn; will switch over to Control.Monad.Validate later
import Control.Monad.Validate qualified as V
  ( ValidateT
    , MonadValidate
    -- , Validate
    , refute
    , dispute
    , tolerate
    )
import Data.HashSet qualified as HS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Optics
import GHC.Generics (Generic)
import Data.Generics.Sum.Constructors
-- import Data.Generics.Product.Types (types)
-- import Prettyprinter (Pretty)
import Data.String.Interpolate (__i)
import Data.String (IsString)
import Data.Text qualified as T
-- import LS.Utils.TextUtils (int2Text, float2Text)
import Data.Foldable qualified as F (toList)

import LS.Types (TypeSig(..), TypedMulti)

-- | Delete this later
data ToLCError = NotYetImplemented T.Text -- SrcPositn
               | MiscError T.Text
  deriving stock (Eq, Show, Generic)

instance Hashable ToLCError

--------------------

data EffValidate e :: Effect where
  Refute :: e -> EffValidate e m a
  Dispute :: e -> EffValidate e m ()
  Tolerate :: m a -> EffValidate e m (Maybe a)
makeEffect ''EffValidate

{-
class (Monad m, Semigroup e) => MonadValidate e m | m -> e where
  -- | Raises a fatal validation error. Aborts the current branch of the validation (i.e. does not
  -- return).
  --
  -- @
  -- >>> 'Control.Monad.Validate.runValidate' ('refute' ["boom"] '>>' 'refute' ["bang"])
  -- 'Left' ["boom"]
  -- @
  refute :: e -> m a


localSeqUnlift
  :: (HasCallStack, SharedSuffix es handlerEs)
  => LocalEnv localEs handlerEs
  -- ^ Local environment.
  -> ((forall r. Eff localEs r -> Eff es r) -> Eff es a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
-}
runValidate :: forall (e :: Type) (es :: [Effect]) (a :: Type)
             . (Semigroup e, V.MonadValidate e (Eff es))
            => Eff (EffValidate e : es) a
            -> Eff es a
runValidate = interpret \localEnv -> \case
  Refute errs -> localSeqUnlift localEnv $ \unlift -> do
    unlift $ refute errs
    -- I think I need to learn more about how Effectful works before proceeding with the adaption

  Dispute errs -> undefined
  Tolerate errs -> undefined



------------------------------------------------

{-
First, I tried following the tutorial and got

data EffValidate e :: Effect where
  Refute :: e -> EffValidate e m a
  Dispute :: e -> EffValidate e m ()
  Tolerate :: m a -> EffValidate e m (Maybe a)

type instance DispatchOf (EffValidate e) = Dynamic

-- | provide an orphan, canonical instance of EffValidate for Eff that delegates to the Validate effect
instance (Semigroup e, MonadValidate e (Eff es), EffValidate e :> es) => MonadValidate e (Eff es) where
  refute = send . Refute
  dispute = send . Dispute
  tolerate = send . Tolerate
but ran into an issue. 

s:210:14: error: [GHC-39999]
    • Could not deduce ‘EffValidate e0 :> es’
        arising from a use of ‘send’
      from the context: (MonadValidate e (Eff es), EffValidate e :> es)
        bound by the instance declaration
        at src/LS/XPile/MathLang/GenericMathLang/TranslateL4.hs:207:10-97
      The type variable ‘e0’ is ambiguous
      Relevant bindings include
        tolerate :: Eff es a -> Eff es (Maybe a)
          (bound at src/LS/XPile/MathLang/GenericMathLang/TranslateL4.hs:210:3)
    • In the first argument of ‘(.)’, namely ‘send’
      In the expression: send . Tolerate
      In an equation for ‘tolerate’: tolerate = send . Tolerate
Someone on a Discord suggested that that issue was due to MonadValidate being a higher-order effect, and suggested using makeEffect; I'm trying that now, but am getting stuck when it comes to how to interpret it / run the effect.

This is what I currently have:

import Effectful (Effect, Eff)
import Effectful.TH (makeEffect)
import Effectful.Dispatch.Dynamic (send, interpret, localSeqUnlift)

import Control.Monad.Validate qualified as V
  ( ValidateT
    , MonadValidate
    -- , Validate
    , refute
    , dispute
    , tolerate )

data EffValidate e :: Effect where
  Refute :: e -> EffValidate e m a
  Dispute :: e -> EffValidate e m ()
  Tolerate :: m a -> EffValidate e m (Maybe a)
makeEffect ''EffValidate

runValidate :: Eff (EffValidate e : es) a -> Eff es a
runValidate = interpret $ \localEnv -> \case
  Refute errs -> localSeqUnlift localEnv $ \unlift -> do
  --- stuck here; presumably we want to delegate to V.refute? but not sure 
I 
-}