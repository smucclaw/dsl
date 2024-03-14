{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}

module LS.XPile.Edn.CPSTranspileM
  ( CPSTranspileM,
    TranspileState (..),
    TranspileResult (..),
    logTranspileMsg,
    runCPSTranspileM,
    withExtendedCtx,
  )
where

import Control.Arrow ((>>>))
import Control.Monad.Cont qualified as Cont
import Control.Monad.State.Strict qualified as State
import Data.Coerce (coerce)
import Data.EDN qualified as EDN
import Data.Hashable (Hashable)
import Data.Text qualified as T
import Flow ((|>))
import GHC.Generics (Generic)
import Generics.Deriving.Monoid (mappenddefault, memptydefault)
import LS.XPile.Edn.Context (Context, HasContext (..), (<++>))
import LS.XPile.Edn.MessageLog
  ( HasMessageLog (..),
    MessageData,
    MessageLog,
    Severity (..),
  )
import Optics qualified
import Optics.TH (camelCaseFields, makeLensesWith, makeLenses)

data TranspileState metadata = TranspileState
  { _context :: Context,
    _messageLog :: MessageLog metadata
  }
  deriving (Eq, Show, Generic, Hashable)

makeLenses ''TranspileState

instance Semigroup (TranspileState metadata) where
  (<>) = mappenddefault

instance Monoid (TranspileState metadata) where
  mempty = memptydefault

instance HasContext (TranspileState metadata) where
  (<++>) vars = Optics.over context (vars <++>)

  -- symbol !? TranspileState {context} = symbol !? context
  (!?) symbol = Optics.view context >>> (symbol !?)

instance HasMessageLog TranspileState metadata where
  logMsg severity =
    Optics.over messageLog . logMsg severity

  getMsgs = Optics.view messageLog >>> getMsgs

logTranspileMsg ::
  State.MonadState (TranspileState metadata) m => Severity -> MessageData metadata -> m ()
logTranspileMsg severity = logMsg severity >>> State.modify

newtype CPSTranspileM metadata t
  = CPSTranspileM
  {cpsTranspileM :: Cont.ContT EDN.TaggedValue (State.State (TranspileState metadata)) t}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      State.MonadState (TranspileState metadata),
      Cont.MonadCont
    )

data TranspileResult metadata = TranspileResult
  { edn :: EDN.TaggedValue,
    finalState :: TranspileState metadata
  }
  deriving (Eq, Show, Generic)

runCPSTranspileM ::
  CPSTranspileM metadata EDN.TaggedValue -> TranspileResult metadata
runCPSTranspileM =
  coerce
    >>> Cont.evalContT
    >>> flip State.runState mempty
    >>> uncurry TranspileResult

-- Resume a suspended stateful computation (captured in a continuation), with the
-- context temporarily extended with some variables.
withExtendedCtx ::
  Foldable m => m T.Text -> CPSTranspileM metadata t -> CPSTranspileM metadata t
withExtendedCtx vars cont = do
  -- Save the current context.
  (Optics.view context -> oldContext) <- State.get
  -- Extend context with vars.
  State.modify (vars <++>)
  -- Resume the suspended computation.
  result <- cont
  -- Restore the old context which we saved.
  State.modify $ Optics.set context oldContext
  -- Return the result of the computation.
  pure result