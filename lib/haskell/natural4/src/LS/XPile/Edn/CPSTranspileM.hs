{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

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
import Data.Bifunctor (Bifunctor(..))
import Data.Hashable (Hashable)

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

data TranspileState metadata = TranspileState
  { context :: Context,
    messageLog :: MessageLog metadata
  }
  deriving (Eq, Show, Generic, Hashable)

data TranspileResult metadata = TranspileResult
  { edn :: EDN.TaggedValue,
    finalState :: TranspileState metadata
  }
  deriving (Eq, Show, Generic)

instance Semigroup (TranspileState metadata) where
  (<>) = mappenddefault

instance Monoid (TranspileState metadata) where
  mempty = memptydefault

instance HasContext (TranspileState metadata) where
  vars <++> state@TranspileState {context} =
    state {context = vars <++> context}

  symbol !? TranspileState {context} = symbol !? context

instance HasMessageLog TranspileState metadata where
  logMsg severity messageData state@TranspileState {messageLog} =
    state {messageLog = logMsg severity messageData messageLog}

  getMsgs = messageLog>>> getMsgs

logTranspileMsg ::
  State.MonadState (TranspileState metadata) m => Severity -> MessageData metadata -> m ()
logTranspileMsg severity = logMsg severity >>> State.modify

runCPSTranspileM ::
  CPSTranspileM metadata EDN.TaggedValue -> TranspileResult metadata
runCPSTranspileM =
  ( coerce ::
      CPSTranspileM metadata EDN.TaggedValue ->
      Cont.ContT EDN.TaggedValue (State.State (TranspileState metadata)) EDN.TaggedValue
  )
    >>> Cont.evalContT
    >>> flip State.runState mempty
    >>> \(edn, finalState) -> TranspileResult {edn, finalState}

-- Resume a suspended computation (captured in a continuation), with the
-- context temporarily extended with some variables.
withExtendedCtx ::
  Foldable m => m T.Text -> CPSTranspileM metadata t -> CPSTranspileM metadata t
withExtendedCtx vars cont = do
  -- Save the current context.
  TranspileState {context} <- State.get
  -- Extend context with vars.
  State.modify (vars <++>)
  -- Resume the suspended computation.
  result <- cont
  -- Restore the old context which we saved.
  State.modify \state -> state {context}
  -- Return the result of the computation.
  pure result