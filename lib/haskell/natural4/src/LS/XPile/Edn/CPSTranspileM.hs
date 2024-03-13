{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}

module LS.XPile.Edn.CPSTranspileM
  ( CPSTranspileM,
    TranspileState (..),
    logTranspileMsg,
    runCPSTranspileM,
    withExtendedCtx,
  )
where

import Control.Arrow ((>>>))
import Control.Monad.Cont qualified as Cont
import Control.Monad.State qualified as State
import Data.EDN qualified as EDN
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import GHC.Generics (Generic)
import Generics.Deriving.Monoid ( mappenddefault, memptydefault )
import LS.XPile.Edn.Context (Context (..), (<++>), HasContext (..))
import Data.Coerce (coerce)
import Flow ((|>))
import LS.XPile.Edn.MessageLog (MessageLog, HasMessageLog (..), Severity, MessageData)

newtype CPSTranspileM t
  = CPSTranspileM (Cont.ContT EDN.TaggedValue (State.State TranspileState) t)
  deriving
    ( Functor,
      Applicative,
      Monad,
      State.MonadState TranspileState,
      Cont.MonadCont
    )

data TranspileState = TranspileState
  { context :: Context,
    messageLog :: MessageLog
  }
  deriving Generic

instance Semigroup TranspileState where
  (<>) = mappenddefault

instance Monoid TranspileState where
  mempty = memptydefault

instance HasContext TranspileState where
  vars <++> state@TranspileState {context} =
    state {context = vars <++> context}
  
  symbol !? TranspileState {context} = symbol !? context

instance HasMessageLog TranspileState where
  logMsg severity messageData state@TranspileState {messageLog} =
    state {messageLog = logMsg severity messageData messageLog}

  getMsgs TranspileState {messageLog} = getMsgs messageLog

logTranspileMsg ::
  (State.MonadState a m, HasMessageLog a) => Severity -> MessageData -> m ()
logTranspileMsg severity messageData = do
  state <- State.get
  state |> logMsg severity messageData |> State.put

runCPSTranspileM :: CPSTranspileM EDN.TaggedValue -> EDN.TaggedValue
runCPSTranspileM = coerce' >>> Cont.evalContT >>> flip State.evalState mempty
  where
    coerce' ::
      CPSTranspileM EDN.TaggedValue ->
      Cont.ContT EDN.TaggedValue (State.State TranspileState) EDN.TaggedValue
    coerce' = coerce

-- Resume a suspended computation (captured in a continuation), with the
-- context temporarily extended with some variables.
withExtendedCtx :: Foldable t => t T.Text -> CPSTranspileM a -> CPSTranspileM a
withExtendedCtx vars cont = do
  -- Save the current context.
  state@TranspileState {context} <- State.get
  -- Extend context with vars.
  state |> (vars <++>) |> State.put
  -- Resume the suspended computation.
  result <- cont
  -- Restore the old context which we saved.
  State.put state {context = context}
  -- Return the result of the computation.
  pure result