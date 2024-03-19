module LS.XPile.Edn.CPSTranspileM
  ( CPSTranspileM,
    TranspileResult (..),
    logTranspileMsg,
    logTranspiledTo,
    runCPSTranspileM,
  )
where

import Control.Arrow ((>>>))
import Control.Monad.Cont qualified as Cont
import Control.Monad.Reader qualified as Reader
import Control.Monad.State.Strict qualified as State
import Data.Bifunctor (Bifunctor (..))
import Data.EDN qualified as EDN
import Data.Hashable (Hashable)
import Data.Text qualified as T
import Flow ((|>))
import GHC.Generics (Generic)
import LS.XPile.Edn.Ast (AstNode)
import LS.XPile.Edn.Context (Context, (<++>))
import LS.XPile.Edn.MessageLog
  ( MessageData (..),
    MessageLog,
    Severity (..),
    logMsg,
  )
-- import Optics qualified
-- import Optics.TH (camelCaseFields, makeLensesWith)

type CPSTranspileM metadata t =
  Cont.ContT
    EDN.TaggedValue
    (Reader.ReaderT Context (State.State (MessageLog metadata)))
    t

logTranspileMsg ::
  Severity -> MessageData metadata -> CPSTranspileM metadata ()
logTranspileMsg severity = State.modify . logMsg severity

logTranspiledTo ::
  AstNode metadata -> EDN.TaggedValue -> CPSTranspileM metadata ()
logTranspiledTo astNode result =
  logTranspileMsg
    Info
    TranspiledTo
      { messageDataAstNode = astNode,
        messageDataResult = EDN.renderText result
      }

-- type TranspileResult metadata = (T.Text, MessageLog metadata)

-- pattern TranspileResult ::
--   T.Text -> MessageLog metadata -> TranspileResult metadata
-- pattern TranspileResult {text, messageLog} = (text, messageLog)

data TranspileResult metadata = TranspileResult
  { ednText :: T.Text,
    messageLog :: MessageLog metadata
  }
  deriving Show

-- makeLensesWith camelCaseFields ''TranspileResult

runCPSTranspileM ::
  CPSTranspileM metadata EDN.TaggedValue -> TranspileResult metadata
runCPSTranspileM =
  Cont.evalContT
    >>> runMempty Reader.runReaderT
    >>> runMempty State.runState
    >>> first EDN.renderText
    >>> uncurry TranspileResult
  where
    runMempty f = flip f mempty