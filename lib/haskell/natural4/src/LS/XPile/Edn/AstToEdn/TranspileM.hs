{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module LS.XPile.Edn.AstToEdn.TranspileM
  ( TranspileM,
    TranspileResult (..),
    ednText,
    messageLog,
    logTranspileMsg,
    logTranspiledTo,
    runTranspileM,
  )
where

import Control.Arrow ((>>>))
import Control.Monad.Cont qualified as Cont
import Control.Monad.Reader qualified as Reader
import Control.Monad.State.Strict qualified as State
import Data.Bifunctor (Bifunctor (first))
import Data.EDN qualified as EDN
import Data.Hashable (Hashable)
import Data.Text qualified as T
import Flow ((|>))
import GHC.Generics (Generic)
import LS.XPile.Edn.Common.Ast (AstNode)
import LS.XPile.Edn.AstToEdn.MessageLog
  ( MessageData (..),
    MessageLog,
    Severity (..),
    logMsg,
  )
import Optics qualified
import Optics.TH (camelCaseFields, makeLensesWith)

type TranspileM metadata t = State.State (MessageLog metadata) t

logTranspileMsg ::
  Severity -> MessageData metadata -> TranspileM metadata ()
logTranspileMsg severity = State.modify . logMsg severity

logTranspiledTo ::
  AstNode metadata -> EDN.TaggedValue -> TranspileM metadata ()
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
  { transpileResultEdnText :: T.Text,
    transpileResultMessageLog :: MessageLog metadata
  }
  deriving Show

makeLensesWith camelCaseFields ''TranspileResult

runTranspileM ::
  TranspileM metadata EDN.TaggedValue -> TranspileResult metadata
runTranspileM =
  flip State.runState mempty
    >>> first EDN.renderText
    >>> uncurry TranspileResult