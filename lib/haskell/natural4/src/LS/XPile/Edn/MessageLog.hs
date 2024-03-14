{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module LS.XPile.Edn.MessageLog
  ( Severity (..),
    Message (..),
    MessageData (..),
    MessageLog,
    HasMessageLog (..),
  )
where

import Control.Arrow ((>>>))
import Data.Coerce (coerce)
import Data.EDN qualified as EDN
import Data.Foldable qualified as Fold
import Data.Hashable (Hashable)
import Data.Text qualified as T
import Deque.Strict qualified as Deque
import GHC.Generics (Generic)
import LS.XPile.Edn.Ast (AstNode)

data Severity
  = Info
  | Warning
  | Error
  deriving (Eq, Ord, Show, Generic, Hashable)

data MessageData metadata
  = TranspiledTo {astNode :: AstNode metadata, result :: T.Text}
  deriving (Eq, Ord, Show, Generic, Hashable)

data Message metadata = Message
  { severity :: Severity,
    messageData :: MessageData metadata
  }
  deriving (Eq, Ord, Show, Generic, Hashable)

newtype MessageLog metadata
  = MessageLog {messageLog :: Deque.Deque (Message metadata)}
  deriving (Eq, Show, Generic)
  deriving newtype Hashable
  deriving (Semigroup, Monoid) via Deque.Deque (Message metadata)

class HasMessageLog t metadata where
  logMsg :: Severity -> MessageData metadata -> t metadata -> t metadata
  getMsgs :: t metadata -> Deque.Deque (Message metadata)

instance HasMessageLog MessageLog metadata where
  logMsg severity messageData =
    coerce >>> Deque.snoc Message {severity, messageData} >>> coerce

  getMsgs = coerce