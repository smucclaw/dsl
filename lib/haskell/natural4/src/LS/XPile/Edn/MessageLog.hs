{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.Edn.MessageLog
  ( Severity (..),
    Message (..),
    MessageData (..),
    MessageLog,
    IsMessageLog (..),
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
import Optics.TH (camelCaseFields, makeLensesWith)

data Severity
  = Info
  | Warning
  | Error
  deriving (Eq, Ord, Show, Generic, Hashable)

data MessageData metadata = TranspiledTo
  { messageDataAstNode :: AstNode metadata,
    messageDataResult :: T.Text
  }
  deriving (Eq, Ord, Show, Generic, Hashable)

makeLensesWith camelCaseFields ''MessageData

data Message metadata = Message
  { messageSeverity :: Severity,
    messageData' :: MessageData metadata
  }
  deriving (Eq, Ord, Show, Generic, Hashable)

makeLensesWith camelCaseFields ''Message

newtype MessageLog metadata
  = MessageLog {messageLog :: Deque.Deque (Message metadata)}
  deriving (Eq, Show, Generic)
  deriving newtype Hashable
  deriving (Semigroup, Monoid) via Deque.Deque (Message metadata)

class IsMessageLog t metadata where
  logMsg :: Severity -> MessageData metadata -> t metadata -> t metadata
  getMsgs :: t metadata -> Deque.Deque (Message metadata)

instance IsMessageLog MessageLog metadata where
  logMsg severity data' =
    coerce
      >>> Deque.snoc
        Message {messageSeverity = severity, messageData' = data'}
      >>> coerce

  getMsgs = coerce