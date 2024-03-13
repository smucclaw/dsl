{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ViewPatterns #-}

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
import Data.Foldable qualified as Fold
import Data.Hashable (Hashable)
import Data.Sequence qualified as Seq
import GHC.Generics (Generic)

data Severity
  = Info
  | Warning
  | Error
  deriving (Eq, Ord, Show, Generic, Hashable)

data MessageData
  deriving (Eq, Ord, Show, Generic, Hashable)

data Message  = Message
  { severity :: Severity,
    messageData :: MessageData
  }
  deriving (Eq, Ord, Show, Generic, Hashable)

newtype MessageLog = MessageLog (Seq.Seq Message)
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass Hashable
  deriving Semigroup via Seq.Seq Message
  deriving Monoid via Seq.Seq Message

class HasMessageLog t where
  logMsg :: Severity -> MessageData -> t -> t
  getMsgs :: t -> [Message]

instance HasMessageLog MessageLog where
  logMsg severity messageData =
    coerce >>> (Seq.|> Message {severity, messageData}) >>> coerce

  getMsgs (coerce -> log :: Seq.Seq Message) = Fold.toList log