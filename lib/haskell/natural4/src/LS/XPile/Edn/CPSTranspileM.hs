{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module LS.XPile.Edn.CPSTranspileM
  ( CPSTranspileM,
    TranspileResult (..),
    logTranspileMsg,
    logTranspiledTo,
    runCPSTranspileM,
    edn,
    messageLog,
  )
where

import Control.Arrow ((>>>))
import Control.Monad.Cont qualified as Cont
import Control.Monad.Reader qualified as Reader
import Control.Monad.State.Strict qualified as State
import Data.Coerce (coerce)
import Data.EDN qualified as EDN
import Data.Hashable (Hashable)
import Data.Text qualified as T
import Flow ((|>))
import GHC.Generics (Generic)
import Generics.Deriving.Monoid (mappenddefault, memptydefault)
import LS.XPile.Edn.Ast (AstNode)
import LS.XPile.Edn.Context (Context, IsContext (..), (<++>))
import LS.XPile.Edn.MessageLog
  ( IsMessageLog (..),
    MessageData (..),
    MessageLog,
    Severity (..),
  )
import Optics qualified
import Optics.TH (camelCaseFields, makeLensesWith)
import Data.Bifunctor (Bifunctor(..))

-- data TranspileState metadata = TranspileState
--   { transpileStateContext :: Context,
--     transpileStateMessageLog :: MessageLog metadata
--   }
--   deriving (Eq, Show, Generic, Hashable)

-- makeLensesWith camelCaseFields ''TranspileState

-- instance Semigroup (TranspileState metadata) where
--   (<>) = mappenddefault

-- instance Monoid (TranspileState metadata) where
--   mempty = memptydefault

-- instance IsContext (TranspileState metadata) where
--   (<++>) vars = Optics.over context (vars <++>)
--   (!?) symbol = Optics.view context >>> (symbol !?)

-- instance IsMessageLog TranspileState metadata where
--   logMsg severity = Optics.over messageLog . logMsg severity
--   getMsgs = Optics.view messageLog >>> getMsgs

newtype CPSTranspileM metadata t
  = CPSTranspileM
  {cpsTranspileM :: Cont.ContT EDN.TaggedValue (Reader.ReaderT Context (State.State (MessageLog metadata))) t}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      Reader.MonadReader Context,
      State.MonadState (MessageLog metadata)
    )

logTranspileMsg ::
  Severity -> MessageData metadata -> CPSTranspileM metadata ()
logTranspileMsg severity = logMsg severity >>> State.modify

logTranspiledTo ::
  AstNode metadata -> EDN.TaggedValue -> CPSTranspileM metadata ()
logTranspiledTo astNode result =
  TranspiledTo
    { messageDataAstNode = astNode,
      messageDataResult = EDN.renderText result
    }
    |> logTranspileMsg Info

data TranspileResult metadata = TranspileResult
  { transpileResultEdn :: T.Text,
    transpileResultMessageLog :: MessageLog metadata
  }
  deriving (Eq, Show, Generic)

makeLensesWith camelCaseFields ''TranspileResult

runCPSTranspileM ::
  CPSTranspileM metadata EDN.TaggedValue -> TranspileResult metadata
runCPSTranspileM =
  coerce
    >>> Cont.evalContT
    >>> runMempty @Context Reader.runReaderT
    >>> runMempty State.runState
    >>> first EDN.renderText
    >>> uncurry TranspileResult
    where
      runMempty :: forall b a c. Monoid b => (a -> b -> c) -> a -> c
      runMempty f = flip f mempty