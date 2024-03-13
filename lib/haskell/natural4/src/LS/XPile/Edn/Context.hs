{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.Edn.Context
  ( Context,
    (!?),
    emptyContext,
    withExtendedCtx,
  )
where

import Control.Monad.State qualified as State
import Data.Coerce (coerce)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.String (IsString)
import Data.String.Interpolate (i)
import Data.String.Interpolate.Conversion (Interpolatable, IsCustomSink)
import Data.Text qualified as T
import Flow ((|>))
import GHC.IsList (IsList)

newtype Context = Context { context :: HashSet T.Text }
  deriving (Eq, Show, IsList)

emptyContext :: Context
emptyContext = []

(<++>) :: Foldable t => Context -> t T.Text -> Context
(coerce -> ctx) <++> vars = vars |> foldr HashSet.insert ctx |> coerce

(!?) :: Context -> T.Text -> Bool
(coerce -> ctx) !? var = var `HashSet.member` ctx

-- Perform a monad action with a context temporarily extended with some variables.
withExtendedCtx ::
  (State.MonadState Context m, Foldable t) => t T.Text -> m b -> m b
withExtendedCtx vars action = do
  -- Get current context.
  oldCtx <- State.get
  -- Add vars to context.
  State.put $ oldCtx <++> vars
  -- Run computation.
  action <- action
  -- Restore old context.
  State.put oldCtx
  -- Return result of computation.
  pure action