{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.Edn.Context
  ( Context,
    (<++>),
    (!?)
  )
where

import Control.Monad.State qualified as State
import Data.Coerce (coerce)
import Data.HashSet qualified as HashSet
import Data.Text qualified as T
import Flow ((|>))

newtype Context = Context {context :: HashSet.HashSet T.Text}
  deriving (Eq, Ord, Show, Semigroup, Monoid)

-- class IsContext t where
--   (<++>) :: Foldable m => m T.Text -> t -> t
--   (!?) :: T.Text -> t -> Bool

-- instance IsContext Context where

(<++>) :: Foldable m => m T.Text -> Context -> Context
vars <++> (coerce -> context) = vars |> foldr HashSet.insert context |> coerce

(!?) :: T.Text -> Context -> Bool
symbol !? (coerce -> context) = symbol `HashSet.member` context