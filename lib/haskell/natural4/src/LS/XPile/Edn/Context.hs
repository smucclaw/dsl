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
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable)
import Data.String (IsString)
import Data.String.Interpolate (i)
import Data.String.Interpolate.Conversion (Interpolatable, IsCustomSink)
import Data.Text qualified as T
import Flow ((|>))
import GHC.Generics (Generic)
import GHC.IsList (IsList)

newtype Context = Context {context :: HashSet T.Text}
  deriving (Eq, Show, Generic, Hashable, IsList, Semigroup, Monoid)

-- class IsContext t where
--   (<++>) :: Foldable m => m T.Text -> t -> t
--   (!?) :: T.Text -> t -> Bool

-- instance IsContext Context where

(<++>) :: Foldable m => m T.Text -> Context -> Context
vars <++> (coerce -> context) = vars |> foldr HashSet.insert context |> coerce

(!?) :: T.Text -> Context -> Bool
symbol !? (coerce -> context) = symbol `HashSet.member` context