{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DerivingVia #-}

module LS.Utils
  ( (|$>),
    maybe2validate,
    mapThenSwallowErrs,
    swallowErrs
  )
where

import Control.Monad.Validate
  ( MonadValidate (refute),
    Validate,
    runValidate,
  )
import Data.Coerce (coerce)
import Data.Either (rights)
import Data.Monoid (Ap (Ap))
import Flow ((|>))

infixl 0 |$>

(|$>) :: Functor f => f a -> (a -> b) -> f b
(|$>) = flip fmap

{-
  The idea is that given a (Validate e :: Type -> Type) and a monoid
  (a :: Type), we want to equip (Validate e a) with a monoid
  structure that combines errors (~ lefts) using the applicative operator <*> of
  (Validate e) and non-errorneous values (~ rights) using the monoid <> of a.

  We use the Ap newtype as defined in Data.Monoid for this.
  Suppose (m :: Type -> Type) is an applicative type constructor and
  (a :: Type) is a monoid.
  (Ap m a) lifts the monoid structure of a up into the applicative m, so that
  the <> operator of (Ap m a) behaves as a combination of <*> on m and <> on a.

  In the case of m = Validate e, the <> op of
    (Ap (Validate e) a) ~ Validate e a
  combines errors using <*> and non-erroneous values using the <> of a.

  Now, this standalone deriving via thing enables (Ap (Validate e)) to inherit the
  MonadValidate structure of (Validate e), so that we can refute (~ throwError)
  directly into (Ap (Validate e) a) without needing to first refute into
  (Validate e a) and then use an Ap constructor to lift it to the newtype Ap.
-}
deriving via Validate e instance
  Semigroup e => MonadValidate e (Ap (Validate e))

maybe2validate :: MonadValidate e m => e -> Maybe a -> m a
maybe2validate errVal = maybe (refute errVal) pure

mapThenSwallowErrs :: (a -> Ap (Validate e) b) -> [a] -> [b]
mapThenSwallowErrs f xs = xs |$> f |> coerce |$> runValidate |> rights

swallowErrs :: [Ap (Validate a) b] -> [b]
swallowErrs = mapThenSwallowErrs id