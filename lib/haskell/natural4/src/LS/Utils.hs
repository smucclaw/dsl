{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module LS.Utils
  ( (|$>),
    maybe2validate,
    mapThenRunValidate,
    mapThenSwallowErrs,
    runMonoidValidate,
    swallowErrs,
    MonoidValidate,
    compose,
    (<||>),
    (<&&>)
  )
where

import Control.Applicative (liftA2)
import Control.Monad.Validate
  ( MonadValidate (refute),
    Validate,
    runValidate
  )
import Data.Coerce (coerce)
import Data.Either (rights, partitionEithers)
import Data.Monoid (Ap (Ap), Endo (Endo))
import Flow ((|>), (.>))

infixl 0 |$>

(|$>) :: Functor f => f a -> (a -> b) -> f b
(|$>) = flip fmap

{-# NOINLINE (|$>) #-}

{-# RULES
  "|$>" forall f g xs.  xs |$> f |$> g = xs |$> (g . f)
#-}

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

type MonoidValidate e a = Ap (Validate e) a

maybe2validate :: MonadValidate e m => e -> Maybe a -> m a
maybe2validate errVal = maybe (refute errVal) pure

mapThenRunValidate :: (a -> MonoidValidate e b) -> [a] -> [Either e b]
mapThenRunValidate f xs = xs |$> f |> coerce |$> runValidate

mapThenSwallowErrs :: (a -> MonoidValidate e b) -> [a] -> [b]
mapThenSwallowErrs f xs = xs |> mapThenRunValidate f |> rights

swallowErrs :: [MonoidValidate e a] -> [a]
swallowErrs = mapThenSwallowErrs id

runMonoidValidate :: MonoidValidate e a -> Either e a
runMonoidValidate x = x |> coerce |> runValidate 

-- | Function composition via the endomorphism monoid.
compose :: forall a. [a -> a] -> a -> a
compose = (coerce :: [a -> a] -> [Endo a]) .> mconcat .> coerce

-- | A simple lifted ('||'), copied from Control.Bool
(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
{-# INLINE (<||>) #-}

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)
{-# INLINE (<&&>) #-}