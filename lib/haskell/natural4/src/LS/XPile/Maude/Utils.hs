{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module LS.XPile.Maude.Utils
  ( (|$>),
    throwDefaultErr,
    multiExprs2qid,
    text2qid,
    test
  )
where

import Control.Monad.Validate (MonadValidate (refute), Validate, runValidate)
import Data.Foldable qualified as Fold
import Data.Monoid (Ap (Ap))
import Data.Text qualified as T
import Flow ((|>))
import LS.Types (MTExpr, mt2text)
import Prettyprinter (Doc, Pretty (pretty))
import Data.Coerce (coerce)

{-
  The idea is that given a (MonadValidate e m) (say, Validate e) and a monoid
  (a :: Type), we want to equip (m a :: Type) (ie Validate e m) with a monoid
  structure that combines errors using the applicative operator <*> and
  non-errorneous values (~ rights) using the monoid <> of a.

  We use the Ap newtype as defined in Data.Monoid for this.
  Suppose (m :: Type -> Type) is an applicative type constructor and
  (a :: Type) is a monoid.
  (Ap m a) lifts the monoid structure of a up into the applicative m, so that
  the <> operator of (Ap m a) behaves as a combination of <*> on m and <> on a.

  In the case of m = Validate b, the <> op of
    (Ap (Validate b) a) ~ Validate b a
  combines errors using <*> and non-erroneous values using the <> of a.

  Now, this standalone deriving via thing enables (Ap m) to inherit the
  MonadValidate structure of m, so that we can refute (~ throwError)
  directly into (Ap (Validate b) a) without needing to first refute into
  (Validate b a) and then use an Ap constructor to lift it to the newtype Ap.
-}
deriving via
  m :: * -> *
  instance
    MonadValidate e m => MonadValidate e (Ap m)

throwDefaultErr :: Ap (Validate (Doc ann)) a
throwDefaultErr = refute "Not supported."

infixl 0 |$>

(|$>) :: Functor f => f a -> (a -> b) -> f b
(|$>) = flip fmap

{-
  Note that (mt2text :: MultiTerm -> Text) and that Multiterm = [MTExpr], but
  sometimes we want to apply this function not just to MultiTerm but to
  NonEmpty MTExpr.
  Hence, in the input type, we use (t MTExpr) with t being a Foldable, and then
  Foldable.toList in the function body.
-}
multiExprs2qid :: Foldable t => t MTExpr -> Doc ann
multiExprs2qid multiExprs = multiExprs |> Fold.toList |> mt2text |> text2qid

text2qid :: T.Text -> Doc ann
text2qid x = ["qid(\"", x, "\")"] |> mconcat |> pretty

test :: Either (Doc ann) (Doc ann)
test =
  -- throwDefaultErr <> throwDefaultErr
  pure "abc" <> pure "def"
    |> (coerce :: Ap (Validate a) b -> Validate a b)
    |> runValidate

-- {-# NOINLINE (|$>) #-}

-- {-# RULES
--   "fmap"    forall f g x. x |$> f |$> g = g . f <$> x
-- #-}

-- {-# RULES
--   "|$>" forall f g xs. xs |$> f |$> g = xs |$> (g . f)
-- #-}

-- safeHead :: (Applicative f, Monoid (f a)) => [a] -> f a
-- safeHead (x : _) = pure x
-- safeHead _ = mempty

-- traverseWith ::
--   (Foldable t1, Foldable t2, Applicative f) =>
--   (a1 -> a2 -> f b) ->
--   t1 a1 ->
--   t2 a2 ->
--   f [b]
-- traverseWith f xs ys =
--   zipWith f xs' ys' |> sequenceA
--   where
--     xs' = toList xs
--     ys' = toList ys

-- map where the function is also passed the index of the current element.
-- mapIndexed :: (Traversable t, Num s) => (s -> a -> b) -> t a -> t b
-- mapIndexed f xs = xs |> mapAccumL g 0 |> snd
--   where
--     g index val = (index + 1, f index val)

-- mapFirst pred f xs applies f to the first element of xs that satisfies pred.
-- mapFirst :: Traversable t => (b -> Bool) -> (b -> b) -> t b -> t b
-- mapFirst pred f xs = xs |> mapAccumL g False |> snd
--   where
--     g False elt@(pred -> True) = (True, f elt)
--     g seen elt = (seen, elt)

--- Like mapIndexed, but uses traverse/sequenceA for short-circuiting.
-- traverseIndexed ::
--   (Traversable t, Num s, Applicative f) =>
--   (s -> a -> f b) -> t a -> f (t b)
-- traverseIndexed f xs = xs |> mapIndexed f |> sequenceA

--- Like zipWith, but uses traverse/sequenceA for short-circuiting.