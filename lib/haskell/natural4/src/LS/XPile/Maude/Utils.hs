{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LS.XPile.Maude.Utils where

import Data.Foldable qualified as Fold
import Data.Monoid (Ap (Ap))
import Data.Text qualified as T
import Flow ((|>))
import LS.Types (MTExpr, mt2text)
import Prettyprinter (Doc, Pretty (pretty))

-- Common utilities
{-
  The idea is that given a (MonadError s m) and a monoid (a :: Type), we want
  to operate on (m a :: Type) as if it were also a monoid.
  Here, we often take (m = Either (Doc ann)) and (a = Maybe (Doc ann)) and we
  want to utilize the (<>) and mempty of (Maybe (Doc ann)), lifted up into
  the Either.

  Suppose (m :: Type -> Type) is a type constructor and (a :: Type) is a monoid.
  Then Data.Monoid defines (newtype Ap m a) which lifts the monoid structure of
  a up into the applicative m.
  More concretely, (Ap m :: Type -> Type) inherits both the applicative
  structure of m and the monoid structure of its input type argument, with:
  - (<>) = liftA2 (<>)
  - mempty = pure mempty
  Moreover, if m is also a monad, then (Ap m) also inherits this monad
  structure.
  The standalone deriving via thing below enables (Ap m) to inherit the
  MonadError instance of m should m also be a MonadError.

  TODO:
  Note that for (m = Either s), the monoid structure of (Ap m a) is
  a bit stupid in that Lefts are left-absorbing under (<>), that is
    Ap (Left x) <> Ap _ = Ap (Left x)
  Thus it may be better to use (m a) directly and manually define a monoid
  structure for that rather than use (Ap m a).

  See http://www.staff.city.ac.uk/~ross/papers/Applicative.pdf
-}
-- deriving via
--   m :: Type -> Type
--   instance
--     MonadError s m => MonadError s (Ap m)

throwDefaultErr :: Ap (Either (Doc ann)) a
throwDefaultErr = Ap $ Left "Not supported."

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