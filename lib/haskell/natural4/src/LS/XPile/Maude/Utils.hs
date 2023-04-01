{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module LS.XPile.Maude.Utils
  ( (|$>),
    throwDefaultErr,
    multiExprs2qid,
    text2qid,
    IsList
  )
where

import Control.Monad.Validate (MonadValidate (refute), Validate, runValidate)
import Data.Coerce (coerce)
import Data.Foldable qualified as Fold
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid (Ap (Ap))
import Data.Text qualified as T
import Data.Type.Bool (type (||))
import Data.Type.Equality (type (==))
import Flow ((|>))
import LS.Types (MTExpr, mt2text)
import Prettyprinter (Doc, Pretty (pretty))

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

throwDefaultErr :: Ap (Validate (Doc ann)) a
throwDefaultErr = refute "Not supported."

infixl 0 |$>

(|$>) :: Functor f => f a -> (a -> b) -> f b
(|$>) = flip fmap

type IsList t = (Foldable t, (t == NonEmpty || t == []) ~ True)

{-
  Note that (mt2text :: MultiTerm -> Text) and that Multiterm = [MTExpr], but
  sometimes we want to apply this function not just to MultiTerm but to
  NonEmpty MTExpr.
-}
multiExprs2qid :: IsList t => t MTExpr -> Doc ann
multiExprs2qid multiExprs = multiExprs |> Fold.toList |> mt2text |> text2qid

text2qid :: T.Text -> Doc ann
text2qid x = ["qid(\"", x, "\")"] |> mconcat |> pretty

-- test :: Either (Doc ann) (Doc ann)
-- test =
--   -- throwDefaultErr <> throwDefaultErr
--   pure "abc" <> pure "def"
--     |> (coerce :: Ap (Validate a) b -> Validate a b)
--     |> runValidate

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