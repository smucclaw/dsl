module Matchable where

-- TODO: make this work!

class Matchable a where
  match  :: (Eq a, Foldable t) => a -> t a -> Bool

newtype ConjList a = ConjList [a]
newtype DisjList a = DisjList [a]

instance Matchable (ConjList a) where
  match x = all (== x)

instance Matchable (DisjList a) where
  match x = any (== x)
