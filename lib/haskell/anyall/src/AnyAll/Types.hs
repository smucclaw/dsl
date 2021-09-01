module AnyAll.Types where

data Label a =
    Pre a
  | PrePost a a
  deriving Show

data Item a =
    Leaf a
  | All (Label a) [Item a]
  | Any (Label a) [Item a]
  deriving Show

