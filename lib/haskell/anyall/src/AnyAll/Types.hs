module AnyAll.Types where

data Label =
    Pre String
  | PrePost String String
  deriving Show

data Item =
    Leaf String
  | All Label [Item]
  | Any Label [Item]
  deriving Show

