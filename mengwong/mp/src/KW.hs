
module KW where

data Keyword a = Every
               | Who    | Which
               | And    | Or       | Unless
               | Must   | May      | Shant
               | Arrow  | Other a
