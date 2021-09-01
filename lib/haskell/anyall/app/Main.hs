module Main where

import AnyAll

main :: IO ()
main = print $ All (Pre "toplevel pre string") [ Leaf "child 1"
                                               , Leaf "child 2" ]
