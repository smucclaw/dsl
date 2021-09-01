module AnyAll
    ( module AnyAll.Types
    , module AnyAll.SVG
    ) where

import AnyAll.Types
import AnyAll.SVG

someFunc :: IO ()
someFunc = print $ All (Pre "toplevel pre string") [ Leaf "child 1"
                                                   , Leaf "child 2" ]
