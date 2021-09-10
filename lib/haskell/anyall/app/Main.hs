{-# LANGUAGE OverloadedStrings #-}

module Main where

import AnyAll
import qualified Data.Map.Strict        as Map
import qualified Data.Text.Lazy         as TL
import           Control.Monad (forM_)


main :: IO ()
main = forM_
  [ Map.empty
  , Map.fromList [("walk",  Right $ Just True)
                 ,("run",   Right $ Just True)
                 ,("eat",   Right $ Just True)
                 ,("drink", Right $ Just True)]
  ] $ ppQTree (All (Pre "both")
               [ Leaf "walk"
               , Any (Pre "either")
                 [ Leaf "eat"
                 , Leaf "drink" ] ])


