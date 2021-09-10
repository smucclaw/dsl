{-# LANGUAGE OverloadedStrings #-}

module Main where

import AnyAll
import qualified Data.Map.Strict        as Map
import qualified Data.Text.Lazy         as TL
import           Control.Monad (forM_)


main :: IO ()
main = do
  forM_
    [ Map.empty
    , Map.fromList [("walk",  Left $ Just True)
                   ,("run",   Left $ Just True)
                   ,("eat",   Left $ Just True)
                   ,("drink", Left $ Just False)]
    , Map.fromList [("walk",  Left $ Just True)
                   ,("run",   Left $ Just False)
                   ,("eat",   Left $ Just True)
                   ,("drink", Left $ Just False)]
    , Map.fromList [("walk",  Right $ Just True)
                   ,("run",   Right $ Just True)
                   ,("eat",   Right $ Just True)
                   ,("drink", Left  $ Just False)]
    , Map.fromList [("walk",  Right $ Just True)
                   ,("run",   Left  $ Just False)
                   ,("eat",   Right $ Just True)
                   ,("drink", Right $ Just True)]
    , Map.fromList [("walk",  Right $ Just True)
                   ,("run",   Right $ Just False  )
                   ,("eat",   Right $ Just True)
                   ,("drink", Left  $ Just True)]
    ] $ ppQTree (All (Pre "all of")
                 [ Leaf "walk"
                 , Leaf "run"
                 , Any (Pre "either")
                   [ Leaf "eat"
                   , Leaf "drink" ] ])
  putStrLn "* LEGEND"
  putStrLn ""
  putStrLn "  <    >  View: UI should display this node or subtree."
  putStrLn "                Typically this marks either past user input or a computed value."
  putStrLn "  [    ]  Ask:  UI should ask user for input."
  putStrLn "                Without this input, we cannot make a hard decision."
  putStrLn "  (    )  Hide: UI can hide subtree or display it in a faded, grayed-out way."
  putStrLn "                This subtree has been made irrelevant by other input."
  putStrLn ""
  putStrLn "   YES    user input True"
  putStrLn "    NO    user input False"
  putStrLn "     ?    user input Unknown"
  putStrLn ""
  putStrLn "   yes    default True"
  putStrLn "    no    default False"
  putStrLn "          default Unknown"
  putStrLn ""
  putStrLn "  Hard means we ignore defaults and consider user input only."
  putStrLn "  Soft means we consider defaults as well to arrive at the answer."
