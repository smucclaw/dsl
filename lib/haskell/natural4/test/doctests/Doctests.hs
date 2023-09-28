module Main where

-- base
import System.Environment (getArgs)

-- doctest-parallel
import Test.DocTest (mainFromCabal)

main :: IO ()
main = do
  mainFromCabal "natural4" =<< getArgs

