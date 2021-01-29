module Main where

import Lib

main :: IO ()
main = do
  getContents >>= someFunc
