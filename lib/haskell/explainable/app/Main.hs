module Main (main) where

import Control.Monad (when)
import Explainable.Lib (runTests_Mathlang)

main :: IO ()
main = do
  when True runTests_Mathlang

-- if 3 > 2 then 5 else 6




