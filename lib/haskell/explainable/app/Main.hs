module Main (main) where

import Explainable.Lib
import qualified Data.Map as Map
import Control.Monad.Trans.State
import Control.Monad (when)

main :: IO ()
main = do
  when True  $ runTests_Mathlang
  when False $ runTests_Taxes

-- if 3 > 2 then 5 else 6




