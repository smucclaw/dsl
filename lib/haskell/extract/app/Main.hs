module Main (main) where

import Lib
import Genre.Insurance

main :: IO ()
main = do
  orgMain
  let mypolicy = mkPolicy policyTemplate2020 -- buying a policy now
                 pa        -- they choose plan A out of A--F
                 (Just ()) -- yes they want the supplementary EN plan
                 (Just p1) -- yes they want the supplementary SP plan, choosing plan 1 out of 1--4
  print mypolicy

