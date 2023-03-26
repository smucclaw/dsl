module Main (main) where

import Lib
import Genre.Insurance
import Control.Monad (forM_)

main :: IO ()
main = do
  orgMain
  let mypolicy = mkPolicy policyTemplate2020 -- buying a policy now
                 pa        -- they choose plan A out of A--F
                 (Just ()) -- yes they want the supplementary EN plan
                 (Just p1) -- yes they want the supplementary SP plan, choosing plan 1 out of 1--4
  print mypolicy
  forM_ [exampleSc1, exampleSc2] $ \sc -> do
    putStrLn "* how much is claimable under this scenario"
    print sc
    putStrLn $ "** under benefit NQQ, sum assured is" ++ show (piNQQ mypolicy)
    putStrLn "the modifiers are"
    print $ pmodaf $ ptNQQ policyTemplate2020
    putStrLn "** result:"
    print $ claimable policyTemplate2020 mypolicy sc piNQQ (pmodaf . ptNQQ)

