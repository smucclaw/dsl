module Main (main) where

import Lib
import Genre.Insurance
import Control.Monad (forM_)

-- | There are two things going on in this application.
--
-- First, we parse and print an entire org file containing a policy
-- contract. We don't actually process this very much, this is just
-- the first part of an extraction work task. The latter parts haven't
-- been done yet, but this re-presentation is intended to make it
-- easier to edit by hand.
--
-- Second, we exercise a simple algebra-driven representation of the
-- policy contract, manually programmed. The policy template and
-- choice of policy instance is all hardcoded here. We try out a
-- couple of hardcoded scenarios and observe that the benefits are
-- correctly selected out of the instance and template, and correctly
-- modified.

main :: IO ()
main = do
  orgMain
  let mypolicy = mkPolicy policyTemplate2020 -- the customer bought a policy in 2020
                 pa        -- they chose plan A out of A--F
                 (Just ()) -- yes they wanted the supplementary EN plan
                 (Just p1) -- yes they wanted the supplementary SP plan, choosing plan 1 out of 1--4
  putStrLn "* the customer has purchased this policy"
  printHS mypolicy
  forM_ [exampleSc1, exampleSc2] $ \sc -> do
    putStrLn "* how much is claimable under this scenario?"
    printHS sc
    putStrLn $ "** under benefit NQQ, sum assured is " ++ show (piNQQ mypolicy)
    putStrLn "the modifiers are"
    printHS $ pmodaf $ ptNQQ policyTemplate2020
    putStrLn $ "** result: " ++ show (claimable policyTemplate2020 mypolicy sc piNQQ (pmodaf . ptNQQ))

-- | helper function to format Haskell source for org-mode
srcHS :: String -> String
srcHS x = unlines [ "#+BEGIN_SRC haskell", x, "#+END_SRC" ]

-- | more helper function to print the formatted Haskell source
printHS :: (Show a) => a -> IO ()
printHS = putStrLn . srcHS . show
