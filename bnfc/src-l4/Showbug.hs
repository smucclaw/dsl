{-# LANGUAGE QuasiQuotes                      #-}

module Showbug where

import System.Environment ( getArgs )
import System.Exit        ( exitFailure, exitSuccess )
import Data.Maybe         ( catMaybes, listToMaybe )
import Text.RE.PCRE ( matchedText, matches, (*=~), (?=~), re )

-- when you run l4 test1.l4 > stdoutfile 2> stderrfile
-- you might get "syntax error at line M, column N before ...
-- so, if you call showbug origfile stdoutfile stderrfile
-- you will get a view of the origfile where the error message complains.
-- note: what ought to be stderr is in stdout

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [origfile, stdoutfile, stderrfile] -> do
      stderr <- readFile stderrfile
      stdout <- readFile stdoutfile
      orig   <- readFile origfile
      let coords = showErrorCoordinates orig $ stdout ++ stderr
      case (coords, stderr) of
        (Nothing,[]) ->       putStr   stdout  >> exitSuccess
        (Nothing,_)  ->       putStr   stderr  >> exitFailure
        (Just xy,_)  -> mapM_ putStrLn xy      >> exitFailure
    [origfile] -> do
      orig  <- readFile origfile
      input <- getContents
      let coords = showErrorCoordinates orig input
      case (coords, input) of
        (Nothing,_)  ->       putStr   input >> exitSuccess
        (Just xy,_)  -> mapM_ putStrLn xy    >> exitFailure
    _ -> usage


-- | showErrorCoordinates origfile input shows the code surrounding errors from input
showErrorCoordinates :: String -> String -> Maybe [String]
showErrorCoordinates origlines input = showAtCoords (lines origlines) <$> getErrorCoordinates input

usage :: IO ()
usage = do
  putStrLn "$ showbug test1.l4 out/test1.out out/test1.err"
  putStrLn "$ l4 test1.l4 | showbug test1.l4"

getErrorCoordinates :: (Read a, Read b) => String -> Maybe (a, b, String)
getErrorCoordinates input =
  listToMaybe $ catMaybes $ runTest <$>
  [([re|syntax error at line \d+, column \d+.*|], [re|\d+|], id)
  ,([re|:.*Err \(\w+ \d+ \d+ \d+\).*|],                  [re|\d+|], drop 1) ]
  where
    runTest (test, extraction, postProcess) = do
      mt <- matchedText $ input ?=~ test
      case postProcess (matches $ mt *=~ extraction) of
        (x:y:z)   -> return (read x, read y, mt)
        otherwise -> Nothing

showAtCoords :: [String] -> (Int, Int, String) -> [String]
showAtCoords origlines (line,col,errormessage) =
  [ "* " ++ errormessage
  , "| " ++ origlines !! (line-2)
  , "| " ++ origlines !! (line-1)
  , "  " ++ nspaces (col - 1) ++ "^"
  , "| " ++ (origlines++[""]) !! line
  ]
  where
    nspaces n = replicate n ' '

