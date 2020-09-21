{-# LANGUAGE QuasiQuotes                      #-}

module Main where

import System.Environment ( getArgs, getProgName )
import System.Exit        ( exitFailure, exitSuccess )
import System.IO
import Control.Monad      ( when )
import Data.Maybe         ( fromMaybe, catMaybes, listToMaybe )
import Text.RE.PCRE
import Data.Function

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
      orig   <- return.lines =<< readFile origfile
      let coords = getErrorCoordinates $ stdout ++ stderr
      case (coords, stderr) of
        (Nothing,[]) ->      putStr   stdout                 >> exitSuccess
        (Nothing,_)  ->      putStr   stderr                 >> exitFailure
        (Just xy,_)  -> mapM putStrLn (showAtCoords orig xy) >> exitFailure
    [origfile] -> do
      orig  <- return.lines =<< readFile origfile
      input <- getContents
      let coords = getErrorCoordinates $ input
      case (coords, input) of
        (Nothing,_)  ->      putStr   input                  >> exitSuccess
        (Just xy,_)  -> mapM putStrLn (showAtCoords orig xy) >> exitFailure
      

usage = do
  putStrLn "$ showbug test1.l4 out/test1.out out/test1.err"
  putStrLn "$ l4 test1.l4 | showbug test1.l4"
      
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

showAtCoords origlines (line,col,errormessage) =
  [ "* " ++ errormessage
  , "| " ++ origlines !! (line-2)
  , "| " ++ origlines !! (line-1)
  , "  " ++ (nspaces $ col - 1) ++ "^"
  , "| " ++ (origlines++[""]) !! (line-0)
  ]
  where
    nspaces n = take n $ repeat ' '

