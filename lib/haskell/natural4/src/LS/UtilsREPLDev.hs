{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-|
Simple utils / convenience functions for prototyping / dev-ing at the REPL.
-}


module LS.UtilsREPLDev
  ( StartDir, BaseFileName,
    pPrint, -- reexport
    filesInDirWithExtn,
    findFileByNameInDir,
    csvsInDir,
    l4csv2rules,
    pRules
  )
where

import Flow ((|>))
import Data.Maybe (fromMaybe, listToMaybe)

import System.FilePath ((</>), takeFileName)
import System.FilePath.Find
  ( always,
    fileName,
    find,
    (==?),
    extension
  )
import Text.Pretty.Simple   ( pShowNoColor, pPrint )

import LS qualified
import LS.Lib (NoLabel (..), Opts (..))
import LS.Utils ((|$>))


-- Getting file paths

type FileExtn = String
type BaseFileName = String
type StartDir = String

filesInDirWithExtn :: FileExtn -> StartDir -> IO [FilePath]
filesInDirWithExtn fextn = find always (extension ==? fextn)

csvsInDir :: StartDir -> IO [FilePath]
csvsInDir = filesInDirWithExtn ".csv"

{-| 
==== __Examples__ 
>>> findFileByNameInDir leTestcasesDir "indentation-databreach.csv"
Just "test/Testcases/LogicalEnglish/indentation-propn-databreach/indentation-databreach.csv"
-}
findFileByNameInDir :: StartDir -> BaseFileName -> IO (Maybe FilePath)
findFileByNameInDir startdir basefnm =
  startdir
      |> find always (fileName ==? basefnm)
      |$> listToMaybe


-- Getting rules from L4 CSVs
--- TODO: Would be convenient to also have a function that allows you to match just on a part of the base filename

{-| 
Util function for getting raw rules from a L4 CSV filepath
Not meant for production apps!
Adapted from Joe's code for testing LP Programs
==== __Examples__ 
>>> l4csv2rules "test/Testcases/LogicalEnglish/" "indentation-databreach.csv"
-}
l4csv2rules :: StartDir -> BaseFileName -> IO [LS.Rule]
l4csv2rules startdir csvFpath =
  findFileByNameInDir startdir csvFpath >>=
    \case
      Nothing -> error "Can't find file"
        -- remember, this is meant to be for internal use by a developer in the REPL
      Just file -> LS.dumpRules
                          Opts
                            { file = NoLabel [file],
                              dbug = False,
                              dstream = False
                            }

{-| 
Util function for __pretty printing (in color)__ raw rules from a L4 CSV filepath
==== __Examples__ 
>>> pRules "test/Testcases/LogicalEnglish/" "indentation-databreach.csv"
>>> pRules "test/Testcases/" "motor-insurance-1.csv"
-}
pRules :: StartDir -> BaseFileName -> IO ()
pRules startdir csvFpath = l4csv2rules startdir csvFpath >>= pPrint