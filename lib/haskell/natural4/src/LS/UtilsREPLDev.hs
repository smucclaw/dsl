{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-|
Simple utils / convenience functions for prototyping / dev-ing at the REPL.
-}


module LS.UtilsREPLDev
  ( filesInDirWithExtn,
    findFileByNameInDir,
    csvsInDir,
    l4csv2rules,
    leTestcasesDir,
    leTestCSVs
  )
where

import Flow ((|>))
import Data.Maybe (fromMaybe, listToMaybe)

import System.FilePath ((</>))
import System.FilePath.Find
  ( always,
    fileName,
    find,
    (==?),
    extension
  )

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
Util function for getting rules from a L4 CSV filepath
Meant to be for interactive use at the REPL by a dev, and not for production apps
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


------- TODO: LE-specific things; to be moved into a better place when I have more time
-- | Convenient, tho not the best practice to put this in this module
leTestcasesDir :: FilePath
leTestcasesDir = "test" </> "Testcases" </> "LogicalEnglish"

-- | Returns a list of csvs in the LE subdir of test/Testcases
leTestCSVs :: IO [FilePath]
leTestCSVs = csvsInDir leTestcasesDir