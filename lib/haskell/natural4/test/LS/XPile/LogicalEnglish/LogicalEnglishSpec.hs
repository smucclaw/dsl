{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
--  Module: LogicalEnglishSpec
--  Description: Defines a hspec Spec for testing the Logical English transpiler.
--
--  This module is responsible for running unit tests for the Logical English
--  transpiler.
--  To define a test case, one can create a new directory in
--  leTestcasesDir, like 'is-num' with the following structure:
--  - {leTestcasesDir}
--      - is-num
--          - is-num.csv
--          - expected.le
--          - config.yml
--  where:
--  - 'is-num.csv' is the name of the input natural4 csv file.
--  - 'expected.le' is a file containing the expected Logical English output.
--  - 'condig.yml' is a YAML configuration file for the unit test.
--
--  Note that:
--  - 'config.yml' should look something like:
--      description: "If IS is followed by a number, number should become *a number*"
--      enabled: true
--  - ie it should contain a string field called 'description', which gives
--    the description of the test case found in that directory, and
--    'enabled' is a boolean (ie 'true' or 'false') indicating if the test
--    case is currently enabled.
--    Test cases which are not enabled are skipped automatically by this module.
--
--  - The file names 'expected.le' and 'config.yml' are hardcoded
--    and so should not be varied
--  - The name of the parent directory and csv file can be changed, but should be
--    kept in sync with each other, ie if the parent directory is 'abc', then
--    the csv file should be called 'abc.csv'.
--
--  After creating a unit test, one can register it to be run by this module
--  by adding the name of the directory as an entry in
--  '{leTestcasesDir}/testcases.yml'.
--  Only those test cases which are listed in this file will be run.
--
--  When Logical English test cases are run via 'stack test', a new file called
--  'actual.le' will appear in the respective directories of each test case
--  that was run.
--  This file represents the actual output of the Logical English transpiler.
module LS.XPile.LogicalEnglish.LogicalEnglishSpec (spec) where

import Control.Monad (join)
import Data.Foldable (for_)
import Data.Maybe (listToMaybe)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Flow ((|>))
import GHC.Generics (Generic)
import LS (Rule)
import LS.Utils ((|$>))
import LS.XPile.LogicalEnglish (toLE)
import LS.XPile.LogicalEnglish.GoldenUtils (goldenLE)
import LS.XPile.LogicalEnglish.Testcase
import LS.XPile.LogicalEnglish.Utils (findWithDepth0)
import LS.XPile.LogicalEnglish.UtilsLEReplDev (leTestcasesDir, letestfnm2rules)
import Safe (tailSafe)
import System.FilePath (takeBaseName, (<.>), (</>))
import System.FilePath.Find
  ( FileType (Directory),
    fileType,
    (==?),
  )
import Test.Hspec (Spec, describe, it, runIO)

-- | The 'Spec' used to test the Logical English transpiler.
spec :: Spec
spec = describe "Logical English" $ do
  directories :: [FilePath] <-
    leTestcasesDir
      |>  findWithDepth0 (fileType ==? Directory)
      -- The first directory will always be leTestcasesDir itself, which is why
      -- we need to take the tail to get rid of it.
      |$> tailSafe
      |> runIO
  for_ directories $ \directory -> do
    directory </> "config.yml"
      |> configFile2spec |> runIO |> join