{-# LANGUAGE BlockArguments #-}

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
module TestLib (mkSpec, TestConfig (..)) where

import Control.Monad (join)
import Data.Foldable (for_)
import Flow ((|>))
-- import LS.XPile.LogicalEnglish.UtilsLEReplDev (leTestcasesDir)

import LS.Rule qualified as LS
import LS.Utils ((|$>))
import Safe (tailSafe)
import System.FilePath ((</>))
import System.FilePath.Find (FileType (Directory), fileType, (==?))
import Test.Hspec (Spec, describe, runIO)
import TestLib.SpecUtils (findWithDepth0)
import TestLib.Testcase (configFile2spec)

data TestConfig = TestConfig
  { description :: String,
    directory :: FilePath,
    fileExt :: String,
    xpileFn :: [LS.Rule] -> String
  }

-- | The 'Spec' used to test the Logical English transpiler.
mkSpec :: TestConfig -> Spec
mkSpec TestConfig {description, directory, fileExt, xpileFn} =
  describe description do
    directories :: [FilePath] <-
      directory
        |> findWithDepth0 (fileType ==? Directory)
        -- The first directory will always be leTestcasesDir itself, which is why
        -- we need to take the tail to get rid of it.
        |$> tailSafe
        |> runIO
    for_ directories \directory ->
      directory </> "config.yml"
        |> configFile2spec fileExt xpileFn |> runIO |> join