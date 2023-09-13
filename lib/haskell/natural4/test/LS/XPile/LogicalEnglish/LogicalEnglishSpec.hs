{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
--  Module: LogicalEnglishSpec
--  Description: Defines a hspec Spec for testing the Logical English transpiler.
--
--  This module is responsible for running unit tests for the Logical English
--  transpiler.
--
--  To define a unit test, one can create a new directory in
--  leTestcasesDir, like 'is-num' with the following structure:
--  - {leTestcasesDir}
--      - is-num
--          - is-num.csv
--          - expected.le
--          - description.txt
--  where:
--  - 'is-num.csv' is the name of the input natural4 csv file.
--  - 'expected.le' is a file containing the expected Logical English output.
--  - 'description.txt' is a file containing a description of the unit test.
--
--  Note that:
--  - The file names 'expected.le' and 'description.txt' are hardcoded
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

import Data.Foldable (for_)
import Data.String.Interpolate (__i)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Yaml qualified as Yaml
import Flow ((|>))
import GHC.Generics (Generic)
import LS (Rule)
import LS.XPile.LogicalEnglish (toLE)
import LS.XPile.LogicalEnglish.GoldenUtils (goldenLE)
import LS.XPile.LogicalEnglish.UtilsLEReplDev (leTestcasesDir, letestfnm2rules)
import System.FilePath ((<.>), (</>))
import Test.Hspec (Spec, describe, it, runIO)

-- | The 'Spec' used to test the Logical English transpiler.
spec :: Spec
spec = describe "Logical English" $ do
  testcaseDirs :: Either Yaml.ParseException [FilePath] <-
    leTestcasesDir </> "testcases.yml"
      |> Yaml.decodeFileEither
      |> runIO
  testcaseDirs |> either (runIO . reportYamlError) runTestcases

runTestcases :: Foldable t => t FilePath -> Spec
runTestcases testcaseDirs =
  for_ testcaseDirs $ \testcaseDir -> do
    let descrFile = "description" <.> "txt"
        l4csvFile = testcaseDir <.> "csv"
    description <- runIO $ readFile $
                    leTestcasesDir </> testcaseDir </> descrFile
    it description $ do
      l4rules <- letestfnm2rules l4csvFile
      let leProgram = l4rules |> toLE |> T.pack
      pure $ goldenLE testcaseDir leProgram

reportYamlError :: Yaml.ParseException -> IO ()
reportYamlError parseExc =
  TIO.putStrLn
    [__i|
      Skipping Logical English tests because an error occured while reading testcases.yml:
      #{parseExc}
    |]