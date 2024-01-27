module LS.XPile.LogicalEnglish.GoldenUtils (goldenLE) where

import LS.XPile.LogicalEnglish.UtilsLEReplDev (leTestcasesDir)
import System.FilePath ((</>), (-<.>), takeBaseName)
import Test.Hspec.Golden (Golden (..))

goldenLE :: FilePath -> String -> Golden String
goldenLE testcase actualOutput =
  Golden
    { output = actualOutput,
      encodePretty = show,
      writeToFile = writeFile,
      readFromFile = readFile,
      goldenFile = leTestcasesDir </> testcaseName </> "expected" -<.> "le",
      actualFile = Just $ leTestcasesDir </> testcaseName </> "actual" -<.> "le",
      failFirstTime = False
    }
  where
    testcaseName = takeBaseName testcase
