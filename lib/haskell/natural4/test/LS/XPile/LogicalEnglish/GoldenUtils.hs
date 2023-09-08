module LS.XPile.LogicalEnglish.GoldenUtils where

import LS.XPile.LogicalEnglish.UtilsLEReplDev ( leTestcasesDir )
import Test.Hspec.Golden ( Golden(..) )
import System.FilePath ( (</>) )


goldenLE :: String -> String -> Golden String
goldenLE name actualOutput =
  Golden {
    output = actualOutput,
    encodePretty = show,
    writeToFile = writeFile,
    readFromFile = readFile,
    goldenFile = leTestcasesDir </> name </> "golden",
    actualFile = Just (leTestcasesDir </> name </> "actual"),
    failFirstTime = False
  }
