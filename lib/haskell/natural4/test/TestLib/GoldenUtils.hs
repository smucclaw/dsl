module TestLib.GoldenUtils (mkGolden) where

import LS.XPile.LogicalEnglish.UtilsLEReplDev (leTestcasesDir)
import System.FilePath ((</>), (-<.>), takeBaseName)
import Test.Hspec.Golden (Golden (..))

mkGolden :: String -> FilePath -> FilePath -> String -> Golden String
mkGolden fileExt dir testcase actualOutput =
  Golden
    { output = actualOutput,
      encodePretty = show,
      writeToFile = writeFile,
      readFromFile = readFile,
      goldenFile = go "expected",
      actualFile = Just $ go "actual",
      failFirstTime = False
    }
  where
    go fileName = dir </> fileName -<.> fileExt
