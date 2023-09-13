module LS.XPile.LogicalEnglish.GoldenUtils (goldenLE) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import LS.XPile.LogicalEnglish.UtilsLEReplDev (leTestcasesDir)
import System.FilePath ((</>))
import Test.Hspec.Golden (Golden (..))

goldenLE :: FilePath -> T.Text -> Golden T.Text
goldenLE name actualOutput =
  Golden
    { output = actualOutput,
      encodePretty = show,
      writeToFile = TIO.writeFile,
      readFromFile = TIO.readFile,
      goldenFile = leTestcasesDir </> name </> "expected.le",
      actualFile = Just $ leTestcasesDir </> name </> "actual.le",
      failFirstTime = False
    }
