module LS.XPile.LogicalEnglish.IsNumGoldenSpec where

import LS.Rule ( Rule(..) )
import LS.XPile.LogicalEnglish ( toLE )
import LS.XPile.LogicalEnglish.GoldenUtils ( goldenLE )
import LS.XPile.LogicalEnglish.UtilsLEReplDev ( letestfnm2rules )
import System.FilePath ( (<.>) )
import Test.Hspec ( describe, it, Spec )


spec :: Spec
spec =
    describe "Logical English" $
    -- check if this is indeed the goal of the test
      it "If IS is followed by a number, number should become *a number*" $
        isNumRules >>= (return . goldenLE baseFileName) . toLE

baseFileName :: FilePath
baseFileName = "is-num"

isNumRules :: IO [Rule]
isNumRules = letestfnm2rules (baseFileName <.> "csv")
