{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module LS.XPile.LogicalEnglish.UtilsLEReplDev
    ( 
    prl,
    leTestcasesDir,
    leTestCSVs,
    testfnmsLE,
    letestfnm2rules,
    rulesForEachOfLEtestegs,

    motorrules,
    indentpdparules,
    
    -- re-export from LS.UtilsREPLDev
    csvsInDir,
    l4csv2rules
    )

where

import System.FilePath ((</>), takeFileName)
import Data.Coerce (coerce)
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty)

import LS.Rule(Rule(..))
import LS.Types (MTExpr, mt2text)
import LS.UtilsREPLDev
    ( csvsInDir, l4csv2rules, pRules, BaseFileName )

import LS (pRelPred)


------------- temp utils / constants for prototyping -----
leTestcasesDir :: FilePath
leTestcasesDir = "test" </> "Testcases" </> "LogicalEnglish"

-- | Print rules from LE test csv
prl :: BaseFileName -> IO ()
prl = pRules leTestcasesDir

-- | Returns a list of csvs in the LE subdir of test/Testcases
leTestCSVs :: IO [FilePath]
leTestCSVs = csvsInDir leTestcasesDir

{- | IO action that gets you a list of basefilenms of .csvs in the testcases dir or any subdir of it

The output will look like

["and-not.csv","date-time.csv","ditto.csv","ditto-with-spaces.csv","filtering-will-not-remove-all-nlas-in-eq-class.csv","x-is-not-y.csv","indentation-propn-databreach.csv","is-in.csv","is-max-terms-simple.csv","is-min-terms-simple.csv","is-not-diff-from.csv","is-num-simple.csv","or-anaphora-with-ditto.csv","product-twiceof.csv","rpc-rpeq-year.csv","rpnary-is-max-x-where.csv","rpnary-is-min-x-where.csv","parentchild-with-period.csv","sum-of-terms-simple.csv","template-regex-filters-out-subbed-phrases.csv","template-regex-is-anchored-start-and-end.csv","vacuous-no-l4-rules.csv"]
-}
testfnmsLE :: IO [BaseFileName]
testfnmsLE = (fmap . fmap) takeFileName (csvsInDir leTestcasesDir)


letestfnm2rules :: BaseFileName -> IO [Rule]
letestfnm2rules = l4csv2rules leTestcasesDir

rulesForEachOfLEtestegs :: IO [[Rule]]
rulesForEachOfLEtestegs = do
  fnms <- testfnmsLE
  traverse letestfnm2rules fnms

-------------

motorrules :: IO [Rule]
motorrules = l4csv2rules ("test" </> "Testcases") "motor-insurance-1.csv"

indentpdparules :: IO [Rule]
indentpdparules = letestfnm2rules "indentation-databreach.csv" 