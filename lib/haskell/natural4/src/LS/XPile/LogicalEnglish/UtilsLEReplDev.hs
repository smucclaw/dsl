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
import Control.Monad
import Data.Monoid (Ap (Ap))
import Data.Text (Text)

import LS.Types (MTExpr, mt2text)
import LS.UtilsREPLDev

import LS.XPile.LogicalEnglish.Common (
    L4Prog,
    Rule,
    (|>)
    )
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


testfnmsLE :: IO [BaseFileName]
testfnmsLE = (fmap . fmap) takeFileName (csvsInDir leTestcasesDir)
{-
["and-not.csv",
"indentation-databreach.csv",
"product.csv",
"rpnary-and.csv",
"parentchild-with-period.csv",
"parentchild-without-period.csv",
"simple-sum.csv"]
-}

letestfnm2rules :: BaseFileName -> IO [Rule]
letestfnm2rules = l4csv2rules leTestcasesDir

rulesForEachOfLEtestegs :: IO [L4Prog]
rulesForEachOfLEtestegs = do
  fnms <- testfnmsLE
  traverse letestfnm2rules fnms

-------------

motorrules :: IO [Rule]
motorrules = l4csv2rules "test/Testcases/" "motor-insurance-1.csv"

indentpdparules :: IO [Rule]
indentpdparules = letestfnm2rules "indentation-databreach.csv" 

-- withnumsrules = letestfnm2rules "with-numbers.csv"
