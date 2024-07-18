{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- TODO: Generalize / make this more easily resuable when time permits
module LS.XPile.MathLang.UtilsLCReplDev
    (
      prl
    , testCSVs
    , testfnm2rules
    , tHcIfWhere
    , tHcIfPandQthenR
    )
where

import System.FilePath ((</>))

import LS.Rule (Rule(..))
import LS.Utils.UtilsREPLDev (BaseFileName, pRules, csvsInDir, l4csv2rules)

testcasesDir :: FilePath
testcasesDir = "test" </> "Testcases" </> "MathLangGeneric"

-- | Print rules from test csv
prl :: BaseFileName -> IO ()
prl = pRules testcasesDir


-- | Returns a list of csvs in the relevant testcases subdir
testCSVs :: IO [FilePath]
testCSVs = csvsInDir testcasesDir

testfnm2rules :: BaseFileName -> IO [Rule]
testfnm2rules = l4csv2rules testcasesDir

---------------------------------------------------------------------------------

tHcIfWhere :: IO [Rule]
tHcIfWhere = testfnm2rules "hc-if-where.csv"

tHcIfPandQthenR :: IO [Rule]
tHcIfPandQthenR = testfnm2rules "hc-if-p-and-q-then-r.csv"
