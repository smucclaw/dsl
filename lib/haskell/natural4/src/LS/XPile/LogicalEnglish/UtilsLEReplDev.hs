{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ImportQualifiedPost #-}

module LS.XPile.LogicalEnglish.UtilsLEReplDev
    ( 
    testfnmsLE,
    letestfnm2rules,
    rulesForEachOfLEtestegs,
    
    -- re-export from LS.UtilsREPLDev
    csvsInDir,
    l4csv2rules,
    leTestcasesDir,
    leTestCSVs
    )

where

import System.FilePath ((</>), takeFileName)
import Control.Monad.Validate (MonadValidate (refute), Validate, runValidate)
import Data.Coerce (coerce)
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty)
import Control.Monad
import Data.MonoTraversable (Element, MonoFoldable (otoList))
import Data.Monoid (Ap (Ap))
import Data.Sequences (SemiSequence)
import Data.Text (Text)

import LS.Types (MTExpr, mt2text)
import LS.UtilsREPLDev qualified as R
import LS.UtilsREPLDev (
    csvsInDir,
    l4csv2rules,
    leTestcasesDir,
    leTestCSVs)

import LS.XPile.LogicalEnglish.Common (
    L4Prog,
    Rule,
    (|>)
    )


------------- temp utils / constants for prototyping -----

testfnmsLE :: IO [R.BaseFileName]
testfnmsLE = (fmap . fmap) takeFileName (csvsInDir leTestcasesDir)

letestfnm2rules :: R.BaseFileName -> IO [Rule]
letestfnm2rules = l4csv2rules leTestcasesDir

rulesForEachOfLEtestegs :: IO [L4Prog]
rulesForEachOfLEtestegs = do
  fnms <- testfnmsLE
  traverse letestfnm2rules fnms
