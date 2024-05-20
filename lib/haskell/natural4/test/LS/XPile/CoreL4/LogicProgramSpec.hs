{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module LS.XPile.CoreL4.LogicProgramSpec (spec) where

import Control.Monad (join)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (isSpace)
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.String.Interpolate (i)
import Flow ((.>), (|>))
import GHC.Generics (Generic)
import LS qualified
import LS.Lib (NoLabel (..), Opts (..))
import LS.Utils ((|$>))
import LS.XPile.CoreL4 (sfl4ToASP, sfl4ToEpilog)
import LS.XPile.CoreL4.LogicProgram.Common (LPLang (..))
import LS.XPile.Logging (XPileLogE, fromxpLogE, xpLog, xpReturn)
import System.FilePath ((</>))
import System.FilePath.Find
  ( always,
    fileName,
    find,
    (==?),
  )
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )

testcases :: HS.HashSet LPTestcase
testcases =
  [ "motor-insurance-0",
    "motor-insurance-1",
    "rodents-and-vermin"
  ]
    |> HS.map dir2testcase
  where
    dir2testcase dir =
      LPTestcase
        { dir,
          csvFile = [i|#{dir}.csv|],
          expectedOutputFiles = [(ASP, "asp.lp"), (Epilog, "epilog.lp")]
        }

spec :: Spec
spec =
  for_ ([ASP, Epilog] :: [LPLang]) \lpLang ->
    describe [i|Testing #{lpLang} transpiler|] $
      for_ testcases $ testcase2spec lpLang

data LPTestcase = LPTestcase
  { dir :: FilePath,
    csvFile :: FilePath,
    expectedOutputFiles :: HM.HashMap LPLang FilePath
  }
  deriving (Eq, Generic, Hashable, Ord, Read, Show)

testcase2spec :: LPLang -> LPTestcase -> Spec
testcase2spec lpLang LPTestcase {..} =
  it dir do
    Just csvFile <- findFileWithName csvFile
    rules :: [LS.Rule] <-
      LS.dumpRules
        Opts
          { file = coerce ([csvFile] :: [FilePath]),
            dbug = False,
            dstream = False
          }

    Just expectedOutputFile <- findFileWithName expectedOutputFile
    expectedOutput :: String <- readFile expectedOutputFile

    let (logicProgram :: String, expectedOutput' :: String) =
          (rules, expectedOutput)
            |> bimap rules2lpStr xpReturn
            |> join bimap (fromxpLogE .> filter (not . isSpace))

    logicProgram `shouldBe` expectedOutput'
  where
    findFileWithName :: FilePath -> IO (Maybe FilePath)
    findFileWithName file =
      "test" </> "Testcases" </> "LogicProgram" </> dir
        |> find always (fileName ==? file)
        |$> listToMaybe

    expectedOutputFile :: FilePath =
      expectedOutputFiles |> HM.lookup lpLang |> fromMaybe ""

    rules2lpStr = case lpLang of
      ASP -> sfl4ToASP
      Epilog -> sfl4ToEpilog
