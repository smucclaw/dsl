{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module LS.XPile.CoreL4.LogicProgramSpec
  ( spec,
  )
where

import Control.Monad (join)
import Data.Bifunctor (Bifunctor (bimap, first))
import Data.Char (isSpace)
import Data.Foldable (for_, traverse_)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.String.Interpolate (i)
import Flow ((|>))
import GHC.Generics (Generic)
import LS (Rule)
import LS qualified
import LS.Lib (NoLabel (..), Opts (..))
import LS.XPile.CoreL4 (sfl4ToASP, sfl4ToEpilog)
import LS.XPile.CoreL4.LogicProgram.Common (LPLang (..))
import Options.Generic (Unwrapped)
import System.FilePath ((</>))
import System.FilePath.Find as Find
  ( always,
    fileName,
    find,
    (==?),
  )
import Test.Hspec
  ( Spec,
    describe,
    it,
    pending,
    shouldBe,
  )

testcases :: HS.HashSet LPTestcase
testcases =
  [ LPTestcase
      { dir = "motor-insurance",
        csvFile = "motor-insurance.csv",
        expectedOutputFiles = [(ASP, "asp.lp"), (Epilog, "epilog.lp")]
      }
  ]

spec :: Spec
spec =
  for_ ([ASP, Epilog] :: [LPLang]) $ \lpLang ->
    describe [i|Testing #{lpLang} transpiler|] $
      for_ testcases $ testcase2spec lpLang

data LPTestcase = LPTestcase
  { dir :: FilePath,
    csvFile :: FilePath,
    expectedOutputFiles :: HM.HashMap LPLang FilePath
  }
  deriving (Eq, Generic, Ord, Read, Show)

instance Hashable LPTestcase

testcase2spec :: LPLang -> LPTestcase -> Spec
testcase2spec lpLang LPTestcase {..} =
  it dir $ do
    Just csvFile <- findFileWithName csvFile
    rules :: [Rule] <-
      LS.dumpRules
        Opts
          { file = NoLabel [csvFile],
            dbug = False,
            dstream = False
          }

    Just expectedOutputFile <- findFileWithName expectedOutputFile
    expectedOutput :: String <- readFile expectedOutputFile

    let (logicProgram, expectedOutput') =
          (rules, expectedOutput)
            |> first rules2lp
            |> join bimap (filter $ not . isSpace)

    logicProgram `shouldBe` expectedOutput'
  where
    findFileWithName :: FilePath -> IO (Maybe FilePath)
    findFileWithName file =
      listToMaybe <$> Find.find always (fileName ==? file) dir'

    dir' :: FilePath = "test" </> "Testcases" </> "LogicProgram" </> dir

    expectedOutputFile :: FilePath =
      expectedOutputFiles |> HM.lookup lpLang |> fromMaybe ""

    rules2lp :: [Rule] -> String =
      case lpLang of
        ASP -> sfl4ToASP
        Epilog -> sfl4ToEpilog