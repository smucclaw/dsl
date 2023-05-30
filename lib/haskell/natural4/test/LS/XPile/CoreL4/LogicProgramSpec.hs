{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LS.XPile.CoreL4.LogicProgramSpec
  ( spec
  )
where

import Control.Monad (join)
import Data.Bifunctor (Bifunctor (bimap, first))
import Data.Char (isSpace)
import Data.Foldable (traverse_)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe, listToMaybe)
import Flow ((|>))
import GHC.Generics (Generic)
import LS (Rule)
import LS qualified
import LS.Lib (NoLabel (..), Opts (..))
import LS.XPile.CoreL4 (sfl4ToASP, sfl4ToEpilog, sfl4ToLogicProgramStr)
import LS.XPile.CoreL4.LogicProgram.Common (LPLang (..))
import Options.Generic (Unwrapped)
import System.Directory
import System.FilePath
import System.FilePath.Find as Find
import Test.Hspec
  ( Spec,
    describe,
    hspec,
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
spec = do
  describe "ASP transpiler" $
    traverse_ (testcase2spec ASP) testcases

  describe "Epilog transpiler" $
    traverse_ (testcase2spec Epilog) testcases

data LPTestcase = LPTestcase
  { dir :: FilePath,
    csvFile :: FilePath,
    expectedOutputFiles :: HM.HashMap LPLang FilePath
  }
  deriving (Eq, Generic, Ord, Read, Show)

instance Hashable LPTestcase

testcase2specs :: LPTestcase -> HM.HashMap LPLang Spec
testcase2specs testcase = HM.fromList $ do
  lpLang <- [ASP, Epilog]
  pure (lpLang, testcase2spec lpLang testcase)

testcase2spec :: LPLang -> LPTestcase -> Spec
testcase2spec lpLang LPTestcase {..} =
  it dir $ do
    let findFile :: FilePath -> IO (Maybe FilePath)
        findFile file =
          listToMaybe <$> Find.find always (fileName ==? file) dir'
        dir' :: FilePath = "test" </> "Testcases" </> "LogicProgram" </> dir
        expectedOutputFile :: FilePath =
          expectedOutputFiles |> HM.lookup lpLang |> fromMaybe ""
        xpileFn :: [Rule] -> String =
          case lpLang of
            ASP -> sfl4ToASP
            Epilog -> sfl4ToEpilog

    Just csvFile <- findFile csvFile
    rules :: [Rule] <-
      LS.dumpRules
        Opts
          { file = NoLabel [csvFile],
            dbug = False,
            dstream = False
          }

    Just expectedOutputFile <- findFile expectedOutputFile
    expectedOutput :: String <- readFile expectedOutputFile

    (rules, expectedOutput)
      |> first xpileFn
      |> join bimap (filter $ not . isSpace)
      |> uncurry shouldBe