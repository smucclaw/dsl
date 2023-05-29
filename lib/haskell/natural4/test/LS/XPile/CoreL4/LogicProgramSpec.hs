{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module LS.XPile.CoreL4.LogicProgramSpec
  ( spec
  )
where

import Control.Monad (join)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (isSpace)
import Data.Map qualified as Map
import Flow ((|>))
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

-- testCsvFiles :: Opts Unwrapped
-- testCsvFiles = Opts {..}
--   where
--     file = NoLabel csvFileNames
--     csvFileNames =
--       [ [i|test/motor_Insurance.csv|]
--       ]
--     dbug = False
--     dstream = False

spec :: Spec
spec = do
  describe "ASP transpiler" $ do
    it "Motor insurance" $ do
    --   csvFile : _ <- Find.find always (extension ==? ".csv") (testcasesDir </> "motor-insurance")

    --   let file = NoLabel [csvFile]
    --       dbug = False
    --       dstream = False
    --       opts :: Opts Unwrapped = Opts {..}
    
    --   rules <- LS.dumpRules opts
    --   rules |> sfl4ToASP |> filter (not . isSpace) |> print
      -- print $ sfl4ToASP rules
      pending

  describe "Epilog transpiler" $ do
    it "pending"
      pending

data LPTestcase = LPTestCase
  { dirName :: String,
    csvFile :: String,
    expectedOutputFiles :: Map.Map LPLang String
  }
  deriving (Eq, Ord, Read, Show)

testcase2specs :: LPTestcase -> Map.Map LPLang Spec
testcase2specs testcase = Map.fromList $ do
  lpLang <- [ASP, Epilog]
  pure (lpLang, testcase2spec lpLang testcase)

testcase2spec :: LPLang -> LPTestcase -> Spec
testcase2spec lpLang LPTestCase {..} =
  it dirName $ do
    let findFile fileName' =
          Find.find always (fileName ==? fileName') (testcasesDir </> dirName)
        testcasesDir = "test" </> "Testcases" </> "LogicProgram"
        expectedOutputFile =
          expectedOutputFiles |> Map.lookup lpLang |> maybe "" show
        xpileFn = case lpLang of
          ASP -> sfl4ToASP
          Epilog -> sfl4ToEpilog

    csvFile : _ <- findFile csvFile
    rules :: [Rule] <-
      LS.dumpRules
        Opts
          { file = NoLabel [csvFile],
            dbug = False,
            dstream = False
          }

    expectedOutputFile : _ <- findFile expectedOutputFile
    expectedOutput <- readFile expectedOutputFile

    let (xpiled, expectedOutput) =
          (xpileFn rules, expectedOutput)
            |> join bimap removeSpaces
        removeSpaces = filter $ not . isSpace

    xpiled `shouldBe` expectedOutput
