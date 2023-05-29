{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module LS.XPile.CoreL4.LogicProgramSpec
  ( spec
  )
where

import Control.Monad.Cont (MonadIO (liftIO))
import Data.Map qualified as Map
import Data.String.Interpolate (i)
import Data.Text qualified as T
import LS qualified
import LS.Lib (NoLabel (..), Opts (..))
import LS.XPile.CoreL4.LogicProgram.Common
import Options.Generic (Unwrapped)
import System.FilePath.Find as Find
import Test.Hspec
  ( Spec,
    describe,
    hspec,
    it,
    pending,
    shouldBe,
  )

testCsvFiles :: Opts Unwrapped
testCsvFiles = Opts {..}
  where
    file = NoLabel csvFileNames
    csvFileNames =
      [ [i|test/motor_Insurance.csv|]
      ]
    dbug = False
    dstream = False

testCasesDir :: FilePath
testCasesDir = "test/TestCases/LogicProgram"

data LPTestCase = LPTestCase
  { name :: T.Text,
    natural4InputFile :: T.Text,
    expectedOutputs :: Map.Map LPLang T.Text
  }
  deriving (Eq, Ord, Read, Show)

spec :: Spec
spec = do
  -- dir <- liftIO $ Find.find (depth ==? 1) undefined testCasesDir
  describe "ASP transpiler" $ do
    it "pending" $ do
      rules <- liftIO $ LS.dumpRules testCsvFiles
      pending

  describe "Epilog transpiler" $ do
    it "pending"
      pending