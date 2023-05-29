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
import LS.XPile.CoreL4 ( sfl4ToASP, sfl4ToEpilog )
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
import System.Directory

-- testCsvFiles :: Opts Unwrapped
-- testCsvFiles = Opts {..}
--   where
--     file = NoLabel csvFileNames
--     csvFileNames =
--       [ [i|test/motor_Insurance.csv|]
--       ]
--     dbug = False
--     dstream = False

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
  describe "ASP transpiler" $ do
    it "Motor insurance" $ do
      csvFile : _ <- Find.find always (extension ==? ".csv") (testCasesDir <> "/motor-insurance")

      let file = NoLabel [csvFile]
          dbug = False
          dstream = False
          opts :: Opts Unwrapped = Opts {..}
    
      rules <- LS.dumpRules opts
      -- print $ sfl4ToASP rules
      pending

  describe "Epilog transpiler" $ do
    it "pending"
      pending