{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

module TestLib.Testcase
  ( configFile2spec,
  )
where

import Control.Monad.Except
  ( MonadError (throwError),
    modifyError,
    runExceptT,
  )
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.String.Interpolate (i)
import Data.Yaml qualified as Y
import Flow ((|>))
import GHC.Generics (Generic)
import LS.Rule qualified as LS
import LS.Utils ((|$>))
import LS.XPile.LogicalEnglish.UtilsLEReplDev (letestfnm2rules)
import System.Directory (doesFileExist)
import System.FilePath (takeBaseName, takeDirectory, (<.>))
import System.FilePath.Find (depth, fileName, (==?))
import System.FilePath.Find qualified as FileFind
import Test.Hspec (Spec, describe, it, pendingWith, runIO)
import TestLib.GoldenUtils (mkGolden)

configFile2spec :: FilePath -> ([LS.Rule] -> String) -> FilePath -> IO Spec
configFile2spec fileExt xpileFn configFile =
  configFile
    |> configFile2testcase fileExt
    |$> either (toSpec xpileFn) (toSpec xpileFn)

configFile2testcase :: String -> FilePath -> IO (Either Error Testcase)
configFile2testcase fileExt configFile = runExceptT do
  exists <- lift $ doesFileExist configFile
  if not exists
    then throwError Error {directory, info = MissingConfigFile}
    else
      configFile
        |> Y.decodeFileThrow
        -- Note that Yaml parse exceptions are thrown as IO errors, which are
        -- for some reason treated as a different error type from other types
        -- of errors.
        -- Hence we need to use modifyError to catch the exception, modify it
        -- into our internal exception type, and then re-throw it.
        |> modifyError yamlParseExc2error
        |$> Testcase directory fileExt
  where
    directory = takeDirectory configFile
    yamlParseExc2error parseExc =
      Error {directory, info = YamlParseExc parseExc}

instance ToSpec Testcase where
  toSpec
    xpileFn
    Testcase {directory, fileExt, config = Config {description, enabled}} =
      describe
        directory
        if enabled
          then it description do
            testcaseName <.> "csv"
              |> letestfnm2rules
              |$> xpileFn
              |$> mkGolden fileExt directory testcaseName
          else it description $ pendingWith "Test case is disabled."
      where
        testcaseName = takeBaseName directory

instance ToSpec Error where
  toSpec _ Error {directory, info} = it directory $ pendingWith $ show info

data Testcase = Testcase
  { directory :: FilePath,
    fileExt :: String,
    config :: Config
  }
  deriving Show

data Config = Config
  { description :: String,
    enabled :: Bool
  }
  deriving (Generic, Y.FromJSON, Show)

data Error = Error
  { directory :: FilePath,
    info :: ErrorInfo
  }
  deriving Show

data ErrorInfo where
  MissingConfigFile :: ErrorInfo
  YamlParseExc :: Y.ParseException -> ErrorInfo

instance Show ErrorInfo where
  show MissingConfigFile = "Missing config.yml file."
  show (YamlParseExc parseExc) = [i|Error parsing YAML file: #{parseExc}|]

class ToSpec a where
  toSpec :: ([LS.Rule] -> String) -> a -> Spec