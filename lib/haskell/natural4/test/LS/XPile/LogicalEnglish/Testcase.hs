{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

module LS.XPile.LogicalEnglish.Testcase
  ( configFile2spec,
  )
where

import Data.String.Interpolate (i)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Except
  ( MonadError (throwError),
    modifyError,
    runExceptT,
  )
import Data.Yaml qualified as Y
import Flow ((|>))
import GHC.Generics (Generic)
import LS.Utils ((|$>))
import LS.XPile.LogicalEnglish (toLE)
import LS.XPile.LogicalEnglish.GoldenUtils (goldenLE)
import LS.XPile.LogicalEnglish.UtilsLEReplDev (letestfnm2rules)
import System.Directory (doesFileExist)
import System.FilePath (takeBaseName, takeDirectory, (<.>))
import System.FilePath.Find (depth, fileName, (==?))
import System.FilePath.Find qualified as FileFind
import Test.Hspec (Spec, describe, it, pendingWith, runIO)

configFile2spec :: FilePath -> IO Spec
configFile2spec configFile =
  configFile
    |> configFile2testcase
    |$> either toSpec toSpec

configFile2testcase :: FilePath -> IO (Either Error Testcase)
configFile2testcase configFile = runExceptT do
  exists <- lift do doesFileExist configFile
  if not exists
    then throwError Error {directory, info = MissingConfigFile}
    else
      configFile
        |> Y.decodeFileThrow
        |> modifyError yamlParseExc2error
        |$> Testcase directory
  where
    directory = takeDirectory configFile
    yamlParseExc2error parseExc =
      Error {directory, info = YamlParseExc parseExc}

instance ToSpec Testcase where
  toSpec Testcase {directory, config = Config {description, enabled}} =
    describe directory
      if enabled
        then it description do
          testcaseName <.> "csv"
            |> letestfnm2rules
            |$> toLE
            |$> goldenLE testcaseName
        else it description $ pendingWith "Test case is disabled."
    where
      testcaseName = takeBaseName directory

instance ToSpec Error where
  toSpec Error {directory, info} = it directory $ pendingWith $ show info

data Testcase = Testcase
  { directory :: FilePath,
    config :: Config
  }
  deriving Show

data Config = Config
  { description :: String,
    enabled :: Bool
  }
  deriving (Generic, Show)

instance Y.FromJSON Config

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
  toSpec :: a -> Spec