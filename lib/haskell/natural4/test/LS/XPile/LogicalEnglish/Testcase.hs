{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

module LS.XPile.LogicalEnglish.Testcase
  ( configFile2spec,
  )
where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Maybe (listToMaybe)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    MonadTrans (lift),
    runExceptT,
  )
import Data.Yaml qualified as Y
import Flow ((|>))
import GHC.Generics (Generic)
import LS (Rule)
import LS.Utils ((|$>))
import LS.XPile.LogicalEnglish (toLE)
import LS.XPile.LogicalEnglish.GoldenUtils (goldenLE)
import LS.XPile.LogicalEnglish.SpecUtils (modifyError)
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
    |$> either error2spec testcase2spec

configFile2testcase :: FilePath -> IO (Either Error Testcase)
configFile2testcase configFile = runExceptT do
  exists <- lift $ doesFileExist configFile
  if not exists
    then throwError Error {directory, info = MissingConfigFile}
    else
      configFile
        |> Y.decodeFileThrow
        |> modifyError yamlParseExc2error
        |$> \config -> Testcase {directory, config}
  where
    directory = takeDirectory configFile
    yamlParseExc2error parseExc =
      Error {directory, info = YamlParseExc parseExc}

testcase2spec :: Testcase -> Spec
testcase2spec Testcase {directory, config = Config {description, enabled}} =
  describe directory
    if enabled
      then it description do
        let testcaseName :: String = takeBaseName directory
        l4rules :: [Rule] <- letestfnm2rules $ testcaseName <.> "csv"
        let leProgram :: T.Text = l4rules |> toLE |> T.pack
        return $ goldenLE testcaseName leProgram
      else it description $ pendingWith "Test case is disabled."

error2spec :: Error -> Spec
error2spec Error {directory, info} = it directory $ pendingWith $ show info

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