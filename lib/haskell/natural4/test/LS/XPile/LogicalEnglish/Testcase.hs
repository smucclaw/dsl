{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module LS.XPile.LogicalEnglish.Testcase
  ( Testcase,
    Error,
    configFile2testcase,
    testcase2spec,
    error2spec
  )
where

import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import qualified System.FilePath.Find as FileFind
import Flow ((|>))
import System.FilePath.Find (fileName, (==?), depth)
import Data.Maybe (listToMaybe)
import LS.Utils ((|$>))
import Test.Hspec (runIO, it, pendingWith, describe, Spec)
import Data.String.Interpolate (i)
import qualified Data.Text as T
import System.FilePath ((<.>), takeDirectory, takeBaseName)
import LS.XPile.LogicalEnglish.UtilsLEReplDev (letestfnm2rules)
import LS.XPile.LogicalEnglish (toLE)
import LS.XPile.LogicalEnglish.GoldenUtils (goldenLE)
import System.Directory ( doesFileExist )
import Data.Bifunctor (Bifunctor(bimap))

configFile2testcase :: FilePath -> IO (Either Error Testcase)
configFile2testcase configFile = do
  exists <- doesFileExist configFile
  if exists
    then
      configFile
        |> Yaml.decodeFileEither
        |$> bimap yamlParseExc2error config2testcase
    else pure $ Left $ Error {directory, info = MissingConfigFile}
  where
    directory = takeDirectory configFile
    config2testcase config = Testcase {directory, config}
    yamlParseExc2error parseExc =
      Error {directory, info = YamlParseExc parseExc}

testcase2spec :: Testcase -> Spec
testcase2spec Testcase {directory, config = Config {..}} =
  describe directory $
    if enabled
      then it description $ do
        let testcaseName = takeBaseName directory
        l4rules <- letestfnm2rules $ testcaseName <.> "csv"
        let leProgram = l4rules |> toLE |> T.pack
        pure $ goldenLE testcaseName leProgram
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

instance Yaml.FromJSON Config

data Error = Error
  { directory :: FilePath,
    info :: ErrorInfo
  }

data ErrorInfo where
  MissingConfigFile :: ErrorInfo
  YamlParseExc :: Yaml.ParseException -> ErrorInfo

instance Show ErrorInfo where
  show MissingConfigFile = "Missing config.yml file."
  show (YamlParseExc parseExc) = [i|Error parsing YAML file: #{parseExc}|]