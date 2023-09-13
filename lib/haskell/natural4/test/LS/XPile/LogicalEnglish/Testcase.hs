{-# LANGUAGE ApplicativeDo #-}
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
import Data.Yaml qualified as Yaml
import Flow ((|>))
import GHC.Generics (Generic)
import LS (Rule)
import LS.XPile.LogicalEnglish (toLE)
import LS.XPile.LogicalEnglish.GoldenUtils (goldenLE)
import LS.XPile.LogicalEnglish.UtilsLEReplDev (letestfnm2rules)
import System.Directory (doesFileExist)
import System.FilePath (takeBaseName, takeDirectory, (<.>))
import System.FilePath.Find (depth, fileName, (==?))
import System.FilePath.Find qualified as FileFind
import Test.Hspec (Spec, describe, it, pendingWith, runIO)
import LS.Utils ((|$>))

configFile2spec :: FilePath -> IO Spec
configFile2spec configFile =
  configFile |> configFile2testcase |$> either error2spec testcase2spec 

configFile2testcase :: FilePath -> IO (Either Error Testcase)
configFile2testcase configFile = do
  exists <- doesFileExist configFile
  if exists
    then do
      yamlParseResult :: Either Yaml.ParseException Config <-
        Yaml.decodeFileEither configFile
      yamlParseResult |> bimap yamlParseExc2error config2testcase |> pure
    else pure $ Left $ Error {directory, info = MissingConfigFile}
  where
    directory = takeDirectory configFile
    config2testcase config = Testcase {directory, config}
    yamlParseExc2error parseExc =
      Error {directory, info = YamlParseExc parseExc}

testcase2spec :: Testcase -> Spec
testcase2spec Testcase {directory, config = Config {description, enabled}} =
  describe directory $
    if enabled
      then it description $ do
        let testcaseName :: String = takeBaseName directory
        l4rules :: [Rule] <- letestfnm2rules $ testcaseName <.> "csv"
        let leProgram :: T.Text = l4rules |> toLE |> T.pack
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
  deriving Show

data ErrorInfo where
  MissingConfigFile :: ErrorInfo
  YamlParseExc :: Yaml.ParseException -> ErrorInfo

instance Show ErrorInfo where
  show MissingConfigFile = "Missing config.yml file."
  show (YamlParseExc parseExc) = [i|Error parsing YAML file: #{parseExc}|]