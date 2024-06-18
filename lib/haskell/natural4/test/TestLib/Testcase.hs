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
import LS.Utils.UtilsREPLDev (l4csv2rules)
import System.Directory (doesFileExist)
import System.FilePath (takeBaseName, takeDirectory, (<.>), (</>))
import Test.Hspec (Spec, describe, it, pendingWith)
import TestLib.Utils (ToText, mkGolden)

configFile2spec :: ToText t => String -> ([LS.Rule] -> t) -> FilePath -> IO Spec
configFile2spec fileExt xpileFn configFile =
  configFile
    |> configFile2testcase fileExt
    |$> toSpec xpileFn

configFile2testcase :: String -> FilePath -> IO (Either Error Testcase)
configFile2testcase fileExt configFile = runExceptT do
  exists <- lift $ doesFileExist configFile
  if not exists
    then throwError Error {dir, info = MissingConfigFile}
    else
      configFile
        |> Y.decodeFileThrow
        -- Note that Yaml parse exceptions are thrown as IO errors, which are
        -- for some reason treated as a different error type from other types
        -- of errors.
        -- Hence we need to use modifyError to catch the exception, modify it
        -- into our internal exception type, and then re-throw it.
        |> modifyError yamlParseExc2error
        |$> \config -> Testcase {dir, fileExt, config}
  where
    dir = takeDirectory configFile
    yamlParseExc2error parseExc =
      Error {dir, info = YamlParseExc parseExc}

toSpec :: ToText t => ([LS.Rule] -> t) -> Either Error Testcase -> Spec
toSpec
  xpileFn
  ( Right
      Testcase
        { dir,
          fileExt,
          config = Config {description, enabled}
        }
    ) =
    describe testcaseName
      if enabled
        then it description do
          testcaseName <.> "csv"
            |> l4csv2rules dir
            |$> xpileFn
            |$> mkGolden fileExt dir testcaseName
        else it description $ pendingWith "Test case is disabled."
    where
      testcaseName = takeBaseName dir

toSpec _ (Left Error {dir, info}) =
  it testcaseName $ pendingWith $ show info
  where
    testcaseName = takeBaseName dir

data Testcase = Testcase
  { dir :: FilePath,
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
  { dir :: FilePath,
    info :: ErrorInfo
  }
  deriving Show

data ErrorInfo where
  MissingConfigFile :: ErrorInfo
  YamlParseExc :: Y.ParseException -> ErrorInfo

instance Show ErrorInfo where
  show MissingConfigFile = "Missing config.yml file."
  show (YamlParseExc parseExc) = [i|Error parsing YAML file: #{parseExc}|]