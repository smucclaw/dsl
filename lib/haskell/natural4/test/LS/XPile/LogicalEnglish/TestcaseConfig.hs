module LS.XPile.LogicalEnglish.TestcaseConfig
  ( TestcaseConfig (..),
    readConfigFile
  )
where

import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)

data TestcaseConfig = TestcaseConfig
  { description :: String,
    enabled :: Bool
  }
  deriving (Generic, Show)

instance Yaml.FromJSON TestcaseConfig

readConfigFile :: FilePath -> IO (Either Yaml.ParseException TestcaseConfig)
readConfigFile = Yaml.decodeFileEither