{-# LANGUAGE BlockArguments #-}

module TestLib (mkSpec, TestConfig (..)) where

import Control.Monad (join)
import Data.Foldable (for_)
import Flow ((|>))
import LS.Rule qualified as LS
import LS.Utils ((|$>))
import Safe (tailSafe)
import System.FilePath ((</>))
import System.FilePath.Find (FileType (Directory), fileType, (==?))
import Test.Hspec (Spec, describe, runIO)
import TestLib.Utils (findWithDepth0)
import TestLib.Testcase (configFile2spec)

data TestConfig = TestConfig
  { description :: String,
    dir :: FilePath,
    fileExt :: String,
    xpileFn :: [LS.Rule] -> String
  }

mkSpec :: TestConfig -> Spec
mkSpec TestConfig {description, dir, fileExt, xpileFn} =
  describe description do
    dirs :: [FilePath] <-
      "test" </> "Testcases" </> dir
        |> findWithDepth0 (fileType ==? Directory)
        -- The first directory will always be the dir containing the testcases
        -- itself, which is why we need to take the tail to get rid of it.
        |$> tailSafe
        |> runIO
    for_ dirs \dir ->
      dir </> "config.yml"
        |> configFile2spec fileExt xpileFn
        |> runIO
        |> join