{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module TestLib.Utils
  ( findWithDepth0,
    mkGolden,
  )
where

import Data.String.Interpolate (i)
import Data.String.Interpolate.Conversion (Interpolatable)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.FilePath (takeBaseName, (-<.>), (</>))
import System.FilePath.Find (FilterPredicate, depth, find, (==?))
import Test.Hspec.Golden (Golden (..))

findWithDepth0 :: FilterPredicate -> FilePath -> IO [FilePath]
findWithDepth0 = find (depth ==? 0)

mkGolden ::
  (Interpolatable True t T.Text) =>
  String ->
  FilePath ->
  FilePath ->
  t ->
  Golden T.Text
mkGolden fileExt dir testcase actualOutput =
  Golden
    { output = actualOutput',
      encodePretty = T.unpack,
      writeToFile = TIO.writeFile,
      readFromFile = TIO.readFile,
      goldenFile = go "expected",
      actualFile = Just $ go "actual",
      failFirstTime = False
    }
  where
    actualOutput' = [i|#{actualOutput}|]
    go fileName = dir </> fileName -<.> fileExt
