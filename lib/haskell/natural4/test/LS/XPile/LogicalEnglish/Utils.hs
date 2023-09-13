module LS.XPile.LogicalEnglish.Utils
  ( findWithDepth0,
  )
where

import qualified System.FilePath.Find as FileFind
import System.FilePath.Find (depth, (==?))

findWithDepth0 :: FileFind.FilterPredicate -> FilePath -> IO [FilePath]
findWithDepth0 = FileFind.find (depth ==? 0) 