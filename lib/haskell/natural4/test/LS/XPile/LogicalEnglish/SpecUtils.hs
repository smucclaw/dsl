module LS.XPile.LogicalEnglish.SpecUtils
  ( findWithDepth0
  )
where

import System.FilePath.Find (FilterPredicate, depth, find, (==?))

findWithDepth0 :: FilterPredicate -> FilePath -> IO [FilePath]
findWithDepth0 = find (depth ==? 0) 