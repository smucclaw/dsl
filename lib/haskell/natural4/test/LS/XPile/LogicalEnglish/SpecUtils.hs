module LS.XPile.LogicalEnglish.SpecUtils
  ( findWithDepth0
  )
where

import Control.Monad.Except
    ( runExceptT, MonadError(throwError), ExceptT )
import System.FilePath.Find (depth, (==?))
import System.FilePath.Find qualified as FileFind

findWithDepth0 :: FileFind.FilterPredicate -> FilePath -> IO [FilePath]
findWithDepth0 = FileFind.find (depth ==? 0) 