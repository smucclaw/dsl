module LS.XPile.LogicalEnglish.SpecUtils
  ( findWithDepth0,
    modifyError
  )
where

import Control.Monad.Except
    ( runExceptT, MonadError(throwError), ExceptT )
import System.FilePath.Find (depth, (==?))
import System.FilePath.Find qualified as FileFind

findWithDepth0 :: FileFind.FilterPredicate -> FilePath -> IO [FilePath]
findWithDepth0 = FileFind.find (depth ==? 0) 

-- | Backported from mtl-2.3.1
modifyError :: MonadError e' m => (e -> e') -> ExceptT e m a -> m a
modifyError f m = runExceptT m >>= either (throwError . f) pure