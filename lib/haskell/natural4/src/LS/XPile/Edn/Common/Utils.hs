{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.Edn.Common.Utils
  ( listToPairs,
    pairsToList,
    splitLast
  )
where

import Control.Arrow ((>>>))
import Data.Foldable qualified as Fold
import Deque.Strict qualified as Deque
import GHC.IsList qualified as IsList
import Safe (tailSafe)

listToPairs :: (Foldable t) => t a -> [(a, a)]
listToPairs (Fold.toList -> xs) =
  [(x, y) | (x, y, index) <- zip3 xs (tailSafe xs) [0..], even index]

pairsToList :: (Foldable t) => t (a, a) -> [a]
pairsToList = foldMap \(x, y) -> [x, y]

splitLast :: [a] -> Maybe ([a], a)
splitLast =
  IsList.fromList
    >>> Deque.uncons
    >>> fmap \(lastElem, xs) -> (Fold.toList xs, lastElem)