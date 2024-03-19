{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.Edn.Utils
  ( listToPairs,
    pairsToList,
  )
where

import Data.Foldable qualified as Fold

listToPairs :: Foldable t => t a -> [(a, a)]
listToPairs (Fold.toList -> xs) =
  [(x, y) | (x, y, index) <- zip3 xs (tail xs) [0..], even index]

pairsToList :: Foldable t => t (a, a) -> [a]
pairsToList = foldMap \(x, y) -> [x, y]