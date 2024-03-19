{-# LANGUAGE BlockArguments #-}

module LS.XPile.Edn.Utils
  ( listToPairs,
    pairsToList,
  )
where

listToPairs :: [a] -> [(a, a)]
listToPairs xs =
  [(x, y) | (x, y, index) <- zip3 xs (tail xs) [0..], even index]

pairsToList :: [(a, a)] -> [a]
pairsToList = foldMap \(x, y) -> [x, y]