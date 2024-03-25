{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module LS.XPile.Edn.L4ToAst.RelToTextTable (relToTextTable) where

import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import LS.Types (RPRel (..), TComparison (..))

relToTextTable :: Map.HashMap RPRel T.Text
relToTextTable =
  [ (RPis, "IS"),
    (RPeq, "IS"),
    (RPlt, "<"),
    (RPlte, "<="),
    (RPgt, ">"),
    (RPgte, ">="),
    (RPelem, "IS IN"),
    (RPnotElem, "IS NOT IN"),
    (RPnot, "IS NOT"),
    (RPand, "AND"),
    (RPor, "OR"),
    (RPTC TBefore, "IS BEFORE"),
    (RPTC TAfter, "IS AFTER")
  ]
