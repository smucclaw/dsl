{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module LS.XPile.Edn.L4ToAst.RPRelToTextTable
  ( rpRelToTextTable,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import LS.Types (RPRel (..), TComparison (..))

rpRelToTextTable :: Map.HashMap RPRel T.Text
rpRelToTextTable =
  [ (RPis, "IS"),
    (RPeq, "IS"),
    (RPlt, "<"),
    (RPlte, "<="),
    (RPgt, ">"),
    (RPgte, ">="),
    (RPelem, "IN"),
    (RPnotElem, "NOT IN"),
    (RPnot, "NOT"),
    (RPand, "AND"),
    (RPor, "OR"),
    (RPTC TBefore, "BEFORE"),
    (RPTC TAfter, "AFTER"),
    (RPsum, "THE SUM OF"),
    (RPproduct, "THE PRODUCT OF"),
    (RPmin, "THE MIN OF"),
    (RPmax, "THE MAX OF")
  ]
