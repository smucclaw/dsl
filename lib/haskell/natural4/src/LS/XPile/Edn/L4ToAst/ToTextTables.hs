{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module LS.XPile.Edn.L4ToAst.ToTextTables
  ( rpRelToTextTable,
    paramTypeToTextTable
  )
where

import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import LS.Types (ParamType (..), RPRel (..), TComparison (..))

rpRelToTextTable :: Map.HashMap RPRel T.Text
rpRelToTextTable =
  [ (RPis, "IS"),
    (RPeq, "EQUALS"),
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
    (RPsum, "SUM"),
    (RPproduct, "PRODUCT"),
    (RPmin, "MIN"),
    (RPmax, "MAX")
  ]

paramTypeToTextTable :: Map.HashMap ParamType T.Text
paramTypeToTextTable =
  [ (TOne, "ONE"),
    (TOptional, "OPTIONAL"),
    (TList0, "LIST"),
    (TList1, "LIST"),
    (TSet0, "SET"),
    (TSet1, "SET")
  ]