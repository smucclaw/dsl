{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module LS.XPile.Edn.L4ToAst.MultiExprKeywords
  ( multiExprKeywords,
  )
where

import Data.HashSet qualified as Set
import Data.Text qualified as T

multiExprKeywords :: Set.HashSet T.Text
multiExprKeywords =
  [ "day",
    "days",
    "week",
    "weeks",
    "month",
    "months",

    "before",
    "after",
    "within",

    "the list of all",
    "such that"
  ]