{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module LS.XPile.MathLang.GenericMathLang.ArithOps
  ( allArithOps,
    arithOps,
    arithCompareOps,
  )
where

import Data.HashSet qualified as Set
import Data.Text qualified as T

allArithOps :: Set.HashSet T.Text
allArithOps = mconcat [arithOps, arithCompareOps]

arithOps :: Set.HashSet T.Text
arithOps = ["+", "*", "-", "/"]

arithCompareOps :: Set.HashSet T.Text
arithCompareOps = ["<", "<=", ">", ">=", "=="]