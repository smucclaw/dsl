{-# LANGUAGE OverloadedLists #-}

module LS.XPile.MathLang.GenericMathLang.RPrel2opTable
  ( NumCompOp (..),
    rpRel2opTable
  )
where

import Data.HashMap.Strict qualified as Map
import Language.Haskell.TH.Syntax (Lift)

import LS.Types (RPRel (..))
import LS.XPile.MathLang.GenericMathLang.GenericMathLangAST
  ( CompOp (..),
    NumOp (..),
  )

rpRel2opTable :: Map.HashMap RPRel NumCompOp
rpRel2opTable =
  [ (RPsum, Num OpPlus),
    (RPproduct, Num OpMul),
    (RPminus, Num OpMinus),
    (RPdivide, Num OpDiv),
    (RPmodulo, Num OpModulo),
    (RPmax, Num OpMaxOf),
    (RPmin, Num OpMinOf),
    (RPlt, Comp OpLt),
    (RPlte, Comp OpLte),
    (RPgt, Comp OpGt),
    (RPgte, Comp OpGte),
    (RPeq, Comp OpNumEq)
  ]

data NumCompOp
  = Num NumOp
  | Comp CompOp
  deriving Lift