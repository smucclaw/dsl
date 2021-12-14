{-# LANGUAGE OverloadedStrings #-}

module LS.Parser where

import LS.BasicTypes
import LS.Types
import LS.Lib
import qualified AnyAll as AA

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import qualified Data.Text.Lazy as Text

expr :: Parser BoolStruct
expr = makeExprParser term table <?> "expression"

term = myindented expr <|> plain <?> "term"

table = [ [ prefix  MPNot AA.Not ]
        , [ binary  Or    aaOr   ]
        , [ binary  And   aaAnd  ]
        ]

aaOr  a b = AA.Any Nothing [ a, b ]
aaAnd a b = AA.All Nothing [ a, b ]

binary  tname f = InfixL  (f <$ pToken tname)
prefix  tname f = Prefix  (f <$ pToken tname)
postfix tname f = Postfix (f <$ pToken tname)

plain = AA.Leaf <$> pOtherVal

myindented = between (pToken GoDeeper) (pToken UnDeeper)
