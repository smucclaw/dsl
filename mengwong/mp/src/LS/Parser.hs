{-# LANGUAGE OverloadedStrings #-}

module LS.Parser where

import LS.BasicTypes
import LS.Types
import LS.Lib
import qualified AnyAll as AA

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import qualified Data.Text.Lazy as Text


data MyItem lbl a =
    MyLeaf                a
  | MyLabel           lbl (MyItem lbl a)
  | MyAll     [MyItem lbl a]
  | MyAny     [MyItem lbl a]
  | MyNot     (MyItem lbl a)
  deriving (Eq, Show)




type MyBoolStruct = MyItem Text.Text Text.Text

expr :: Parser MyBoolStruct
expr = makeExprParser term table <?> "expression"

term = myindented expr <|> try (MyLabel <$> pOtherVal <*> plain) <|> plain <?> "term"

table = [ {- [ mylabel ]
        ,-} [ prefix  MPNot MyNot ]
        , [ binary  Or    myOr   ]
        , [ binary  And   myAnd  ]
        ]

myAnd :: MyItem lbl a -> MyItem lbl a -> MyItem lbl a
myAnd (MyAll a) (MyAll b) = MyAll (a <>  b)
myAnd (MyAll a) b         = MyAll (a <> [b])
myAnd        a  (MyAll b) = MyAll (a :   b)
myAnd        a  b         = MyAll [a ,   b]

myOr :: MyItem lbl a -> MyItem lbl a -> MyItem lbl a
myOr  (MyAny a) (MyAny b) = MyAny (a <> b)
myOr  (MyAny a) b         = MyAny (a <> [b])
myOr         a  (MyAny b) = MyAny (a :   b)
myOr         a  b         = MyAny [a ,   b]



binary  tname f = InfixL  (f <$ pToken tname)
prefix  tname f = Prefix  (f <$ pToken tname)
postfix tname f = Postfix (f <$ pToken tname)
mylabel         = Prefix  (MyLabel <$> try pOtherVal)

plain = MyLeaf <$> pOtherVal

myindented = between (pToken GoDeeper) (pToken UnDeeper)

-- 

