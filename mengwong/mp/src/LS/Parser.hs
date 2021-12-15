{-# LANGUAGE OverloadedStrings #-}

module LS.Parser where

import LS.Types
import LS.Tokens
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

getAll :: MyItem lbl a -> [MyItem lbl a]
getAll (MyAll xs) = xs
getAll x = [x]

myAnd :: MyItem lbl a -> MyItem lbl a -> MyItem lbl a
myAnd (MyLabel lbl a@(MyLeaf _)) b = MyLabel lbl $ MyAll (a :  getAll b)
myAnd a b                          = MyAll (getAll a <> getAll b)

getAny :: MyItem lbl a -> [MyItem lbl a]
getAny (MyAny xs) = xs
getAny x = [x]

myOr :: MyItem lbl a -> MyItem lbl a -> MyItem lbl a
myOr (MyLabel lbl a@(MyLeaf _)) b = MyLabel lbl $ MyAny (a :  getAny b)
myOr a b                          = MyAny (getAny a <> getAny b)



binary  tname f = InfixR  (f <$ pToken tname)
prefix  tname f = Prefix  (f <$ pToken tname)
postfix tname f = Postfix (f <$ pToken tname)
mylabel         = Prefix  (MyLabel <$> try pOtherVal)

plain = MyLeaf <$> pOtherVal

myindented = between (pToken GoDeeper) (pToken UnDeeper)

-- 

