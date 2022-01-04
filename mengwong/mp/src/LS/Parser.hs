{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
  deriving (Functor)

deriving instance Functor (AA.Item' a)

type MyBoolStruct = MyItem MultiTerm

pBoolStruct :: Parser BoolStruct
pBoolStruct = toBoolStruct <$> expr pOtherVal

toBoolStruct :: Show a => MyBoolStruct a -> AA.Item a
toBoolStruct (MyLeaf txt) = AA.Leaf txt
toBoolStruct (MyLabel lab (MyAll xs)) = AA.All (Just (AA.Pre (Text.unwords lab))) (map toBoolStruct xs)
toBoolStruct (MyLabel lab (MyAny xs)) = AA.Any (Just (AA.Pre (Text.unwords lab))) (map toBoolStruct xs)
toBoolStruct (MyAll mis) = AA.All Nothing (map toBoolStruct mis)
toBoolStruct (MyAny mis) = AA.Any Nothing (map toBoolStruct mis)
toBoolStruct (MyNot mi') = AA.Not (toBoolStruct mi')
toBoolStruct (MyLabel  lab (MyLabel lab2 x)) = toBoolStruct (MyLabel (lab <> lab2) x)
toBoolStruct (MyLabel _lab (MyLeaf x)) = toBoolStruct (MyLeaf x)
toBoolStruct (MyLabel _lab (MyNot x)) = AA.Not $ toBoolStruct x

expr,term,notLabelTerm :: (Show a) => Parser a -> Parser (MyBoolStruct a)
expr p = makeExprParser (term p) table <?> "expression"
term p = debugName "term p" $ do
  try (debugName "term p/1:label" $ do
          lbl <- someDeep pNumOrText <* debugName "matching EOL" dnl
          debugPrint $ "got label then EOL: " ++ show lbl
          inner <- expr p
          debugPrint $ "got inner: " ++ show inner
          return $ MyLabel lbl inner)
    <|> debugName "term p/notLabelTerm" (notLabelTerm p)

notLabelTerm p =
  try (debugName "term p/2:myindented expr p" (myindented (expr p)))
  <|> try (debugName "term p/3:plain p" (plain p) <?> "term")

table :: [[Operator Parser (MyBoolStruct a)]]
table = [ [ prefix  MPNot  MyNot  ]
        , [ binary  Or    myOr   ]
        , [ binary  And   myAnd  ]
        ]

{- see note in README.org under "About the src/Parser.hs" -}

getAll :: MyItem lbl a -> [MyItem lbl a]
getAll (MyAll xs) = xs
getAll x = [x]

-- | Extracts leaf labels and combine 'All's into a single 'All'
myAnd :: MyItem lbl a -> MyItem lbl a -> MyItem lbl a
myAnd (MyLabel lbl a@(MyLeaf _)) b = MyLabel lbl $ MyAll (a :  getAll b)
myAnd a b                          = MyAll (getAll a <> getAll b)

getAny :: MyItem lbl a -> [MyItem lbl a]
getAny (MyAny xs) = xs
getAny x = [x]

myOr :: MyItem lbl a -> MyItem lbl a -> MyItem lbl a
myOr (MyLabel lbl a@(MyLeaf _)) b = MyLabel lbl $ MyAny (a :  getAny b)
myOr a b                          = MyAny (getAny a <> getAny b)

binary :: MyToken -> (a -> a -> a) -> Operator Parser a
binary  tname f = InfixR  (f <$ (debugName ("binary(" <> show tname <> ")") $ pToken tname))
prefix,postfix :: MyToken -> (a -> a) -> Operator Parser a
prefix  tname f = Prefix  (f <$ pToken tname)
postfix tname f = Postfix (f <$ pToken tname)
mylabel :: Operator Parser (MyBoolStruct Text.Text)
mylabel         = Prefix  (MyLabel <$> try (manyDeep pOtherVal))

plain :: Functor f => f a -> f (MyItem lbl a)
plain p = MyLeaf <$> p

-- we can't use the expr style parser to deal with things that go deeper; only expressions whose terms are at basically the same level of indentation. so we don't use it for the insides of relationalpredicates, only linking them up with a AND/OR/NOT to create a BoolStructR
