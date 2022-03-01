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
pBoolStruct = prePostParse pOtherVal

prePostParse :: Show a => Parser a -> Parser (AA.Item a)
prePostParse base = ppp $ toBoolStruct <$> expr base

-- [TODO]: consider upgrading anyall's Item a to be a Label [TL.Text] rather than Label TL.Text
-- when we do that, we won't have to Text.unwords lab below.

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
  try (debugName "term p/1a:label directly above" $ do
        (lbl, inner) <- (,)
          $*| (someLiftSL pNumOrText <* liftSL (lookAhead pNumOrText))
          |>< expr p
        debugPrint $ "got label, then inner immediately below: " ++ show lbl
        debugPrint $ "got inner: " <> show inner
        return $ MyLabel lbl inner)
    <|>
    try (debugName "term p/b:label to the left of line below, with EOL" $ do
        lbl <- someSLPlain pNumOrText <* debugName "matching EOL" dnl
        debugPrint $ "got label then EOL: " ++ show lbl
        inner <- expr p
        debugPrint $ "got inner: " ++ show inner
        return $ MyLabel lbl inner)
    <|> debugName "term p/notLabelTerm" (notLabelTerm p)


notLabelTerm p =
  try (debugName "term p/2:someIndentation expr p" (someIndentation (expr p)))
  <|> try (debugName "term p/3:plain p" (plain p) <?> "term")

table :: [[Operator Parser (MyBoolStruct a)]]
table = [ [ prefix  MPNot  MyNot  ]
        , [ binary  Or    myOr   ]
        , [ binary  And   myAnd  ]
        , [ binary  SetLess   setLess  ]
        , [ binary  SetPlus   myOr  ]
        ]

-- SetPlus is an Or
-- SetLess is an And Not:   X LESS Y is X AND NOT Y

{- see note in README.org under "About the src/Parser.hs" -}

  -- term p/notLabelTerm has returned MyLabel ["pay"] (MyLabel ["to","the King"] (MyLeaf (("amount" :| ["$20"],Nothing) :| []))) :4_4:UnDeeper:

getAll :: MyItem lbl a -> [MyItem lbl a]
getAll (MyAll xs) = xs
getAll x = [x]

-- | Extracts leaf labels and combine 'All's into a single 'All'
myAnd :: MyItem lbl a -> MyItem lbl a -> MyItem lbl a
myAnd (MyLabel lbl a@(MyLeaf _)) b = MyLabel lbl $ MyAll (a :  getAll b)
myAnd a b                          = MyAll (getAll a <> getAll b)

setLess :: MyItem lbl a -> MyItem lbl a -> MyItem lbl a
setLess a (MyAll ((MyLeaf l):bs))
  | all (\b -> case b of
            MyNot _ -> True
            _       -> False
        ) bs = MyAll (a : MyNot (MyLeaf l) : bs)
setLess a b = MyAll (getAll a <> [MyNot b])

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










ppp :: Show a => Parser (AA.Item a) -> Parser (AA.Item a)
ppp base = -- local (\rc -> rc { debug = True }) $
  try noPrePost <|> try (withPrePost noPrePost) <|> withPreOnly noPrePost
  where
    noPrePost = debugName "ppp inner" base

-- how many UnDeepers do we count between the end of this line and the start of the next (where, presumably, we get an OR)
expectUnDeepers :: Parser Int
expectUnDeepers = debugName "expectUnDeepers" $ lookAhead $ do
  ignored <- manyTill (pNumOrText <|> "GD" <$ pToken GoDeeper) (lookAhead (pToken UnDeeper))
  debugPrint $ "ignoring " ++ show ignored
  udps <- some (pToken UnDeeper)
  debugPrint $ "matched undeepers " ++ show udps
  return $ length udps


withPrePost, withPreOnly :: Show a => Parser (AA.Item a) -> Parser (AA.Item a)
withPrePost basep = debugName "withPrePost" $ do
  (pre, body, post) <- (,,)
   -- this places the "cursor" in the column above the OR, after a sequence of pOtherVals,
    -- and to the left of the first, topmost term in the boolstruct
    $*| debugName "pre part" (fst <$> (pOtherVal /+= aboveNextLineKeyword))
    |-| debugName "made it to inner base parser" basep
    |<* debugName "post part" slMultiTerm -- post part
    |<$ undeepers
  return $ relabelpp body (Text.unwords pre) (Text.unwords post)
  where
    relabelpp :: AA.Item a -> Text.Text -> Text.Text -> AA.Item  a
    relabelpp (AA.All Nothing xs) pre post = AA.All (Just $ AA.PrePost pre post) xs
    relabelpp (AA.Any Nothing xs) pre post = AA.Any (Just $ AA.PrePost pre post) xs
    relabelpp _ _ _ = error "RelationalPredicates: relabelpp failed"

withPreOnly basep = debugName "withPreOnly" $ do
  (pre, body) <- (,)
   -- this places the "cursor" in the column above the OR, after a sequence of pOtherVals,
    -- and to the left of the first, topmost term in the boolstruct
    $*| debugName "pre part" (fst <$> (pOtherVal /+= aboveNextLineKeyword))
    |-| debugName "made it to inner parser" basep
    |<$ undeepers
  return $ relabelp body (Text.unwords pre)
  where
    relabelp :: AA.Item a -> Text.Text -> AA.Item  a
    relabelp  (AA.All Nothing xs) pre      = AA.All (Just $ AA.Pre     pre)      xs
    relabelp  (AA.Any Nothing xs) pre      = AA.Any (Just $ AA.Pre     pre)      xs
    relabelp  _ _ = error "RelationalPredicates: relabelp failed"


-- | represent the RHS part of an (LHS = Label Pre, RHS = first-term-of-a-BoolStruct) start of a BoolStruct
-- 
-- suppose the input is:
-- > | MEANS | each of the following | elements |       |  | apple   |
-- > |       |                       |          |   OR  |  | banana  |
-- > |       |                       |          |   OR  |  | cabbage |
-- > |       | qualifies             |          |       |  |         |
--
-- the calling function (withPrePost or withPreOnly) calls us as a (pOtherVal /+= aboveNextLineKeyword)
-- the aboveNextLineKeyword matches exactly a GoDeeper, GoDeeper, apple, UnDeeper, UnDeeper, OR as a lookAhead.
-- it does this by counting the number of UnDeepers seen before the OR, and it matches the same number of GoDeepers;
-- it fails on any other number of GoDeepers encountered, so the caling /+= can break on the correct location between LHS,RHS

aboveNextLineKeyword :: SLParser ([Text.Text],MyToken)
aboveNextLineKeyword = mkSL $ debugName "aboveNextLineKeyword" $ do
  undp_count <- expectUnDeepers
  debugPrint $ "aNLK: determined undp_count = " ++ show undp_count
  ((_,slmt),n) <- runSL $ (,)
                 $*| return () -- this just gets us from (,,) into the SLParser context
                 ->| 1
                 |*| slMultiTerm
                 |-- (\d -> debugPrint $ "aNLK: current depth is " ++ show d)
  (tok,m) <- runSL $ id
             +>| n
             |<| choice (pToken <$> [ LS.Types.Or, LS.Types.And, LS.Types.Unless ])

  debugPrint $ "aNLK: slMultiTerm is " ++ show slmt

  if n == undp_count
    then return ((slmt, tok), m)
    else fail $ "aNLK: expecting depth " ++ show undp_count ++ " but the cursor seems to be placed such that we have " ++ show n ++ "; a different backtrack will probably fare better"
  
-- aboveNextLineKeyword has returned ((["foo1","foo2","foo3"],Or),1)
-- aboveNextLineKeyword has returned ((["foo2","foo3"],       Or),0)
-- aboveNextLineKeyword has returned ((["foo3"],              Or),-1) -- to get this, maxDepth=0
