{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Abstract parser functions that help build other parsers.

This module imports Control.Monad.Combinators.Expr which is the basis for the BoolStruct family of parsers.

-}
module LS.Parser where

import LS.Types
import LS.Rule
import LS.Tokens
import AnyAll qualified as AA

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Data.Text qualified as Text
import Data.List.NonEmpty (NonEmpty ((:|)))

data MyItem lbl a =
    MyLeaf                a
  | MyLabel  lbl (Maybe lbl) (MyItem lbl a)
  | MyAll     [MyItem lbl a]
  | MyAny     [MyItem lbl a]
  | MyNot     (MyItem lbl a)
  deriving (Eq, Show)
  deriving (Functor)

-- hm, shouldn't this be a MyItem (Maybe MultiTerm) to capture scenarios where there is no "any of the following" text?
type MyBoolStruct = MyItem MultiTerm

pBoolStruct :: Parser BoolStructT
pBoolStruct = prePostParse pOtherVal

-- | a boolstruct may carry optional labels -- see `AnyAll.Types.Label` for details. Here, we parse for pre-labels and post-labels
prePostParse :: (Show a, PrependHead a) => Parser a -> Parser (AA.OptionallyLabeledBoolStruct a)
prePostParse base = either fail pure . toBoolStruct =<< expr base

-- [TODO]: consider upgrading anyall's Item a to be a Label [TL.Text] rather than Label TL.Text
-- when we do that, we won't have to Text.unwords lab below.

toBoolStruct :: (Show a, PrependHead a) => MyBoolStruct a -> Either String (AA.OptionallyLabeledBoolStruct a)
toBoolStruct (MyLeaf txt)                    = pure $ AA.mkLeaf txt
toBoolStruct (MyLabel pre Nothing (MyAll xs))     = AA.mkAll (Just (AA.Pre     (mt2text pre))) <$> mapM toBoolStruct xs
toBoolStruct (MyLabel pre Nothing (MyAny xs))     = AA.mkAny (Just (AA.Pre     (mt2text pre))) <$> mapM toBoolStruct xs
toBoolStruct (MyLabel pre (Just post) (MyAll xs)) = AA.mkAll (Just (AA.PrePost (mt2text pre) (mt2text post))) <$> mapM toBoolStruct xs
toBoolStruct (MyLabel pre (Just post) (MyAny xs)) = AA.mkAny (Just (AA.PrePost (mt2text pre) (mt2text post))) <$> mapM toBoolStruct xs
toBoolStruct (MyAll mis)                     = AA.mkAll Nothing <$> mapM toBoolStruct mis
toBoolStruct (MyAny mis)                     = AA.mkAny Nothing <$> mapM toBoolStruct mis
toBoolStruct (MyNot mi')                     = AA.mkNot <$> toBoolStruct mi'
toBoolStruct (MyLabel pre post (MyLabel pre2 post2 _))  = Left $ "Nested labels not supported: " ++ show (MyLabel pre post (MyLeaf ()), MyLabel pre2 post2 (MyLeaf ()))
toBoolStruct (MyLabel pre _post (MyLeaf x))        = Left $ "Label " ++ show pre ++ " cannot be applied to a leaf: " ++ show x
toBoolStruct (MyLabel pre _post (MyNot x))         = Left $ "Label (" ++ show pre ++ ") followed by negation (" ++ show (MyNot x) ++ ") is not allowed"


-- | we build an expression parser on the primitives provided by the parser-combinators package.
-- These parsers look for AnyAll Labels above and below a base expression.
expr,exprIndent, term,termIndent, notLabelTerm :: (Show a) => Parser a -> Parser (MyBoolStruct a)
expr p = ppp $ debugName "expression" (makeExprParser (term p) table <?> "expression")
term p = termIndent p

exprIndent p = ppp $ debugName "expression indentable" (makeExprParser (termIndent p) table <?> "expression indentable")
termIndent p = debugName "termIndent p" $ do
  try (debugName "term p/1a:label ends directly above next line" $ do
        (lbl, inner) <- (,)
          $*| (someLiftSL pMTExpr <* liftSL (lookAhead pMTExpr))
          |>< expr p
        debugPrint $ "1a: got label, then inner immediately below: " ++ show lbl
        debugPrint $ "1a: got inner: " <> show inner
        return $ MyLabel lbl Nothing inner)
    <|>
    try (debugName "term p/1b:label ends to the left of line below, with EOL" $ do
        (lbl, inner) <- (,)
          $*| (someLiftSL pMTExpr) <* liftSL (debugName "matching EOL" dnl)
          |>< expr p
        debugPrint $ "1b: got label to the left, with EOL: " ++ show lbl
        debugPrint $ "1b: got inner: " ++ show inner
        return $ MyLabel lbl Nothing inner)
    <|>
    try (debugName "term p/1c:label ends to the right of line below" $ do
        (lbl,inner) <- (,)
          $*| (someLiftSL pMTExpr)
          |<| expr p
          |<$ undeepers
        debugPrint $ "1c: got label to the right of next line: " ++ show lbl
        debugPrint $ "1c: got inner: " ++ show inner
        return $ MyLabel lbl Nothing inner)
    <|>
     debugName "term p/notLabelTerm" (notLabelTerm p)


notLabelTerm p =
  try (debugName "term p/2:someIndentation expr p" (someIndentation (expr p)))
  <|> try (debugName "term p/3:plain p" (plain p) <?> "term")

table :: (Show a) => [[Operator Parser (MyBoolStruct a)]]
table = [ [ prefix  MPNot  MyNot  ]
        , [ binary  Or    myOr   ]
        , [ binary  Unless myUnless  ]
        , [ binary  And   myAnd ]
        -- , [ Prefix labelPrefix]
        , [ binary  SetLess   setLess  ]
        , [ binary  SetPlus   myOr  ]
        ]

labelPrefix :: Parser (MyBoolStruct a -> MyBoolStruct a)
labelPrefix = fmap (flip MyLabel Nothing . (:[])) . debugName "labelPrefix" $ do
  try (pMTExpr <* notEnd)

notEnd :: Parser ()
notEnd = notFollowedBy (pToken UnDeeper <|> pToken GoDeeper *> pTokenOneOf (Typically :| []))

-- SetPlus is an Or
-- SetLess is an And Not:   X LESS Y is X AND NOT Y

{- see note in README.org under "About the src/Parser.hs" -}

  -- term p/notLabelTerm has returned MyLabel ["pay"] (MyLabel ["to","the King"] (MyLeaf (("amount" :| ["$20"],Nothing) :| []))) :4_4:UnDeeper:

getAll :: MyItem lbl a -> [MyItem lbl a]
getAll (MyAll xs) = xs
getAll x = [x]

-- | Extracts leaf labels and combine 'All's into a single 'All'
myAnd,myOr,myUnless,setLess :: (Show lbl, Show a) => MyItem lbl a -> MyItem lbl a -> MyItem lbl a
myAnd (MyLabel pre post a@(MyLeaf _)) b = MyLabel pre post $ MyAll (a :  getAll b)
myAnd a b                          = MyAll (getAll a <> getAll b)

myOr (MyLabel pre post a@(MyLeaf _)) b = MyLabel pre post $ MyAny (a :  getAny b)
myOr a b                          = MyAny (getAny a <> getAny b)

myUnless (MyLabel pre post (MyAll as)) b = -- trace "myUnless: path 1" $
                                           MyLabel pre post $ MyAll (as ++ [MyNot b])
myUnless a (MyLabel pre post (MyAll bs)) = -- trace "myUnless: path 2" $
                                           MyLabel pre post $ MyAll (MyNot a: bs)
myUnless a b                             = -- trace "myUnless: path 3" $
                                           -- trace ("myUnless: path 3: a = " <> show a) $
                                           -- trace ("myUnless: path 3: b = " <> show b) $
                                           MyAll (a : [MyNot b])

setLess a (MyAll ((MyLeaf l):bs))
  | all (\b -> case b of
            MyNot _ -> True
            _       -> False
        ) bs = MyAll (a : MyNot (MyLeaf l) : bs)
setLess a b = MyAll (getAll a <> [MyNot b])

getAny :: MyItem lbl a -> [MyItem lbl a]
getAny (MyAny xs) = xs
getAny x = [x]

binary :: MyToken -> (a -> a -> a) -> Operator Parser a
binary  tname f = InfixR  (f <$ (debugName ("binary(" <> show tname <> ")") $ pToken tname))
prefix,postfix :: MyToken -> (a -> a) -> Operator Parser a
prefix  tname f = Prefix  (f <$ pToken tname)
postfix tname f = Postfix (f <$ pToken tname)
mylabel :: Operator Parser (MyBoolStruct Text.Text)
mylabel         = Prefix  (MyLabel <$> try (manyDeep pMTExpr) <*> pure Nothing)

plain :: Functor f => f a -> f (MyItem lbl a)
plain p = MyLeaf <$> p










ppp :: Show a => Parser (MyBoolStruct a) -> Parser (MyBoolStruct a)
ppp base = -- local (\rc -> rc { debug = True }) $
  try noPrePost <|>                  --          foo AND bar
  try (withPrePost noPrePost) <|>    -- both     foo AND bar    are required
  withPreOnly noPrePost              -- both     foo AND bar
  where
    noPrePost = debugName "(noPrePost) ppp inner" base

-- how many UnDeepers do we count between the end of this line and the start of the next (where, presumably, we get an OR)
expectUnDeepers :: Parser Int
expectUnDeepers = debugName "expectUnDeepers" $ lookAhead $ do
  ignored <- manyTill (pMTExpr <|> MTT "GD" <$ pToken GoDeeper) (lookAhead (pToken UnDeeper))
  debugPrint $ "ignoring " ++ show ignored
  udps <- some (pToken UnDeeper)
  debugPrint $ "matched undeepers " ++ show udps
  return $ length udps


withPrePost, withPreOnly :: Show a => Parser (MyBoolStruct a) -> Parser (MyBoolStruct a)
withPrePost basep = debugName "withPrePost" $ do
  (pre, body, post) <- (,,)
   -- this places the "cursor" in the column above the OR, after a sequence of pOtherVals,
    -- and to the left of the first, topmost term in the boolstruct
    $*| debugName "pre part" (fst <$> (pMTExpr /+= aboveNextLineKeyword))
    |-| debugName "made it to inner base parser" basep
    |<* debugName "post part" slMultiTerm -- post part
    |<$ undeepers
  return $ relabelpp body pre post
  where
    relabelpp :: MyBoolStruct a -> MultiTerm -> MultiTerm -> MyBoolStruct a
    relabelpp bs pre post = MyLabel pre (Just post) bs

withPreOnly basep = do -- debugName "withPreOnly" $ do
  (pre, body) <- (,)
   -- this places the "cursor" in the column above the OR, after a sequence of pOtherVals,
    -- and to the left of the first, topmost term in the boolstruct
    $*| debugName "pre part" (fst <$> (pMTExpr /+= aboveNextLineKeyword))
    |-| debugName "made it to inner parser" basep
    |<$ undeepers
  return $ relabelp body pre
  where
    relabelp :: MyBoolStruct a -> MultiTerm -> MyBoolStruct  a
    relabelp bs pre = MyLabel pre Nothing bs


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

aboveNextLineKeyword :: SLParser (MultiTerm,MyToken)
aboveNextLineKeyword = mkSL $ debugName "aboveNextLineKeyword" $ do
  undp_count <- expectUnDeepers
  debugPrint $ "aNLK: determined undp_count = " ++ show undp_count
  (slmt,n) <- runSL $ godeeper 1
                 *> (|>>) slMultiTerm
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

-- slightly different, unfortunately. see test/Spec.hs
aboveNextLineKeyword2 :: SLParser (MultiTerm,MyToken)
aboveNextLineKeyword2 = debugName "aboveNextLineKeyword2" $ do
  (_,x,y) <- (,,)
                 $*| return ((),0::Int)
                 ->| 1
                 |*| slMultiTerm
                 |<| choice (pToken <$> [ LS.Types.Or, LS.Types.And, LS.Types.Unless ])
  return (x,y)

