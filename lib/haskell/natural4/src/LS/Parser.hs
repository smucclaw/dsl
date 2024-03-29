{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Abstract parser functions that help build other parsers.

This module imports Control.Monad.Combinators.Expr which is the basis for the BoolStruct family of parsers.

-}
module LS.Parser
  ( MyBoolStruct,
    MyItem (MyAll, MyAny, MyLabel, MyLeaf, MyNot),
    aboveNextLineKeyword2,
    binary,
    expr,
    pBoolStruct,
    prefix,
    prePostParse,
  )
where

import AnyAll qualified as AA
import Control.Monad.Combinators.Expr
  ( Operator (InfixR, Postfix, Prefix),
    makeExprParser,
  )
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Tuple.Curry (Curry, uncurryN)
import LS.Rule (Parser)
import LS.Tokens
  ( IsParser (debugName, debugPrint),
    SLParser,
    dnl,
    godeeper,
    liftSL,
    manyDeep,
    mkSL,
    pMTExpr,
    pOtherVal,
    pToken,
    pTokenOneOf,
    runSL,
    slMultiTerm,
    someIndentation,
    someLiftSL,
    undeepers,
    ($*|),
    (+>|),
    (->|),
    (/+=),
    (|*|),
    (|--),
    (|-|),
    (|<$),
    (|<*),
    (|<|),
    (|><),
    (|>>),
  )
import LS.Types
  ( BoolStructT,
    MTExpr (MTT),
    MultiTerm,
    MyToken
      ( And,
        GoDeeper,
        MPNot,
        Or,
        SetLess,
        SetPlus,
        Typically,
        UnDeeper,
        Unless
      ),
    PrependHead,
    mt2text,
  )
import Text.Megaparsec
  ( MonadParsec (lookAhead, notFollowedBy, try),
    choice,
    manyTill,
    some,
    (<?>),
    (<|>),
  )

data MyItem lbl a =
    MyLeaf                a
  | MyLabel  lbl (Maybe lbl) (MyItem lbl a)
  | MyAll     [MyItem lbl a]
  | MyAny     [MyItem lbl a]
  | MyNot     (MyItem lbl a)
  deriving (Eq, Functor, Show)

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
toBoolStruct (MyLabel pre Nothing (MyAll xs))     = AA.mkAll (Just (AA.Pre     (mt2text pre))) <$> traverse toBoolStruct xs
toBoolStruct (MyLabel pre Nothing (MyAny xs))     = AA.mkAny (Just (AA.Pre     (mt2text pre))) <$> traverse toBoolStruct xs
toBoolStruct (MyLabel pre (Just post) (MyAll xs)) = AA.mkAll (Just (AA.PrePost (mt2text pre) (mt2text post))) <$> traverse toBoolStruct xs
toBoolStruct (MyLabel pre (Just post) (MyAny xs)) = AA.mkAny (Just (AA.PrePost (mt2text pre) (mt2text post))) <$> traverse toBoolStruct xs
toBoolStruct (MyAll mis)                     = AA.mkAll Nothing <$> traverse toBoolStruct mis
toBoolStruct (MyAny mis)                     = AA.mkAny Nothing <$> traverse toBoolStruct mis
toBoolStruct (MyNot mi')                     = AA.mkNot <$> toBoolStruct mi'
toBoolStruct (MyLabel pre post (MyLabel pre2 post2 _))  = Left $ "Nested labels not supported: " ++ show (MyLabel pre post (MyLeaf ()), MyLabel pre2 post2 (MyLeaf ()))
toBoolStruct (MyLabel pre _post (MyLeaf x))        = Left $ "Label " ++ show pre ++ " cannot be applied to a leaf: " ++ show x
toBoolStruct (MyLabel pre _post (MyNot x))         = Left $ "Label (" ++ show pre ++ ") followed by negation (" ++ show (MyNot x) ++ ") is not allowed"


-- | we build an expression parser on the primitives provided by the parser-combinators package.
-- These parsers look for AnyAll Labels above and below a base expression.
expr,exprIndent, term,termIndent, notLabelTerm :: (Show a) => Parser a -> Parser (MyBoolStruct a)
expr p = ppp $ debugName "expression" (makeExprParser (term p) table <?> "expression")
term = termIndent

exprIndent p = ppp $ debugName "expression indentable" (makeExprParser (termIndent p) table <?> "expression indentable")
termIndent p = debugName "termIndent p" do
  try (debugName "term p/1a:label ends directly above next line" do
        (lbl, inner) <- (,)
          $*| (someLiftSL pMTExpr <* liftSL (lookAhead pMTExpr))
          |>< expr p
        debugPrint [i|1a: got label, then inner immediately below: #{lbl}|]
        debugPrint [i|1a: got inner: #{inner}|]
        pure $ MyLabel lbl Nothing inner)
    <|>
    try (debugName "term p/1b:label ends to the left of line below, with EOL" do
        (lbl, inner) <- (,)
          $*| someLiftSL pMTExpr <* liftSL (debugName "matching EOL" dnl)
          |>< expr p
        debugPrint [i|1b: got label to the left, with EOL: #{lbl}|]
        debugPrint [i|1b: got inner: #{inner}|]
        pure $ MyLabel lbl Nothing inner)
    <|>
    try (debugName "term p/1c:label ends to the right of line below" do
        (lbl,inner) <- (,)
          $*| someLiftSL pMTExpr
          |<| expr p
          |<$ undeepers
        debugPrint [i|1c: got label to the right of next line: #{lbl}|]
        debugPrint [i|1c: got inner: #{inner}|]
        pure $ MyLabel lbl Nothing inner)
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
labelPrefix =
  flip MyLabel Nothing . (: [])
    <$> debugName "labelPrefix" do try (pMTExpr <* notEnd)

notEnd :: Parser ()
notEnd =
  notFollowedBy $
    pToken UnDeeper <|> pToken GoDeeper *> pTokenOneOf (Typically :| [])

-- SetPlus is an Or
-- SetLess is an And Not:   X LESS Y is X AND NOT Y

{- see note in README.org under "About the src/Parser.hs" -}

  -- term p/notLabelTerm has returned MyLabel ["pay"] (MyLabel ["to","the King"] (MyLeaf (("amount" :| ["$20"],Nothing) :| []))) :4_4:UnDeeper:

getAll :: MyItem lbl a -> [MyItem lbl a]
getAll (MyAll xs) = xs
getAll x = [x]

-- | Extracts leaf labels and combine 'All's into a single 'All'
myAnd :: MyItem lbl a -> MyItem lbl a -> MyItem lbl a
myAnd = myAndOr MyAll getAll

myOr :: MyItem lbl a -> MyItem lbl a -> MyItem lbl a
myOr = myAndOr MyAny getAny

myAndOr ::
  ([MyItem lbl a1] -> MyItem lbl a2) ->
  (MyItem lbl a1 -> [MyItem lbl a1]) ->
  MyItem lbl a1 ->
  MyItem lbl a1 ->
  MyItem lbl a2
myAndOr ctor get (MyLabel pre post a@(MyLeaf _)) b =
  MyLabel pre post $ ctor $ a : get b
myAndOr ctor get a b = ctor $ get a <> get b

myUnless :: MyItem lbl a -> MyItem lbl a -> MyItem lbl a
myUnless (MyLabel pre post (MyAll as)) b = -- trace "myUnless: path 1" $
                                           MyLabel pre post $ MyAll (as ++ [MyNot b])
myUnless a (MyLabel pre post (MyAll bs)) = -- trace "myUnless: path 2" $
                                           MyLabel pre post $ MyAll (MyNot a: bs)
myUnless a b                             = -- trace "myUnless: path 3" $
                                           -- trace ("myUnless: path 3: a = " <> show a) $
                                           -- trace ("myUnless: path 3: b = " <> show b) $
                                           MyAll (a : [MyNot b])

setLess :: MyItem lbl a -> MyItem lbl a -> MyItem lbl a
setLess a (MyAll (MyLeaf l : bs))
  | all isMyNot bs = MyAll (a : MyNot (MyLeaf l) : bs)
  where
    isMyNot (MyNot _) = True
    isMyNot _ = False
setLess a b = MyAll $ getAll a <> [MyNot b]

getAny :: MyItem lbl a -> [MyItem lbl a]
getAny (MyAny xs) = xs
getAny x = [x]

binary :: MyToken -> (a -> a -> a) -> Operator Parser a
binary = binaryPrefixPostfix InfixR \tname -> debugName [i|binary(#{tname})|]

prefix :: MyToken -> (a -> a) -> Operator Parser a
prefix  = binaryPrefixPostfix Prefix \_ x -> x

postfix :: MyToken -> (a -> a) -> Operator Parser a
postfix  = binaryPrefixPostfix Postfix \_ x -> x

binaryPrefixPostfix ::
  Functor f => (f a -> b1) -> (MyToken -> Parser MyToken -> f b2) -> MyToken -> a -> b1
binaryPrefixPostfix ctor debug tname f = ctor $ f <$ debug tname (pToken tname)

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
expectUnDeepers = debugName "expectUnDeepers" $ lookAhead do
  ignored <- manyTill (pMTExpr <|> MTT "GD" <$ pToken GoDeeper) (lookAhead (pToken UnDeeper))
  debugPrint [i|ignoring #{ignored}|]
  udps <- some (pToken UnDeeper)
  debugPrint [i|matched undeepers #{udps}|]
  pure $ length udps

withPrePost :: Show a => Parser (MyBoolStruct a) -> Parser (MyBoolStruct a)
withPrePost = debugName "withPrePost"
  . withPrePostOnly (,,) (|<* debugName "post part" slMultiTerm) relabelpp
  where
    relabelpp :: MultiTerm -> MyBoolStruct a -> MultiTerm -> MyBoolStruct a
    relabelpp pre bs post = MyLabel pre (Just post) bs

withPreOnly :: Show a => Parser (MyBoolStruct a) -> Parser (MyBoolStruct a)
withPreOnly = -- debugName "withPreOnly" .
  withPrePostOnly (,) id relabelp
  where
    relabelp :: MultiTerm -> MyBoolStruct a -> MyBoolStruct a
    relabelp pre = MyLabel pre Nothing

withPrePostOnly ::
  (Show a, Curry (t -> b1) b2) =>
  ([MTExpr] -> a -> b3) ->
  (SLParser b3 -> SLParser t) ->
  b2 ->
  Parser a -> Parser b1
withPrePostOnly tupleCtor postPart relabel basep = do
  preBodyPost <-
    -- this places the "cursor" in the column above the OR, after a sequence of pOtherVals,
    -- and to the left of the first, topmost term in the boolstruct
    postPart
      ( tupleCtor
          $*| debugName "pre part" (fst <$> (pMTExpr /+= aboveNextLineKeyword))
          |-| debugName "made it to inner base parser" basep
      )
      -- \|<* debugName "" slMultiTerm
      |<$ undeepers
  pure $ uncurryN relabel preBodyPost

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
aboveNextLineKeyword = mkSL $ debugName "aboveNextLineKeyword" do
  undp_count <- expectUnDeepers
  debugPrint [i|aNLK: determined undp_count = #{undp_count}|]
  (slmt,n) <- runSL $ godeeper 1
                 *> (|>>) slMultiTerm
                 |-- (\d -> debugPrint [i|aNLK: current depth is #{d}|])
  (tok,m) <- runSL $ id
             +>| n
             |<| choice (pToken <$> [ LS.Types.Or, LS.Types.And, LS.Types.Unless ])

  debugPrint [i|aNLK: slMultiTerm is #{slmt}|]

  if n == undp_count
    then pure ((slmt, tok), m)
    else fail [i|aNLK: expecting depth #{undp_count} but the cursor seems to be placed such that we have #{n}; a different backtrack will probably fare better|]

-- aboveNextLineKeyword has returned ((["foo1","foo2","foo3"],Or),1)
-- aboveNextLineKeyword has returned ((["foo2","foo3"],       Or),0)
-- aboveNextLineKeyword has returned ((["foo3"],              Or),-1) -- to get this, maxDepth=0

-- slightly different, unfortunately. see test/Spec.hs
aboveNextLineKeyword2 :: SLParser (MultiTerm,MyToken)
aboveNextLineKeyword2 = debugName "aboveNextLineKeyword2" do
  (_,x,y) <- (,,)
                 $*| return ((),0::Int)
                 ->| 1
                 |*| slMultiTerm
                 |<| choice (pToken <$> [ LS.Types.Or, LS.Types.And, LS.Types.Unless ])
  pure (x,y)
