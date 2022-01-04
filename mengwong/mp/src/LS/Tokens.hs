{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module LS.Tokens (module LS.Tokens, module Control.Monad.Reader) where

import qualified Data.Set           as Set
import qualified Data.Text.Lazy as Text
import Text.Megaparsec
import Control.Monad.Reader (asks, local)
import Control.Monad.Writer.Lazy
import Data.List (intercalate)

import LS.Types
import Debug.Trace (traceM)
import Control.Applicative (liftA2)

-- "discard newline", a reference to GNU Make
dnl :: Parser MyToken
-- -- dnl = many $ pToken EOL
dnl = pToken EOL
-- dnl = some $ pToken EOL

pDeontic :: Parser Deontic
pDeontic = (pToken Must  >> return DMust)
           <|> (pToken May   >> return DMay)
           <|> (pToken Shant >> return DShant)

pNumber :: Parser Integer
pNumber = token test Set.empty <?> "number"
  where
    test (WithPos _ _ _ (TNumber n)) = Just n
    test _ = Nothing

-- return the text inside an Other value. This implicitly serves to test for Other, similar to a pToken test.
pOtherVal :: Parser Text.Text
pOtherVal = token test Set.empty <?> "Other text"
  where
    test (WithPos _ _ _ (Other t)) = Just t
    test _ = Nothing

getToken :: Parser MyToken
getToken = token test Set.empty <?> "any token"
  where
    test (WithPos _ _ _ tok) = Just tok

getWithPos :: Parser String
getWithPos = token test Set.empty <?> "any token"
  where
    test wp@(WithPos _ _ _ tok)
      | tok `elem` [GoDeeper, UnDeeper, EOL] = showpos wp
      | otherwise                            = showpos wp
    showtok wp = Just $ show $ tokenVal wp
    showpos wp = Just $
      show (unPos $ sourceLine   $ startPos wp) ++ "." ++
      show (unPos $ sourceColumn $ startPos wp) ++ ":" ++
      show (tokenVal wp)

tokenViewColumnSize :: Int
tokenViewColumnSize = 15

myTraceM :: String -> Parser ()
myTraceM x = whenDebug $ do
  nestDepth <- asks nestLevel
  lookingAt <- lookAhead getWithPos <|> ("EOF" <$ eof)
  traceM $ leftPad lookingAt tokenViewColumnSize <> indentShow nestDepth <> x
  where
    indentShow depth = concat $ replicate depth "| "
    leftPad str n = take n $ str <> repeat ' '

getTokenNonDeep :: Parser MyToken
getTokenNonDeep = token test Set.empty <?> "any token except GoDeeper / UnDeeper"
  where
    test (WithPos _ _ _ GoDeeper) = Nothing
    test (WithPos _ _ _ UnDeeper) = Nothing
    test (WithPos _ _ _ tok) = Just tok

getTokenNonEOL :: Parser MyToken
getTokenNonEOL = token test Set.empty <?> "any token except EOL"
  where
    test (WithPos _ _ _ EOL) = Nothing
    test (WithPos _ _ _ tok) = Just tok


-- pInt :: Parser Int
-- pInt = token test Set.empty <?> "integer"
--   where
--     test (WithPos _ _ _ (Int n)) = Just n
--     test _ = Nothing

-- pSum :: Parser (Int, Int)
-- pSum = do
--   a <- pInt
--   _ <- pToken Plus
--   b <- pInt
--   return (a, b)

-- egStream :: String -> MyStream
-- egStream x = MyStream x (parseMyStream x)







pSrcRef :: Parser (Maybe RuleLabel, Maybe SrcRef)
pSrcRef = do
  rlabel' <- optional pRuleLabel
  leftY  <- lookAhead pYLocation -- this is the column where we expect IF/AND/OR etc.
  leftX  <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.
  srcurl <- asks sourceURL
  return (rlabel', Just $ SrcRef srcurl srcurl leftX leftY Nothing)


pNumAsText :: Parser Text.Text
pNumAsText = debugName "pNumAsText" $ do
  (TNumber n) <- pTokenMatch isNumber (TNumber 1234)
  return (Text.pack $ show n)
  where
    isNumber (TNumber _) = True
    isNumber _           = False

-- ["investment"] Is ["savings"] becomes
-- investment(savings)

-- ["Minsavings"] Is ["500"] becomes
-- Minsavings is 500

-- it all depends if the first letter is uppercase
-- ["dependents"] Is ["5"] becomes
-- dependents(5)
-- ["Dependents"] Is ["5"] becomes
-- dependents is 5


pRuleLabel :: Parser RuleLabel
pRuleLabel = debugName "pRuleLabel" $ do
  (RuleMarker i sym) <- pTokenMatch isRuleMarker (RuleMarker 1 "§")
  actualLabel  <- someIndentation pOtherVal
  return (sym, i, actualLabel)
  where
    isRuleMarker (RuleMarker _ _) = True
    isRuleMarker _                = False

debugName :: Show a => String -> Parser a -> Parser a
debugName dname p = do
  debugPrint dname
  res <- local (increaseNestLevel dname) p
  myTraceM $ "\\ " <> dname <> " has returned " <> show res
  return res

debugPrint :: String -> Parser ()
debugPrint str = whenDebug $ do
--  lookingAt <- lookAhead getToken <|> (EOF <$ eof)
--  leftX     <- lookAhead pXLocation
  myTraceM $ "/ " <> str
    -- <> "; currently at " ++ show leftX
    -- <> "; looking at: " <> show lookingAt


-- force debug=true for this subpath
alwaysdebugName :: Show a => String -> Parser a -> Parser a
alwaysdebugName dname p = local (\rc -> rc { debug = True }) $ debugName dname p

pMultiTerm :: Parser MultiTerm
pMultiTerm = debugName "pMultiTerm calling someDeep choice" $ someDeep pNumOrText

pNumOrText :: Parser Text.Text
pNumOrText = pOtherVal <|> pNumAsText

-- one or more P, monotonically moving to the right, returned in a list
someDeep :: (Show a) => Parser a -> Parser [a]
someDeep p = debugName "someDeep" $
  manyIndentation ( (:)
                    <$> debugName "someDeep first part calls base directly" p
                    <*> debugName "someDeep second part calls manyDeep" (manyDeep p)
                  )

-- zero or more P, monotonically moving to the right, returned in a list
manyDeep :: (Show a) => Parser a -> Parser [a]
manyDeep p =
  debugName "manyDeep" $
  (debugName "manyDeep calling someDeep" (try $ someDeep p)
    <|>
    debugName "someDeep failed, manyDeep defaulting to retun []" (return [])
  )

someDeepThen :: (Show a, Show b) => Parser a -> Parser b -> Parser ([a],b)
someDeepThen p1 p2 = someIndentation $ manyDeepThen p1 p2
someDeepThenMaybe :: (Show a, Show b) => Parser a -> Parser b -> Parser ([a],Maybe b)
someDeepThenMaybe p1 p2 = someIndentation $ manyDeepThenMaybe p1 p2


-- continuation:
-- what if you want to match something like
-- foo foo foo foo foo (bar)
manyDeepThen :: (Show a, Show b) => Parser a -> Parser b -> Parser ([a],b)
manyDeepThen p1 p2 = debugName "someDeepThen" $ do
  p <- try (debugName "someDeepThen/initial" p1)
  (lhs, rhs) <- donext
  return (p:lhs, rhs)
  where
    donext = debugName "going inner" (try $ someIndentation $ manyDeepThen p1 p2)
             <|> debugName "going rhs" base
    base = debugName "manyDeepThen/base" $ do
      rhs <- try (manyIndentation p2)
      return ([], rhs)

manyDeepThenMaybe :: (Show a, Show b) => Parser a -> Parser b -> Parser ([a],Maybe b)
manyDeepThenMaybe p1 p2 = debugName "manyDeepThenMaybe" $ do
  p <- try (debugName "manyDeepThenMaybe/initial" p1)
  (lhs, rhs) <- donext
  return (p:lhs, rhs)
  where
    donext = debugName "going inner" (try $ someIndentation $ manyDeepThenMaybe p1 p2)
             <|> debugName "going rhs" base
    base = debugName "manyDeepThenMaybe/base" $ do
      rhs <- optional $ try (manyIndentation p2)
      return ([], rhs)




-- combinators for compound expressions on the same line
-- e.g. "foo IS bar AND baz IS quux"
-- contains (foo IS bar)   -- a RelationalPredicate
--          (baz IS quux)  -- another RelationalPredicate
-- and then they are joined by "AND" to form a BoolStructR
--
-- if they were broken across multiple lines that would be fine -- the foo and the baz would be at the same indentation level because after bar we would get a bunch of UnDeepers.
-- but compound expressions on the same line don't consume UnDeepers
-- so we need this family of combinators to operate

-- typical usage:
fourIs :: Parser (MyToken,MyToken,MyToken,MyToken)
fourIs = debugName "fourIs" $ do
  (,,,)
    $>| pT
    |>| pT
    |>| pT
    |<< pT
  where pT = debugName "Is/An" (pToken Is <|> pToken A_An)

threeIs :: Parser (MyToken, (MyToken,MyToken) ,MyToken)
threeIs = debugName "threeIs" $ do
  (,,)
    $>| pT
    ||| pTT
    |<< pT
  where pT  = debugName "Is/An" (pToken Is <|> pToken A_An)
        pTT = debugName "(pT,pT)" $ (,) $>| pT |>| pT
    

($>|)  :: Show a =>        (a -> b)      -> Parser  a        -> Parser  (b,Int)
($||)  :: Show a =>        (a -> b)      -> Parser (a, Int)  -> Parser  (b,Int)
(|>|)  :: Show a => Parser (a -> b, Int) -> Parser  a        -> Parser  (b,Int)
(|||)  :: Show a => Parser (a -> b, Int) -> Parser (a, Int)  -> Parser  (b,Int)
(||<)  :: Show a => Parser (a -> b, Int) -> Parser (a, Int)  -> Parser   b
(|<<)  :: Show a => Parser (a -> b, Int) -> Parser  a        -> Parser   b     
(|:|)  :: Show a => Parser (a     , Int) ->                     Parser ([a],Int) -- some
(|.|)  :: Show a => Parser (a     , Int) ->                     Parser ([a],Int) -- many
(..|)  :: Show a => Parser  a            ->                     Parser ([a],Int) -- many
(.:|)  :: Show a => Parser  a            ->                     Parser ([a],Int) -- some

(..|) x = (|.|) (id $>| x)
(.:|) x = (|:|) (id $>| x)

(|:|) p = debugName "|:| someLike" $ do
  (p1,n) <- p
  fmap (n+) <$> (try (deeper p1) <|> nomore)
  where
    deeper p1 = debugName "deeper" $ do
      deepers <- debugName "some GoDeeper" $ some (pToken GoDeeper)
      (next,m) <- (|:|) p
      return (p1:next, m + length deepers)
    nomore = debugName "noMore" $ return ([],0)
infixl 4 |:|, ..|, .:|
  
(|.|) p = debugName "|.| manyLike" $ do
  try ((|:|) p) <|> return ([],0)

f $>| p2 = do
  r <- debugName "$>|" p2
  return (f r,0)
infixl 4 $>|



f $|| p2 = do
  (r,n) <- debugName "$||" p2
  return (f r,n)
infixl 4 $||

p1 |>| p2 = do
  (l,n) <- p1
  deepers <- some (debugName "GoDeeper" $ pToken GoDeeper)
  r <- debugName "|>| going right" p2
  return (l r, n + length deepers )
infixl 4 |>|


p1 ||| p2 = do
  (l,n) <- p1
  deepers <- some (debugName "GoDeeper" $ pToken GoDeeper)
  (r,m) <- debugName "||| going right" p2
  return (l r, n + length deepers + m )
infixl 4 |||

  
p1 |<< p2 = do
  (l,n) <- p1
  deepers <- some (debugName "GoDeeper" $ pToken GoDeeper)
  r <- debugName "|<< going right and closing" p2 <* float (n + length deepers)
  return (l r)
infixl 4 |<<
  
p1 ||< p2 = do
  (l,n) <- p1
  deepers <- some (debugName "GoDeeper" $ pToken GoDeeper)
  (r,m) <- debugName "||< going right and closing" p2
  _     <- float (n + length deepers + m)
  return (l r)
infixl 4 ||<
  
  
float :: Int -> Parser ()
float n = debugName "float" $ do
  debugPrint $ "sameLine/float: reached end of line; now need to clear " ++ show n ++ " UnDeepers"
  replicateM_ n (pToken UnDeeper)
  debugPrint "sameLine: success!"








-- indent at least 1 tab from current location
someIndentation :: (Show a) => Parser a -> Parser a
someIndentation p = debugName "someIndentation" $
  myindented (manyIndentation p)

someIndentation' :: Parser a -> Parser a
someIndentation' p = myindented' (manyIndentation' p)

-- 0 or more tabs indented from current location
manyIndentation :: (Show a) => Parser a -> Parser a
manyIndentation p =
  debugName "manyIndentation/leaf?" (try p)
  <|>
  debugName "manyIndentation/deeper; calling someIndentation" (try $ someIndentation p)

manyIndentation' :: Parser a -> Parser a
manyIndentation' p =
  (try p)
  <|>
  (try $ someIndentation' p)

myindented :: (Show a) => Parser a -> Parser a
myindented = between
             (debugName "myindented: consuming GoDeeper" $ pToken GoDeeper)
             (debugName "myindented: consuming UnDeeper" $ pToken UnDeeper)

myindented' :: Parser a -> Parser a
myindented' = between
             (debugName "myindented: consuming GoDeeper" $ pToken GoDeeper)
             (debugName "myindented: consuming UnDeeper" $ pToken UnDeeper)

myoutdented :: (Show a) => Parser a -> Parser a
myoutdented = between
              (debugName "outdented: consuming UnDeeper" $ pToken UnDeeper)
              (debugName "outdented: consuming GoDeeper" $ pToken GoDeeper)
  --
-- maybe move this to indented.hs
--

-- everything in p2 must be at least the same depth as p1
indented :: (Show a, Show b) => Int -> Parser (a -> b) -> Parser a -> Parser b
indented d p1 p2 = do
  f     <- p1
  y     <- case d of
    0 -> manyIndentation p2
    _ -> someIndentation p2
  return $ f y

indentedTuple :: (Show a, Show b) => Int -> Parser a -> Parser b -> Parser (a,b)
indentedTuple d p1 p2 = do
  indented d ((,) <$> p1) p2

-- return one or more items at the same depth.
-- the interesting thing about this function is the *absence* of someIndentation/manyIndentation
sameDepth, sameMany :: (Show a) => Parser a -> Parser [a]
sameDepth p = debugName "sameDepth" $ some p
sameMany  p = debugName "sameMany"  $ many p

indentedTuple0, indentedTuple1 :: (Show a, Show b) => Parser a -> Parser b -> Parser (a,b)
indentedTuple0 = indentedTuple 0
infixr 4 `indentedTuple0`

indentedTuple1 = indentedTuple 1
infixr 4 `indentedTuple1`

indented0, indented1 :: (Show a, Show b) => Parser (a -> b) -> Parser a -> Parser b
indented0 = indented 0
infixl 4 `indented0`

indented1 = indented 1
infixl 4 `indented1`

-- while an "indent2" is easy enough -- Constructor <$> pOne `indentChain` pTwo
-- an indent3 isn't as easy as just stacking on another      `indentChain` pThree
-- you have to do it this way instead.
indent3 :: (Show a, Show b, Show c, Show d) => (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
indent3 f p1 p2 p3 = debugName "indent3" $ do
  p1' <- p1
  someIndentation $ liftA2 (f p1') p2 (someIndentation p3)

optIndentedTuple :: (Show a, Show b) => Parser a -> Parser b -> Parser (a, Maybe b)
optIndentedTuple p1 p2 = debugName "optIndentedTuple" $ do
  (,) <$> p1 `optIndented` p2

optIndented :: (Show a, Show b) => Parser (Maybe a -> b) -> Parser a -> Parser b
infixl 4 `optIndented`
optIndented p1 p2 = debugName "optIndented" $ do
  f <- p1
  y <- optional (someIndentation p2)
  return $ f y

-- let's do us a combinator that does the same as `indentedTuple0` but in applicative style
indentChain :: Parser (a -> b) -> Parser a -> Parser b
indentChain p1 p2 = p1 <*> someIndentation' p2
infixl 4 `indentChain`

showStack :: Parser String
showStack = do
  callStack <- asks parseCallStack
  return $ intercalate " > " $ reverse callStack

pAnyText :: Parser Text.Text
pAnyText = tok2text <|> pOtherVal

tok2text :: Parser Text.Text
tok2text = choice
    [ "IS"     <$ pToken Is
    , "=="     <$ pToken TokEQ
    , "<"      <$ pToken TokLT
    , "<="     <$ pToken TokLTE
    , ">"      <$ pToken TokGT
    , ">="     <$ pToken TokGTE
    , "IN"     <$ pToken TokIn
    , "NOT IN" <$ pToken TokNotIn
    ]

-- | Like `\m -> do a <- m; tell [a]; return a` but add the value before the child elements instead of after
tellIdFirst :: (Functor m) => WriterT (DList w) m w -> WriterT (DList w) m w
tellIdFirst = mapWriterT . fmap $ \(a, m) -> (a, singeltonDL a <> m)

pToken :: MyToken -> Parser MyToken
pToken c = pTokenMatch (== c) c

pTokenAnyDepth :: MyToken -> Parser MyToken
pTokenAnyDepth c = pTokenMatch (== c) c

