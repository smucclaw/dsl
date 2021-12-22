{-# LANGUAGE OverloadedStrings #-}

module LS.Tokens where

import qualified Data.Set           as Set
import qualified Data.Text.Lazy as Text
import Text.Megaparsec
import Control.Monad.Reader (asks, local)
import Control.Monad.Writer.Lazy
import Data.Maybe (isJust)
import Data.List (intercalate)

import LS.Types
import Debug.Trace (traceM)

-- "discard newline", a reference to GNU Make
dnl :: Parser [MyToken]
-- -- dnl = many $ pToken EOL
dnl = pure []
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
    test (WithPos _ _ _ TypeSeparator) = Just "::" -- TODO FIXME -- this was here to allow GIVEN ParamText to contain a type signature
    test (WithPos _ _ _ (Other t)) = Just t
    test _ = Nothing

getToken :: Parser MyToken
getToken = token test Set.empty <?> "any token"
  where
    test (WithPos _ _ _ tok) = Just tok

myTraceM :: String -> Parser ()
myTraceM x = whenDebug $ do
  nestDepth <- asks nestLevel
  lookingAt <- lookAhead getToken <|> (EOF <$ eof)
  traceM $ leftPad (show lookingAt) 12 <> indentShow nestDepth <> x
  where
    indentShow depth = concat $ replicate depth "| "
    leftPad str n = take n $ str <> repeat ' '

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
  actualLabel  <- manyIndentation pOtherVal -- <* dnl
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
--  depth     <- asks callDepth
--  leftX     <- lookAhead pXLocation
  myTraceM $ "/ " <> str
    -- <> " running. callDepth min=" <> show depth
    -- <> "; currently at " ++ show leftX
    -- <> "; looking at: " <> show lookingAt


-- force debug=true for this subpath
alwaysdebugName :: Show a => String -> Parser a -> Parser a
alwaysdebugName dname p = local (\rc -> rc { debug = True }) $ debugName dname p

pMultiTerm :: Parser MultiTerm
pMultiTerm = debugName "pMultiTerm" $ manyDeep $ choice
  [ debugName "pMT: first, pOtherVal"   pOtherVal
  , debugName "pMT: second, pNumAsText" pNumAsText ]

-- one or more P, monotonically moving to the right, returned in a list
someDeep :: (Show a) => Parser a -> Parser [a]
someDeep p =
  debugName "someDeep" $
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
  
-- indent at least 1 tab from current location
someIndentation :: (Show a) => Parser a -> Parser a
someIndentation p =
  debugName "someIndentation" $
  myindented (manyIndentation p)

-- 0 or more tabs indented from current location
manyIndentation :: (Show a) => Parser a -> Parser a
manyIndentation p =
  debugName "manyIndentation" $
  try p <|> someIndentation p

myindented :: (Show a) => Parser a -> Parser a
myindented = between (pToken GoDeeper) (pToken UnDeeper)

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

oldindentedTuple d p1 p2 = do
  x     <- tracedepth "left  = " id     p1
  _     <- tracedepth "deep  = " isJust (optional dnl)
  y     <- tracedepth "right = " id     p2
  myTraceM "success; returning"
  return (x,y)
  where
    tracedepth :: Show y => String -> (x -> y) -> Parser x -> Parser x
    tracedepth lr f p = do
      depth <- asks callDepth
      leftX <- lookAhead pXLocation
      leftY <- lookAhead pYLocation
      next  <- lookAhead getToken <|> (EOF <$ eof)
      let prefix = "indentedTuple(" ++ show d ++ "): checkDepth " ++ show depth ++ "; "
      myTraceM $ prefix ++ "at line " ++ show leftY ++ ", col " ++ show leftX ++ "; looking at " ++ show next
      s     <- p
      myTraceM $ prefix ++ lr ++ " matched " ++ show (f s)
      return s

indentedTuple0, indentedTuple1 :: (Show a, Show b) => Parser a -> Parser b -> Parser (a,b)
indentedTuple0 = indentedTuple 0
infixl 4 `indentedTuple0`

indentedTuple1 = indentedTuple 1
infixl 4 `indentedTuple1`

indented0, indented1 :: (Show a, Show b) => Parser (a -> b) -> Parser a -> Parser b
indented0 = indented 0
infixl 4 `indented0`

indented1 = indented 1
infixl 4 `indented1`

-- problem is, if the RHS is optional, then an indented1 would wrongly require a GoDeeper!
-- so what we should do, is this:
-- optIndented1 returns Just if the thing is found, indented; or Nothing if the thing is not found when indented

optIndentedTuple :: (Show a, Show b) => Parser a -> Parser b -> Parser (a, Maybe b)
optIndentedTuple p1 p2 = debugName "optIndentedTuple" $ do
  (,) <$> p1 `optIndented` p2

optIndented :: (Show a, Show b) => Parser (Maybe a -> b) -> Parser a -> Parser b
infixl 4 `optIndented`
optIndented p1 p2 = debugName "optIndented" $ do
  f <- p1
  y <- optional (someIndentation p2)
  return $ f y

-- | withDepth n p sets the depth to n for parser p
withDepth :: Depth -> Parser a -> Parser a
withDepth n p = do
  names <- getNames
  myTraceM (names ++ " setting withDepth(" ++ show n ++ ")")
  local (\st -> st {callDepth= n}) p
  where
    getNames = do
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
pToken c = -- checkDepth >>
  pTokenMatch (== c) c

pTokenAnyDepth :: MyToken -> Parser MyToken
pTokenAnyDepth c = pTokenMatch (== c) c

-- | check that the next token is at at least the current level of indentation
checkDepth :: Parser ()
checkDepth = do
  depth <- asks callDepth
  leftX <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.
  if leftX <  depth
    then myTraceM $ "checkDepth: current location " ++ show leftX ++ " is left of minimum depth " ++ show depth ++ "; considering parse fail"
    -- else myTraceM $ "checkDepth: current location " ++ show leftX ++ " is right of minimum depth " ++ show depth ++ "; guard succeeds"
    else pure ()
  guard $ leftX >= depth

