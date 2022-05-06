{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE FlexibleInstances  #-}  -- One more extension.
{-# LANGUAGE StandaloneDeriving #-}  -- To derive Show
{-# LANGUAGE TupleSections #-}


module LS.Lib where

-- import qualified Data.Tree      as Tree
import qualified Data.Text.Lazy as Text
-- import Data.Text.Lazy.Encoding (decodeUtf8)
import Text.Megaparsec
import Data.ByteString.Lazy (ByteString)
import qualified Data.Csv as Cassava
import qualified Data.Vector as V
import Data.Vector ((!), (!?))
import Text.Pretty.Simple (pPrintString, pStringNoColor)
import qualified AnyAll as AA
import qualified Text.PrettyPrint.Boxes as Box
import           Text.PrettyPrint.Boxes hiding ((<>))
import System.Environment (lookupEnv)
import qualified Data.ByteString.Lazy as BS
import qualified Data.List.Split as DLS
import Text.Parser.Permutation
import qualified Data.List.NonEmpty as NE
import Options.Generic
import Data.Maybe (listToMaybe, maybeToList)

import LS.Types
import LS.Tokens
import LS.Parser
import LS.ParamText
import LS.RelationalPredicates
import LS.Error ( errorBundlePrettyCustom )
import Control.Monad.Writer.Lazy

-- import LS.XPile.CoreL4
-- import Data.ByteString.Lazy.UTF8 (toString)
import Data.List (transpose)
import Debug.Trace (trace)
import Data.Void (Void)
import Data.Either (rights)

-- our task: to parse an input CSV into a collection of Rules.
-- example "real-world" input can be found at https://docs.google.com/spreadsheets/d/1qMGwFhgPYLm-bmoN2es2orGkTaTN382pG2z3RjZ_s-4/edit

-- the wrapping 'w' here is needed for <!> defaults and <?> documentation
data Opts w = Opts { demo :: w ::: Bool <!> "False"
                   , only :: w ::: String <!> "" <?> "native | tree | svg | babyl4 | corel4 | prolog | uppaal | vue | grounds | checklist"
                   , dbug :: w ::: Bool <!> "False"
                   , extd :: w ::: Bool <!> "False" <?> "unhide grounds carrying typical values"
                   , file :: w ::: NoLabel [String] <?> "filename..."
                   , dstream :: w ::: Bool <!> "False"
                   }
  deriving (Generic)
instance ParseRecord (Opts Wrapped)
deriving instance Show (Opts Unwrapped)

-- technique for getting varargs argv https://github.com/Gabriel439/Haskell-Optparse-Generic-Library/issues/65
newtype NoLabel a = NoLabel a  deriving (Generic, Show)

instance ParseFields a => ParseRecord (NoLabel a)
instance ParseFields a => ParseFields (NoLabel a) where
  parseFields msg _ _ def = fmap NoLabel (parseFields msg Nothing Nothing def)



getConfig :: Opts Unwrapped -> IO RunConfig
getConfig o = do
  mpd <- lookupEnv "MP_DEBUG"
  mpj <- lookupEnv "MP_JSON"
  mpn <- lookupEnv "MP_NLG"
  return RC
        { debug       = maybe (dbug o) (read :: String -> Bool) mpd
        , printstream = maybe (dstream o) (read :: String -> Bool) mpd
        , callDepth = 0
        , oldDepth = 0
        , parseCallStack = []
        , sourceURL = "STDIN"
        , asJSON = maybe False (read :: String -> Bool) mpj
        , toNLG = maybe False (read :: String -> Bool) mpn
        , toBabyL4  = only o == "babyl4" || only o == "corel4"
        , toProlog  = only o == "prolog"
        , toUppaal  = only o == "uppaal"
        , toGrounds = only o == "grounds"
        , toChecklist = only o == "checklist"
        , toVue     = only o == "vue"
        , saveAKA = False
        , wantNotRules = False
        , extendedGrounds = extd o
        }


parseRules :: Opts Unwrapped -> IO [Either (ParseErrorBundle MyStream Void) [Rule]]
parseRules o = do
  runConfig <- getConfig o
  let files = getNoLabel $ file o
  if null files
    then parseSTDIN runConfig { sourceURL="STDIN" }
    else concat <$> mapM (\file -> parseFile runConfig {sourceURL=Text.pack file} file) files

  where
    getNoLabel (NoLabel x) = x
    getBS "-"   = BS.getContents
    getBS other = BS.readFile other
    parseSTDIN rc = do
      bs <- BS.getContents
      mapM (parseStream rc "STDIN") (exampleStreams bs)
    parseFile rc filename = do
      bs <- getBS filename
      mapM (parseStream rc filename) (exampleStreams bs)
    parseStream rc filename stream = do
      case runMyParser id rc pToplevel filename stream of
        Left bundle -> do
          putStrLn $ "* error while parsing " ++ filename
          putStr (errorBundlePrettyCustom bundle)
          putStrLn "** stream"
          printStream stream
          return (Left bundle)
        -- Left bundle -> putStr (errorBundlePretty bundle)
        -- Left bundle -> pPrint bundle
        Right ([], []) -> return $ Right []
        Right (xs, xs') -> do
          when (printstream rc) $ printStream stream
          return $ Right (xs ++ xs')


dumpRules :: Opts Unwrapped -> IO [Rule]
dumpRules opts = concat . rights <$> parseRules opts


printStream :: MonadIO m => MyStream -> m ()
printStream = pPrintString . renderStream

renderStream :: MyStream -> String
renderStream stream = unwords $ renderToken . tokenVal <$> unMyStream stream

pRenderStream :: MyStream -> String
pRenderStream = Text.unpack . pStringNoColor . renderStream

exampleStream :: ByteString -> MyStream
exampleStream s = case getStanzas <$> asCSV s of
                    Left errstr -> error errstr
                    Right rawsts -> stanzaAsStream (head rawsts)

exampleStreams :: ByteString -> [MyStream]
exampleStreams s = case getStanzas <$> asCSV s of
                    Left errstr -> error errstr
                    Right rawsts -> stanzaAsStream <$> rawsts

    -- the raw input looks like this:
dummySing :: ByteString
dummySing =
  -- ",,,,\n,EVERY,person,,\n,WHO,walks,// comment,continued comment should be ignored\n,AND,runs,,\n,AND,eats,,\n,AND,drinks,,\n,MUST,,,\n,->,sing,,\n"
  -- ",,,,\n,EVERY,person,,\n,WHO,eats,,\n,OR,drinks,,\n,MUST,,,\n,->,sing,,\n"
  ",,,,\n,EVERY,person,,\n,WHO,walks,// comment,continued comment should be ignored\n,AND,runs,,\n,AND,eats,,\n,OR,drinks,,\n,MUST,,,\n,->,sing,,\n"

indentedDummySing :: ByteString
indentedDummySing =
  ",,,,\n,EVERY,person,,\n,WHO,walks,,\n,AND,runs,,\n,AND,eats,,\n,OR,,drinks,\n,,AND,swallows,\n,MUST,,,\n,>,sing,,\n"


--
-- the desired output has type Rule
--


--
-- we begin by stripping comments and extracting the stanzas. Cassava gives us Vector Vector Text.
--

asBoxes :: RawStanza -> String
asBoxes _rs =
  render $ nullBox Box.<> nullBox Box.<> nullBox Box.<> Box.char 'a' Box.<> Box.char 'b' Box.<> Box.char 'c'


asCSV :: ByteString -> Either String RawStanza
asCSV s =
  let decoded = Cassava.decode Cassava.NoHeader s :: Either String RawStanza
  in equalizeLines <$> preprocess decoded
  where
    preprocess :: Either String RawStanza -> Either String RawStanza
    preprocess x = do
      vvt <- x
      -- process // comments by setting all righter elements to empty.
      -- if we ever need to maximize efficiency we can consider rewriting this to not require a Vector -> List -> Vector trip.
      return $ rewriteDitto $ fmap trimLegalSource . trimComment False . V.toList <$> vvt
    -- ignore the () at the beginning of the line. Here it actually trims any (...) from any position but this is good enough for now
    trimLegalSource x = let asChars = Text.unpack x
                        in if not (null asChars)
                              && head asChars == '('
                              && last asChars == ')'
                           then ""
                           else x
    trimComment _       []                           = V.empty
    trimComment True  (_x:xs)                        = V.cons "" $ trimComment True xs
    trimComment False (x:xs) | Text.take 2 (Text.dropWhile (== ' ') x)
                               `elem` Text.words "// -- ##"
                                                     = trimComment True (x:xs) -- a bit baroque, why not just short-circuit here?
    trimComment False (x:xs)                         = V.cons x $ trimComment False xs

-- [TODO]: left trim all blank columns

-- | Make sure all lines have the same length
equalizeLines :: RawStanza -> RawStanza
equalizeLines stanza = fmap (pad $ succ maxLen) stanza -- <> V.singleton V.empty
  where
    maxLen = maximum $ fmap length stanza

pad :: Int -> V.Vector Text.Text -> V.Vector Text.Text
pad n v = v <> V.replicate (n - V.length v) ""


rewriteDitto :: V.Vector (V.Vector Text.Text) -> RawStanza
rewriteDitto vvt = V.imap (V.imap . rD) vvt
  where
    rD :: Int -> Int -> Text.Text -> Text.Text
    rD row col "\"" = -- first non-blank above
      let aboves = V.filter (`notElem` ["", "\""]) $ (! col) <$> V.slice 0 row vvt
      in if V.null aboves
         then error $ "line " ++ show (row+1) ++ " column " ++ show (col+1) ++ ": ditto lacks referent (upward nonblank cell)"
         else V.last aboves
    rD _   _   orig = orig


getStanzas :: RawStanza -> [RawStanza]
getStanzas rs = splitPilcrows `concatMap` chunks
  -- traceM ("getStanzas: extracted range " ++ (Text.unpack $ pShow toreturn))
  where chunks = getChunks rs

        -- traceStanzas xs = trace ("stanzas: " ++ show xs) xs

splitPilcrows :: RawStanza -> [RawStanza]
splitPilcrows rs = map (listsToStanza . transpose) splitted
  where
    listsToStanza = V.fromList . map V.fromList
    stanzaToLists = map V.toList . V.toList
    rst = transpose $ stanzaToLists rs
    splitted = (DLS.split . DLS.dropDelims . DLS.whenElt) (all (== "¶")) rst

-- highlight each chunk using range attribute.
-- method: cheat and use Data.List.Split's splitWhen to chunk on paragraphs separated by newlines
getChunks :: RawStanza -> [RawStanza]
getChunks rs = [rs]
  -- let listChunks = (DLS.split . DLS.keepDelimsR . DLS.whenElt) emptyRow [ 0 .. V.length rs - 1 ]
  --     containsMagicKeyword rowNr = V.any (`elem` magicKeywords) (rs ! rowNr)
  --     emptyRow rowNr = V.all Text.null (rs ! rowNr)
  --     wantedChunks = [ firstAndLast neRows
  --                    | rows <- listChunks
  --                    ,    any containsMagicKeyword rows
  --                      || all emptyRow rows
  --                    , Just neRows <- pure $ NE.nonEmpty rows
  --                    ]
  --     toreturn = extractLines rs <$> glueLineNumbers wantedChunks
  -- in -- trace ("getChunks: input = " ++ show [ 0 .. V.length rs - 1 ])
  --    -- trace ("getChunks: listChunks = " ++ show listChunks)
  --    -- trace ("getChunks: wantedChunks = " ++ show wantedChunks)
  --    -- trace ("getChunks: returning " ++ show (length toreturn) ++ " stanzas: " ++ show toreturn)
  -- toreturn

firstAndLast :: NE.NonEmpty Int -> (Int, Int)
firstAndLast xs = (NE.head xs, NE.last xs)

-- because sometimes a chunk followed by another chunk is really part of the same chunk.
-- so we glue contiguous chunks together.
glueLineNumbers :: [(Int,Int)] -> [(Int,Int)]
glueLineNumbers ((a0, a1) : (b0, b1) : xs)
  | a1 + 1 == b0 = glueLineNumbers $ (a0, b1) : xs
  | otherwise = (a0, a1) : glueLineNumbers ((b0, b1) : xs)
glueLineNumbers [x] = [x]
glueLineNumbers [] = []

extractLines :: RawStanza -> (Int,Int) -> RawStanza
extractLines rs (y0, yLast) = V.slice y0 (yLast - y0 + 1) rs

vvlookup :: RawStanza -> (Int, Int) -> Maybe Text.Text
vvlookup rs (x,y) = rs !? y >>= (!? x)


-- gaze Down 1 (== "UNLESS") >> gaze rs Right 1





-- a multistanza is multiple stanzas separated by pilcrow symbols

-- a stanza is made up of:
--    a stanza head followed by
--      zero or more (one or more blank lines followed by
--                    a stanza fragment)
-- a stanza fragment is
--    a line starting with a BoolConnector or an IF
--    followed by one or more blank lines
--    followed by other keywords we recognize, like a MUST
-- a stanza head is
--    a group of lines where the left-most nonblank, noncitation, nonITIS column contains one of EVERY / WHEN / IF etc
--    or the leftmost nonblank, noncitation column is IT IS

--
-- putting the above together, we arrive at a MyStream object ready for proper parsing.
--

stanzaAsStream :: RawStanza -> MyStream
stanzaAsStream rs =
  let vvt = rs
  in
  -- MyStream (Text.unpack $ decodeUtf8 s) [ WithPos {..}
  MyStream rs $ parenthesize [ WithPos {..}
             | y <- [ 0 .. V.length vvt       - 1 ]
             , x <- [ 0 .. V.length (vvt ! y) - 1 ]
             , let pos = SourcePos "" (mkPos $ y + 1) (mkPos $ x + 1)
                   rawToken = vvt ! y ! x
                   tokenLength = 1
                   parserCtx = Nothing
                  --  tokenLength = fromIntegral $ Text.length rawToken + 1 & \r -> Debug.trace (show r) r
                  --  tokenLength = fromIntegral $ Text.length rawToken + 1 & Debug.trace <$> show <*> id  -- same as above line, but with reader applicative
                  --  tokenLength = fromIntegral $ Text.length rawToken + 1  -- without debugging
             , let paren = [GoDeeper | y < V.length vvt - 1 && toToken (vvt ! (y+1) ! x) `elem` map pure [And, Or]]
             , tokenVal <- toToken rawToken ++ paren
             , tokenVal `notElem` [ Empty, Checkbox ]
             ]
  where
    parenthesize :: [WithPos MyToken] -> [WithPos MyToken]
    parenthesize mys =
      tail $ insertParens [] (withSOF : mys ++ [withEOF])
    sofPos = SourcePos "" pos1 pos1
    lastLine = V.length rs - 1
    lastCol  = V.length (rs ! lastLine) - 1
    eofPos = SourcePos "" (mkPos $ lastLine + 1) (mkPos $ lastCol + 1)
    withSOF = WithPos sofPos 1 Nothing SOF
    withEOF = WithPos eofPos 1 Nothing EOL
    col = unPos . sourceColumn . pos
    lin = unPos . sourceLine   . pos

    insertParens pars [] = replicate (length pars) (WithPos eofPos 1 Nothing UnDeeper)
    insertParens parens (a : xs)
      | tokenVal a == GoDeeper = a : insertParens (a:parens) xs
      -- Close an open parenthesis when we are to the left of it
      | (par:pars) <- parens, col par > col a = (UnDeeper <$ a) : insertParens pars (a:xs)
    insertParens parens (a : xs@(b : _))
      | tokenVal a /= SOF
      , col a < col b
      , lin a < lin b =  trace ("Lib preprocessor: inserting EOL between " <> show (tokenVal a) <> " and " <> show (tokenVal b)) $
                        a : eolToken : goDp : insertParens (goDp : parens) xs
        --- | foo |     |    | foo   EOL | -- special case: we add an EOL to show the indentation crosses multiple lines.
        --- |     | bar | -> |     ( bar | -- for example, in a ParamText, the "bar" line gives a parameter to the "foo" line


      --- | col a < col b = a                                     --- | foo | bar | -> | foo ( bar | -- ordinary case: every indentation adds a GoDeeper.
      ---                   : (goDp <$> [1 .. (col b - col a)])

      --- | col a > col b = a                                     --- |     | foo |                  -- ordinary case: every outdentation adds an UnDeeper; no EOL added.
      ---                   : (unDp <$> [1 .. (col a - col b)])   --- | bar |     | -> | foo ) bar |
      | lin a < lin b =  a : eolToken : insertParens parens xs

      where
        goDp = GoDeeper <$ b
        eolToken = a { tokenVal = EOL, pos = (pos a) { sourceColumn = mkPos $ col a + 1 } }
    insertParens parens (a : xs) = a : insertParens parens xs

-- MyStream is the primary input for our Parsers below.
--

pToplevel :: Parser [Rule]
pToplevel = pRules <* eof

-- do not allow NotARule parsing

pRules, pRulesOnly, pRulesAndNotRules :: Parser [Rule]
pRulesOnly = do
  some pRule <* eof

pRules = pRulesOnly

pRulesAndNotRules = do
  wanted   <- try $ many pRule
  notarule <- optional (notFollowedBy eof *> pNotARule)
  next <- [] <$ eof <|> pRules
  wantNotRules <- asks wantNotRules
  return $ wanted ++ next ++
    if wantNotRules then maybeToList notarule else []

pNotARule :: Parser Rule
pNotARule = debugName "pNotARule" $ do
  myTraceM "pNotARule: starting"
  toreturn <- NotARule <$> manyDeep getTokenNonDeep
  myTraceM "pNotARule: returning"
  return toreturn

-- the goal is tof return a list of Rule, which an be either regulative or constitutive:
pRule :: Parser Rule
pRule = debugName "pRule" $ do
  _ <- many dnl
  notFollowedBy eof

  leftY  <- lookAhead pYLocation -- this is the column where we expect IF/AND/OR etc.
  leftX  <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.
  srcurl <- asks sourceURL
  let srcref = SrcRef srcurl srcurl leftX leftY Nothing

  -- foundRule <- (pRegRule <?> "regulative rule")
  foundRule <- pRegRule
    <|> (pTypeDefinition   <?> "ontology definition")
    <|> (c2hornlike <$> pConstitutiveRule <?> "constitutive rule")
    <|> (pScenarioRule <?> "scenario rule")
    <|> (pHornlike <?> "DECIDE ... IS ... Horn rule")
    <|> ((\rl -> RuleGroup (Just rl) Nothing) <$> pRuleLabel <?> "standalone rule section heading")
    <|> debugName "pRule: unwrapping indentation and recursing" (myindented pRule)

  return $ foundRule { srcref = Just srcref }

-- if we get back a constitutive, we can rewrite it to a Hornlike here


pTypeDefinition :: Parser Rule
pTypeDefinition = debugName "pTypeDefinition" $ do
  maybeLabel <- optional pRuleLabel -- TODO: Handle the SL
  (proto,g,u) <- permute $ (,,)
    <$$> defineLimb
    <|?> (Nothing, givenLimb)
    <|?> (Nothing, uponLimb)
  return $ proto { given = snd <$> g, upon = snd <$> u, rlabel = maybeLabel }
  where
    defineLimb = do
      _dtoken <- pToken Define
      (name,super)  <- (,) $>| pNameParens |>< optional pTypeSig
      myTraceM $ "got name = " <> show name
      myTraceM $ "got super = " <> show super
      has   <- optional (pToken Has *> someIndentation (sameDepth pTypeDefinition))
      myTraceM $ "got has = " <> show has
      enums <- optional pOneOf
      myTraceM $ "got enums = " <> show enums
      return $ TypeDecl
        { name
        , super
        , has
        , enums
        , given = Nothing
        , upon = Nothing
        , rlabel  = noLabel
        , lsource = noLSource
        , srcref  = noSrcRef
        , defaults = mempty, symtab = mempty
        }

    givenLimb = debugName "pHornlike/givenLimb" . pretendEmpty $ Just <$> preambleParamText [Given]
    uponLimb  = debugName "pHornlike/uponLimb"  . pretendEmpty $ Just <$> preambleParamText [Upon]



-- | parse a Scenario stanza
pScenarioRule :: Parser Rule
pScenarioRule = debugName "pScenarioRule" $ do
  rlabel <- pToken ScenarioTok *> someIndentation (someDeep pOtherVal)
  (expects,givens) <- permute $ (,)
    <$$> some (manyIndentation pExpect)
    <|?> ([], many ( pretendEmpty $ pToken Given >> someIndentation pGivens) )
  return $ Scenario
    { scgiven = concat givens
    , expect  = expects
    , rlabel  = Just ("SCENARIO",1,Text.unwords rlabel)
    , lsource = Nothing, srcref = Nothing
    , defaults = [], symtab   = []
    }

pExpect :: Parser Expect
pExpect = debugName "pExpect" $ do
  _expect  <- pToken Expect
  (relPred, _whenpart) <- someIndentation relPredSamelineWhen
  return $ ExpRP relPred

-- | we want to parse two syntaxes:
-- @
-- GIVEN   a
--         b
--         c
-- @
-- and the other syntax is
-- @
-- GIVEN   a
-- GIVEN   b
-- GIVEN   c
-- @
-- the caller uses pToken Given >> someIndentation pGivens, so that handles the first case
-- I am going to guess the pretendEmpty helps to handle the second case.

pGivens :: Parser [RelationalPredicate]
pGivens = debugName "pGivens" $ do
  sameDepth pRelationalPredicate


pRegRule :: Parser Rule
pRegRule = debugName "pRegRule" $ do
  maybeLabel <- optional pRuleLabel -- TODO: Handle the SL
  tentative  <- pRegRuleNormal
  -- tentative  <- try pRegRuleSugary
  --                 <|> try pRegRuleNormal
                  <|> (pToken Fulfilled >> return RegFulfilled)
                  <|> (pToken Breach    >> return RegBreach)

  return $ tentative { rlabel = maybeLabel }

-- "You MAY" has no explicit PARTY or EVERY keyword:
--
--  You MAY  BEFORE midnight
--       ->  eat a potato
--       IF  a potato is available
--
--  You MAY
--       ->  eat a potato
--   BEFORE  midnight
--       IF  a potato is available

pRegRuleSugary :: Parser Rule
pRegRuleSugary = debugName "pRegRuleSugary" $ do
  entityname         <- AA.Leaf . multiterm2pt <$> someDeep pOtherVal            -- You ... but no AKA allowed here
  _leftX             <- lookAhead pXLocation
  let keynamewho = pure ((RParty, entityname), Nothing)
  (rulebody,henceLimb,lestLimb) <- someIndentation ((,,)
                                                     <$> permutationsReg keynamewho
                                                     <*> optional (pHenceLest Hence)
                                                     <*> optional (pHenceLest Lest)
                                                   )
  let poscond = snd <$> mergePBRS (rbpbrs   rulebody)
  let negcond = snd <$> mergePBRS (rbpbrneg rulebody)
      toreturn = Regulative
                 { subj     = entityname
                 , rkeyword  = RParty
                 , who      = Nothing
                 , cond     = addneg poscond negcond
                 , deontic  = rbdeon rulebody
                 , action   = rbaction rulebody
                 , temporal = rbtemporal rulebody
                 , hence    = henceLimb
                 , lest     = lestLimb
                 , rlabel   = Nothing -- rule label
                 , lsource  = Nothing -- legal source
                 , srcref   = Nothing -- internal SrcRef
                 , upon     = listToMaybe (snd <$> rbupon  rulebody)
                 , given    = NE.nonEmpty $ foldMap NE.toList (snd <$> rbgiven rulebody)    -- given
                 , having   = rbhaving rulebody
                 , wwhere   = rbwhere rulebody
                 , defaults = []
                 , symtab   = []
                 }
  myTraceM $ "pRegRuleSugary: the positive preamble is " ++ show poscond
  myTraceM $ "pRegRuleSugary: the negative preamble is " ++ show negcond
  myTraceM $ "pRegRuleSugary: returning " ++ show toreturn
  return toreturn

-- EVERY   person
-- WHO     sings
--    AND  walks
-- MAY     eat a potato
-- BEFORE  midnight
-- IF      a potato is available
--    AND  the potato is not green

pRegRuleNormal :: Parser Rule
pRegRuleNormal = debugName "pRegRuleNormal" $ do
  let keynamewho = (,) <$> pActor [REvery,RParty,RTokAll]
                   <*> optional (preambleBoolStructR [Who,Which,Whose])
  rulebody <- permutationsReg keynamewho
  henceLimb                   <- optional $ pHenceLest Hence
  lestLimb                    <- optional $ pHenceLest Lest
  myTraceM $ "pRegRuleNormal: permutations returned rulebody " ++ show rulebody

  let poscond = snd <$> mergePBRS (rbpbrs   rulebody)
  let negcond = snd <$> mergePBRS (rbpbrneg rulebody)

  let toreturn = Regulative
                 { subj     = snd $ rbkeyname rulebody
                 , rkeyword  = fst $ rbkeyname rulebody
                 , who      = snd <$> rbwho rulebody
                 , cond     = addneg poscond negcond
                 , deontic  = rbdeon rulebody
                 , action   = rbaction rulebody
                 , temporal = rbtemporal rulebody
                 , hence    = henceLimb
                 , lest     = lestLimb
                 , rlabel   = Nothing -- rule label
                 , lsource  = Nothing -- legal source
                 , srcref   = Nothing -- internal SrcRef
                 , upon     = listToMaybe (snd <$> rbupon  rulebody)    -- given
                 , given    = NE.nonEmpty $ foldMap NE.toList (snd <$> rbgiven rulebody)    -- given
                 , having   = rbhaving rulebody
                 , wwhere   = rbwhere rulebody
                 , defaults = []
                 , symtab   = []
                 }
  myTraceM $ "pRegRuleNormal: the positive preamble is " ++ show poscond
  myTraceM $ "pRegRuleNormal: the negative preamble is " ++ show negcond
  myTraceM $ "pRegRuleNormal: returning " ++ show toreturn
  -- let appendix = pbrs ++ nbrs ++ ebrs ++ defalias
  -- myTraceM $ "pRegRuleNormal: with appendix = " ++ show appendix
  -- return ( toreturn : appendix )
  return toreturn


pHenceLest :: MyToken -> Parser Rule
pHenceLest henceLest = debugName ("pHenceLest-" ++ show henceLest) $ do
  pToken henceLest *> someIndentation innerRule
  where
    innerRule =
      try pRegRule
      <|> RuleAlias <$> (optional (pToken Goto) *> someDeep pOtherVal)

pTemporal :: Parser (Maybe (TemporalConstraint Text.Text))
pTemporal = eventually <|> specifically <|> vaguely
  where
    eventually   = debugName "pTemporal/eventually"   $ mkTC <$> pToken Eventually <*> pure (Just 0) <*> pure ""
    specifically = debugName "pTemporal/specifically" $ mkTC $>| sometime |*| liftSLOptional pNumber |>< pOtherVal
    vaguely      = debugName "pTemporal/vaguely"      $ Just . TemporalConstraint TVague (Just 0) <$> pOtherVal
    sometime     = choice $ map pToken [ Before, After, By, On ]

pPreamble :: [RegKeywords] -> Parser RegKeywords
pPreamble toks = choice (try . pTokenish <$> toks)

-- "PARTY Bob       AKA "Seller"
-- "EVERY Seller"
pActor :: [RegKeywords] -> Parser (RegKeywords, BoolStructP)
pActor keywords = debugName ("pActor " ++ show keywords) $ do
  -- add pConstitutiveRule here -- we could have "MEANS"
  preamble     <- pPreamble keywords
  -- entitytype   <- lookAhead pNameParens
  entitytype   <- someIndentation pNameParens
  let boolEntity = AA.Leaf $ multiterm2pt entitytype
  -- omgARule <- pure <$> try pConstitutiveRule <|> (mempty <$ pNameParens)
  -- myTraceM $ "pActor: omgARule = " ++ show omgARule
  -- tell $ listToDL omgARule
  return (preamble, boolEntity)

-- Every man AND woman     AKA Adult
--       MEANS human
--         AND age >= 21
--  MUST WITHIN 200 years
--    -> die



pDoAction ::  Parser BoolStructP
pDoAction = do
  _ <- debugName "pDoAction/Do" $ pToken Do
  debugName "pDoAction/pAction" $ pAction


pAction :: Parser BoolStructP
pAction = debugName "pAction calling dBoolStructP" dBoolStructP


-- we create a permutation parser returning one or more RuleBodies, which we treat as monoidal,
-- though later we may object if there is more than one.

mkRBfromDT :: ((RegKeywords, BoolStructP )  -- every person
              ,Maybe (Preamble, BoolStructR)) -- who is red and blue
           -> (Deontic, Maybe (TemporalConstraint Text.Text))
           -> [(Preamble, BoolStructR)] -- positive  -- IF / WHEN
           -> [(Preamble, BoolStructR)] -- negative  -- UNLESS
           -> [(Preamble, ParamText )] -- upon  conditions
           -> [(Preamble, ParamText )] -- given conditions
           -> Maybe ParamText          -- having
           -> [Rule]
           -> BoolStructP
           -> RuleBody
mkRBfromDT (rbkn,rbwho) (rbd,rbt) rbpb rbpbneg rbu rbg rbh rbwhere rba  =
  RuleBody rba rbpb rbpbneg rbd rbt rbu rbg rbh rbkn rbwho rbwhere

mkRBfromDA :: ((RegKeywords, BoolStructP ) -- every person or thing
              ,Maybe (Preamble, BoolStructR)) -- who is red and blue
           -> (Deontic, BoolStructP)
           -> Maybe (TemporalConstraint Text.Text)
           -> [(Preamble, BoolStructR)] -- whenif
           -> [(Preamble, BoolStructR)] -- unless
           -> [(Preamble, ParamText )] -- upon  conditions
           -> [(Preamble, ParamText )] -- given conditions
           -> Maybe ParamText         -- having
           -> [Rule]
           -> RuleBody
mkRBfromDA (rbkn,rbwho) (rbd,rba) rbt rbpb rbpbneg rbu rbg rbh rbwhere
  = RuleBody rba rbpb rbpbneg rbd rbt rbu rbg rbh rbkn rbwho rbwhere


preambleRelPred :: [MyToken] -> Parser (Preamble, RelationalPredicate)
preambleRelPred preambles = do
  preamble <- choice (try . pToken <$> preambles)
  relpred  <- someIndentation pRelationalPredicate
  return (preamble, relpred)

permutationsReg :: Parser ((RegKeywords, BoolStructP), Maybe (Preamble, BoolStructR))
                -> Parser RuleBody
permutationsReg keynamewho =
  debugName "permutationsReg" $ do
  try ( debugName "regulative permutation with deontic-temporal" $ nopermute ( mkRBfromDT
            !<$$> keynamewho
            !<||> try pDT
            <&&> whatnot
            !<||> pDoAction
          ) )
  <|>
  try  ( debugName "regulative permutation with deontic-action" $ nopermute ( mkRBfromDA
            !<$$> keynamewho
            !<||> pDA
            !<|?> (Nothing, pTemporal)
            <&&> whatnot
          ) )
  where
    whatnot x = x
                !<|?> ([], some $ preambleBoolStructR [When, If])   -- syntactic constraint, all the if/when need to be contiguous.
                !<|?> ([], some $ preambleBoolStructR [Unless]) -- unless
                !<|?> ([], some $ preambleParamText [Upon])   -- upon
                !<|?> ([], some $ preambleParamText [Given])  -- given
                !<|?> (Nothing, Just . snd <$> preambleParamText [Having])  -- having
                !<|?> ([], (debugName "WHERE" $ pToken Where) >> someIndentation (some pHornlike))  -- WHERE ends up in the wwhere attribute of a Regulative

    (<&&>) = flip ($) -- or we could import Data.Functor ((&))
    infixl 1 <&&>
    -- Disable permutations
    -- (!<||>) = (<||>)
    (!<||>) = (<*>)
    infixl 1 !<||>
    -- (!<|?>) = (<|?>)
    x !<|?> (a,b) = x <*> (b <|> pure a)
    infixl 1 !<|?>
    (!<$$>) = (<$>)
    infixl 2 !<$$>
    nopermute = id
    -- nopermute = permute

-- the Deontic/temporal/action form
-- MAY EVENTUALLY
--  -> pay
pDT :: Parser (Deontic, Maybe (TemporalConstraint Text.Text))
pDT = debugName "pDT" $ do
  (pd,pt) <- (,)
    $>| pDeontic
    |>< optional pTemporal
  return (pd, join pt)

-- the Deontic/Action/Temporal form
pDA :: Parser (Deontic, BoolStructP)
pDA = debugName "pDA" $ do
  pd <- pDeontic
  pa <- pAction
  return (pd, pa)

preambleBoolStructP :: [MyToken] -> Parser (Preamble, BoolStructP)
preambleBoolStructP wanted = debugName ("preambleBoolStructP " <> show wanted)  $ do
  condWord <- choice (try . pToken <$> wanted)
  myTraceM ("preambleBoolStructP: found: " ++ show condWord)
  ands <- dBoolStructP -- (foo AND (bar OR baz), [constitutive and regulative sub-rules])
  return (condWord, ands)




-- [TODO]: Actually parse ParamTexts and not just single cells
dBoolStructP ::  Parser BoolStructP
dBoolStructP = debugName "dBoolStructP calling exprP" $ do
  either fail pure . toBoolStruct =<< exprP

exprP :: Parser (MyBoolStruct ParamText)
exprP = debugName "expr pParamText" $ do
  raw <- expr pParamText
  -- rewrite the raw returned from expr pParamText
  -- expr pParamText has returned MyLabel "pay" (MyLeaf (     ("to"     :| ["the King"],Nothing)
  --                                                      :| [("amount" :| ["$20"]     ,Nothing)]))
  -- to MyLeaf (("pay" :| [], Nothing)
  --            :| [("to"     :| ["the King"], Nothing)
  --               ,("amount" :| ["$20"]     , Nothing)[))

  return $ case raw of
    MyLabel pre post myitem -> prefixFirstLeaf pre myitem
    x -> x
  where
    prefixFirstLeaf :: [Text.Text] -> MyBoolStruct ParamText -> MyBoolStruct ParamText
    -- locate the first MyLeaf in the boolstruct and jam the lbl in as the first line
    prefixFirstLeaf p (MyLeaf x)           = MyLeaf (prefixItem p x)
    prefixFirstLeaf p (MyLabel pre post myitem) = MyLabel pre post (prefixFirstLeaf p myitem)
    prefixFirstLeaf p (MyAll (x:xs))       = MyAll (prefixFirstLeaf p x : xs)
    prefixFirstLeaf p (MyAll [])           = MyAll [MyLeaf $ mt2pt p]
    prefixFirstLeaf p (MyAny [])           = MyAny [MyLeaf $ mt2pt p]
    prefixFirstLeaf p (MyAny (x:xs))       = MyAny (prefixFirstLeaf p x : xs)
    prefixFirstLeaf p (MyNot  x    )       = MyNot (prefixFirstLeaf p x)

    prefixItem :: [Text.Text] -> ParamText -> ParamText
    prefixItem t pt = NE.cons (NE.fromList t, Nothing) pt


pAndGroup ::  Parser BoolStructP
pAndGroup = debugName "pAndGroup" $ do
  orGroup1 <- pOrGroup
  orGroupN <- many $ pToken And *> pOrGroup
  let toreturn = if null orGroupN
                 then orGroup1
                 else AA.All Nothing (orGroup1 : orGroupN)
  return toreturn

pOrGroup ::  Parser BoolStructP
pOrGroup = debugName "pOrGroup" $ do
  elem1    <- pElement
  elems    <- many $ pToken Or *> pElement
  let toreturn = if null elems
                 then elem1
                 else AA.Any Nothing (elem1 : elems)
  return toreturn

pAtomicElement ::  Parser BoolStructP
pAtomicElement = debugName "pAtomicElement" $ do
  try pNestedBool
    <|> pNotElement
    <|> pLeafVal

-- [TODO]: switch all this over the the Expr parser

pElement :: Parser BoolStructP
pElement = debugName "pElement" $ do
        try (hornlikeAsElement <$> tellIdFirst (debugName "nested pHornlike" pHornlike))
    <|> pAtomicElement

-- Makes a leaf with just the name of a constitutive rule
-- constitutiveAsElement ::  Rule -> BoolStructP
-- constitutiveAsElement cr = AA.Leaf $ multiterm2pt $ name cr

-- Makes a leaf with just the name of a hornlike rule
hornlikeAsElement ::  Rule -> BoolStructP
hornlikeAsElement hlr = AA.Leaf $ multiterm2pt $ name hlr

pNotElement :: Parser BoolStructP
pNotElement = debugName "pNotElement" $ do
  inner <- pToken MPNot *> pElement
  return $ AA.Not inner

pLeafVal ::  Parser BoolStructP
pLeafVal = debugName "pLeafVal" $ do
  leafVal <- pParamText
  myTraceM $ "pLeafVal returning " ++ show leafVal
  return $ AA.Leaf leafVal

-- [TODO]: we should be able to get rid of pNestedBool and just use a recursive call into dBoolStructP without pre-checking for a pBoolConnector. Refactor when the test suite is a bit more comprehensive.

pNestedBool ::  Parser BoolStructP
pNestedBool = debugName "pNestedBool" $ do
  -- "foo AND bar" is a nestedBool; but just "foo" is a leafval.
  (leftX,foundBool) <- lookAhead (pLeafVal >> optional dnl >> (,) <$> lookAhead pXLocation <*> pBoolConnector)
  myTraceM $ "pNestedBool matched " ++ show foundBool ++ " at location " ++ show leftX
  dBoolStructP

-- helper functions for parsing

anything :: Parser [WithPos MyToken]
anything = many anySingle




pHornClause2 :: Parser HornClause2
pHornClause2 = do
  hhead <- pHornHead2
  _when <- pToken When
  hbody <- pHornBody2
  return $ HC2 hhead (Just hbody)

pHornHead2 :: Parser RelationalPredicate
pHornHead2 = pRelationalPredicate

pHornBody2 :: Parser BoolStructR
pHornBody2 = pBSR



