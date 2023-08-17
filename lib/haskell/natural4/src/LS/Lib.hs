{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Parser functions not organized into their own separate modules elsewhere.

This includes some top-leve parsers like pRules and pBoolStruct.
-}

module LS.Lib where

-- import qualified Data.Tree      as Tree
-- import Data.Text.Encoding (decodeUtf8)

import AnyAll qualified as AA
-- import LS.XPile.CoreL4
-- import Data.ByteString.Lazy.UTF8 (toString)

import Control.Monad.Combinators.Expr
import Control.Monad.Writer.Lazy
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BS
import Data.Csv qualified as Cassava
import Data.Either (rights)
import Data.List (transpose)
import Data.List.NonEmpty qualified as NE
import Data.List.Split qualified as DLS
import Data.Maybe (listToMaybe, maybeToList)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LT
import Data.Vector ((!), (!?))
import Data.Vector qualified as V
import Data.Void (Void)
import LS.Error (errorBundlePrettyCustom)
import LS.Parser
  ( MyBoolStruct,
    MyItem (MyAll, MyAny, MyLabel, MyLeaf, MyNot),
    binary,
    expr,
    prefix,
  )
import LS.RelationalPredicates
import LS.Rule
import LS.Tokens
import LS.Types
import Options.Generic
  ( Generic,
    ParseFields (..),
    ParseRecord,
    Unwrapped,
    Wrapped,
    type (:::),
    type (<!>),
    type (<?>),
  )
import System.Environment (lookupEnv)
import Text.Megaparsec
  ( MonadParsec (eof, lookAhead, notFollowedBy, try),
    ParseErrorBundle,
    SourcePos (SourcePos, sourceColumn, sourceLine),
    anySingle,
    choice,
    many,
    mkPos,
    optional,
    pos1,
    some,
    unPos,
    (<?>),
    (<|>),
  )
import Text.Parser.Permutation (permute, (<$$>), (<|?>), (<||>))
import Text.Pretty.Simple (pPrintString, pStringNoColor)
import Text.PrettyPrint.Boxes (nullBox, render)
import Text.PrettyPrint.Boxes qualified as Box

-- our task: to parse an input CSV into a collection of Rules.
-- example "real-world" input can be found at https://docs.google.com/spreadsheets/d/1qMGwFhgPYLm-bmoN2es2orGkTaTN382pG2z3RjZ_s-4/edit

-- the wrapping 'w' here is needed for <!> defaults and <?> documentation
data Opts w = Opts { demo :: w ::: Bool <!> "False"
                   , only :: w ::: String <!> "" <?> "native | tree | svg | babyl4 | corel4 | prolog | uppaal | vue | grounds | checklist"

                   , workdir   :: w ::: String <!> ""  <?> "workdir to save all the output files to"
                   , uuiddir   :: w ::: String <!> "no-uuid"  <?> "uuid prefix to follow the workdir"
                   , toprolog  :: w ::: Bool   <!> "True"  <?> "prolog-like syntax representing the predicate logic"
                   , toscasp   :: w ::: Bool   <!> "True"  <?> "sCasp-like syntax representing the predicate logic"
                   , tonative  :: w ::: Bool   <!> "True"  <?> "native Haskell data structure of the AST"
                   , topetri   :: w ::: Bool   <!> "True"  <?> "a petri-net Dot file of the state graph"
                   , toaasvg   :: w ::: Bool   <!> "True"  <?> "an anyall SVG of the decision trees"
                   , tocorel4  :: w ::: Bool   <!> "True"  <?> "in core-l4 syntax"
                   , tobabyl4  :: w ::: Bool   <!> "True"  <?> "in baby-l4 syntax (directly via AST)"
                   , toasp     :: w ::: Bool   <!> "True"  <?> "in ASP syntax"
                   , toepilog  :: w ::: Bool   <!> "True"  <?> "in Epilog syntax"
                   , todmn     :: w ::: Bool   <!> "True"  <?> "in DMN syntax"
                   , tojson    :: w ::: Bool   <!> "True"  <?> "anyall representation dumped as raw JSON"
                   , tovuejson :: w ::: Bool   <!> "True"  <?> "anyall representation dumped as JSON for the web app (currently Vue) to pick up"
                   , topurs    :: w ::: Bool   <!> "True"  <?> "anyall representation dumped as Purescript source code for mv'ing into RuleLib/*.purs"
                   , tomd      :: w ::: Bool   <!> "True"  <?> "nlg markdown"
                   , togftrees      :: w ::: Bool   <!> "True"  <?> "nlg trees"
                   , togrounds :: w ::: Bool   <!> "True"  <?> "ground terms"
                   , tots      :: w ::: Bool   <!> "True"  <?> "typescript"
                   , tonl      :: w ::: Bool   <!> "True"  <?> "natural language"
                   , tomaude    :: w ::: Bool   <!> "True"  <?> "maude"
                   , tocheckl  :: w ::: Bool   <!> "False" <?> "ground terms phrased in checklist syntax"

                   , tointro   :: w ::: Bool   <!> "True" <?> "introduction to transpilation"

                   , dbug :: w ::: Bool <!> "False"
                   , extd :: w ::: Bool <!> "False" <?> "unhide grounds carrying typical values"
                   , file :: w ::: NoLabel [String] <?> "filename..."
                   , dstream :: w ::: Bool <!> "False"
                   }
  deriving (Generic)

instance ParseRecord (Opts Wrapped)
deriving instance Show (Opts Unwrapped)

-- [TODO]
-- | a convention for representing a transpiler's interface
-- - what comment syntax do we use?
-- - what file extension do we use?
-- - what is the command line parameter?
-- (this can also be a typeclass if we want.)
data Transpiler = XPiler
  { comment   :: ()
  , extension :: ()
  , cliParam  :: ()
  }
  deriving (Show)

-- technique for getting varargs argv https://github.com/Gabriel439/Haskell-Optparse-Generic-Library/issues/65
newtype NoLabel a = NoLabel a
  deriving (Generic, Show)

instance ParseFields a => ParseRecord (NoLabel a)
instance ParseFields a => ParseFields (NoLabel a) where
  parseFields msg _ _ def = NoLabel <$> parseFields msg Nothing Nothing def



getConfig :: Opts Unwrapped -> IO RunConfig
getConfig o = do
  mpd <- lookupEnv "MP_DEBUG"
  mpn <- lookupEnv "MP_NLG"
  return RC
        { debug       = maybe (dbug o) (read :: String -> Bool) mpd
        , printstream = maybe (dstream o) (read :: String -> Bool) mpd
        , callDepth = 0
        , oldDepth = 0
        , parseCallStack = []
        , sourceURL = "STDIN"
        , asJSON = only o == "json" -- maybe False (read :: String -> Bool) mpj
        , toNLG = maybe False (read :: String -> Bool) mpn
        , toBabyL4  = only o == "babyl4" || only o == "corel4"
        , toASP     = only o == "asp"
        , toProlog  = only o == "prolog"
        , toSCasp   = only o == "scasp"
        , toUppaal  = only o == "uppaal"
        , toGrounds = only o == "grounds"
        , toChecklist = only o == "checklist"
        , toVue     = only o == "vue"
        , toHTML    = only o == "html"
        , toTS      = only o `elem` words "typescript ts"
        , saveAKA = False
        , wantNotRules = False
        , extendedGrounds = extd o
        , runNLGtests = False
        }


-- | Each stanza gets parsed separately, which is why we have a top-level IO [Rule].
-- 
-- At some point we added functionality that allowed sub-rules to be defined inline within a top-level rule, which is why we now have IO [... [Rule]].
--
-- Note that sub-rules are themselves rules, which is why we only have one Rule type here.
--
-- Shouldn't the idea of sub-rules and top-level rules be reflected in a type hierarchy?
--
parseRules :: Opts Unwrapped -> IO [Either (ParseErrorBundle MyStream Void) [Rule]] -- [TODO] why inner [Rule] and not just a plain Rule? Give explanation in comment.
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
          putStrLn (errorBundlePrettyCustom bundle)
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
pRenderStream = Text.unpack . LT.toStrict . pStringNoColor . renderStream

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
equalizeLines stanza = fmap (pad maxLen) stanza
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
  where chunks = getChunks rs

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
             , tokenVal <- toToken (Text.strip rawToken) -- " IS " becomes "IS"
             , tokenVal `notElem` [ Empty, TokTrue, TokFalse ] -- ignore TRUE and FALSE values ... so long as our policy is to ignore checkboxes, that is.
             ]
  where
    parenthesize :: [WithPos MyToken] -> [WithPos MyToken]
    parenthesize mys =
      tail . concat $ zipWith insertParen (withSOF:mys) (mys ++ [withEOF]) -- [TODO] this may be a source of slowness. Consider switching to Text, and if that doesn't help, to Data.Array?
    eofPos = SourcePos "" pos1 pos1
    withEOF = WithPos eofPos 1 Nothing EOF
    withSOF = WithPos eofPos 1 Nothing SOF
    insertParen a@WithPos { pos = aPos }
                b@WithPos { pos = bPos }
      | tokenVal a /= SOF &&
        aCol <  bCol &&
        aLin <  bLin =  -- trace ("Lib preprocessor: inserting EOL between " <> show (tokenVal a) <> " and " <> show (tokenVal b)) $
                        a : a { tokenVal = EOL }            --- | foo |     |    | foo   EOL | -- special case: we add an EOL to show the indentation crosses multiple lines.
                        : (goDp <$> [1 .. (bCol - aCol)])   --- |     | bar | -> |     ( bar | -- for example, in a ParamText, the "bar" line gives a parameter to the "foo" line

      | aCol <  bCol =  a                                   --- | foo | bar | -> | foo ( bar | -- ordinary case: every indentation adds a GoDeeper.
                        : (goDp <$> [1 .. (bCol - aCol)])

      | aCol >  bCol =  a                                   --- |     | foo |                  -- ordinary case: every outdentation adds an UnDeeper; no EOL added.
                        : (unDp <$> [1 .. (aCol - bCol)])   --- | bar |     | -> | foo ) bar |

      | otherwise    = [a]                                  --- | foo |       -> | foo   bar | -- at the same level, no ( or ) added.
                                                            --- | bar |
      where
        aCol = unPos . sourceColumn $ aPos
        bCol = unPos . sourceColumn $ bPos
        aLin = unPos . sourceLine   $ aPos
        bLin = unPos . sourceLine   $ bPos
        goDp n = let newPos = aPos { sourceColumn = mkPos (aCol + n) }
                 in b { tokenVal = GoDeeper, pos = newPos }
        unDp n = let newPos = bPos { sourceColumn = mkPos (bCol + n) }
                 in a { tokenVal = UnDeeper, pos = newPos }
-- MyStream is the primary input for our Parsers below.
--

pToplevel :: Parser [Rule]
pToplevel = pRules <* eof

-- do not allow NotARule parsing

pRules, pRulesOnly, pRulesAndNotRules :: Parser [Rule]
pRulesOnly = do
  debugName "pRulesOnly: some" $ concat <$>
    some (debugName "trying semicolon *> pRule" $
          try (debugName "semicolon" semicolonBetweenRules
               *> optional dnl
               *> manyIndentation (sameDepth (try pRule))
               <* optional dnl)
         )
    <* semicolonBetweenRules
    <* eof

semicolonBetweenRules :: Parser [MyToken]
semicolonBetweenRules = many (manyIndentation (Semicolon <$ some (pToken Semicolon)))

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
  _ <- debugName "many dnl" $ many dnl
  notFollowedBy eof

  leftY  <- lookAhead pYLocation -- this is the column where we expect IF/AND/OR etc.
  leftX  <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.
  srcurl <- asks sourceURL
  let srcref = SrcRef srcurl srcurl leftX leftY Nothing

  foundRule <- (pRegRule <?> "regulative rule")
    <|> try (pTypeDeclaration   <?> "type declaration -- ontology definition")
    <|> try (pVarDefn           <?> "variable definition")
    <|> try (c2hornlike <$> pConstitutiveRule <?> "constitutive rule")
    <|> try (pScenarioRule <?> "scenario rule")
    <|> try (pHornlike <?> "DECIDE ... IS ... Horn rule")
    <|> ((\rl -> RuleGroup (Just rl) Nothing) <$> pRuleLabel <?> "standalone rule section heading")

  return $ foundRule { srcref = Just srcref }


-- TypeDecl
pTypeDeclaration :: Parser Rule
pTypeDeclaration = debugName "pTypeDeclaration" $ do
  maybeLabel <- optional pRuleLabel -- TODO: Handle the SL
  (proto,g,u) <- permute $ (,,)
    <$$> pToken Declare *> someIndentation declareLimb
    <|?> (Nothing, givenLimb)
    <|?> (Nothing, uponLimb)
  return $ proto { given = snd <$> g, upon = snd <$> u, rlabel = maybeLabel }
  where
    -- [TODO] this doesn't correctly parse something that looks like
    -- DECLARE whatever
    --     HAS this
    --     HAS that

    -- it treats the "that" as a child of "this", which is wrong.
    -- workaround: remove the "HAS" from the "that" line
    -- but it would be better to fix up the parser here so that we don't allow too many undeepers.

    parseHas = debugName "parseHas" $ concat <$> many (flip const $>| pToken Has |>| sameDepth declareLimb)
    declareLimb = do
      ((name,super),has) <- debugName "pTypeDeclaration/declareLimb: sameOrNextLine slKeyValuesAka parseHas" $ slKeyValuesAka |&| parseHas
      myTraceM $ "got name = " <> show name
      myTraceM $ "got super = " <> show super
      myTraceM $ "got has = " <> show has
      enums <- optional pOneOf
      myTraceM $ "got enums = " <> show enums
      return $ TypeDecl
        { name = NE.toList name
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

    givenLimb = debugName "pTypeDeclaration/givenLimb" . pretendEmpty $ Just <$> preambleParamText [Given]
    uponLimb  = debugName "pTypeDeclaration/uponLimb"  . pretendEmpty $ Just <$> preambleParamText [Upon]


-- VarDefn gets turned into a Hornlike rule
pVarDefn :: Parser Rule
pVarDefn = debugName "pVarDefn" $ do
  maybeLabel <- optional pRuleLabel
  (proto,g,u,w) <- permute $ (,,,)
    <$$> pToken Define *> defineLimb
    <|?> (Nothing, givenLimb)
    <|?> (Nothing, uponLimb)
    <|?> (Nothing, whenCase)
  return $ proto { given = snd <$> g, upon = snd <$> u, rlabel = maybeLabel
                 -- this is the same as addWhen from RelationalPredicates
                 , clauses = if not (null (clauses proto))
                             then [ hc2 { hBody = hBody hc2 <> w }
                                  | hc2 <- clauses proto
                                  ]
                             -- this really should be restricted to only MEANS and IS
                             else [ ]
                 }
  where
    defineLimb = debugName "pVarDefn/defineLimb" $ do
      (name,mytype) <- manyIndentation pKeyValuesAka
      myTraceM $ "got name = " <> show name
      myTraceM $ "got mytype = " <> show mytype
      hases   <- concat <$> some (pToken Has *> someIndentation (debugName "sameDepth pParamTextMustIndent" $ sameDepth pParamTextMustIndent))
      myTraceM $ "got hases = " <> show hases
      return $ defaultHorn
        { name = NE.toList name
        , keyword = Define
        , super = mytype
        , given = Nothing -- these get overwritten immediately above in the return
        , clauses = [ HC { hHead = RPParamText has, hBody = Nothing }
                    | has <- hases
                    ]
        }

    givenLimb = debugName "pVarDefn/givenLimb" . pretendEmpty $ Just <$> preambleParamText [Given]
    uponLimb  = debugName "pVarDefn/uponLimb"  . pretendEmpty $ Just <$> preambleParamText [Upon]

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

-- | this is intended to parse:
-- @EXPECT   The Sky IS Blue
-- which should turn into RPConstraint ["The Sky"] RPis ["Blue"]
--
-- @EXPECT   NOT The Sky IS Blue
-- turns into RPBoolStructR [] RPis (AA.Not (AA.Leaf (RPConstraint ["The Sky"] RPis ["Blue"])))
-- which isn't great because the `[] RPis` was just made up to let the type fit
--
-- maybe in a glorious future we can have that parse into
-- RPConstraint (RPNot (RPIs ["The Sky", "Blue"]))

pExpect :: Parser Expect
pExpect = debugName "pExpect" $ do
  _expect  <- pToken Expect
  relPred <- someIndentation $
             try (do
                     (tmp, _when) <- rpSameNextLineWhen
                     return tmp
                 )
             <|> RPBoolStructR [] RPis <$> pBSR
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
  manyIndentation $ choice
                [ try $ (\r -> r { rlabel = maybeLabel }) <$> pRegRuleNormal
                , try $ (\r -> r { rlabel = maybeLabel }) <$> pRegRuleSugary
                , try (pToken Fulfilled >> return RegFulfilled)
                , try (pToken Breach    >> return RegBreach)
                ]

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
  entityname         <- AA.mkLeaf . multiterm2pt <$> someDeep pMTExpr            -- You ... but no AKA allowed here
  _leftX             <- lookAhead pXLocation
  let keynamewho = pure ((RParty, entityname), Nothing)
  (rulebody,henceLimb,lestLimb) <- someIndentation ((,,)
                                                     <$> permutationsReg keynamewho
                                                     <*> optional (pHenceLest Hence)
                                                     <*> optional (pHenceLest Lest)
                                                   )
  let poscond = snd <$> mergePBRS (rbpbrs   rulebody)
  let negcond = snd <$> mergePBRS (rbpbrneg rulebody)
      gvn     = NE.nonEmpty $ foldMap NE.toList (snd <$> rbgiven rulebody)
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
                 , given    = gvn    -- given
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

stackGiven :: Maybe ParamText -> Rule -> Rule
stackGiven gvn r@Regulative  {..} = r { given = gvn <> given }
stackGiven gvn r@Hornlike    {..} = r { given = gvn <> given }
stackGiven gvn r@TypeDecl    {..} = r { given = gvn <> given }
stackGiven gvn r@Constitutive{..} = r { given = gvn <> given }
stackGiven _   r                  = r

pRegRuleNormal :: Parser Rule
pRegRuleNormal = debugName "pRegRuleNormal" $ do
  let keynamewho = (,) <$> pActor [REvery,RParty,RTokAll]
                   <*> optional (manyIndentation (preambleBoolStructR [Who,Which,Whose]))
  rulebody <- permutationsReg keynamewho
  let gvn     = NE.nonEmpty $ foldMap NE.toList (snd <$> rbgiven rulebody)
      poscond = snd <$> mergePBRS (rbpbrs   rulebody)
      negcond = snd <$> mergePBRS (rbpbrneg rulebody)
  henceLimb                   <- optional $ stackGiven gvn <$> pHenceLest Hence
  lestLimb                    <- optional $ stackGiven gvn <$> pHenceLest Lest
  myTraceM $ "pRegRuleNormal: permutations returned rulebody " ++ show rulebody

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
                 , upon     = listToMaybe (snd <$> rbupon  rulebody)
                 , given    = gvn
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
      try (debugName "pHenceLest -> innerRule -> pRule" pRule)
      <|> RuleAlias <$> (optional (pToken Goto) *> someDeep pMTExpr)

pTemporal :: Parser (Maybe (TemporalConstraint Text.Text))
pTemporal = eventually <|> specifically <|> vaguely
  where
    eventually   = debugName "pTemporal/eventually"   $ mkTC <$> pToken Eventually <*> pure (Just 0) <*> pure ""
    specifically = debugName "pTemporal/specifically" $ mkTC $>| sometime |*| liftSLOptional (floor <$> pNumber) |>< pOtherVal
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
  let boolEntity = AA.mkLeaf $ multiterm2pt entitytype
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
pDoAction = debugName "pDoAction" $ snd <$> preambleBoolStructP [ Do ]

pAction :: Parser BoolStructP
pAction = debugName "pAction calling pParamText" (AA.mkLeaf <$> pParamText)

-- we create a permutation parser returning one or more RuleBodies, which we treat as monoidal,
-- though later we may object if there is more than one.

mkRBfromDT :: BoolStructP
           -> ((RegKeywords, BoolStructP )  -- every person
              ,Maybe (Preamble, BoolStructR)) -- who is red and blue
           -> (Deontic, Maybe (TemporalConstraint Text.Text))
           -> [(Preamble, BoolStructR)] -- positive  -- IF / WHEN
           -> [(Preamble, BoolStructR)] -- negative  -- UNLESS
           -> [(Preamble, ParamText )] -- upon  conditions
           -> [(Preamble, ParamText )] -- given conditions
           -> Maybe ParamText          -- having
           -> [Rule]
           -> RuleBody
mkRBfromDT rba (rbkn,rbwho) (rbd,rbt) rbpb rbpbneg rbu rbg rbh rbwhere =
  RuleBody rba rbpb rbpbneg rbd rbt rbu rbg rbh rbkn rbwho rbwhere

mkRBfromDA :: (Deontic, BoolStructP)
           -> ((RegKeywords, BoolStructP ) -- every person or thing
              ,Maybe (Preamble, BoolStructR)) -- who is red and blue
           -> Maybe (TemporalConstraint Text.Text)
           -> [(Preamble, BoolStructR)] -- whenif
           -> [(Preamble, BoolStructR)] -- unless
           -> [(Preamble, ParamText )] -- upon  conditions
           -> [(Preamble, ParamText )] -- given conditions
           -> Maybe ParamText         -- having
           -> [Rule]
           -> RuleBody
mkRBfromDA (rbd,rba) (rbkn,rbwho) rbt rbpb rbpbneg rbu rbg rbh rbwhere
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
  try ( debugName "regulative permutation with deontic-temporal" $ permute ( mkRBfromDT
            <$$> pDoAction
            <||> keynamewho
            <||> try pDT
            <&&> whatnot
          ) )
  <|>
  try ( debugName "regulative permutation with deontic-action" $ permute ( mkRBfromDA
            <$$> try pDA
            <||> keynamewho
            <|?> (Nothing, pTemporal)
            <&&> whatnot
          ) )
  where
    whatnot x = x
                <|?> ([], some $ preambleBoolStructR [When, If])   -- syntactic constraint, all the if/when need to be contiguous.
                <|?> ([], some $ preambleBoolStructR [Unless]) -- unless
                <|?> ([], some $ preambleParamText [Upon])   -- upon
                <|?> ([], some $ preambleParamText [Given])  -- given
                <|?> (Nothing, Just . snd <$> preambleParamText [Having])  -- having
                <|?> ([], (debugName "WHERE" $ pToken Where) >> someIndentation (some (pHornlike' False)))  -- WHERE ends up in the wwhere attribute of a Regulative

    (<&&>) = flip ($) -- or we could import Data.Functor ((&))
    infixl 1 <&&>

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
  pa <- someIndentation dBoolStructP
  return (pd, pa)

preambleBoolStructP :: [MyToken] -> Parser (Preamble, BoolStructP)
preambleBoolStructP wanted = debugName ("preambleBoolStructP " <> show wanted)  $ do
  condWord <- choice (try . pToken <$> wanted)
  myTraceM ("preambleBoolStructP: found: " ++ show condWord)
  ands <- dBoolStructP -- (foo AND (bar OR baz), [constitutive and regulative sub-rules])
  return (condWord, ands)


dBoolStructP ::  Parser BoolStructP
dBoolStructP = debugName "dBoolStructP" $ do
  makeExprParser (manyIndentation $ AA.mkLeaf <$> pParamText)
         [ [ prefix MPNot   (\x   -> AA.mkNot x) ]
         , [ binary Or      (\x y -> AA.mkAny Nothing [x, y]) ]
         , [ binary Unless  (\x y -> AA.mkAll Nothing [x, AA.mkNot y]) ]
         , [ binary And     (\x y -> AA.mkAll Nothing [x, y]) ]
         ]

exprP :: Parser (MyBoolStruct ParamText)
exprP = debugName "expr pParamText" $ do
  raw <- expr pParamText

  return $ case raw of
    MyLabel pre _post myitem -> prefixFirstLeaf pre myitem
    x -> x
  where
    prefixFirstLeaf :: MultiTerm -> MyBoolStruct ParamText -> MyBoolStruct ParamText
    -- locate the first MyLeaf in the boolstruct and jam the lbl in as the first line
    prefixFirstLeaf p (MyLeaf x)           = MyLeaf (prefixItem p x)
    prefixFirstLeaf p (MyLabel pre post myitem) = MyLabel pre post (prefixFirstLeaf p myitem)
    prefixFirstLeaf p (MyAll (x:xs))       = MyAll (prefixFirstLeaf p x : xs)
    prefixFirstLeaf p (MyAll [])           = MyAll [MyLeaf $ mt2pt p]
    prefixFirstLeaf p (MyAny [])           = MyAny [MyLeaf $ mt2pt p]
    prefixFirstLeaf p (MyAny (x:xs))       = MyAny (prefixFirstLeaf p x : xs)
    prefixFirstLeaf p (MyNot  x    )       = MyNot (prefixFirstLeaf p x)

    prefixItem :: MultiTerm -> ParamText -> ParamText
    prefixItem t pt = NE.cons (NE.fromList t, Nothing) pt


pAndGroup ::  Parser BoolStructP
pAndGroup = debugName "pAndGroup" $ do
  orGroup1 <- pOrGroup
  orGroupN <- many $ pToken And *> pOrGroup
  let toreturn = if null orGroupN
                 then orGroup1
                 else AA.mkAll Nothing (orGroup1 : orGroupN)
  return toreturn

pOrGroup ::  Parser BoolStructP
pOrGroup = debugName "pOrGroup" $ do
  elem1    <- pElement
  elems    <- many $ pToken Or *> pElement
  let toreturn = if null elems
                 then elem1
                 else AA.mkAny Nothing (elem1 : elems)
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

-- Makes a leaf with just the name of a hornlike rule
hornlikeAsElement ::  Rule -> BoolStructP
hornlikeAsElement hlr = AA.mkLeaf $ multiterm2pt $ name hlr

pNotElement :: Parser BoolStructP
pNotElement = debugName "pNotElement" $ do
  inner <- pToken MPNot *> pElement
  return $ AA.mkNot inner

pLeafVal ::  Parser BoolStructP
pLeafVal = debugName "pLeafVal" $ do
  leafVal <- pParamText
  myTraceM $ "pLeafVal returning " ++ show leafVal
  return $ AA.mkLeaf leafVal

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
  return $ HC hhead (Just hbody)

pHornHead2 :: Parser RelationalPredicate
pHornHead2 = pRelationalPredicate

pHornBody2 :: Parser BoolStructR
pHornBody2 = pBSR



