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


module LS.Lib where

-- import qualified Data.Tree      as Tree
import qualified Data.Text.Lazy as Text
-- import Data.Text.Lazy.Encoding (decodeUtf8)
import Text.Megaparsec
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.Csv as Cassava
import qualified Data.Vector as V
import Data.Vector ((!), (!?))
import Text.Pretty.Simple (pPrint)
import qualified AnyAll as AA
import qualified Text.PrettyPrint.Boxes as Box
import           Text.PrettyPrint.Boxes hiding ((<>))
import System.Environment (lookupEnv)
import qualified Data.ByteString.Lazy as BS
import qualified Data.List.Split as DLS
import Text.Parser.Permutation
import Data.Aeson.Encode.Pretty
import qualified Data.List.NonEmpty as NE
import Options.Generic
import Data.Maybe (listToMaybe, maybeToList)

import LS.Types
import LS.Tokens
import LS.Parser
import LS.ParamText
import LS.RelationalPredicates
import LS.Error ( errorBundlePrettyCustom )
import LS.NLG (nlg)
import Control.Monad.Writer.Lazy

import LS.XPile.CoreL4
-- import LS.XPile.Prolog
import Data.List (transpose)
import qualified LS.XPile.Uppaal as Uppaal
import Debug.Trace (trace)

-- our task: to parse an input CSV into a collection of Rules.
-- example "real-world" input can be found at https://docs.google.com/spreadsheets/d/1qMGwFhgPYLm-bmoN2es2orGkTaTN382pG2z3RjZ_s-4/edit

-- the wrapping 'w' here is needed for <!> defaults and <?> documentation
data Opts w = Opts { demo :: w ::: Bool <!> "False"
                   , only :: w ::: String <!> "" <?> "native | tree | svg | babyl4 | corel4 | prolog | uppaal"
                   , dbug :: w ::: Bool <!> "False"
                   }
  deriving (Generic)
instance ParseRecord (Opts Wrapped)
deriving instance Show (Opts Unwrapped)


getConfig :: Opts Unwrapped -> IO RunConfig
getConfig o = do
  mpd <- lookupEnv "MP_DEBUG"
  mpj <- lookupEnv "MP_JSON"
  mpn <- lookupEnv "MP_NLG"
  return RC
        { debug = maybe (dbug o) (read :: String -> Bool) mpd
        , callDepth = 0
        , parseCallStack = []
        , sourceURL = "STDIN"
        , asJSON = maybe False (read :: String -> Bool) mpj
        , toNLG = maybe False (read :: String -> Bool) mpn
        , toBabyL4 = only o == "babyl4" || only o == "corel4"
        , toProlog = only o == "prolog"
        , toUppaal = only o == "uppaal"
        , saveAKA = False
        , wantNotRules = False
        }



someFunc :: Opts Unwrapped -> IO ()
someFunc opts = do
  runConfig <- getConfig opts
  myinput <- BS.getContents
  runExample runConfig myinput

-- printf debugging infrastructure





runExample :: RunConfig -> ByteString -> IO ()
runExample rc str = forM_ (exampleStreams str) $ \stream ->
    case runMyParser id rc pToplevel "dummy" stream of
      Left bundle -> do
        putStr (errorBundlePrettyCustom bundle)
        printStream stream
      -- Left bundle -> putStr (errorBundlePretty bundle)
      -- Left bundle -> pPrint bundle
      Right ([], []) -> return ()
      Right (xs, xs') -> do
        let rules = xs ++ xs'
        when (asJSON rc) $
          putStrLn $ toString $ encodePretty rules
        when (toNLG rc) $ do
          naturalLangSents <- mapM nlg xs
          mapM_ (putStrLn . Text.unpack) naturalLangSents
        when (toBabyL4 rc) $ do
          pPrint $ sfl4ToCorel4 rules
--        when (toProlog rc) $ do
--          pPrint $ sfl4ToProlog rules
        when (toUppaal rc) $ do
          pPrint $ Uppaal.toL4TA rules
          putStrLn $ Uppaal.taSysToString $ Uppaal.toL4TA rules
        unless (asJSON rc || toBabyL4 rc || toNLG rc || toProlog rc) $ do
          pPrint rules
          printStream stream

printStream :: MonadIO m => MyStream -> m ()
printStream stream = pPrint (tokenVal <$> unMyStream stream)

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
  in preprocess decoded
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

-- TODO: left trim all blank columns

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
             , let startPos = SourcePos "" (mkPos $ y + 1) (mkPos $ x + 1)
                   endPos   = SourcePos "" (mkPos $ y + 1) (mkPos $ x + 1) -- same
                   rawToken = vvt ! y ! x
                   tokenLength = 1
                  --  tokenLength = fromIntegral $ Text.length rawToken + 1 & \r -> Debug.trace (show r) r
                  --  tokenLength = fromIntegral $ Text.length rawToken + 1 & Debug.trace <$> show <*> id  -- same as above line, but with reader applicative
                  --  tokenLength = fromIntegral $ Text.length rawToken + 1  -- without debugging
             , tokenVal <- toToken rawToken
             , tokenVal `notElem` [ Empty, Checkbox ]
             ]
  where
    parenthesize :: [WithPos MyToken] -> [WithPos MyToken]
    parenthesize mys =
      tail . concat $ zipWith insertParen (withSOF:mys) (mys ++ [withEOF])
    eofPos = SourcePos "" pos1 pos1
    withEOF = WithPos eofPos eofPos 1 EOF
    withSOF = WithPos eofPos eofPos 1 SOF
    insertParen a@WithPos {   endPos = aPos }
                b@WithPos { startPos = bPos }
      | tokenVal a /= SOF &&
        aCol <  bCol &&
        aLin <  bLin =  trace ("Lib preprocessor: inserting EOL between " <> show (tokenVal a) <> " and " <> show (tokenVal b)) $
                        a : a { tokenVal = EOL }         --- | foo |     |    | foo   EOL | -- special case: we add an EOL to show the indentation crosses multiple lines.
                        : replicate (aCol - bCol) unDp   --- |     | bar | -> |     ( bar |

      | aCol <  bCol =  a                                --- | foo | bar | -> | foo ( bar | -- ordinary case: every indentation adds a GoDeeper.
                        : replicate (bCol - aCol) goDp

      | aCol >  bCol =  a                                --- |     | foo |                  -- ordinary case: every outdentation adds an UnDeeper; no EOL added.
                        : replicate (aCol - bCol) unDp   --- | bar |     | -> | foo ) bar |

      | otherwise    = [a]                            
      where
        aCol = unPos . sourceColumn $ aPos
        bCol = unPos . sourceColumn $ bPos
        aLin = unPos . sourceLine   $ aPos
        bLin = unPos . sourceLine   $ bPos
        goDp = b { tokenVal = GoDeeper }
        unDp = a { tokenVal = UnDeeper }
-- MyStream is the primary input for our Parsers below.
--

pToplevel :: Parser [Rule]
pToplevel = pRules <* eof

pRules :: Parser [Rule]
pRules = do
  wanted   <- many (try pRule)
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
pRule = do
  _ <- many dnl
  notFollowedBy eof
  try (debugName "pRule: unwrapping indentation" $ myindented pRule)
    <|> try (pRegRule <?> "regulative rule")
--     <|> try (pTypeDefinition   <?> "ontology definition")
-- --  <|> try (pMeansRule <?> "nullary MEANS rule")
--    <|> try (pConstitutiveRule <?> "constitutive rule")
--     <|> try (pScenarioRule <?> "scenario rule")
    <|> try (pHornlike <?> "DECIDE ... IS ... Horn rule")
    <|> try (RuleGroup . Just <$> pRuleLabel <?> "standalone rule section heading")


pTypeDefinition :: Parser Rule
pTypeDefinition = debugName "pTypeDefinition" $ do
  (proto,g,u) <- permute $ (,,)
    <$$> defineLimb
    <|?> (Nothing, givenLimb)
    <|?> (Nothing, uponLimb)
  return $ proto { given = snd <$> g, upon = snd <$> u }
  where
    defineLimb = do
      _dtoken <- pToken Define
      name  <- pNameParens
      myTraceM $ "got name = " <> show name
      super <- someIndentation $ optional pTypeSig
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
        }

    givenLimb = debugName "pHornlike/givenLimb" $ Just <$> preambleParamText [Given]
    uponLimb  = debugName "pHornlike/uponLimb"  $ Just <$> preambleParamText [Upon]
--        X MEANS    Y
-- DECIDE X MEANS    Y
-- DEEM   X MEANS    Y
--          IS       Y
--          INCLUDES Y
--                     WHEN / IF
--                               GIVEN

pMeansRule :: Parser Rule
pMeansRule = debugName "pMeansRule" $ do
  leftY  <- lookAhead pYLocation
  leftX  <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.
  srcurl <- asks sourceURL
  let srcref = SrcRef srcurl srcurl leftX leftY Nothing

  
  
  ((_d,d),gs,w,i,u,includes) <- permute $ (,,,,,)
    <$$> preambleParamText [Deem, Decide]
    <|?> ([], some $ preambleParamText [Given, Upon])
    <|?> ([], some $ preambleBoolStructR [When])
    <|?> ([], some $ preambleBoolStructR [If])
    <|?> ([], some $ preambleBoolStructR [Unless])
    <|?> ([], some $ preambleBoolStructR [Includes])

  -- let's extract the new term from the deem line
  let givens = concatMap (concatMap NE.toList . NE.toList . untypePT . snd) gs :: [Text.Text]
      dnew   = [ word | word <- concatMap NE.toList $ NE.toList (untypePT d), word `notElem` givens ]
  if length dnew /= 1
    then error "DEEM should identify exactly one term which was not previously found in the GIVEN line"
    else return $ Constitutive
         { name = dnew
         , keyword = Given
         , letbind = AA.Leaf $ RPParamText d
         , cond = addneg
                  (snd <$> mergePBRS (w<>i<>includes))
                  (snd <$> mergePBRS u)
         , given = NE.nonEmpty $ foldMap NE.toList (snd <$> gs)
         , rlabel = noLabel
         , lsource = noLSource
         , srcref = Just srcref
         }

pScenarioRule :: Parser Rule
pScenarioRule = debugName "pScenarioRule" $ do
  rlabel <- optional pRuleLabel
  leftY  <- lookAhead pYLocation -- this is the column where we expect IF/AND/OR etc.
  leftX  <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.
  srcurl <- asks sourceURL
  let srcref = SrcRef srcurl srcurl leftX leftY Nothing
  (expects,givens) <- permute $ (,)
    <$$> some pExpect
    <|?> ([], pToken Given >> pGivens)
  return $ Scenario
    { scgiven = givens
    , expect  = expects
    , rlabel = rlabel, lsource = Nothing, srcref = Just srcref
    }

pExpect :: Parser HornClause
pExpect = debugName "pExpect" $ do
  _expect  <- pToken Expect
  expect   <- someIndentation $ pRelationalPredicate
  whenpart <- someIndentation $ optional pWhenPart

  return $ HC
    { relPred = expect
    , relWhen = whenpart
    }
  where
    pWhenPart :: Parser HornBody
    pWhenPart = do
      _when   <- pToken When
      HBRP . AA.Leaf <$> pRelationalPredicate
      -- TODO: add support for more complex boolstructs of relational predicates
          
pGivens :: Parser [RelationalPredicate]
pGivens = debugName "pGiven" $ do
  sameDepth pRelationalPredicate


pRegRule :: Parser Rule
pRegRule = debugName "pRegRule" $ do
  maybeLabel <- optional pRuleLabel
  tentative  <- (try pRegRuleSugary
                  <|> pRegRuleNormal
                  <|> (pToken Fulfilled >> return RegFulfilled)
                  <|> (pToken Breach    >> return RegBreach)
                )
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
  entityname         <- AA.Leaf . multiterm2pt <$> pNameParens            -- You
  _leftX             <- lookAhead pXLocation
  let keynamewho = pure ((Party, entityname), Nothing)
  rulebody           <- someIndentation (permutationsReg keynamewho)
  -- TODO: refactor and converge the rest of this code block with Normal below
  henceLimb          <- optional $ pHenceLest Hence
  lestLimb           <- optional $ pHenceLest Lest
  let poscond = snd <$> mergePBRS (rbpbrs   rulebody)
  let negcond = snd <$> mergePBRS (rbpbrneg rulebody)
      toreturn = Regulative
                 { subj     = entityname
                 , keyword  = Party
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
  let keynamewho = (,) <$> pActor [Every,Party,TokAll]
                   <*> optional (try (manyIndentation (preambleBoolStructR [Who,Which,Whose])))
  rulebody <- permutationsReg keynamewho
  henceLimb                   <- optional $ pHenceLest Hence
  lestLimb                    <- optional $ pHenceLest Lest
  myTraceM $ "pRegRuleNormal: permutations returned rulebody " ++ show rulebody

  let poscond = snd <$> mergePBRS (rbpbrs   rulebody)
  let negcond = snd <$> mergePBRS (rbpbrneg rulebody)

  let toreturn = Regulative
                 { subj     = snd $ rbkeyname rulebody
                 , keyword  = fst $ rbkeyname rulebody
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
  pToken henceLest *> someIndentation (try pRegRule <|> RuleAlias <$> (pOtherVal <* dnl))



pTemporal :: Parser (Maybe (TemporalConstraint Text.Text))
pTemporal = eventually <|> specifically <|> vaguely
  where
    eventually   = debugName "pTemporal/eventually"   $ mkTC <$> pToken Eventually <*> pure 0 <*> pure ""
    specifically = debugName "pTemporal/specifically" $ indent3 mkTC sometime pNumber pOtherVal
    vaguely      = debugName "pTemporal/vaguely"      $ Just . TemporalConstraint TVague 0 <$> pOtherVal
    sometime     = choice $ map pToken [ Before, After, By, On ]

pPreamble :: [MyToken] -> Parser Preamble
pPreamble toks = choice (try . pToken <$> toks)

-- "PARTY Bob       AKA "Seller"
-- "EVERY Seller"
pActor :: [MyToken] -> Parser (Preamble, BoolStructP)
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

mkRBfromDT :: BoolStructP
           -> ((Preamble, BoolStructP )  -- every person
              ,Maybe (Preamble, BoolStructR)) -- who is red and blue
           -> (Deontic, Maybe (TemporalConstraint Text.Text))
           -> [(Preamble, BoolStructR)] -- positive  -- IF / WHEN
           -> [(Preamble, BoolStructR)] -- negative  -- UNLESS
           -> [(Preamble, ParamText )] -- upon  conditions
           -> [(Preamble, ParamText )] -- given conditions
           -> Maybe ParamText          -- having
           -> RuleBody
mkRBfromDT rba (rbkn,rbw) (rbd,rbt) rbpb rbpbneg rbu rbg rbh = RuleBody rba rbpb rbpbneg rbd rbt rbu rbg rbh rbkn rbw

mkRBfromDA :: (Deontic, BoolStructP)
           -> ((Preamble, BoolStructP ) -- every person or thing
              ,Maybe (Preamble, BoolStructR)) -- who is red and blue
           -> Maybe (TemporalConstraint Text.Text)
           -> [(Preamble, BoolStructR)] -- whenif
           -> [(Preamble, BoolStructR)] -- unless
           -> [(Preamble, ParamText )] -- upon  conditions
           -> [(Preamble, ParamText )] -- given conditions
           -> Maybe ParamText         -- having
           -> RuleBody
mkRBfromDA (rbd,rba) (rbkn,rbw) rbt rbpb rbpbneg rbu rbg rbh = RuleBody rba rbpb rbpbneg rbd rbt rbu rbg rbh rbkn rbw


preambleRelPred :: [MyToken] -> Parser (Preamble, RelationalPredicate)
preambleRelPred preambles = do
  preamble <- choice (try . pToken <$> preambles)
  relpred  <- pRelationalPredicate
  return (preamble, relpred)

permutationsReg :: Parser ((Preamble, BoolStructP), Maybe (Preamble, BoolStructR))
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

    (<&&>) = flip ($) -- or we could import Data.Functor ((&))
    infixl 1 <&&>

-- the Deontic/temporal/action form
-- MAY EVENTUALLY
--  -> pay
pDT :: Parser (Deontic, Maybe (TemporalConstraint Text.Text))
pDT = debugName "pDT" (fmap join <$> pDeontic `optIndentedTuple` pTemporal)

-- the Deontic/Action/Temporal form
pDA :: Parser (Deontic, BoolStructP)
pDA = debugName "pDA" $ do
  pd <- pDeontic
  pa <- pAction
  return (pd, pa)

preambleBoolStructP :: [MyToken] -> Parser (Preamble, BoolStructP)
preambleBoolStructP wanted = debugName ("preambleBoolStructP " <> show wanted)  $ do
  leftX     <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.
  condWord <- choice (try . pToken <$> wanted)
  myTraceM ("preambleBoolStructP: found: " ++ show condWord)
  ands <- withDepth leftX dBoolStructP -- (foo AND (bar OR baz), [constitutive and regulative sub-rules])
  return (condWord, ands)




-- TODO: Actually parse ParamTexts and not just single cells
dBoolStructP ::  Parser BoolStructP
dBoolStructP = debugName "dBoolStructP calling exprP" $ do
  toBoolStruct <$> exprP

exprP :: Parser (MyBoolStruct ParamText)
exprP = debugName "expr pParamText" $ do
  raw <- expr pParamText
  -- rewrite the raw returned from expr pParamText
  -- expr pParamText has returned MyLabel "pay" (MyLeaf (("to" :| ["the King"],Nothing) :| [("amount" :| ["$20"],Nothing)]))
  -- to MyLeaf (("pay" :| [], Nothing) :| [("to" :| ["the King"], Nothing) ...
  return $ case raw of
    MyLabel lbl myitem -> prefixFirstLeaf lbl myitem
    x -> x
  where
    prefixFirstLeaf :: Text.Text -> MyBoolStruct ParamText -> MyBoolStruct ParamText
    -- locate the first MyLeaf in the boolstruct and jam the lbl in as the first line
    prefixFirstLeaf p (MyLeaf x)           = MyLeaf (prefixItem p x)
    prefixFirstLeaf p (MyLabel lbl myitem) = MyLabel lbl (prefixFirstLeaf p myitem)
    prefixFirstLeaf p (MyAll (x:xs))       = MyAll (prefixFirstLeaf p x : xs)
    prefixFirstLeaf p (MyAll [])           = MyAll [MyLeaf $ text2pt p]
    prefixFirstLeaf p (MyAny [])           = MyAny [MyLeaf $ text2pt p]
    prefixFirstLeaf p (MyAny (x:xs))       = MyAny (prefixFirstLeaf p x : xs)
    prefixFirstLeaf p (MyNot  x    )       = MyNot (prefixFirstLeaf p x)

    prefixItem :: Text.Text -> ParamText -> ParamText
    prefixItem t pt = NE.cons (NE.head $ text2pt t) pt

-- dBoolStructP = debugName "dBoolStructP" $ do
--   pAndGroup -- walks AND eats OR drinks

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
  depth <- asks callDepth
  elem1    <- withDepth (depth + 1) pElement
  elems    <- many $ pToken Or *> withDepth (depth+1) pElement
  let toreturn = if null elems
                 then elem1
                 else AA.Any Nothing (elem1 : elems)
  return toreturn

pAtomicElement ::  Parser BoolStructP
pAtomicElement = debugName "pAtomicElement" $ do
  try pNestedBool
    <|> pNotElement
    <|> pLeafVal

pElement :: Parser BoolStructP
pElement = debugName "pElement" $ do
        try (hornlikeAsElement <$> tellIdFirst pHornlike)
    <|> pAtomicElement

-- Makes a leaf with just the name of a constitutive rule
-- constitutiveAsElement ::  Rule -> BoolStructP
-- constitutiveAsElement cr = AA.Leaf $ multiterm2pt $ name cr

-- Makes a leaf with just the name of a hornlike rule
hornlikeAsElement ::  Rule -> BoolStructP
hornlikeAsElement hlr = AA.Leaf $ multiterm2pt $ name hlr

pNotElement :: Parser BoolStructP
pNotElement = debugName "pNotElement" $ do
  depth <- asks callDepth
  inner <- pToken MPNot *> withDepth (depth+1) pElement
  return $ AA.Not inner

pLeafVal ::  Parser BoolStructP
pLeafVal = debugName "pLeafVal" $ do
  leafVal <- pParamText
  myTraceM $ "pLeafVal returning " ++ show leafVal
  return $ AA.Leaf leafVal

-- TODO: we should be able to get rid of pNestedBool and just use a recursive call into dBoolStructP without pre-checking for a pBoolConnector. Refactor when the test suite is a bit more comprehensive.

pNestedBool ::  Parser BoolStructP
pNestedBool = debugName "pNestedBool" $ do
  -- "foo AND bar" is a nestedBool; but just "foo" is a leafval.
  (leftX,foundBool) <- lookAhead (pLeafVal >> optional dnl >> (,) <$> lookAhead pXLocation <*> pBoolConnector)
  myTraceM $ "pNestedBool matched " ++ show foundBool ++ " at location " ++ show leftX
  withDepth leftX dBoolStructP

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
pHornBody2 = pBoolStructR

