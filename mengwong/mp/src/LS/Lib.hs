{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE FlexibleInstances  #-}  -- One more extension.
{-# LANGUAGE StandaloneDeriving #-}  -- To derive Show


module LS.Lib where

-- import qualified Data.Tree      as Tree
import qualified Data.Text.Lazy as Text
-- import Data.Text.Lazy.Encoding (decodeUtf8)
import Text.Megaparsec
import qualified Data.Set           as Set
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.Csv as Cassava
import qualified Data.Vector as V
import Data.Vector ((!), (!?))
import Data.Maybe (fromMaybe)
import Text.Pretty.Simple (pPrint)
import qualified AnyAll as AA
import qualified Text.PrettyPrint.Boxes as Box
import           Text.PrettyPrint.Boxes hiding ((<>))
import System.Environment (lookupEnv)
import qualified Data.ByteString.Lazy as BS
import qualified Data.List.Split as DLS
import Text.Parser.Permutation
import Debug.Trace
import Data.Aeson.Encode.Pretty
import Data.List.NonEmpty ( NonEmpty((:|)), nonEmpty, toList )
import Options.Generic

import LS.Types
import LS.Error ( errorBundlePrettyCustom )
import LS.NLG (nlg)
import Control.Monad.Reader (asks, local)
import Control.Monad.Writer.Lazy
-- import Data.Foldable (fold)

import LS.XPile.BabyL4
import LS.XPile.Prolog
import qualified Data.List.NonEmpty as NE
import Data.List (transpose)

-- our task: to parse an input CSV into a collection of Rules.
-- example "real-world" input can be found at https://docs.google.com/spreadsheets/d/1qMGwFhgPYLm-bmoN2es2orGkTaTN382pG2z3RjZ_s-4/edit

-- the wrapping 'w' here is needed for <!> defaults and <?> documentation
data Opts w = Opts { demo :: w ::: Bool <!> "False"
                   , only :: w ::: String <!> "" <?> "native | tree | svg | babyl4 | prolog"
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
        , toBabyL4 = only o == "babyl4"
        , toProlog = only o == "prolog"
        }



someFunc :: Opts Unwrapped -> IO ()
someFunc opts = do
  runConfig <- getConfig opts
  myinput <- BS.getContents
  runExample runConfig myinput

-- printf debugging infrastructure

whenDebug :: Parser () -> Parser ()
whenDebug act = do
  isDebug <- asks debug
  when isDebug act

myTraceM :: String -> Parser ()
myTraceM x = whenDebug $ do
  callDepth <- asks nestLevel
  traceM $ indentShow callDepth <> x
  where
    indentShow depth = concat $ replicate depth "| "

debugPrint :: String -> Parser ()
debugPrint str = whenDebug $ do
  lookingAt <- lookAhead (getToken :: Parser MyToken)
  depth <- asks callDepth
  myTraceM $ "/ " <> str <> " running. depth=" <> show depth <> "; looking at: " <> show lookingAt

-- force debug=true for this subpath
alwaysdebugName :: Show a => String -> Parser a -> Parser a
alwaysdebugName name p = local (\rc -> rc { debug = True }) $ debugName name p

debugName :: Show a => String -> Parser a -> Parser a
debugName name p = do
  debugPrint name
  res <- local (increaseNestLevel name) p
  myTraceM $ "\\ " <> name <> " has returned " <> show res
  return res

-- | withDepth n p sets the depth to n for parser p
withDepth :: Depth -> Parser a -> Parser a
withDepth n = local (\st -> st {callDepth= n})

runExample :: RunConfig -> ByteString -> IO ()
runExample rc str = forM_ (exampleStreams str) $ \stream ->
    case runMyParser id rc pRule "dummy" stream of
      Left bundle -> putStr (errorBundlePrettyCustom bundle)
      -- Left bundle -> putStr (errorBundlePretty bundle)
      -- Left bundle -> pPrint bundle
      Right ([], []) -> return ()
      Right (xs, xs') -> do
        when (asJSON rc) $
          putStrLn $ toString $ encodePretty (xs ++ xs')
        when (toNLG rc) $ do
          naturalLangSents <- mapM nlg xs
          mapM_ (putStrLn . Text.unpack) naturalLangSents
        when (toBabyL4 rc) $ do
          pPrint $ sfl4ToBabyl4 $ xs ++ xs'
        when (toProlog rc) $ do
          pPrint $ sfl4ToProlog $ xs ++ xs'
        unless (asJSON rc || toBabyL4 rc || toNLG rc || toProlog rc) $
          pPrint $ xs ++ xs'

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
      return $ fmap trimLegalSource <$> trimComment False . V.toList <$> vvt
    -- ignore the () at the beginning of the line. Here it actually trims any (...) from any position but this is good enough for now
    trimLegalSource x = let asChars = Text.unpack x
                        in if length asChars > 0
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
getChunks rs =
  let listChunks = (DLS.split . DLS.keepDelimsR . DLS.whenElt) emptyRow [ 0 .. V.length rs - 1 ]
      containsMagicKeyword rowNr = V.any (`elem` magicKeywords) (rs ! rowNr)
      emptyRow rowNr = V.all Text.null (rs ! rowNr)
      wantedChunks = [ firstAndLast neRows
                     | rows <- listChunks
                     ,    any containsMagicKeyword rows
                       || all emptyRow rows
                     , Just neRows <- pure $ NE.nonEmpty rows
                     ]
      toreturn = extractLines rs <$> glueLineNumbers wantedChunks
  in -- trace ("getChunks: input = " ++ show [ 0 .. V.length rs - 1 ])
     -- trace ("getChunks: listChunks = " ++ show listChunks)
     -- trace ("getChunks: wantedChunks = " ++ show wantedChunks)
     -- trace ("getChunks: returning " ++ show (length toreturn) ++ " stanzas: " ++ show toreturn)
     toreturn

firstAndLast :: NonEmpty Int -> (Int, Int)
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
stanzaAsStream rs = do
  let vvt = rs
  -- MyStream (Text.unpack $ decodeUtf8 s) [ WithPos {..}
  MyStream rs [ WithPos {..}
             | y <- [ 0 .. V.length vvt - 1 ]
             , x <- [ 0 .. V.length (vvt ! y) + 0 ] -- we append a fake ";" token at the end of each line to represent EOL
             , let startPos = SourcePos "" (mkPos $ y + 1) (mkPos $ x + 1)
                   endPos   = SourcePos "" (mkPos $ y + 1) (mkPos $ x + 1) -- same
                   rawToken = if x == V.length (vvt ! y) then ";" else vvt ! y ! x
                   tokenLength = 1
                  --  tokenLength = fromIntegral $ Text.length rawToken + 1 & \r -> Debug.trace (show r) r
                  --  tokenLength = fromIntegral $ Text.length rawToken + 1 & Debug.trace <$> show <*> id  -- same as above line, but with reader applicative
                  --  tokenLength = fromIntegral $ Text.length rawToken + 1  -- without debugging
                   tokenVal = toToken rawToken
             , tokenVal `notElem` [ Empty, Checkbox ]
             ]

-- deriving (Eq, Ord, Show)

--
-- MyStream is the primary input for our Parsers below.
--

-- the goal is tof return a list of Rule, which an be either regulative or constitutive:
pRule :: Parser [Rule]
pRule = withDepth 1 $ do
  _ <- optional dnl
  try ((:[]) <$> pRegRule <?> "regulative rule")
    <|> ((:[]) <$ pToken Define `indented0` pTypeDefinition   <?> "ontology definition")
    <|> try ((:[]) <$> pDeemRule <?> "deem rule")
    <|> try ((:[]) <$> pConstitutiveRule <?> "constitutive rule")
    <|> (eof >> return [])

pTypeSig :: Parser TypeSig
pTypeSig = debugName "pTypeSig" $ do
  _           <- pToken TypeSeparator <|> pToken Is
  simpletype <|> inlineenum
  where
    simpletype = do
      cardinality <- optional $ choice [ TOne      <$ pToken One
                                       , TOne      <$ pToken A_An
                                       , TOptional <$ pToken Optional
                                       , TList0    <$ pToken List0
                                       , TList1    <$ pToken List1 ]
      base        <- pOtherVal <* dnl
      return $ SimpleType (fromMaybe TOne cardinality) base
    inlineenum = do
      InlineEnum TOne <$> pOneOf

pOneOf :: Parser ParamText
pOneOf = id <$ pToken OneOf `indented0` pParamText

pTypeDefinition :: Parser Rule
pTypeDefinition = debugName "pTypeDefinition" $ do
  name  <- AA.Leaf . text2pt <$> pOtherVal
  myTraceM $ "got name = " <> show name
  super <- optional pTypeSig
  myTraceM $ "got super = " <> show super
  _     <- optional dnl
  has   <- optional (id <$ pToken Has `indented1` many ( (,)
                                                         <$> (pure <$> pKeyValues) -- basically a single-line ParamText
                                                         <*> optional pTypeSig
                                                         <* optional dnl))
  myTraceM $ "got has = " <> show has
  enums <- optional pOneOf
  myTraceM $ "got enums = " <> show enums

  return $ TypeDecl
    { name
    , super
    , has
    , enums
    , rlabel  = noLabel
    , lsource = noLSource
    , srcref  = noSrcRef
    }

pDeemRule :: Parser Rule
pDeemRule = debugName "pDeemRule" $ do
  leftY  <- lookAhead pYLocation
  leftX  <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.
  srcurl <- asks sourceURL
  let srcref = SrcRef srcurl srcurl leftX leftY Nothing

  ((_d,d),gs,w,i,u,means,is,includes) <- permute $ (,,,,,,,)
    <$$> preambleParamText [Deem]
    <|?> ([], some $ preambleParamText [Given])
    <|?> ([], some $ preambleBoolRules [When])
    <|?> ([], some $ preambleBoolRules [If])
    <|?> ([], some $ preambleBoolRules [Unless])
    <|?> ([], some $ preambleBoolRules [Means])
    <|?> ([], some $ preambleBoolRules [Is])
    <|?> ([], some $ preambleBoolRules [Includes])

  -- let's extract the new term from the deem line
  let givens = concatMap (concatMap toList . toList . snd) gs :: [Text.Text]
      dnew   = [ word | word <- concatMap toList $ toList d, word `notElem` givens ]
  if length dnew /= 1
    then error "DEEM should identify exactly one term which was not previously found in the GIVEN line"
    else return $ Constitutive
         { name = AA.Leaf . text2pt $ head dnew -- we lose the ordering
         , keyword = Given
         , letbind = AA.Leaf d
         , orig = [(Deem, AA.Leaf d)] ++ means ++ is ++ includes
         , cond = addneg
                  (snd <$> mergePBRS (w++i))
                  (snd <$> mergePBRS u)
         , given = nonEmpty $ foldMap toList (snd <$> gs)
         , rlabel = noLabel
         , lsource = noLSource
         , srcref = Just srcref
         }

pConstitutiveRule :: Parser Rule
pConstitutiveRule = debugName "pConstitutiveRule" $ do
  leftY              <- lookAhead pYLocation
  (name,namealias)   <- pNameParens
  leftX              <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.
  srcurl <- asks sourceURL
  let srcref = SrcRef srcurl srcurl leftX leftY Nothing
  let defalias = maybe mempty (\t -> singeltonDL (DefNameAlias t name Nothing (Just srcref))) (AA.Leaf . text2pt <$> namealias)
  tell defalias -- use Writer to append a mini rule that associates the alias with the name

  ( (copula, mletbind), whenifs, unlesses, givens ) <-
    withDepth leftX $ permutationsCon [Means,Is,Includes] [When,If] [Unless] [Given]

  return $ Constitutive
    { name = name
    , keyword = copula
    , letbind = mletbind
    , cond = addneg
             (snd <$> mergePBRS whenifs)
             (snd <$> mergePBRS unlesses)
    , given = nonEmpty $ foldMap toList (snd <$> givens)
    , rlabel = noLabel
    , lsource = noLSource
    , srcref = Just srcref
    , orig = []
    }

pRegRule :: Parser Rule
pRegRule = debugName "pRegRule" $
  (try pRegRuleSugary
    <|> try pRegRuleNormal
    <|> (pToken Fulfilled >> return RegFulfilled)
    <|> (pToken Breach    >> return RegBreach)
  ) <* optional dnl

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
  entitytype         <- pOtherVal
  leftX              <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.

  rulebody           <- withDepth leftX (permutationsReg [Every,Party,TokAll] [When,If] [Unless] [Upon] [Given] [Having])
  -- TODO: refactor and converge the rest of this code block with Normal below
  henceLimb          <- optional $ pHenceLest Hence
  lestLimb           <- optional $ pHenceLest Lest
  let poscond = snd <$> mergePBRS (rbpbrs   rulebody)
  let negcond = snd <$> mergePBRS (rbpbrneg rulebody)
      toreturn = Regulative
                 (AA.Leaf $ text2pt entitytype)
                 Party
                 Nothing
                 (addneg poscond negcond)
                 (rbdeon rulebody)
                 (rbaction rulebody)
                 (rbtemporal rulebody)
                 henceLimb
                 lestLimb
                 Nothing -- rule label
                 Nothing -- legal source
                 Nothing -- internal SrcRef
                 (snd <$> rbupon  rulebody)
                 (nonEmpty $ foldMap toList (snd <$> rbgiven rulebody))    -- given
                 (rbhaving rulebody)
                 []
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
  leftX              <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.
  (keyword, name)   <- try (pActor [Party, Every, TokAll])
  -- (Who, (BoolStruct,[Rule]))
  whoBool                     <- optional (withDepth leftX (preambleBoolRules [Who]))
  -- the below are going to be permutables
  myTraceM $ "pRegRuleNormal: preambleBoolRules returned " ++ show whoBool
  rulebody <- permutationsReg [Every,Party,TokAll] [When, If] [Unless] [Upon] [Given] [Having]
  henceLimb                   <- optional $ pHenceLest Hence
  lestLimb                    <- optional $ pHenceLest Lest
  myTraceM $ "pRegRuleNormal: permutations returned rulebody " ++ show rulebody

  -- qualifying conditions generally; we merge all positive groups (When, If) and negative groups (Unless)
  let poscond = snd <$> mergePBRS (rbpbrs   rulebody)
  let negcond = snd <$> mergePBRS (rbpbrneg rulebody)

  let toreturn = Regulative
                 name
                 keyword
                 (snd <$> whoBool)
                 (addneg poscond negcond)
                 (rbdeon rulebody)
                 (rbaction rulebody)
                 (rbtemporal rulebody)
                 henceLimb
                 lestLimb
                 Nothing -- rule label
                 Nothing -- legal source
                 Nothing -- internal SrcRef
                 (snd <$> rbupon  rulebody)    -- given
                 (nonEmpty $ foldMap toList (snd <$> rbgiven rulebody))    -- given
                 (rbhaving rulebody)
                 []
  myTraceM $ "pRegRuleNormal: the positive preamble is " ++ show poscond
  myTraceM $ "pRegRuleNormal: the negative preamble is " ++ show negcond
  myTraceM $ "pRegRuleNormal: returning " ++ show toreturn
  -- let appendix = pbrs ++ nbrs ++ ebrs ++ defalias
  -- myTraceM $ "pRegRuleNormal: with appendix = " ++ show appendix
  -- return ( toreturn : appendix )
  return toreturn

-- this is probably going to need cleanup
addneg :: Maybe BoolStructP -> Maybe BoolStructP -> Maybe BoolStructP
addneg Nothing  Nothing   = Nothing
addneg Nothing  (Just n)  = Just $ AA.Not n
addneg (Just p) (Just n)  = Just $ AA.All [p, AA.Not n]
addneg (Just p) Nothing   = Just p

pHenceLest :: MyToken -> Parser Rule
pHenceLest henceLest = debugName ("pHenceLest-" ++ show henceLest) $ do
  id <$ pToken henceLest `indented1` (try pRegRule <|> RuleAlias <$> pOtherVal)


-- combine all the boolrules under the first preamble keyword
mergePBRS :: [(Preamble, BoolRulesP)] -> Maybe (Preamble, BoolRulesP)
mergePBRS [] = Nothing
mergePBRS (  (w, br) : [])  = Just (w, br)
mergePBRS xs                = Just (fst . head $ xs, foldl1 (<>) (snd <$> xs))

pTemporal :: Parser (Maybe (TemporalConstraint Text.Text))
pTemporal = eventually <|> specifically <|> vaguely
  where
    eventually   = mkTC <$> pToken Eventually <*> pure ""
    specifically = mkTC <$> sometime          <*> pOtherVal
    sometime     = choice $ map pToken [ Before, After, By, On ]
    vaguely      = Just . TVague <$> pOtherVal

pPreamble :: [MyToken] -> Parser Preamble
pPreamble toks = choice (try . pToken <$> toks)

-- "PARTY Bob       AKA "Seller"
-- "EVERY Seller"
pActor :: [MyToken] -> Parser (Preamble, BoolStructP)
pActor keywords = debugName ("pActor " ++ show keywords) $ do
  leftY       <- lookAhead pYLocation
  leftX       <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.
  -- add pConstitutiveRule here -- we could have "MEANS"
  preamble    <- pPreamble keywords
  (entitytype, entityalias)   <- lookAhead pNameParens
  omgARule <- pure <$> try pConstitutiveRule <|> (mempty <$ pNameParens)
  myTraceM $ "pActor: omgARule = " ++ show omgARule
  srcurl <- asks sourceURL
  let srcref = SrcRef srcurl srcurl leftX leftY Nothing
  let defalias = maybe mempty (\t -> singeltonDL (DefNameAlias t entitytype Nothing (Just srcref))) (AA.Leaf . text2pt <$> entityalias)
  tell $ defalias <> listToDL omgARule
  return (preamble, entitytype)

-- three tokens of the form | some thing | Aka | A Thing |
-- Aka means "also known as"
pNameParens :: Parser (BoolStructP, Maybe ConstitutiveName)
pNameParens = debugName "pNameParens" $ do
  entitytype  <- dBoolRules
  entityalias <- optional (pToken Aka *> pOtherVal)
  _ <- dnl
  return (entitytype, entityalias)

pDoAction ::  Parser BoolStructP
pDoAction = pToken Do >> pAction

-- everything in p2 must be at least the same depth as p1
indented :: Int -> Parser (a -> b) -> Parser a -> Parser b
indented d p1 p2 = do
  leftX <- lookAhead pXLocation
  f     <- p1
  y     <- withDepth (leftX + d) p2
  return $ f y

indented0 :: Parser (a -> b) -> Parser a -> Parser b
indented0 = indented 0
infixl 4 `indented0`

indented1 :: Parser (a -> b) -> Parser a -> Parser b
indented1 = indented 1
infixl 4 `indented1`

pAction :: Parser BoolRulesP
pAction = dBoolRules

pParamText :: Parser ParamText
pParamText = debugName "pParamText" $ do
  (:|) <$> (pKeyValues <* dnl <?> "paramText head") `indented0` pParams

  -- === flex for
  --     (myhead, therest) <- (pKeyValues <* dnl) `indented0` pParams
  --     return $ myhead :| therest

type KVsPair = NonEmpty Text.Text -- so really there are multiple Values

pParams :: Parser [KVsPair]
pParams = many $ pKeyValues <* dnl    -- head (name+,)*

pKeyValues :: Parser KVsPair
pKeyValues = debugName "pKeyValues" $ (:|) <$> pOtherVal `indented1` many pOtherVal

-- we create a permutation parser returning one or more RuleBodies, which we treat as monoidal,
-- though later we may object if there is more than one.

mkRBfromDT :: BoolStructP
           -> (Preamble, BoolStructP ) -- every person
           -> [(Preamble, BoolRulesP)] -- positive  -- IF / WHEN
           -> [(Preamble, BoolRulesP)] -- negative  -- UNLESS
           -> [(Preamble, BoolRulesP)] -- upon  conditions
           -> [(Preamble, ParamText )] -- given conditions
           -> Maybe ParamText               -- having
           -> (Deontic, Maybe (TemporalConstraint Text.Text))
           -> RuleBody
mkRBfromDT rba rbkn rbpb rbpbneg rbu rbg rbh (rbd,rbt) = RuleBody rba rbpb rbpbneg rbd rbt rbu rbg rbh rbkn

mkRBfromDA :: (Deontic, BoolStructP)
           -> (Preamble, BoolStructP ) -- every person or thing
           -> [(Preamble, BoolRulesP)]
           -> [(Preamble, BoolRulesP)]
           -> [(Preamble, BoolRulesP)] -- upon  conditions
           -> [(Preamble, ParamText )] -- given conditions
           -> Maybe ParamText         -- having
           -> Maybe (TemporalConstraint Text.Text)
           -> RuleBody
mkRBfromDA (rbd,rba) rbkn rbpb rbpbneg rbu rbg rbh rbt = RuleBody rba rbpb rbpbneg rbd rbt rbu rbg rbh rbkn

permutationsCon :: [MyToken] -> [MyToken] -> [MyToken] -> [MyToken]
                -> Parser ( ( Preamble               -- preamble = copula   (means,deem)
                            , BoolRulesP)            -- the rhs of the let binding is always a BoolStructP -- because the leaf of a BoolStructP can be a ParamText!
                          , [(Preamble, BoolRulesP)] -- positive conditions (when,if)
                          , [(Preamble, BoolRulesP)] -- negative conditions (unless)
                          , [(Preamble, ParamText)] -- given    (given params)
                          )
permutationsCon copula ifwhen l4unless l4given =
  debugName ("permutationsCon"
             <> ": copula="   <> show copula
             <> ", positive=" <> show ifwhen
             <> ", negative=" <> show l4unless
             <> ", given="    <> show l4given
            ) $ do
  permute $ (,,,)
    <$$> preambleBoolRules copula
    <|?> ([], some $ preambleBoolRules ifwhen)
    <|?> ([], some $ preambleBoolRules l4unless)
    <|?> ([], some $ preambleParamText l4given)

-- degustates
--     MEANS eats
--        OR drinks
--      WHEN weekend

preambleParamText :: [MyToken] -> Parser (Preamble, ParamText)
preambleParamText preambles = do
  preamble <- choice (try . pToken <$> preambles)
  paramtext <- pParamText
  return (preamble, paramtext)

permutationsReg :: [MyToken] -> [MyToken] -> [MyToken] -> [MyToken] -> [MyToken] -> [MyToken] -> Parser RuleBody
permutationsReg l4every ifwhen l4unless l4upon l4given l4having =
  debugName ("permutationsReg"
             <> ": keyword="  <> show l4every
             <> ": positive=" <> show ifwhen
             <> ", negative=" <> show l4unless
             <> ", upon="     <> show l4upon
             <> ", given="    <> show l4given
             <> ", having="   <> show l4having
            ) $ do
  try ( debugName "regulative permutation with deontic-temporal" $ permute ( mkRBfromDT
            <$$> pDoAction
            <||> pActor l4every
            <|?> ([], some $ preambleBoolRules ifwhen)   -- syntactic constraint, all the if/when need to be contiguous.
            <|?> ([], some $ preambleBoolRules l4unless) -- unless
            <|?> ([], some $ preambleBoolRules l4upon)   -- upon
            <|?> ([], some $ preambleParamText l4given)  -- given
            <|?> (Nothing, Just . snd <$> preambleParamText l4having)  -- having
            <||> try pDT
          ) )
  <|>
  try ( debugName "regulative permutation with deontic-action" $ permute ( mkRBfromDA
            <$$> try pDA
            <||> pActor l4every
            <|?> ([], some $ preambleBoolRules ifwhen) -- syntactic constraint, all the if/when need to be contiguous.
            <|?> ([], some $ preambleBoolRules l4unless) -- syntactic constraint, all the if/when need to be contiguous.
            <|?> ([], some $ preambleBoolRules l4upon)   -- upon
            <|?> ([], some $ preambleParamText l4given)  -- given
            <|?> (Nothing, pure . snd <$> preambleParamText l4having)  -- having
            <|?> (Nothing, pTemporal <* dnl)
          ) )


-- the Deontic/temporal/action form
-- MAY EVENTUALLY
--  -> pay
pDT :: Parser (Deontic, Maybe (TemporalConstraint Text.Text))
pDT = debugName "pDT" $ do
  pd <- pDeontic
  pt <- optional pTemporal <* dnl
  return (pd, fromMaybe Nothing pt)

-- the Deontic/Action/Temporal form
pDA :: Parser (Deontic, BoolStructP)
pDA = debugName "pDA" $ do
  pd <- pDeontic
  pa <- pAction
  return (pd, pa)

preambleBoolRules :: [MyToken] -> Parser (Preamble, BoolRulesP)
preambleBoolRules wanted = debugName ("preambleBoolRules " <> show wanted)  $ do
  leftX     <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.
  condWord <- choice (try . pToken <$> wanted)
  myTraceM ("preambleBoolRules: found: " ++ show condWord)
  ands <- withDepth leftX dBoolRules -- (foo AND (bar OR baz), [constitutive and regulative sub-rules])
  return (condWord, ands)

dBoolRules ::  Parser BoolRulesP
dBoolRules = debugName "dBoolRules" $ do
  pAndGroup -- walks AND eats OR drinks

pAndGroup ::  Parser BoolRulesP
pAndGroup = debugName "pAndGroup" $ do
  orGroup1 <- pOrGroup
  orGroupN <- many $ pToken And *> pOrGroup
  let toreturn = if null orGroupN
                 then orGroup1
                 else AA.All (orGroup1 : orGroupN)
  return toreturn

pOrGroup ::  Parser BoolRulesP
pOrGroup = debugName "pOrGroup" $ do
  depth <- asks callDepth
  elem1    <- withDepth (depth + 1) pElement
  elems    <- many $ pToken Or *> withDepth (depth+1) pElement
  let toreturn = if null elems
                 then elem1
                 else AA.Any  (elem1 : elems)
  return toreturn

pElement ::  Parser BoolRulesP
pElement = debugName "pElement" $ do
  -- think about importing Control.Applicative.Combinators so we get the `try` for free
  try pNestedBool
    <|> pNotElement
    <|> try (constitutiveAsElement <$> tellIdFirst pConstitutiveRule)
    <|> pLeafVal

-- | Like `\m -> do a <- m; tell [a]; return a` but add the value before the child elements instead of after
tellIdFirst :: (Functor m) => WriterT (DList w) m w -> WriterT (DList w) m w
tellIdFirst = mapWriterT . fmap $ \(a, m) -> (a, singeltonDL a <> m)

-- Makes a leaf with just the name of a constitutive rule
constitutiveAsElement ::  Rule -> BoolRulesP
constitutiveAsElement cr = name cr
-- constitutiveAsElement _ = error "constitutiveAsElement: cannot convert an empty list of rules to a BoolRules structure!"

pNotElement :: Parser BoolRulesP
pNotElement = debugName "pNotElement" $ do
  inner <- pToken MPNot *> pElement
  return $ AA.Not inner

pLeafVal ::  Parser BoolRulesP
pLeafVal = debugName "pLeafVal" $ do
  leafVal <- pParamText
  myTraceM $ "pLeafVal returning " ++ show leafVal
  return $ AA.Leaf leafVal

-- should be possible to merge pLeafVal with pNestedBool.

pNestedBool ::  Parser BoolRulesP
pNestedBool = debugName "pNestedBool" $ do
  -- "foo AND bar" is a nestedBool; but just "foo" is a leafval.
  foundBool <- lookAhead (pLeafVal >> pBoolConnector)
  myTraceM $ "pNestedBool matched " ++ show foundBool
  dBoolRules

pBoolConnector :: Parser MyToken
pBoolConnector = debugName "pBoolConnector" $ do
  pToken And <|> pToken Or <|> pToken Unless

-- helper functions for parsing

anything :: Parser [WithPos MyToken]
anything = many anySingle

-- "discard newline", a reference to GNU Make
dnl :: Parser [MyToken]
dnl = some $ pToken EOL

pDeontic :: Parser Deontic
pDeontic = (pToken Must  >> return DMust)
           <|> (pToken May   >> return DMay)
           <|> (pToken Shant >> return DShant)

-- return the text inside an Other value. This implicitly serves to test for Other, similar to a pToken test.
pOtherVal :: Parser Text.Text
pOtherVal = token test Set.empty <?> "Other text"
  where
    test (WithPos _ _ _ (TypeSeparator)) = Just "::" -- TODO FIXME -- this was here to allow GIVEN ParamText to contain a type signature
    test (WithPos _ _ _ (Other t)) = Just t
    test _ = Nothing

getToken :: Parser MyToken
getToken = token test Set.empty <?> "any token"
  where
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

