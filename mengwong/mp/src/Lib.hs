{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE BangPatterns #-}

module Lib where

-- import qualified Data.Tree      as Tree
import qualified Data.Text.Lazy as Text
-- import Data.Text.Lazy.Encoding (decodeUtf8)
import Text.Megaparsec
import qualified Data.Set           as Set
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.Csv as Cassava
import qualified Data.Vector as V
import Generic.Data (Generic)
import Data.Vector ((!), (!?))
import Data.Maybe (fromMaybe, catMaybes)
import Text.Pretty.Simple (pPrint)
import Control.Monad (guard, when, forM_)
import qualified AnyAll as AA
import qualified Text.PrettyPrint.Boxes as Box
import           Text.PrettyPrint.Boxes hiding ((<>))
import System.Environment (lookupEnv)
import qualified Data.ByteString.Lazy as BS
import qualified Data.List.Split as DLS
import Text.Parser.Permutation
import Debug.Trace
import Data.Aeson.Encode.Pretty

import Types
import Error
import Control.Monad.Reader (ReaderT(runReaderT), asks, MonadReader (local))

-- our task: to parse an input CSV into a collection of Rules.
-- example "real-world" input can be found at https://docs.google.com/spreadsheets/d/1qMGwFhgPYLm-bmoN2es2orGkTaTN382pG2z3RjZ_s-4/edit

someFunc :: IO ()
someFunc = do
  mpd <- lookupEnv "MP_DEBUG"
  mpj <- lookupEnv "MP_JSON"
  let runConfig = RC
        { debug = (maybe False (read :: String -> Bool) mpd)
        , callDepth = 0
        , parseCallStack = []
        , sourceURL = "STDIN"
        , asJSON = (maybe False (read :: String -> Bool) mpj)
        }        
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
  traceM $ indent callDepth <> x
  where
    indent depth = concat $ replicate depth "| "

debugPrint :: String -> Parser ()
debugPrint str = whenDebug $ do
  lookingAt <- lookAhead (getToken :: Parser MyToken)
  depth <- asks callDepth
  myTraceM $ "/ " <> str <> " running. depth=" <> show depth <> "; looking at: " <> show lookingAt

debugName :: Show a => String -> Parser a -> Parser a
debugName name p = do
  debugPrint name
  res <- local (increaseNestLevel name) p
  myTraceM $ "\\ " <> name <> " has returned " <> show res
  return res

-- | withDepth n p sets the depth to n for parser p
withDepth :: Depth -> Parser a -> Parser a
withDepth n = local (\st -> st {callDepth= n})

-- | check that the next token is at at least the current level of indentation
checkDepth :: Parser ()
checkDepth = do
  depth <- asks callDepth
  leftX <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.
  guard $ leftX >= depth


runExample :: RunConfig -> ByteString -> IO ()
runExample rc str = forM_ (exampleStreams str) $ \stream ->
    case runParser (runReaderT (pRule <* eof) rc) "dummy" stream of
      Left bundle -> putStr (errorBundlePrettyCustom bundle)
      -- Left bundle -> putStr (errorBundlePretty bundle)
      -- Left bundle -> pPrint bundle
      Right xs -> if (asJSON rc)
                  then putStrLn $ toString $ encodePretty xs
                  else pPrint xs

exampleStream :: ByteString -> MyStream
exampleStream s = case getStanzas (asCSV s) of
                    Left errstr -> error errstr
                    Right rawsts -> stanzaAsStream s (head rawsts)

exampleStreams :: ByteString -> [MyStream]
exampleStreams s = case getStanzas (asCSV s) of
                    Left errstr -> error errstr
                    Right rawsts -> stanzaAsStream s <$> rawsts

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
      return $ trimComment False . V.toList <$> vvt
    trimComment _       []                           = V.empty
    trimComment True  (_x:xs)                        = V.cons "" $ trimComment True xs
    trimComment False (x:xs) | Text.take 2 (Text.dropWhile (== ' ') x)
                               `elem` Text.words "// -- ##"
                                                     = trimComment True (x:xs) -- a bit baroque, why not just short-circuit here?
    trimComment False (x:xs)                         = V.cons x $ trimComment False xs

getStanzas :: Monad m => m RawStanza -> m [RawStanza]
getStanzas esa = do
  rs <- esa
  let chunks = getChunks $ Location rs (0,0) ((0,0),(V.length (rs ! (V.length rs - 1)) - 1, V.length rs - 1))
      toreturn = extractRange <$> glueChunks chunks
  -- traceM ("getStanzas: extracted range " ++ (Text.unpack $ pShow toreturn))
  return toreturn

-- because sometimes a chunk followed by another chunk is really part of the same chunk.
-- so we glue contiguous chunks together.
glueChunks :: [Location] -> [Location]
glueChunks (a:b:z) =
  let (( lxa,lya),(_rxa,rya)) = range a
      ((_lxb,lyb),( rxb,ryb)) = range b
  in
    if rya + 1 == lyb
    then glueChunks $ a { range = ((lxa, lya),(rxb,ryb)) } : z
    else a : glueChunks (b : z)
glueChunks x = x

-- highlight each chunk using range attribute.
-- method: cheat and use Data.List.Split's splitWhen to chunk on paragraphs separated by newlines
getChunks :: Location -> [Location]
getChunks loc@(Location rs (_cx,_cy) ((_lx,_ly),(_rx,ry))) =
  let listChunks = (DLS.split . DLS.keepDelimsR . DLS.whenElt) (\i -> V.all Text.null $ rs ! i) [ 0 .. ry ]
      wantedChunks = [ rows
                     | rows <- listChunks
                     , any (\row ->
                               V.any (\w -> w `elem` Text.words "EVERY PARTY MUST MAY WHEN INCLUDES MEANS IS IF UNLESS")
                               (rs ! row)
                           ) rows
                       ||
                       all (\row -> V.all Text.null (rs ! row))
                       rows
                     ]
      toreturn = setRange loc <$> (filter (not . null) wantedChunks)
  in -- trace ("getChunks: input = " ++ show [ 0 .. ry ])
     -- trace ("getChunks: listChunks = " ++ show listChunks)
     -- trace ("getChunks: wantedChunks = " ++ show wantedChunks)
     -- trace ("getChunks: returning " ++ show (length toreturn) ++ " stanzas: " ++ show toreturn)
     toreturn

-- is the cursor on a line that has nothing in it?
blankLine :: Location -> Bool
blankLine loc = all Text.null $ currentLine loc

extractRange :: Location -> RawStanza
extractRange (Location rawStanza _cursor ((lx,ly),(rx,ry))) =
  let slicey = -- trace ("extractRange: given rawStanza " ++ show rawStanza)
               -- trace ("extractRange: trying to slice " ++ show xy)
               -- trace ("extractRange: trying to slice y " ++ show (ly, ry-ly+1))
               V.slice ly (ry-ly+1) rawStanza
      slicex = -- trace ("extractRange: got slice y " ++ show slicey)
               -- trace ("extractRange: trying to slice x " ++ show (lx, rx-lx+1))
               V.slice lx (rx-lx+1) <$> slicey
  in -- trace ("extractRange: got slice x " ++ show slicex)
     slicex

setRange :: Location -> [Int] -> Location
setRange loc@(Location _rawStanza _c ((_lx,_ly),(_rx,_ry))) ys =
  let cursorToEndLine  = moveTo loc (0,       last ys)
      lineLen = lineLength cursorToEndLine - 1
      cursorToEndRange = moveTo loc (lineLen, last ys)
  in -- trace ("setRange: loc = " ++ show loc)
     -- trace ("setRange: ys = " ++ show ys)
     loc { cursor =  (0,head ys)
         , range  = ((0,head ys),cursor cursorToEndRange) }

data Location = Location
                { rawStanza :: RawStanza
                , cursor    :: (Int,Int)
                , range     :: ((Int,Int),(Int,Int))
                } deriving (Eq, Show)
data Direction = N | E | S | W deriving (Eq, Show)
type Distance = Int

matches :: Location -> Direction -> Distance -> (Text.Text -> Bool) -> [Text.Text]
matches loc dir dis f = getCurrentCell <$> searchIn loc dir dis f

searchIn :: Location -> Direction -> Distance -> (Text.Text -> Bool) -> [Location]
searchIn loc dir dis f = filter (f . getCurrentCell) $ getRange loc dir dis

getRange :: Location -> Direction -> Distance -> [Location]
getRange loc dir dis = [ move loc dir n | n <- [ 1 .. dis ] ]

toEOL :: Location -> [Location]
toEOL loc = [ move loc E n | n <- [ 1 .. lineRemaining loc ] ]

currentLine :: Location -> [Text.Text]
currentLine loc = getCurrentCell <$> toEOL (lineStart loc)

lineStart :: Location -> Location
lineStart loc = loc { cursor = (0, curY loc) }

lineRemaining :: Location -> Distance
lineRemaining loc = lineLength loc - curX loc - 1

lineLength :: Location -> Distance
lineLength loc = let (_cx, cy) = cursor loc in V.length (rawStanza loc ! cy)

curX, curY :: Location -> Int
curX (Location _ (cx,_) _) = cx
curY (Location _ (_,cy) _) = cy

moveTo :: Location -> (Int,Int) -> Location
moveTo loc c = loc { cursor = c }

move :: Location -> Direction -> Distance -> Location
move loc dir dis = do
  let (cx, cy) = cursor loc
      newCursor = case dir of
                    E -> (cx + dis, cy + 000)
                    S -> (cx + 000, cy + dis)
                    W -> (cx - dis, cy + 000)
                    N -> (cx + 000, cy - dis)
  loc { cursor = newCursor }

getCurrentCell :: Location -> Text.Text
getCurrentCell loc = fromMaybe "" (vvlookup (rawStanza loc) (cursor loc))

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

stanzaAsStream :: ByteString -> RawStanza -> MyStream
stanzaAsStream _s rs = do
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
  try (pRegRule <?> "regulative rule")
    <|> (pConstitutiveRule <?> "constitutive rule")
    <|> (eof >> return [])

pConstitutiveRule :: Parser [Rule]
pConstitutiveRule = debugName "pConstitutiveRule" $ do
  leftY              <- lookAhead pYLocation
  checkDepth
  (term,termalias)   <- pTermParens
  leftX              <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.
  defWord            <- pToken Means <|> pToken Is <|> pToken Includes
  myTraceM $ "pConstitutiveRule: matched defWord " ++ show defWord
  myTraceM $ "pConstitutiveRule: \"" ++ Text.unpack term ++ "\" " ++ show defWord ++ "..."
  (ands,rs) <- withDepth leftX dBoolRules -- (foo AND (bar OR baz), [constitutive and regulative sub-rules])

  srcurl <- asks sourceURL
  let srcref = SrcRef srcurl srcurl leftX leftY Nothing
  let defalias = maybe [] (\t -> pure (DefTermAlias t term Nothing (Just srcref))) termalias

  return $ Constitutive term ands Nothing Nothing Nothing : rs ++ defalias

pRegRule :: Parser [Rule]
pRegRule = debugName "pRegRule" $ (try pRegRuleSugary <|> pRegRuleNormal) <* optional dnl

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

pRegRuleSugary :: Parser [Rule]
pRegRuleSugary = debugName "pRegRuleSugary" $ do
  entitytype         <- pOtherVal
  leftX              <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.

  rulebody           <- withDepth leftX (permutations [When,If])
  -- TODO: refactor and converge the rest of this code block with Normal below
  henceLimb          <- optional $ pHenceLest Hence
  lestLimb           <- optional $ pHenceLest Lest
  let (who, (ands, brs)) = mergePBRS (if null (rbpbrs rulebody) then [(Always, (Nothing, []))] else rbpbrs rulebody)
      toreturn = Regulative
                 entitytype
                 Nothing
                 ands
                 (rbdeon rulebody)
                 (rbaction rulebody)
                 (rbtemporal rulebody)
                 henceLimb
                 lestLimb
                 Nothing -- rule label
                 Nothing -- legal source
                 Nothing -- internal SrcRef
  myTraceM $ "pRegRuleSugary: the specifier is " ++ show who
  myTraceM $ "pRegRuleSugary: returning " ++ show toreturn
  myTraceM $ "pRegRuleSugary: with appendix brs = " ++ show brs
  return ( toreturn : brs )

-- EVERY   person
-- WHO     sings
--    AND  walks
-- MAY     eat a potato
-- BEFORE  midnight
-- IF      a potato is available
--    AND  the potato is not green

pRegRuleNormal :: Parser [Rule]
pRegRuleNormal = debugName "pRegRuleNormal" $ do
  leftX              <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.
  checkDepth
  (_party_every, entitytype, _entityalias, defalias)   <- try (pActor Party) <|> pActor Every
  -- (Who, (BoolStruct,[Rule]))
  whoBool                     <- optional (withDepth leftX (preambleBoolRules [Who]))
  -- the below are going to be permutables
  myTraceM $ "pRegRuleNormal: preambleBoolRules returned " ++ show whoBool
  rulebody <- permutations [When, If] [Unless]
  henceLimb                   <- optional $ pHenceLest Hence
  lestLimb                    <- optional $ pHenceLest Lest
  myTraceM $ "pRegRuleNormal: permutations returned rulebody " ++ show rulebody

  -- qualifying conditions generally; combine all WHEN/IF/UNLESS into a single IF structure, adding NOT along the way
  let (posPreamble, (pcbs, pbrs)) = mergePBRS (if null (rbpbrs   rulebody) then [(Always, (Nothing, []))] else rbpbrs rulebody)
  let (negPreamble, (ncbs, nbrs)) = mergePBRS (if null (rbpbrneg rulebody) then [(Always, (Nothing, []))] else rbpbrneg rulebody)

  -- qualifying conditions for the subject entity
  let (ewho, (ebs, ebrs)) = fromMaybe (Always, (Nothing, [])) whoBool

  let toreturn = Regulative
                 entitytype
                 (newPre (Text.pack $ show ewho) <$> ebs)
                 cbs
                 (rbdeon rulebody)
                 (rbaction rulebody)
                 (rbtemporal rulebody)
                 henceLimb
                 lestLimb
                 Nothing -- rule label
                 Nothing -- legal source
                 Nothing -- internal SrcRef
  myTraceM $ "pRegRuleNormal: the positive preamble is " ++ show posPreamble
  myTraceM $ "pRegRuleNormal: the negative preamble is " ++ show negPreamble
  myTraceM $ "pRegRuleNormal: returning " ++ show toreturn
  myTraceM $ "pRegRuleNormal: with appendix brs = " ++ show brs
  return ( toreturn : brs ++ ebrs ++ defalias )

pHenceLest :: MyToken -> Parser [Rule]
pHenceLest henceLest = debugName ("pHenceLest-" ++ show henceLest) $ do
  leftX              <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.
  checkDepth
  _ <- pToken henceLest
  withDepth (leftX + 1) pRegRule

mergePBRS :: [(Preamble, BoolRules)] -> (Preamble, BoolRules)
mergePBRS xs =
  let (w,(a,b)) = head xs
      pre_a = fst . snd <$> tail xs
      toreturn = (w,( a <> mconcat pre_a
                    ,      concat (b : (snd . snd <$> tail xs) )))
  in -- trace ("mergePBRS: called with " ++ show xs)
     -- trace ("mergePBRS: about to return " ++ show toreturn)
     toreturn

pTemporal :: Parser (Maybe (TemporalConstraint Text.Text))
pTemporal = ( do
                t0 <- pToken Eventually
                return (mkTC t0 "")
            ) <|> do
  t1 <- pToken Before <|> pToken After <|> pToken By
  t2 <- pOtherVal
  return $ mkTC t1 t2

-- "PARTY Bob       (the "Seller")
-- "EVERY Seller"
pActor :: MyToken -> Parser (MyToken, Text.Text, Maybe Text.Text, [Rule])
pActor party = debugName ("pActor " ++ show party) $ do
  leftY       <- lookAhead pYLocation
  leftX       <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.
  -- add pConstitutiveRule here -- we could have "MEANS"
  _           <- pToken party
  (entitytype, entityalias)   <- lookAhead pTermParens
  omgARule <- try pConstitutiveRule <|> ([] <$ pTermParens)
  myTraceM $ "pActor: omgARule = " ++ show omgARule
  srcurl <- asks sourceURL
  let srcref = SrcRef srcurl srcurl leftX leftY Nothing
  let defalias = maybe [] (\t -> pure (DefTermAlias t entitytype Nothing (Just srcref))) entityalias
  return (party, entitytype, entityalias, defalias ++ omgARule)

-- two tokens of the form | some thing | ("A Thing") | ; |
pTermParens :: Parser (Text.Text, Maybe Text.Text)
pTermParens = debugName "pTermParens" $ do
  entitytype  <- pOtherVal
  entityalias <- optional pOtherVal -- TODO: add test here to see if the pOtherVal has the form    ("xxx")
  _ <- dnl
  return (entitytype, entityalias)  

pDoAction ::  Parser ActionType
pDoAction = pToken Do >> pAction

pAction ::  Parser ActionType
pAction = do
  action <- pOtherVal       <* dnl
  params <- many (((,) <$> pOtherVal <*> many pOtherVal) <* dnl)    -- head (term+,)*
  return (action, params)

-- we create a permutation parser returning one or more RuleBodies, which we treat as monoidal,
-- though later we may object if there is more than one.

data RuleBody = RuleBody { rbaction   :: ActionType -- pay(to=Seller, amount=$100)
                         , rbpbrs     :: [(Preamble, BoolRules)] -- not subject to the party
                         , rbpbrneg   :: [(Preamble, BoolRules)] -- negative global conditions
                         , rbdeon     :: Deontic
                         , rbtemporal :: Maybe (TemporalConstraint Text.Text)
                         }
                      deriving (Eq, Show, Generic)

mkRBfromDT :: ActionType
           -> [(Preamble, BoolRules)] -- positive  -- IF / WHEN
           -> [(Preamble, BoolRules)] -- negative  -- UNLESS
           -> (Deontic, Maybe (TemporalConstraint Text.Text))
           -> RuleBody
mkRBfromDT rba rbpb rbpbneg (rbd,rbt) = RuleBody rba rbpb rbpbneg rbd rbt

mkRBfromDA :: (Deontic, ActionType)
           -> [(Preamble, BoolRules)]
           -> [(Preamble, BoolRules)]
           -> Maybe (TemporalConstraint Text.Text)
           -> RuleBody
mkRBfromDA (rbd,rba) rbpb rbpbneg rbt = RuleBody rba rbpb rbpbneg rbd rbt

permutations :: [MyToken] -> [MyToken] -> Parser RuleBody
permutations ifwhen unless = debugName ("permutations positive=" <> show ifwhen <> ", negative=" <> show unless) $ do
  try ( debugName "permutation with deontic-temporal" $ permute ( mkRBfromDT
            <$$> pDoAction
            <|?> ([], some $ preambleBoolRules ifwhen) -- syntactic constraint, all the if/when need to be contiguous.
            <|?> ([], some $ preambleBoolRules unless) -- syntactic constraint, all the if/when need to be contiguous.
            <||> try pDT
          ) )
  <|>
  try ( debugName "permutation with deontic-action" $ permute ( mkRBfromDA
            <$$> try pDA
            <|?> ([], some $ preambleBoolRules ifwhen) -- syntactic constraint, all the if/when need to be contiguous.
            <|?> ([], some $ preambleBoolRules unless) -- syntactic constraint, all the if/when need to be contiguous.
            <|?> (Nothing, pTemporal <* dnl)
          ) )
    

-- the Deontic/temporal/action form
-- MAY EVENTUALLY
--  -> pay
pDT :: Parser (Deontic, Maybe (TemporalConstraint Text.Text))
pDT = debugName "pDT" $ do
  pd <- pDeontic
  pt <- (optional pTemporal) <* dnl
  return (pd, fromMaybe Nothing pt)
  
-- the Deontic/Action/Temporal form
pDA :: Parser (Deontic, ActionType)
pDA = debugName "pDA" $ do
  pd <- pDeontic
  pa <- pAction
  return (pd, pa)
  
  

newPre :: Text.Text -> AA.Item Text.Text -> AA.Item Text.Text
newPre _ (AA.Leaf x) = AA.Leaf x
newPre _ (AA.Not  x) = AA.Not  x
newPre t (AA.All (AA.Pre     _p)    x) = AA.All (AA.Pre     t   ) x
newPre t (AA.All (AA.PrePost _p pp) x) = AA.All (AA.PrePost t pp) x
newPre t (AA.Any (AA.Pre     _p)    x) = AA.Any (AA.Pre     t   ) x
newPre t (AA.Any (AA.PrePost _p pp) x) = AA.Any (AA.PrePost t pp) x

preambleBoolRules :: [MyToken] -> Parser (Preamble, BoolRules)
preambleBoolRules whoifwhen = debugName "preambleBoolRules" $ do
  leftX     <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.
  myTraceM ("preambleBoolRules: x location is " ++ show leftX)
  checkDepth
  depth <- asks callDepth
  myTraceM ("preambleBoolRules: passed guard! depth is " ++ show depth)
  myTraceM $ "preambleBoolRules: Expecting one of: " ++ show whoifwhen
  debugPrint "preambleBoolRules"
  condWord <- choice (try . pToken <$> whoifwhen)
  myTraceM ("preambleBoolRules: found condWord: " ++ show condWord)
  (ands,rs) <- withDepth leftX dBoolRules -- (foo AND (bar OR baz), [constitutive and regulative sub-rules])
--   let bs = if subForest ands) == 1 -- upgrade the single OR child of the AND group to the top level
--            then newPre (Text.pack $ show condWord) (head ands)
--            else AA.All (AA.Pre (Text.pack $ show condWord)) ands -- return the AND group

  return (condWord, (ands, rs))

dBoolRules ::  Parser BoolRules
dBoolRules = debugName "dBoolRules" $ do
  pAndGroup -- walks AND eats OR drinks

pAndGroup ::  Parser BoolRules
pAndGroup = debugName "pAndGroup" $ do
  orGroup1 <- pOrGroup
  orGroupN <- many $ dToken And *> pOrGroup
  let toreturn = if null orGroupN
                 then orGroup1
                 else ( Just (AA.All (AA.Pre "all of:") (catMaybes $ fst <$> (orGroup1 : orGroupN)))
                      , concatMap snd (orGroup1 : orGroupN) )
  return toreturn

pOrGroup ::  Parser BoolRules
pOrGroup = debugName "pOrGroup" $ do
  depth <- asks callDepth
  elem1    <- withDepth (depth + 1) pElement
  elems    <- many $ dToken Or *> withDepth (depth+1) pElement
  let toreturn = if null elems
                 then elem1
                 else ( Just (AA.Any (AA.Pre "any of:") (catMaybes $ fst <$> (elem1 : elems)))
                      , concatMap snd (elem1 : elems) )
  return toreturn

pElement ::  Parser BoolRules
pElement = debugName "pElement" $ do
  -- think about importing Control.Applicative.Combinators so we get the `try` for free
  try pNestedBool
    <|> try (constitutiveAsElement <$> pConstitutiveRule)
    <|> pLeafVal

constitutiveAsElement :: [Rule] -> BoolRules
constitutiveAsElement (cr:rs) = (Just (AA.Leaf (term cr)), cr:rs)
constitutiveAsElement [] = error "constitutiveAsElement: cannot convert an empty list of rules to a BoolRules structure!"

pLeafVal ::  Parser BoolRules
pLeafVal = debugName "pLeafVal" $ do
  checkDepth
  leafVal <- pOtherVal <* dnl
  myTraceM $ "pLeafVal returning " ++ Text.unpack leafVal
  return (Just (AA.Leaf leafVal), [])

-- should be possible to merge pLeafVal with pNestedBool.

pNestedBool ::  Parser BoolRules
pNestedBool = debugName "pNestedBool" $ do
  -- "foo AND bar" is a nestedBool; but just "foo" is a leafval.
  foundBool <- lookAhead (pLeafVal >> pBoolConnector)
  myTraceM $ "pNestedBool matched " ++ show foundBool
  dBoolRules

pBoolConnector :: Parser MyToken
pBoolConnector = debugName "pBoolConnector" $ do
  checkDepth
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

