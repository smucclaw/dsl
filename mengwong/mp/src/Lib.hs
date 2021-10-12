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
import qualified Data.Csv as Cassava
import qualified Data.Vector as V
import Data.Void (Void)
import Data.Vector ((!), (!?))
import Data.Maybe (listToMaybe, fromMaybe)
import Text.Pretty.Simple (pPrint)
import Control.Monad (guard, when)
import Control.Monad.State
import qualified AnyAll as AA
import qualified Text.PrettyPrint.Boxes as Box
import           Text.PrettyPrint.Boxes hiding ((<>))
import System.Environment (lookupEnv)
import qualified Data.ByteString.Lazy as BS
import qualified Data.List.Split as DLS

import Debug.Trace

-- import qualified Debug.Trace as Debug
-- import Data.Function

import Types
import Error

-- our task: to parse an input CSV into a collection of Rules.
-- example "real-world" input can be found at https://docs.google.com/spreadsheets/d/1qMGwFhgPYLm-bmoN2es2orGkTaTN382pG2z3RjZ_s-4/edit

someFunc :: IO ()
someFunc = do
  mpd <- lookupEnv "MP_DEBUG"
  let runConfig = RC (maybe False (read :: String -> Bool) mpd) 0
  myinput <- BS.getContents
  runExample runConfig myinput

-- TODO: integrate debugging and callstack depth with the runConfig, thread it through all the functions here
-- printf debugging infrastructure
debuggery = False
myTraceM x = when debuggery (traceM x)
debugPrint depth str = when debuggery $ do
  lookingAt <- lookAhead (getToken :: Parser MyToken)
  myTraceM $ indent <> str <> " running. depth=" <> show depth <> "; looking at: " <> show lookingAt
  where
    indent = replicate depth ' '

runExample :: RunConfig -> ByteString -> IO ()
runExample rc str = case runParser (pRule <* eof) "dummy" (exampleStream str) of
  Left bundle -> putStr (errorBundlePrettyCustom bundle)
  -- Left bundle -> putStr (errorBundlePretty bundle)
  -- Left bundle -> pPrint bundle
  Right xs -> pPrint xs

exampleStream :: ByteString -> MyStream
exampleStream s = case getStanzas (asCSV s) of
                    Left errstr -> error errstr
                    Right rawsts -> stanzaAsStream s (head rawsts)

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
asBoxes rs =
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
    trimComment True  (x:xs)                         = V.cons "" $ trimComment True xs
    trimComment False (x:xs) | Text.take 2 (Text.dropWhile (== ' ') x)
                               `elem` Text.words "// -- ##"
                                                     = trimComment True (x:xs)
    trimComment False (x:xs)                         = V.cons x $ trimComment False xs

getStanzas esa = do
  rs <- esa
  let chunks = getChunks $ Location rs (0,0) ((0,0),(V.length rs - 1,V.length (rs ! (V.length rs -1)) - 1))
  return $ extractRange <$> glueChunks chunks

-- because sometimes a chunk followed by another chunk is really part of the same chunk
glueChunks :: [Location] -> [Location]
glueChunks xs = xs -- LOLOL

-- highlight each chunk using range attribute.
-- method: cheat and use Data.List.Split's splitWhen
getChunks :: Location -> [Location]
getChunks loc@(Location rs (cx,cy) ((lx,ly),(rx,ry))) =
  let listChunks = DLS.splitWhen (\i -> V.all Text.null $ rs ! i) [ 0 .. ry ]
      vvChunks = fmap (rs !) <$> listChunks
      wantedChunks = V.filter (\chunkrows -> V.any (\w -> w `V.elem` Text.words "EVERY") chunkrows) vvChunks
  in setRange loc <$> wantedChunks

-- is the cursor on a line that has nothing in it?
blankLine :: Location -> Bool
blankLine loc = all Text.null $ currentLine loc

extractRange :: Location -> RawStanza
extractRange (Location rawStanza cursor ((lx,ly),(rx,ry))) =
  V.slice lx (rx-lx) $ V.slice ly (ry-ly) rawStanza

setRange :: Location -> [Int] -> Location
setRange loc@(Location rawStanza c ((lx,ly),(rx,ry))) ys =
  let cursorToEndLine  = moveTo loc (1,       last ys)
      lineLen = lineLength cursorToEndLine - 1
      cursorToEndRange = moveTo loc (lineLen, last ys)
  in loc { cursor =  (1,head ys)
         , range  = ((1,head ys),cursor cursorToEndRange) }

data Location = Location
                { rawStanza :: RawStanza
                , cursor    :: (Int,Int)
                , range     :: ((Int,Int),(Int,Int))
                }
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
currentLine loc = getCurrentCell <$> (toEOL $ lineStart loc)

lineStart :: Location -> Location
lineStart loc = loc { cursor = (0, curY loc) }
  
lineRemaining :: Location -> Distance
lineRemaining loc = lineLength loc - curX loc - 1

lineLength :: Location -> Distance
lineLength loc = let (cx, cy) = cursor loc in V.length (rawStanza loc ! cy)

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
stanzaAsStream s rs = do
  let vvt = rs
  -- MyStream (Text.unpack $ decodeUtf8 s) [ WithPos {..}
  MyStream rs [ WithPos {..}
             | y <- [ 0 .. V.length vvt - 1 ]
             , x <- [ 0 .. V.length (vvt ! y) - 1 ]
             , let startPos = SourcePos "" (mkPos $ y + 1) (mkPos $ x + 1)
                   endPos   = SourcePos "" (mkPos $ y + 1) (mkPos $ x + 1) -- same
                   rawToken = vvt ! y ! x
                   tokenLength = 1
                  --  tokenLength = fromIntegral $ Text.length rawToken + 1 & \r -> Debug.trace (show r) r
                  --  tokenLength = fromIntegral $ Text.length rawToken + 1 & Debug.trace <$> show <*> id  -- same as above line, but with reader applicative
                  --  tokenLength = fromIntegral $ Text.length rawToken + 1  -- without debugging
                   tokenVal = toToken rawToken
             ]

-- deriving (Eq, Ord, Show)

--
-- MyStream is the primary input for our Parsers below.
--

-- the goal is tof return a list of Rule, which an be either regulative or constitutive:
pRule :: Parser [Rule]
pRule = do
  dnl
  try (pRegRule 1 <?> "regulative rule")
    <|> (pConstitutiveRule 1 <?> "constitutive rule")

pConstitutiveRule :: Depth -> Parser [Rule]
pConstitutiveRule depth = do
  term               <- (pOtherVal <* dnl) <?> "defined term"
  leftX              <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.
  guard $ leftX >= depth
  defWord            <- (pToken Means <|> pToken Is)  <* dnl
  myTraceM $ "pConstitutiveRule: \"" ++ Text.unpack term ++ "\" " ++ show defWord ++ "..."
  (ands,rs) <- dBoolRules leftX -- (foo AND (bar OR baz), [constitutive and regulative sub-rules])
  let toreturn = Constitutive term ands : rs
  myTraceM $ "pConstitutiveRule: returning " ++ show toreturn
  return toreturn

pRegRule :: Depth -> Parser [Rule]
pRegRule depth = do
  leftX              <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.
  guard $ leftX >= depth
  entitytype         <- pToken Every *> many pEmpty *> pOtherVal <* dnl
  pbr                <- optional (preambleBoolRules leftX)                      <* dnl
  deontic            <- pDeontic                                                <* dnl
  temporal           <- listToMaybe <$> many pOtherVal                          <* dnl
  action             <- pToken Do *> many pEmpty *> (Text.unwords <$> many pOtherVal)          <* dnl
  let (who, (ands, brs)) = fromMaybe (Always, (AA.Any (AA.Pre "always") [], [])) pbr -- if there is no WHO line
      toreturn = Regulative entitytype (newPre (Text.pack $ show who) ands) deontic action temporal
  myTraceM $ "pRegRule: the specifier is " ++ show who
  myTraceM $ "pRegRule: returning " ++ show toreturn
  return ( toreturn : brs )
  where
    newPre :: Text.Text -> AA.Item Text.Text -> AA.Item Text.Text
    newPre t (AA.Leaf x) = AA.Leaf x
    newPre t (AA.All (AA.Pre     p)    x) = AA.All (AA.Pre     t   ) x
    newPre t (AA.All (AA.PrePost p pp) x) = AA.All (AA.PrePost t pp) x
    newPre t (AA.Any (AA.Pre     p)    x) = AA.Any (AA.Pre     t   ) x
    newPre t (AA.Any (AA.PrePost p pp) x) = AA.Any (AA.PrePost t pp) x
    
preambleBoolRules :: Depth -> Parser (Preamble, BoolRules)
preambleBoolRules depth = do
  leftX     <- lookAhead pXLocation -- this is the column where we expect IF/AND/OR etc.
  myTraceM ("preambleBoolRules: x location is " ++ show leftX)
  guard $ leftX >= depth
  myTraceM ("preambleBoolRules: passed guard! depth is " ++ show depth)
  condWord <- (pToken Who <|> pToken When <|> pToken If)   <* dnl
  myTraceM ("preambleBoolRules: found condWord: " ++ show condWord)
  (ands,rs) <- dBoolRules leftX -- (foo AND (bar OR baz), [constitutive and regulative sub-rules])
--   let bs = if subForest ands) == 1 -- upgrade the single OR child of the AND group to the top level
--            then newPre (Text.pack $ show condWord) (head ands)
--            else AA.All (AA.Pre (Text.pack $ show condWord)) ands -- return the AND group
  
  let toreturn = (condWord, (ands, rs))
  myTraceM $ "preambleBoolRules: returning " ++ show toreturn
  return toreturn

dBoolRules :: Depth -> Parser BoolRules
dBoolRules depth = do
  debugPrint depth "dBoolRules"
  ands     <- pAndGroup depth -- walks AND eats OR drinks
  myTraceM $ "dBoolRules: returning ands: " ++ show ands
  return ands

pAndGroup :: Depth -> Parser BoolRules
pAndGroup depth = do
  debugPrint depth "pAndGroup"
  currentX <- lookAhead pXLocation -- we are positioned at the OtherVal
  orGroup1 <- pOrGroup depth                                                <* dnl
  orGroupN <- many $ dToken depth And *> many pEmpty *> pOrGroup depth      <* dnl
  let toreturn = if null orGroupN
                 then orGroup1
                 else ( AA.All (AA.Pre "all of:") (map fst (orGroup1 : orGroupN))
                      , concatMap snd (orGroup1 : orGroupN) )
  myTraceM $ "pAndGroup: returning " ++ show toreturn
  return toreturn

pOrGroup :: Depth -> Parser BoolRules
pOrGroup depth = do
  debugPrint depth "pOrGroup"
  elem1    <- pElement (depth + 1)                                      <* dnl
  elems    <- many $ dToken depth Or *> many pEmpty *> pElement (depth+1)    <* dnl
  let toreturn = if null elems
                 then elem1
                 else ( AA.Any (AA.Pre "any of:") (fst <$> (elem1 : elems))
                      , concatMap snd (elem1 : elems) )
  myTraceM $ "pOrGroup: returning " ++ show toreturn
  return toreturn

pElement :: Depth -> Parser BoolRules
pElement depth = do
  debugPrint depth "pElement"
  -- think about importing Control.Applicative.Combinators so we get the `try` for free
  try (pNestedBool depth)
    <|> try (constitutiveAsElement <$> pConstitutiveRule depth)
    <|> try (pLeafVal depth)

constitutiveAsElement :: [Rule] -> BoolRules
constitutiveAsElement (cr:rs) = (AA.Leaf (term cr), cr:rs)
constitutiveAsElement [] = error "constitutiveAsElement: cannot convert an empty list of rules to a BoolRules structure!"

pLeafVal :: Depth -> Parser BoolRules
pLeafVal depth = do
  debugPrint depth "pLeafVal"
  currentX <- lookAhead pXLocation
  guard $ currentX >= depth
  leafVal <- pOtherVal <* dnl
  myTraceM $ "pLeafVal returning " ++ Text.unpack leafVal
  return (AA.Leaf leafVal, [])

-- should be possible to merge pLeafVal with pNestedBool.

pNestedBool :: Depth -> Parser BoolRules
pNestedBool depth = do
  debugPrint depth "pNestedBool"
  -- "foo AND bar" is a nestedBool; but just "foo" is a leafval.
  foundBool <- lookAhead (pLeafVal depth >> pBoolConnector depth)
  myTraceM $ "pNestedBool matched " ++ show foundBool
  toreturn <- dBoolRules depth
  myTraceM $ "pNestedBool returning " ++ show toreturn
  return toreturn

pBoolConnector depth = do
  debugPrint depth "pBoolConnector"
  currentX <- lookAhead pXLocation
  guard $ currentX >= depth
  andor <- (pToken And <|> pToken Or <|> pToken Unless) <* dnl
  myTraceM $ "pBoolConnector returning " ++ (show andor)
  return andor

-- helper functions for parsing

anything :: Parser [WithPos MyToken]
anything = many anySingle

pEmpty :: Parser MyToken
pEmpty = pToken Empty <|> pToken Checkbox

-- "discard newline", a reference to GNU Make
dnl :: Parser [MyToken]
dnl = many pEmpty

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

