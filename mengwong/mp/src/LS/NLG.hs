{-# OPTIONS_GHC -Wno-name-shadowing #-}
module LS.NLG (
    nlg
    ) where

import LS.UDExt
import LS.Types ( Deontic(..),
      EntityType,
      TemporalConstraint (..),
      BoolStruct,
      BoolStructP,
      Rule(..),
      pt2text, text2pt, ParamText, ruleName, TComparison (..)
      -- bsp2text
      )
import PGF ( CId, Expr, linearize, mkApp, mkCId, startCat, parse, readType, showExpr )
import UDAnnotations ( UDEnv(..), getEnv )
import qualified Data.Text.Lazy as Text
import Data.Char (toLower)
import Data.Void (Void)
import Data.List.NonEmpty (toList)
import UD2GF (getExprs)
import AnyAll (Item(..))
import qualified AnyAll as AA
import Data.Maybe ( fromJust, fromMaybe )
import Data.List ( elemIndex, intercalate )
import Replace.Megaparsec ( sepCap )
import Text.Megaparsec
    ( (<|>), anySingle, match, parseMaybe, manyTill, Parsec )
import Text.Megaparsec.Char (char)
import Data.Either (rights)

-- typeprocess to run a python
import System.IO ()
import System.Process.Typed ( proc, readProcessStdout_ )
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Control.Monad.IO.Class
import Control.Monad (join)

myUDEnv :: IO UDEnv
myUDEnv = getEnv (path "UDApp") "Eng" "UDS"
  where path x = "grammars/" ++ x

unpacked :: L8.ByteString -> String
unpacked x = drop (fromMaybe (-1) $ elemIndex '[' conll) conll
    where conll = filter (not . (`elem` "\n")) $ L8.unpack x

patterns :: (Char, Char) -> Parsec Void String String
patterns (a,b) = do
  char a
  join <$> manyTill
          ((fst <$> match (patterns (a,b))) <|> (pure <$> anySingle))
          (char b)

grabStrings :: (Char, Char) -> String -> [String]
grabStrings (a,b) txt =
  rights $ fromJust $ parseMaybe (sepCap (patterns (a,b))) txt

getString :: String -> String
getString txt = intercalate "\n" [ intercalate "\t" $ grabStrings ('\'','\'') l | l <- grabStrings ('[',']') txt ]

getPy :: Control.Monad.IO.Class.MonadIO m => String -> m L8.ByteString
getPy x = readProcessStdout_ (proc "python3" ["src/L4/sentence.py", x])

parseCoNLLU :: UDEnv -> String -> [[Expr]]
parseCoNLLU = getExprs []

parseOut :: UDEnv -> Text.Text -> IO Expr
parseOut env txt = do
  let str = Text.unpack txt
  getConll <- getPy str
  let conll = getString $ unpacked getConll
  let exprs = parseCoNLLU env conll -- env -> str -> [[expr]]
      expr = case exprs of
        (x : _xs) : _xss -> x  -- TODO: add code that tries to parse with the words in lowercase, if at first it doesn't succeed
        _ -> mkApp (mkCId "dummy_N") [] -- dummy expr
  mapM_ print exprs
  putStrLn conll
  return expr

peel :: Expr -> Expr
peel subj = gf $ fromJust $ fromGUDS (fg subj)

fromGUDS :: GUDS -> Maybe GNP
fromGUDS x = case x of
  Groot_only (GrootN_ someNP) -> Just someNP
  _ -> Nothing
--  Groot_nsubj (rootV_ someVP) (nsubj_ someNP) -> GRelNP someNP (GRelVP someVP)


nlg :: Rule -> IO Text.Text
nlg rl = do
   env <- myUDEnv
   annotatedRule <- parseFields env rl
   -- TODO: here let's do some actual NLG
   gr <- readPGF "grammars/UDExt.pgf"
   let lang = head $ languages gr
       subjectRaw = subjA annotatedRule
       actionRaw = actionA annotatedRule
       finalTree = mkApp (mkCId "subjAction") [peel subjectRaw, actionRaw]
       linText = linearize gr lang finalTree
       linTree = showExpr [] finalTree
   return (Text.pack (linText ++ "\n" ++ linTree))


parseFields :: UDEnv -> Rule -> IO AnnotatedRule
parseFields env rl = case rl of
  Regulative {} -> do
    subjA'  <- parseBool env (subj rl)
    whoA'   <- mapM (parseBool env) (who rl)
    condA'   <- return Nothing --if
    let deonticA' = parseDeontic (deontic rl)    :: CId
    actionA' <- parseBool env (action rl)
    temporalA' <- mapM (parseTemporal env) (temporal rl)
    uponA' <- parseUpon env (upon rl)
    givenA' <- mapM (parseGiven env) (given rl)
    return RegulativeA {
      subjA = subjA',
      whoA = whoA',
      condA = condA',
      deonticA = deonticA',
      actionA = actionA',
      temporalA = temporalA',
      uponA = uponA',
      givenA = givenA'
    }
  Constitutive {} -> do
    givenA' <- mapM (parseGiven env) (given rl)
    nameA' <- parseName env (name rl)
    condA'   <- return Nothing -- when/if/unless
    return ConstitutiveA {
      givenA = givenA',
      nameA = nameA',
      condA = condA'
    }
  -- meansA <- parseMeans --    keyword  :: MyToken       -- Means
  -- includesA <-  -- keyword :: MyToken  Includes, Is, Deem
  -- deemsA <-  -- keyword :: MyToken  Deem
  -- letbindA <-
    --name     :: ConstitutiveName   -- the thing we are defining
    -- letbind  :: BoolStructP   -- might be just a bunch of words to be parsed downstream
    -- rlabel   :: Maybe Text.Text
    -- lsource  :: Maybe Text.Text
    -- srcref   :: Maybe SrcRef
    -- orig     :: [(Preamble, BoolStructP)]
  _ -> error "parseFields: rule type not supported yet"
  where
    parseGiven :: UDEnv -> ParamText -> IO Expr
    parseGiven env pt = parseOut env $ pt2text pt

    -- ConstitutiveName is Text.Text
    parseName :: UDEnv -> Text.Text -> IO Expr
    parseName env txt = parseOut env txt

    parseBool :: UDEnv -> BoolStructP -> IO Expr
    parseBool env bsp = parseOut env (bsp2text bsp)

    parseUpon :: UDEnv -> [BoolStructP] -> IO (Maybe Expr)
    parseUpon env (bs:_) = do
      parse <- parseOut env (bsp2text bs)
      return $ Just parse
    parseUpon _ [] = return Nothing

    parseDeontic :: Deontic -> CId
    parseDeontic d = case d of
        DMust  -> mkCId "must_Deontic"
        DMay   -> mkCId "may_Deontic"
        DShant -> mkCId "shant_Deontic"

    parseTemporal :: UDEnv -> TemporalConstraint Text.Text -> Expr
    parseTemporal env (TemporalConstraint cmp time unit) = parse' "Adv"  env (Text.unwords [Text.pack (tcompToStr cmp), Text.pack $ show time, unit])

    tcompToStr :: TComparison -> String
    tcompToStr TBefore = "before"
    tcompToStr TAfter = "after"
    tcompToStr TBy = "by"
    tcompToStr TOn = "on"
    tcompToStr TVague = ""
    -- parseTemporal env (TBefore event unit)  = parse' "Adv"  env (Text.unwords [Text.pack "before", Text.pack $ show event, unit])
    -- parseTemporal env (TAfter event unit)  = parse' "Adv"  env (Text.unwords [Text.pack "after", Text.pack $ show event, unit])
    -- parseTemporal env (TBy event unit)  = parse' "Adv"  env (Text.unwords [Text.pack "by", Text.pack $ show event, unit])
    -- parseTemporal env (TOn event unit)  = parse' "Adv"  env (Text.unwords [Text.pack "on", Text.pack $ show event, unit])

    parseUpon :: UDEnv -> BoolStructP -> Expr
    parseUpon env bs = parse' "Adv" env (Text.unwords [Text.pack "upon", bsp2text bs])

parseFields _env rl = error $ "Unsupported rule type " ++ show rl

-- BoolStruct is from Types, and Item is from AnyAll
-- TODO: for now only return the first thing
-- later: BoolStruct -> PGF.Expr -- mimic the structure in GF grammar
bs2text :: BoolStruct -> Text.Text
bs2text (Leaf txt) = txt
bs2text (All _) = Text.pack "walk"
bs2text (Any _) = Text.pack "walk"
bs2text (Not _) = Text.pack "walk"


bsp2text :: BoolStructP -> Text.Text
bsp2text (AA.Leaf pt) = pt2text pt
bsp2text (AA.Not  x)  = Text.pack "not " <> bsp2text x
bsp2text (AA.Any xs) =  Text.unwords (bsp2text <$> xs)
bsp2text (AA.All xs) =  Text.unwords (bsp2text <$> xs)
-- and possibily we want to have interspersed BoolStructs along the way

------------------------------------------------------------
-- Ignore everything below for now


data AnnotatedRule = RegulativeA
            { subjA     :: Expr                      -- man AND woman AND child
            , whoA      :: Maybe Expr                -- who walks and (eats or drinks) (RS)
            , condA     :: Maybe Expr                -- if it is a saturday (Adv)
            , deonticA  :: CId                       -- must (CId -- a hack, will change later)
            , actionA   :: Expr                      -- sing / pay the king $20 (VP)
            , temporalA :: Maybe Expr                -- before midnight (Adv)
            , uponA     :: Maybe Expr                -- UPON entering the club (event prereq trigger)
            , givenA    :: Maybe Expr                -- GIVEN an Entertainment flag was previously set in the history trace
            -- TODO later
            -- , henceA    :: Maybe [AnnotatedRule]     -- hence [UDS]
            -- , lestA     :: Maybe [AnnotatedRule]     -- lest [UDS]
            -- , rlabelA   :: Maybe Text.Text -- TODO what are these?
            -- , lsourceA  :: Maybe Text.Text
            -- , srcrefA   :: Maybe SrcRef
            }
            | ConstitutiveA
                { nameA       :: Expr   -- the thing we are defining
                -- , keyword  :: MyToken       -- Means, Includes, Is, Deem
                , letbindA    :: BoolStructP   -- might be just a bunch of words to be parsed downstream
                , condA       :: Maybe Expr -- a boolstruct set of conditions representing When/If/Unless
                , givenA      :: Maybe Expr
                -- , rlabel    :: Maybe Text.Text
                -- , lsource   :: Maybe Text.Text
                -- , srcref    :: Maybe SrcRef
                -- , orig      :: [(Preamble, BoolStructP)]
                }
          deriving (Eq, Show)
