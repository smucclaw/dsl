module LS.NLG (
    nlg
    ) where

import LS.Types
    ( Deontic(..),
      EntityType,
      TemporalConstraint (..),
      ParamText,
      BoolStruct(..),
      Rule(..), BoolStructP )
import PGF ( CId, Expr, linearize, mkApp, mkCId, showExpr )
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

parseOut :: UDEnv -> String -> IO Expr
parseOut env str = do
  getConll <- getPy str
  let conll = getString $ unpacked getConll
  let exprs = parseCoNLLU env conll -- env -> str -> [[expr]]
  mapM_ print exprs
  putStr conll
  return $ head $ head exprs

nlg :: Rule -> IO Text.Text
nlg rl = do
   env <- myUDEnv
   annotatedRule <- parseFields env rl
   let gr = pgfGrammar env
       lang = actLanguage env
       subjectRaw = everyA annotatedRule
       linText = linearize gr lang subjectRaw
       linTree = showExpr [] subjectRaw
   return (Text.pack (linText ++ "\n" ++ linTree))

parseFields :: UDEnv -> Rule -> IO AnnotatedRule
parseFields env rl@(Regulative {}) = do
    everyA'  <- parseEvery env (every rl)
    -- whoA'   <- fmap (parseWho env) (who rl)  :: Maybe Expr
    whoA' <- return Nothing
    condA'   <- return Nothing
    let deonticA' = parseDeontic (deontic rl)    :: CId
    actionA' <- parseAction env (action rl)
    -- temporalA' <- fmap (parseTemporal env) (temporal rl)  :: Maybe Expr
    temporalA' <- return Nothing
    uponA' <- return Nothing
    -- givenA' <- fmap (parseGiven env) (given rl) :: Expr
    givenA' <- return Nothing
    return RegulativeA {
      everyA = everyA',
      whoA = whoA',
      condA = condA',
      deonticA = deonticA',
      actionA = actionA',
      temporalA = temporalA',
      uponA = uponA',
      givenA = givenA'
    }
  where
    parseEvery :: UDEnv -> EntityType -> IO Expr
    parseEvery env text = parseOut env (map toLower $ Text.unpack text)

    parseWho :: UDEnv -> BoolStruct -> IO Expr
    parseWho env bs = parseOut env $ Text.unpack $ bs2text bs

    parseGiven :: UDEnv -> BoolStruct -> IO Expr
    parseGiven env bs = parseOut env $ Text.unpack $ bs2text bs

    parseAction :: UDEnv -> ParamText -> IO Expr
    parseAction env at = parseOut env $ Text.unpack $ at2text at

    parseDeontic :: Deontic -> CId
    parseDeontic d = case d of
        DMust  -> mkCId "must_Deontic"
        DMay   -> mkCId "may_Deontic"
        DShant -> mkCId "shant_Deontic"

    parseTemporal :: UDEnv -> TemporalConstraint Text.Text -> IO Expr
    parseTemporal env (TAfter event)  = return $ mkApp (mkCId "foo") []


    parseUpon :: UDEnv -> BoolStructP -> Expr
    parseUpon env bs = parse' "Adv" env (Text.unwords [Text.pack "upon", bsp2text bs])

bs2text :: BoolStruct -> Text.Text
bs2text (Leaf txt) = txt
bs2text (All _) = Text.pack "walk"
bs2text (Any _) = Text.pack "walk"
bs2text (Not _) = Text.pack "walk"

at2text :: a -> Text.Text
at2text _ = Text.pack "dummy"

bsp2text :: BoolStructP -> Text.Text
bsp2text (AA.Leaf pt)  = Text.pack "walk" --pt2text pt
-- bsp2text (AA.Not  x)   = "not " <> bsp2text x
bsp2text (AA.Any xs) =  Text.unwords (bsp2text <$> xs)
bsp2text (AA.All xs) =  Text.unwords (bsp2text <$> xs)
-- and possibily we want to have interspersed BoolStructs along the way

------------------------------------------------------------
-- Ignore everything below for now


data AnnotatedRule = RegulativeA
            { everyA    :: Expr                      -- every person (NP)
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
          deriving (Eq, Show)