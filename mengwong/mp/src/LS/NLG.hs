{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}

module LS.NLG (
    nlg
    ) where

import LS.UDExt
import LS.Types ( Deontic(..),
      EntityType,
      TemporalConstraint (..), TComparison(..),
      ParamText,
      BoolStruct(..),
      ConstitutiveName,
      Rule(..), BoolStructP, BoolStructR, rp2text, pt2text, bsp2text, bsr2text )
import PGF ( readPGF, languages, CId, Expr, linearize, mkApp, mkCId, showExpr )
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
    where conll = filter (not . (`elem` ("\n" :: String))) $ L8.unpack x

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
    whoA'   <- mapM (parseBSR env) (who rl)
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

    parseBSR :: UDEnv -> BoolStructR -> IO Expr
    parseBSR env bsr = parseOut env (bsr2text bsr)

    parseUpon :: UDEnv -> Maybe ParamText -> IO (Maybe Expr)
    parseUpon env mpt = sequence $ parseOut env . pt2text <$> mpt

    parseDeontic :: Deontic -> CId
    parseDeontic d = case d of
        DMust  -> mkCId "must_Deontic"
        DMay   -> mkCId "may_Deontic"
        DShant -> mkCId "shant_Deontic"

    -- TODO: add GF funs for  ParseTemporal
    -- It will look like this:
    {- parseUpon env bs = do
        rawExpr <- parseOut env event
        let gfFun = getGFFun (TAfter/TWhatever/…) -- should we move on to the Haskell version of the abstract syntax?
        return $ <gfFun applied to rawExpr>  -- either use PGF.mkApp, or with Haskell version of abstract syntax
      -}
    parseTemporal :: UDEnv -> TemporalConstraint Text.Text -> IO Expr
    parseTemporal env tc = case tc of
      TemporalConstraint TAfter  n tunit -> parseOut env $ "after "   <> Text.pack (show n) <> " " <> tunit
      TemporalConstraint TBefore n tunit -> parseOut env $ "before "  <> Text.pack (show n) <> " " <> tunit
      TemporalConstraint TBy     n tunit -> parseOut env $ "by "      <> Text.pack (show n) <> " " <> tunit
      TemporalConstraint TOn     n tunit -> parseOut env $ "on "      <> Text.pack (show n) <> " " <> tunit
      TemporalConstraint TVague  n tunit -> parseOut env $ "vaguely " <> Text.pack (show n) <> " " <> tunit

    {- TODO: do we want to give this more structure in the GF grammar as well?
      so that the GF tree looks like
         Upon (GerundVP some_VP)
      instead of
         PrepNP upon_Prep (GerundVP some_VP)
      in the latter case, the fact that this is an "upon" sentence is hidden in a lexical function upon_Prep
      in the former, we know from the first constructor that this is an "upon" sentence
    -}



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
