{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, NamedFieldPuns #-}

module LS.NLP.NLG where

import LS.NLP.UDExt
import LS.Types ( Deontic(..),
      EntityType,
      TemporalConstraint (..), TComparison(..),
      ParamText,
      BoolStruct(..),
      RuleName,
      Rule(..), BoolStructP, BoolStructR, rp2text, pt2text, bsp2text, bsr2text, rp2texts, RelationalPredicate (RPBoolStructR), HornClause2 (hHead) )
import PGF ( readPGF, languages, CId, Expr, linearize, mkApp, mkCId, showExpr, readExpr )
import UDAnnotations ( UDEnv(..), getEnv )
import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy         (Text)
import Data.Char (toLower)
import Data.List.Split (splitOn)
import Data.Void (Void)
-- import Data.List.NonEmpty (toList)
import UD2GF (getExprs)
import AnyAll (Item(..))
import qualified AnyAll as AA
import Data.Maybe ( fromJust, fromMaybe, catMaybes, mapMaybe )
import Data.List ( elemIndex, intercalate )
import Replace.Megaparsec ( sepCap )
import Text.Megaparsec
    ( (<|>), anySingle, match, parseMaybe, manyTill, Parsec )
import Text.Megaparsec.Char (char)
import Data.Either (rights)
import Debug.Trace (trace)

-- typeprocess to run a python
import System.IO ()
import System.Process.Typed ( proc, readProcessStdout_ )
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Control.Monad.IO.Class
import Control.Monad (join)

myUDEnv :: IO UDEnv
myUDEnv = getEnv (gfPath "UDApp") "Eng" "UDS"

dummyExpr :: PGF.Expr
dummyExpr = fromJust $ readExpr "root_only (rootN_ (MassNP (UseN dummy_N)))" -- dummy expr

gfPath :: String -> String
gfPath x = "grammars/" ++ x

-- Parsing text with udpipe via external Python process
udParse :: Text.Text -> IO String
udParse txt = do
  let str = Text.unpack txt
  conllRaw <- getPy str :: IO L8.ByteString
  return $ mkConlluString $ unpack conllRaw

getPy :: Control.Monad.IO.Class.MonadIO m => String -> m L8.ByteString
getPy x = readProcessStdout_ (proc "python3" ["src/L4/sentence.py", x])

unpack :: L8.ByteString -> String
unpack x = drop (fromMaybe (-1) $ elemIndex '[' conll) conll
  where conll = filter (not . (`elem` ("\n" :: String))) $ L8.unpack x

mkConlluString :: String -> String
mkConlluString txt = intercalate "\n" [ intercalate "\t" $ grabStrings ('\'','\'') l | l <- grabStrings ('[',']') txt ]
  where
    patterns :: (Char, Char) -> Parsec Void String String
    patterns (a,b) = do
      char a
      join <$> manyTill
              ((fst <$> match (patterns (a,b))) <|> (pure <$> anySingle))
              (char b)

    grabStrings :: (Char, Char) -> String -> [String]
    grabStrings (a,b) txt =
      rights $ fromJust $ parseMaybe (sepCap (patterns (a,b))) txt


parseConllu :: UDEnv -> String -> Maybe Expr
parseConllu env str = trace ("\nconllu:\n" ++ str) $
  case getExprs [] env str of
    (x : _xs) : _xss -> Just x
    _ -> Nothing


parseOut :: UDEnv -> Text.Text -> IO Expr
parseOut env txt = do
  conll <- udParse txt -- Initial parse
  lowerConll <- udParse (Text.map toLower txt) -- fallback: if parse fails with og text, try parsing all lowercase
  let expr = case parseConllu env conll of -- env -> str -> [[expr]]
               Just e -> e
               Nothing -> case parseConllu env lowerConll of
                            Just e' -> e'
                            Nothing -> dummyExpr
  putStrLn $ showExpr [] expr
  return expr
-----------------------------------------------------------------------------

nlg :: Rule -> IO Text.Text
nlg rl = do
   env <- myUDEnv
   annotatedRule <- parseFields env rl
   -- TODO: here let's do some actual NLG
   gr <- readPGF (gfPath "UDExt.pgf")
   let lang = head $ languages gr
   case annotatedRule of
      RegulativeA {
        subjA
      , whoA
      , condA
      , deonticA
      , actionA
      , temporalA
      , uponA
      , givenA
      } -> do
        let deonticAction = mkApp deonticA [actionA]
            subjWho = applyMaybe "Who" whoA (gf $ peelNP subjA)
            subj = mkApp (mkCId "Every") [subjWho]
            king_may_sing = mkApp (mkCId "subjAction") [subj, deonticAction]
            king_may_sing_upon = applyMaybe "Upon" uponA king_may_sing
            -- mkApp2 "Who" <$> whoA
            existingQualifiers = [(name,expr) |
                                  (name,Just expr) <- [("Cond", condA),
                                                       ("Temporal", temporalA),
                                                       ("Upon", uponA),
                                                       ("Given", givenA)]]
--            finalTree = doNLG existingQualifiers king_may_sing -- determine information structure based on which fields are Nothing
            finalTree = king_may_sing_upon
            linText = linearize gr lang finalTree
            linTree = showExpr [] finalTree
        return (Text.pack (linText ++ "\n" ++ linTree))
      HornlikeA {
        clausesA
      } -> do
        let linTrees_exprs = Text.unlines [
              Text.pack (linText ++ "\n" ++ linTree)
              | tree <- clausesA
              , let linText = linearize gr lang tree
              , let linTree = showExpr [] tree ]

        return linTrees_exprs
      _ -> return "()"

doNLG :: [(String,Expr)] -> Expr -> Expr
doNLG = error "not implemented"

applyMaybe :: String -> Maybe Expr -> Expr -> Expr
applyMaybe _ Nothing action = action
applyMaybe name (Just expr) action = mkApp (mkCId name) [expr, action]

mkApp2 :: String -> Expr -> Expr -> Expr
mkApp2 name a1 a2 = mkApp (mkCId name) [a1, a2]

parseFields :: UDEnv -> Rule -> IO AnnotatedRule
parseFields env rl = case rl of
  Regulative {} -> do
    subjA'  <- parseBool env (subj rl)
    whoA'   <- mapM (parseBSR env) (who rl)
    condA'  <- mapM (parseBSR env) (cond rl)
    let deonticA' = parseDeontic (deontic rl)    :: CId
    actionA' <- parseBool env (action rl)
    temporalA' <- mapM (parseTemporal env) (temporal rl)
    uponA' <- mapM (parseParamText env) (upon rl)
    givenA' <- mapM (parseParamText env) (given rl)
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
    givenA' <- mapM (parseParamText env) (given rl)
    nameA' <- parseName env (name rl)
    condA'   <- mapM (parseBSR env) (cond rl) -- when/if/unless
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
  DefNameAlias {
    name = [nm]
  , detail = [det]
  } -> return $ Alias nm det
  Hornlike {
    clauses = cls:_
  } -> case hHead cls of
        RPBoolStructR _ _ bsr -> do
          expr <- bsr2gf env bsr
          return $ HornlikeA {clausesA = [expr]}
        _ -> error "parseFields: doesn't support Hornlike yet"

  _ -> error "parseFields: rule type not supported yet"
  where
    -- ConstitutiveName is [Text.Text]
    parseName :: UDEnv -> [Text.Text] -> IO Expr
    parseName env txt = parseOut env (Text.unwords txt)

    parseBool :: UDEnv -> BoolStructP -> IO Expr
    parseBool env bsp = parseOut env (bsp2text bsp)

    parseBSR :: UDEnv -> BoolStructR -> IO Expr
    parseBSR env bsr = parseOut env (bsr2text bsr)

    parseParamText :: UDEnv -> ParamText -> IO Expr
    parseParamText env pt = parseOut env $ pt2text pt

    parseDeontic :: Deontic -> CId
    parseDeontic d = case d of
        DMust  -> mkCId "Must"
        DMay   -> mkCId "May"
        DShant -> mkCId "Shant"

    parseTemporal :: UDEnv -> TemporalConstraint Text.Text -> IO Expr
    parseTemporal env (TemporalConstraint keyword time tunit) =
      parseOut env $ kw2txt keyword <> time2txt time <> " " <> tunit
      where
        kw2txt tcomp = Text.pack $ case tcomp of
          TBefore -> "before "
          TAfter -> "after "
          TBy -> "by "
          TOn -> "on "
          TVague -> "vaguely "
        time2txt t = Text.pack $ maybe "" show t

------------------------------------------------------------
-- Let's try to parse a BoolStructR into a GF list
-- First use case: "any unauthorised [access,use,…]  of personal data"

bsr2gf :: UDEnv -> BoolStructR -> IO PGF.Expr
bsr2gf env bsr = case bsr of
  AA.Leaf rp -> do
    let access = rp2text rp
    parseOut env access -- returns a UDS -- TODO: parse single words in a lexicon and only larger phrases with UD!
  AA.Any (Just (AA.PrePost any_unauthorised of_personal_data)) access_use_copying -> do
    cnsUDS <- mapM (bsr2gf env) access_use_copying
    let listcn = GListCN $ mapMaybe (cnFromUDS . fg) cnsUDS -- TODO: generalise to things that are not nouns
    amodUDS <- parseOut env any_unauthorised
    let amod = peelAP amodUDS
    advUDS <- parseOut env of_personal_data
    let nmod = peelNP advUDS -- TODO: actually check if (1) the adv is from PrepNP and (2) the Prep is "of"
    return (gf $
      GCN_AP_Conj_CNs_of_NP amod (LexConj "or_Conj") listcn nmod)
  _ -> return dummyExpr


-----------------------------------------------------------------------------
-- Manipulating GF trees

dummyNP :: GNP
dummyNP = Gwhoever_NP

dummyAP :: GAP
dummyAP = GStrAP (GString [])

dummyAdv :: GAdv
dummyAdv = LexAdv "now_Adv"

peelNP :: Expr -> GNP
peelNP np = fromMaybe dummyNP (npFromUDS $ fg np)

peelAP :: Expr -> GAP
peelAP ap = fromMaybe dummyAP (apFromUDS $ fg ap)

peelAdv :: Expr -> GAdv
peelAdv adv = fromMaybe dummyAdv (advFromUDS $ fg adv)

npFromUDS :: GUDS -> Maybe GNP
npFromUDS x = case x of
  Groot_only (GrootN_ someNP) -> Just someNP
  Groot_only (GrootAdv_ (GPrepNP _ someNP)) -> Just someNP -- extract NP out of an Adv
  _ -> Nothing
--  Groot_nsubj (rootV_ someVP) (nsubj_ someNP) -> GRelNP someNP (GRelVP someVP)

cnFromUDS :: GUDS -> Maybe GCN
cnFromUDS x | Just np <- npFromUDS x = np2cn np
            | otherwise              = Nothing
  where
    np2cn :: GNP -> Maybe GCN
    np2cn np = case np of
      GMassNP   cn          -> Just cn
      GDetCN    _det cn     -> Just cn
      GGenModNP _num _np cn -> Just cn
      GExtAdvNP np   _adv   -> np2cn np
      GAdvNP    np   _adv   -> np2cn np
      GRelNP    np  _rs     -> np2cn np
      GPredetNP _pre np     -> np2cn np
      _                     -> Nothing

apFromUDS :: GUDS -> Maybe GAP
apFromUDS x = case x of
  Groot_only (GrootA_ someAP) -> Just someAP
  _ -> Nothing

advFromUDS :: GUDS -> Maybe GAdv
advFromUDS x = case x of
  Groot_only (GrootAdv_ someAdv) -> Just someAdv
  _ -> Nothing

-----------------------------------------------------------------------------
-- Ignore everything below for now


data AnnotatedRule = RegulativeA
            { subjA     :: Expr                      -- man AND woman AND child
            , whoA      :: Maybe Expr                -- who walks and (eats or drinks)
            , condA     :: Maybe Expr                -- if it is a saturday
            , deonticA  :: CId                       -- must, may
            , actionA   :: Expr                      -- sing / pay the king $20
            , temporalA :: Maybe Expr                -- before midnight
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
            | Alias Text Text -- TODO: where to use this info?
            | HornlikeA {clausesA :: [Expr]}
          deriving (Eq, Show)
