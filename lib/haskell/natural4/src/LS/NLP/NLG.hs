{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, NamedFieldPuns, FlexibleContexts #-}

module LS.NLP.NLG where

import LS.NLP.UDExt
import LS.Types ( Deontic(..),
      EntityType,
      TemporalConstraint (..), TComparison(..),
      ParamText,
      BoolStruct(..),
      RuleName,
      Rule(..), BoolStructP, BoolStructR, rp2text, pt2text, bsp2text, bsr2text, rp2texts, RelationalPredicate (RPBoolStructR), HornClause2 (hHead) )
import PGF ( readPGF, languages, CId, Expr, linearize, mkApp, mkCId, readExpr, Morpho, Lemma, Analysis, buildMorpho, lookupMorpho, inferExpr, showType, ppTcError, PGF )
import qualified PGF as PGF
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
import Data.List ( elemIndex, intercalate, group, sort, sortOn )
import Data.List.Extra (maximumOn)
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
import qualified GF.Text.Pretty as GfPretty

showExpr = PGF.showExpr []

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
  putStrLn $ showExpr expr
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
            subjWho = applyMaybe "Who" whoA (gf dummyNP) --(gf $ peelNP subjA)
            subj = mkApp (mkCId "Every") [subjWho]
            king_may_sing = mkApp (mkCId "subjAction") [subj, deonticAction]
            king_may_sing_upon = applyMaybe "Upon" uponA king_may_sing
            -- mkApp2 "Who" <$> whoA
            existingQualifiers = [(name,expr) |
                                  (name,Just expr) <- [("Cond", condA),
                                                       ("Temporal", temporalA),
                                                       ("Upon", uponA),
                                                       ("Given", givenA)]]
            finalTree = doNLG existingQualifiers king_may_sing -- determine information structure based on which fields are Nothing
            -- finalTree = king_may_sing_upon
            linText = linearize gr lang finalTree
            linTree = showExpr finalTree
        return (Text.pack (linText ++ "\n" ++ linTree))
      HornlikeA {
        clausesA
      } -> do
        let linTrees_exprs = Text.unlines [
              Text.pack (linText ++ "\n" ++ linTree)
              | tree <- clausesA
              , let linText = linearize gr lang tree
              , let linTree = showExpr tree ]

        return linTrees_exprs
      _ -> return "()"

doNLG :: [(String,Expr)] -> Expr -> Expr
doNLG [("Cond", condA), ("Temporal", temporalA)] king = mkApp (mkCId "CondTemporal") [condA, temporalA, king]
doNLG [("Cond", condA), ("Upon", uponA)] king = mkApp (mkCId "CondUpon") [condA, uponA, king]
doNLG [("Cond", condA), ("Given", givenA)] king = mkApp (mkCId "CondGiven") [condA, givenA, king]


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

    -- NB. we assume here only structure like "before 3 months", not "before the king sings"
    parseTemporal :: UDEnv -> TemporalConstraint Text.Text -> IO Expr
    parseTemporal env (TemporalConstraint keyword time tunit) = do
      uds <- parseOut env $ kw2txt keyword <> time2txt time <> " " <> tunit
      let adv = peelAdv uds
      return $ gf adv
      where
        kw2txt tcomp = Text.pack $ case tcomp of
          TBefore -> "before "
          TAfter -> "after "
          TBy -> "by "
          TOn -> "on "
          TVague -> "around "
        time2txt t = Text.pack $ maybe "" show t

------------------------------------------------------------
-- Let's try to parse a BoolStructR into a GF list
-- First use case: "any unauthorised [access,use,…]  of personal data"

-- buildMorpho :: PGF -> Language -> Morpho
makeMorpho :: UDEnv -> Morpho
makeMorpho env =
  buildMorpho (pgfGrammar env) (actLanguage env)

-- lookupMorpho :: Morpho -> String -> [(Lemma, Analysis)]
-- mkApp :: CId -> [Expr] -> Expr
parseLex :: UDEnv -> String -> [Expr]
parseLex env str =
  [ mkApp cid [] | (cid, analy) <- lookupMorpho (makeMorpho env) str]

findType :: PGF -> PGF.Expr -> String
findType pgf e = case inferExpr pgf e of
  Left te -> error $ GfPretty.render $ ppTcError te -- gives string of error
  Right (_, typ) -> showType [] typ -- string of type

-- Given a list of ambiguous words like
-- [[access_V, access_N], [use_V, use_N], [copying_N, copy_V], [disclosure_N]]
-- return a disambiguated list: [access_N, use_N, copying_N, disclosure_N]
disambiguateList :: PGF -> [[PGF.Expr]] -> [PGF.Expr]
disambiguateList pgf access_use_copying =
  if all isSingleton access_use_copying
    then concat access_use_copying
    else [ w | ws <- access_use_copying
         , w <- ws
         , findType pgf w == unambiguousType (map (map (findType pgf)) access_use_copying)
    ]
  where
    unambiguousType :: [[String]]-> String
    unambiguousType access_use_copying
      | not (any isSingleton access_use_copying) =
         -- all wordlists are either empty or >1: take the most frequent cat
         head $ maximumOn length $ Data.List.group $ Data.List.sort $ concat access_use_copying
      | otherwise =
         -- Some list has exactly 1 cat, use it to disambiguate the rest
         head $ head $ sortOn length access_use_copying

    isSingleton [_] = True
    isSingleton _ = False

-- Meant to be called from inside an Any or All
-- If we encounter another Any or All, fall back to bsr2gf
bsr2gfAmb :: UDEnv -> BoolStructR -> IO [PGF.Expr]
bsr2gfAmb env bsr = case bsr of
  AA.Leaf rp -> do
    let access = rp2text rp
    let checkWords = length $ Text.words access
    putStrLn "morpho"
    print $ lookupMorpho (makeMorpho env) (Text.unpack access)
    putStrLn "---"
    print $ map showExpr (parseLex env (Text.unpack access))
    putStrLn "***"
    case checkWords of
      1 -> return $ parseLex env (Text.unpack access)
      _ -> singletonList $ parseOut env access
  -- In any other case, call the full bsr2gf
  _ -> singletonList $ bsr2gf env bsr
  where
    singletonList x = (:[]) `fmap` x

bsr2gf :: UDEnv -> BoolStructR -> IO PGF.Expr
bsr2gf env bsr = case bsr of
  -- This happens only if the full BoolStructR is just a single Leaf
  -- In typical case, the BoolStructR is a list of other things, and in such case, bsr2gfAmb is called on the list
  AA.Leaf rp -> do
    let access = rp2text rp
    parseOut env access  -- Always returns a UDS, don't check if it's a single word (no benefit because there's no context anyway)
  AA.Any (Just (AA.PrePost any_unauthorised of_personal_data)) access_use_copying -> do
    -- 1) Parse the actual contents. This can be
    contentsAmb <- mapM (bsr2gfAmb env) access_use_copying
    let contents = disambiguateList (pgfGrammar env) (contentsAmb :: [[Expr]])
        contentsUDS = map (toUDS (pgfGrammar env)) contents        -- contents may come from UD parsing or lexicon, force them to be same type
        listcn = GListCN $ mapMaybe cnFromUDS contentsUDS

        -- -- Here we need to determine which GF type the contents are
        -- -- TODO: what if they are different types?
        -- advs = mapMaybe advFromUDS contentsUDS
        -- aps =  mapMaybe apFromUDS contentsUDS
        -- cns = mapMaybe cnFromUDS contentsUDS
        -- dets = mapMaybe detFromUDS contentsUDS


    -- print "contentsUDS"
    -- print $ map (showExpr . gf) contentsUDS
    -- 2) Parse the premodifier
    premodUDS <- parseOut env any_unauthorised

    -- 3) Parse the postmodifier
    postmodUDS <- parseOut env of_personal_data

    -- TODO: add more options, if the postmodifier is not a prepositional phrase
    let postmod = peelNP postmodUDS -- TODO: actually check if (1) the adv is from PrepNP and (2) the Prep is "of"

    -- TODO: add more options in constructTree, depending on whether
    let tree = constructTree listcn (LexConj "or_Conj") postmod (fg premodUDS)
    return tree

  _ -> return dummyExpr


toUDS :: PGF -> Expr -> GUDS
toUDS pgf e = case findType pgf e of
  "UDS" -> fg e -- it's already a UDS
  "NP" -> Groot_only (GrootN_                 (fg e))
  "CN" -> Groot_only (GrootN_ (GMassNP        (fg e)))
  "N"  -> Groot_only (GrootN_ (GMassNP (GUseN (fg e))))
  "AP" -> Groot_only (GrootA_          (fg e))
  "A"  -> Groot_only (GrootA_ (GPositA (fg e)))
  "VP" -> Groot_only (GrootV_        (fg e))
  "V"  -> Groot_only (GrootV_ (GUseV (fg e)))
  "Adv"-> Groot_only (GrootAdv_ (fg e))
  "Det"-> Groot_only (GrootDet_ (fg e))
  "Quant"-> Groot_only (GrootQuant_ (fg e))
  _ -> fg dummyExpr

constructTree :: GListCN -> GConj -> GNP -> GUDS -> Expr
constructTree cns conj nmod qualUDS = finalTree
  where
    amod = case getRoot qualUDS of
      [] -> dummyAP
      x:_ -> case x of
        (GrootA_ am) -> am
        (GrootDetA_ _det am) -> am
        _ -> dummyAP
    cn = GCN_AP_Conj_CNs_of_NP amod conj cns nmod
    maybedet = case getRoot qualUDS of
      [] -> Nothing
      x:_ -> case x of
        (GrootDetA_ d _) -> Just d
        (GrootDet_ d) -> Just d
        (GrootQuant_ q) -> Just $ GDetQuant q GNumSg
        _ -> Nothing
    finalTree = gf $ case maybedet of
      Nothing -> GMassNP cn
      Just det -> GDetCN det cn


-----------------------------------------------------------------------------
-- Manipulating GF trees

dummyNP :: GNP
dummyNP = Gwhoever_NP

dummyAP :: GAP
dummyAP = GStrAP (GString [])

dummyAdv :: GAdv
dummyAdv = LexAdv "never_Adv"

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
cnFromUDS x = np2cn =<< npFromUDS x
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

detFromUDS :: GUDS -> Maybe GDet
detFromUDS x = case x of
  Groot_only (GrootDet_ someDet) -> Just someDet
  _ -> Nothing

getRoot :: Tree a -> [Groot]
getRoot rt@(GrootA_ _) = [rt]
getRoot rt@(GrootN_ _) = [rt]
getRoot rt@(GrootV_ _) = [rt]
getRoot rt@(GrootDet_ _) = [rt]
getRoot rt@(GrootDetA_ _ _) = [rt]
getRoot rt@(GrootQuant_ _) = [rt]
getRoot x = composOpMonoid getRoot x
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
