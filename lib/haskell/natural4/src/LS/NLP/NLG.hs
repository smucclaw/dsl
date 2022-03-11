{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, NamedFieldPuns, FlexibleContexts #-}

module LS.NLP.NLG where

import LS.NLP.UDExt
import LS.Types ( TemporalConstraint (..), TComparison(..),
      ParamText,
      Rule(..),
      BoolStructP, BoolStructR,
      RelationalPredicate(..), HornClause2(..), RPRel(..), HasToken (tokenOf),
      rp2text, pt2text, bsp2text, mt2text, tm2mt)
import PGF ( readPGF, languages, CId, Expr, linearize, mkApp, mkCId, readExpr, buildMorpho, lookupMorpho, inferExpr, showType, ppTcError, PGF )
import qualified PGF
import UDAnnotations ( UDEnv(..), getEnv )
import qualified Data.Text.Lazy as Text
import Data.Char (toLower)
import Data.Void (Void)
-- import Data.List.NonEmpty (toList)
import UD2GF (getExprs)
import qualified AnyAll as AA
import Data.Maybe ( fromJust, fromMaybe, catMaybes, mapMaybe )
import Data.List ( elemIndex, intercalate, group, sort, sortOn, nub )
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
import Data.List.NonEmpty (NonEmpty((:|)))

showExpr :: Expr -> String
showExpr = PGF.showExpr []

myUDEnv :: IO UDEnv
myUDEnv = getEnv (gfPath "UDApp") "Eng" "UDS"

nlgExtPGF :: IO PGF
nlgExtPGF = readPGF (gfPath "UDExt.pgf")

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
      _ <- char a
      join <$> manyTill
              (fst <$> match (patterns (a,b)) <|> pure <$> anySingle)
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
--  conll <- udParse txt -- Initial parse
  lowerConll <- udParse (Text.map toLower txt) -- fallback: if parse fails with og text, try parsing all lowercase
  -- let expr = case parseConllu env conll of -- env -> str -> [[expr]]
  --              Just e -> e
  --              Nothing -> fromMaybe dummyExpr (parseConllu env lowerConll)
  let expr = fromMaybe dummyExpr (parseConllu env lowerConll)
  putStrLn $ showExpr expr
  return expr
-----------------------------------------------------------------------------

nlg :: Rule -> IO Text.Text
nlg rl = do
   env <- myUDEnv
   annotatedRule <- parseFields env rl
   -- TODO: here let's do some actual NLG
   gr <- nlgExtPGF
   let lang = head $ languages gr
   case annotatedRule of
      RegulativeA {subjA, keywordA, whoA, condA, deonticA, actionA, temporalA, uponA, givenA} -> do
        let deonticAction = mkApp deonticA [actionA]
            subjWho = applyMaybe "Who" whoA (gf $ peelNP subjA)
            subj = mkApp keywordA [subjWho]
            king_may_sing = mkApp (mkCId "subjAction") [subj, deonticAction]
            existingQualifiers = [(name,expr) |
                                  (name,Just expr) <- [("Cond", fmap (gf . toUDS gr) condA),
                                                       ("Temporal", temporalA),
                                                       ("Upon", uponA),
                                                       ("Given", givenA)]]
            finalTree = doNLG existingQualifiers king_may_sing -- determine information structure based on which fields are Nothing
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
      RegBreachA -> return "IT'S A BREACH >:( >:( >:("
      _ -> return "()"

doNLG :: [(String,Expr)] -> Expr -> Expr
-- Single modifiers
doNLG [("Cond", condA)] king = mkApp (mkCId "Cond") [condA, king]
doNLG [("Upon", uponA)] king = mkApp (mkCId "Upon") [uponA, king]
doNLG [("Temporal", temporalA)] king = mkApp (mkCId "Temporal") [temporalA, king]
doNLG [("Given", givenA)] king = mkApp (mkCId "Given") [givenA, king]
-- Combinations of two
doNLG [("Cond", condA), ("Temporal", temporalA)] king = mkApp (mkCId "CondTemporal") [condA, temporalA, king]
doNLG [("Cond", condA), ("Upon", uponA)] king = mkApp (mkCId "CondUpon") [condA, uponA, king]
doNLG [("Cond", condA), ("Given", givenA)] king = mkApp (mkCId "CondGiven") [condA, givenA, king]
doNLG _ expr = expr


applyMaybe :: String -> Maybe Expr -> Expr -> Expr
applyMaybe _ Nothing action = action
applyMaybe name (Just expr) action = mkApp (mkCId name) [expr, action]

mkApp2 :: String -> Expr -> Expr -> Expr
mkApp2 name a1 a2 = mkApp (mkCId name) [a1, a2]

parseFields :: UDEnv -> Rule -> IO AnnotatedRule
parseFields env rl = case rl of
  Regulative {subj, rkeyword, who, cond, deontic, action, temporal, upon, given, having} -> do
    subjA <- parseBool env subj
    let keywordA = keyword2cid $ tokenOf rkeyword
    whoA <- mapM (bsr2gf env) who
    condA <- mapM (bsr2gf env) cond
    let deonticA = keyword2cid deontic
    actionA <- parseBool env action
    temporalA <- mapM (parseTemporal env) temporal
    uponA <- mapM (parseParamText env) upon
    givenA <- mapM (parseParamText env) given
    havingA <- mapM (parseParamText env) having
    return RegulativeA {subjA, keywordA, whoA, condA, deonticA, actionA, temporalA, uponA, givenA, havingA}
  Constitutive {name, keyword, cond, given} -> do
    let keywordA = keyword2cid keyword
    nameA <- parseName env name
    condA <- mapM (bsr2gf env) cond
    givenA <- mapM (parseParamText env) given
    return ConstitutiveA {nameA, keywordA, condA, givenA}
  Hornlike {name, keyword, given, upon, clauses} -> do
    let keywordA = keyword2cid keyword
    nameA <- parseName env name
    givenA <- mapM (parseParamText env) given
    uponA <- mapM (parseParamText env) upon
    clausesA <- mapM (parseHornClause env keywordA) clauses
    return HornlikeA {nameA, keywordA, givenA, uponA, clausesA}
  TypeDecl {name, {-super, has,-} enums, given, upon} -> do
    nameA <- parseName env name
    --superA <- TODO: parse TypeSig
    let superA = Just dummyExpr
    enumsA <- mapM (parseParamText env) enums
    givenA <- mapM (parseParamText env) given
    uponA <- mapM (parseParamText env) upon
    return TypeDeclA {nameA, superA, enumsA, givenA, uponA}
  Scenario {scgiven, expect} -> do
    let fun = mkCId "RPis" -- TODO fix this properly
    scgivenA <- mapM (parseRP env fun) scgiven
    expectA <- mapM (parseHornClause env fun) expect
    return ScenarioA {scgivenA, expectA}
  DefNameAlias { name, detail, nlhint } -> do
    nameA <- parseName env name
    detailA <- parseMulti env detail
    return DefNameAliasA {nameA, detailA, nlhintA=nlhint}
  RegFulfilled -> return RegFulfilledA
  RegBreach -> return RegBreachA
  _ -> return RegBreachA
  where
    parseHornClause :: UDEnv -> CId -> HornClause2 -> IO Expr
    parseHornClause env fun (HC2 rp Nothing) = parseRP env fun rp
    parseHornClause env fun (HC2 rp (Just bsr)) = do
      extGrammar <- nlgExtPGF -- use extension grammar, because bsr2gf can return funs from UDExt
      db_is_NDB_UDFragment <- fg `fmap` parseRP env fun rp -- TODO: dangerous assumption, not all parseRPs return UDFragment
      db_occurred_UDS <- toUDS extGrammar `fmap` bsr2gf env bsr
      let hornclause = GHornClause2 db_is_NDB_UDFragment db_occurred_UDS
      return $ gf hornclause

    -- TODO: switch to GUDS or GUDFragment? How can we know which type they return?
    -- Something a bit more typed than Expr would feel safer
    parseRP :: UDEnv -> CId -> RelationalPredicate -> IO Expr
    parseRP env _f (RPParamText pt) = parseParamText env pt
    parseRP env _f (RPMT txts) = parseMulti env txts
    parseRP env fun (RPConstraint sky is blue) = do
      skyUDS <- parseMulti env sky
      blueUDS <- parseMulti env blue
      let skyNP = gf $ peelNP skyUDS
      let rprel = if is==RPis then fun else keyword2cid is
      return $ mkApp rprel [skyNP, blueUDS]
    parseRP env fun (RPBoolStructR sky is blue) = do
      gr <- nlgExtPGF
      skyUDS <- parseMulti env sky
      blueUDS <- (gf . toUDS gr) `fmap` bsr2gf env blue
      let skyNP = gf $ peelNP skyUDS -- TODO: make npFromUDS more robust for different sentence types
      let rprel = if is==RPis then fun else keyword2cid is
      return $ mkApp rprel [skyNP, blueUDS]

    -- ConstitutiveName is [Text.Text]
    parseMulti :: UDEnv -> [Text.Text] -> IO Expr
    parseMulti env txt = parseOut env (Text.unwords txt)

    parseName :: UDEnv -> [Text.Text] -> IO Text.Text
    parseName _env txt = return (Text.unwords txt)

    parseBool :: UDEnv -> BoolStructP -> IO Expr
    parseBool env bsp = parseOut env (bsp2text bsp)

    parseParamText :: UDEnv -> ParamText -> IO Expr
    parseParamText env pt = parseOut env $ pt2text pt

    keyword2cid :: (Show a) => a -> CId
    keyword2cid = mkCId . show

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
-- Let's try to parse a BoolStructP into a GF list
-- First use case: "notify the PDPC in te form and manner specified at … with a notification msg and a list of individuals for whom …"
bsp2gf :: UDEnv -> BoolStructP -> IO Expr
bsp2gf env bsp = case bsp of
  AA.Leaf (action :| mods) -> do
    let actionStr = mt2text $ tm2mt action
        modStrs = map (mt2text . tm2mt) mods
        -- then parse those in GF and put in appropriate trees
    return undefined
  _ -> error "bsp2gf: not supported yet"
  -- AA.All m_la its -> _
  -- AA.Any m_la its -> _
  -- AA.Not it -> _

------------------------------------------------------------
-- Let's try to parse a BoolStructR into a GF list
-- First use case: "any unauthorised [access,use,…]  of personal data"

-- lookupMorpho :: Morpho -> String -> [(Lemma, Analysis)]
-- mkApp :: CId -> [Expr] -> Expr
parseLex :: UDEnv -> String -> [Expr]
parseLex env str = {-- trace ("parseLex:" ++ show result)  --} result
  where
    lexicalCats :: [String]
    lexicalCats = words "N V A N2 N3 V2 A2 VA V2V VV V3 VS V2A V2S V2Q Adv AdV AdA AdN ACard CAdv Conj Interj PN Prep Pron Quant Det Card Text Predet Subj"
    result = nub [ mkApp cid []
              | (cid, _analy) <- lookupMorpho morpho str
              , let expr = mkApp cid []
              , findType parsingGrammar expr `elem` lexicalCats
            ]
    morpho = buildMorpho parsingGrammar lang
    parsingGrammar = pgfGrammar env -- use the parsing grammar, not extension grammar
    lang = actLanguage env

findType :: PGF -> PGF.Expr -> String
findType pgf e = case inferExpr pgf e of
  Left te -> error $ "Tried to infer type of:\n\t* " ++ showExpr e ++ "\nGot the error:\n\t* " ++ GfPretty.render (ppTcError te) -- gives string of error
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
    case checkWords of
      1 -> return $ parseLex env (Text.unpack access)
      _ -> singletonList $ parseOut env access
  -- In any other case, call the full bsr2gf
  _ -> singletonList $ bsr2gf env bsr
  where
    singletonList x = (:[]) `fmap` x

bsr2gf :: UDEnv -> BoolStructR -> IO Expr
bsr2gf env bsr = case bsr of
  -- This happens only if the full BoolStructR is just a single Leaf
  -- In typical case, the BoolStructR is a list of other things, and in such case, bsr2gfAmb is called on the list
  AA.Leaf rp -> do
    let access = rp2text rp
    parseOut env access  -- Always returns a UDS, don't check if it's a single word (no benefit because there's no context anyway)

  AA.Any Nothing contents -> do
    -- 1) Parse the actual contents. This can be
    contentsUDS <- parseAndDisambiguate env contents
    -- print $ map (showExpr . gf) contentsUDS

        -- Here we need to determine which GF type the contents are
        -- TODO: what if they are different types?
    let treeAdv :: Maybe Expr
        treeAdv = case mapMaybe advFromUDS contentsUDS :: [GAdv] of
                    []    -> Nothing
                    [adv] -> Just $ gf adv
                    advs  -> Just $ gf $ GConjAdv (LexConj "or_Conj") (GListAdv advs)

        treeAP :: Maybe Expr
        treeAP = case mapMaybe apFromUDS contentsUDS :: [GAP] of
                    []   -> Nothing
                    [ap] -> Just $ gf ap
                    aps  -> Just $ gf $ GConjAP (LexConj "or_Conj") (GListAP aps)

        treeCN :: Maybe Expr
        treeCN = case mapMaybe cnFromUDS contentsUDS :: [GCN] of
                    []   -> Nothing
                    [cn] -> Just $ gf cn
                    cns  -> Just $ gf $ GConjCN (LexConj "or_Conj") (GListCN cns)
        treeDet :: Maybe Expr
        treeDet = case mapMaybe detFromUDS contentsUDS :: [GDet] of
                    []    -> Nothing
                    [det] -> Just $ gf det
                    dets  -> Just $ gf $ GConjNP (LexConj "or_Conj") (GListNP $ map GDetNP dets)

    -- add dummyExpr into the list, so if all 4 are Nothing, at least program doesn't crash
    return $ head $ catMaybes [treeAP, treeAdv, treeCN, treeDet, Just dummyExpr]

  AA.Any (Just (AA.PrePost any_unauthorised of_personal_data)) access_use_copying -> do
    -- 1) Parse the actual contents. This can be
    contentsUDS <- parseAndDisambiguate env access_use_copying
    let listcn = GListCN $ mapMaybe cnFromUDS contentsUDS
    -- 2) Parse the premodifier
    amodUDS <- parseOut env any_unauthorised
    -- 3) Parse the postmodifier
    nmodUDS <- parseOut env of_personal_data
    -- TODO: add more options, if the postmodifier is not a prepositional phrase
    let personal_data = peelNP nmodUDS -- TODO: actually check if (1) the adv is from PrepNP and (2) the Prep is "of"

    -- TODO: add more options in constructTree, depending on whether
    let tree = constructTreeAPCNsOfNP listcn (LexConj "or_Conj") personal_data (fg amodUDS)
    return tree

  _ -> return dummyExpr

parseAndDisambiguate :: UDEnv -> [BoolStructR] -> IO [GUDS]
parseAndDisambiguate env text = do
  contentsAmb <- mapM (bsr2gfAmb env) text
  let parsingGrammar = pgfGrammar env -- here we use the parsing grammar, not extension grammar!
      contents = disambiguateList parsingGrammar contentsAmb
  return $ map (toUDS parsingGrammar) contents

constructTreeAPCNsOfNP :: GListCN -> GConj -> GNP -> GUDS -> Expr
constructTreeAPCNsOfNP cns conj nmod qualUDS = finalTree
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
 -- "Quant"-> Groot_only (GrootQuant_ (fg e))
  "Quant"-> Groot_only (GrootDet_ (GDetQuant (fg e) GNumSg))
  "ACard" -> Groot_only (GrootDet_ (GACard2Det (fg e)))
  "AdA" -> Groot_only (GrootAdA_ (fg e)) -- added from gf
  _ -> trace ("unable to convert to UDS: " ++ showExpr e) (fg dummyExpr)

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

-- peelDet :: Expr -> GDet
-- peelDet det = fromMaybe dummyDet (detFromUDS $ fg det)

npFromUDS :: GUDS -> Maybe GNP
npFromUDS x = case x of
  Groot_only (GrootN_ someNP) -> Just someNP
  Groot_only (GrootAdv_ (GPrepNP _ someNP)) -> Just someNP -- extract NP out of an Adv
  Groot_nsubj (GrootV_ someVP) (Gnsubj_ someNP) -> Just $ GSentNP someNP (GEmbedVP someVP) --
  _ -> Nothing

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
-- AnnotatedRule, almost isomorphic to LS.Types.Rule

data AnnotatedRule = RegulativeA
            { subjA     :: Expr                      -- man AND woman AND child
            , keywordA  :: CId                       -- Every , Party, All — GF funs
            , whoA      :: Maybe Expr                -- who walks and (eats or drinks)
            , condA     :: Maybe Expr                -- if it is a saturday
            , deonticA  :: CId                       -- must, may
            , actionA   :: Expr                      -- sing / pay the king $20
            , temporalA :: Maybe Expr                -- before midnight
            -- , henceA    :: Maybe [AnnotatedRule]     -- hence [UDS]
            -- , lestA     :: Maybe [AnnotatedRule]     -- lest [UDS]
            , uponA     :: Maybe Expr                -- UPON entering the club (event prereq trigger)
            , givenA    :: Maybe Expr                -- GIVEN an Entertainment flag was previously set in the history trace
            -- skipping rlabel, lsource, srcref
            , havingA   :: Maybe Expr  -- HAVING sung...
            -- , wwhereA   :: [AnnotatedRule]
            -- TODO: what are these?
--            , defaults :: [RelationalPredicate] -- SomeConstant IS 500 ; MentalCapacity TYPICALLY True
--            , symtab   :: [RelationalPredicate] -- SomeConstant IS 500 ; MentalCapacity TYPICALLY True
            }
            | ConstitutiveA {
              nameA     :: Text.Text   -- the thing we are defining
            , keywordA  :: CId       -- Means, Includes, Is, Deem
            , condA     :: Maybe Expr -- a boolstruct set of conditions representing When/If/Unless
            , givenA    :: Maybe Expr
            -- skipping letbind, rlabel, lsurce, srcref, defaults, symtab
            }
            | HornlikeA {
              nameA     :: Text.Text           -- colour
            , keywordA  :: CId            -- decide / define / means
            , givenA    :: Maybe Expr    -- applicant has submitted fee
            , uponA     :: Maybe Expr    -- second request occurs
            , clausesA  :: [Expr]
            -- skipping letbind, rlabel, lsurce, srcref, defaults, symtab
            }
            | TypeDeclA {
              nameA     :: Text.Text  --      DEFINE Sign
            , superA    :: Maybe Expr     --                  :: Thing
            --, hasA      :: Maybe [AnnotatedRule]      -- HAS foo :: List Hand \n bar :: Optional Restaurant
            , enumsA    :: Maybe Expr   -- ONE OF rock, paper, scissors (basically, disjoint subtypes)
            , givenA    :: Maybe Expr
            , uponA     :: Maybe Expr
            -- skipping letbind, rlabel, lsurce, srcref, defaults, symtab
            }
            | ScenarioA {
              scgivenA  :: [Expr]
            , expectA   :: [Expr]      -- investment is savings when dependents is 5
            -- skipping letbind, rlabel, lsurce, srcref, defaults, symtab
            }
            | DefNameAliasA { -- inline alias, like     some thing AKA Thing
              nameA   :: Text.Text  -- "Thing" -- the thing usually said as ("Thing")
            , detailA :: Expr  -- ["some", "thing"]
            , nlhintA :: Maybe Text.Text   -- "lang=en number=singular"
            }
            | RegFulfilledA  -- trivial top
            | RegBreachA     -- trivial bottom
{- skipping the following
            | DefTypically -- inline default assignment, like     some hemisphere TYPICALLY North
            { name   :: RuleName  -- the name of the enclosing rule scope context -- a bit tricky to retrieve so typically just the termhead for now. FIXME
            , defaults :: [RelationalPredicate] -- usually an RPParamText or RPMT. higher order not quite explored yet.
            , srcref :: Maybe SrcRef
            }
          | RuleAlias RuleName -- internal softlink to a rule label (rlabel), e.g. HENCE NextStep
          | RuleGroup { rlabel :: Maybe RuleLabel
                      , srcref :: Maybe SrcRef }  -- § NextStep

          | NotARule [MyToken] -}
