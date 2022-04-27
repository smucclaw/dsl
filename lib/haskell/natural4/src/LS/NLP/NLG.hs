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
      Expect(..),
      rp2text, pt2text, bsr2text, KVsPair)
import PGF ( readPGF, languages, CId, Expr, linearize, mkApp, mkCId, lookupMorpho, inferExpr, showType, ppTcError, PGF )
import qualified PGF
import UDAnnotations ( UDEnv(..), getEnv )
import qualified Data.Text.Lazy as Text
import Data.Char (toLower, isUpper)
import UD2GF (getExprs)
import qualified AnyAll as AA
import Data.Maybe ( fromMaybe, catMaybes, mapMaybe )
import Data.List ( group, sort, sortOn, nub )
import Data.List.Extra (maximumOn)
--import Debug.Trace (trace)
import qualified GF.Text.Pretty as GfPretty
import Data.List.NonEmpty (NonEmpty((:|)))
import UDPipe (loadModel, runPipeline, Model)
import Control.Monad (when)
import System.Environment (lookupEnv)
import Data.Vector.Internal.Check (doChecks)
import Data.Aeson (SumEncoding(contentsFieldName))
import Text.Megaparsec (pos1)

data NLGEnv = NLGEnv
  { udEnv :: UDEnv
  , udpipeModel :: Model
  , verbose :: Bool
  }

showExpr :: Expr -> String
showExpr = PGF.showExpr []

modelFilePath :: FilePath
modelFilePath = gfPath "english-ewt-ud-2.5-191206.udpipe"

myNLGEnv :: IO NLGEnv
myNLGEnv = do
  udEnv <- getEnv (gfPath "UDApp") "Eng" "UDS"
  mpn <- lookupEnv "MP_NLG"
  let verbose = maybe False (read :: String -> Bool) mpn
  when verbose $ putStrLn "\n-----------------------------\n\nLoading UDPipe model..."
  udpipeModel <- either error id <$> loadModel modelFilePath
  when verbose $ putStrLn "Loaded UDPipe model"
  -- let
  --   parsingGrammar = pgfGrammar udEnv -- use the parsing grammar, not extension grammar
  --   lang = actLanguage udEnv
  --   gfMorpho = buildMorpho parsingGrammar lang
  return $ NLGEnv {udEnv, udpipeModel, verbose}

nlgExtPGF :: IO PGF
nlgExtPGF = readPGF (gfPath "UDExt.pgf")

dummyExpr :: String -> PGF.Expr
dummyExpr msg = gf $ Groot_only (GrootN_ (GUsePN (GStrPN (GString msg)))) -- dummy expr

gfPath :: String -> String
gfPath x = "grammars/" ++ x

-- | Parse text with udpipe via udpipe-hs, then convert the result into GF via gf-ud
parseUD :: NLGEnv -> Text.Text -> IO GUDS
parseUD env txt = do
  when (not $ verbose env) $ -- when not verbose, just short output to reassure user we're doing something
    putStrLn ("    NLG.parseUD: parsing " <> "\"" <> Text.unpack txt <> "\"")
--  conll <- udpipe txt -- Initial parse
  lowerConll <- udpipe (lowerButPreserveAllCaps txt) -- fallback: if parse fails with og text, try parsing all lowercase
  when (verbose env) $ putStrLn ("\nconllu:\n" ++ lowerConll)
  -- let expr = case ud2gf conll of
  --              Just e -> e
  --              Nothing -> fromMaybe errorMsg (ud2gf lowerConll)
  let expr = fromMaybe errorMsg (ud2gf lowerConll)
  when (verbose env) $ putStrLn ("The UDApp tree created by ud2gf:\n" ++ showExpr expr)
  return $ fg expr
  where
    errorMsg = dummyExpr $ "parseUD: fail to parse " ++ Text.unpack txt

    udpipe :: Text.Text -> IO String
    udpipe txt = do
      let str = Text.unpack txt
      when (verbose env) $ putStrLn "Running UDPipe..."
      result <- runPipeline (udpipeModel env) str
      -- when (verbose env) $ putStrLn ("UDPipe result: " ++ show result) -- this is a bit extra verbose
      return $ either error id result

    ud2gf :: String -> Maybe Expr
    ud2gf str = case getExprs [] (udEnv env) str of
      (x : _xs) : _xss -> Just x
      _ -> Nothing

    lowerButPreserveAllCaps :: Text.Text -> Text.Text
    lowerButPreserveAllCaps txt = Text.unwords
      [ if all isUpper (Text.unpack wd)
          then wd
          else Text.map toLower wd
      | wd <- Text.words txt
      ]

-----------------------------------------------------------------------------

nlg :: Rule -> IO Text.Text
nlg rl = do
   env <- myNLGEnv
   annotatedRule <- parseFields env rl
   -- TODO: here let's do some actual NLG
   gr <- nlgExtPGF
   let lang = head $ languages gr
   case annotatedRule of
      RegulativeA {subjA, keywordA, whoA, condA, deonticA, actionA, temporalA, uponA, givenA} -> do
        let deonticAction = mkApp deonticA [gf $ toUDS gr actionA] -- TODO: or change type of DMust to take VP instead?
            subjWho = applyMaybe "Who" whoA (gf $ peelNP subjA)
            subj = mkApp keywordA [subjWho]
            king_may_sing = mkApp (mkCId "subjAction") [subj, deonticAction]
            existingQualifiers = [(name,expr) |
                                  (name,Just expr) <- [("Cond", gf . toUDS gr <$> condA),
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
      ConstitutiveA {nameA, keywordA, condA, givenA} -> do
        putStrLn "Constitutive rule:"
        putStrLn $ Text.unpack nameA
        mapM_ (putStrLn . showExpr) (catMaybes [condA, givenA])
        return ""
      TypeDeclA {nameA, superA, enumsA, givenA, uponA} -> do
        putStrLn "Type declaration:"
        putStrLn $ Text.unpack nameA
        mapM_ (putStrLn . showExpr) (catMaybes [superA, enumsA, givenA, uponA])
        return ""
      ScenarioA {scgivenA, expectA} -> do
        putStrLn "Scenario:"
        mapM_ (putStrLn . showExpr) scgivenA
        mapM_ (putStrLn . showExpr) expectA
        return ""
      DefNameAliasA { nameA, detailA, nlhintA } -> do
        let tree = maybe detailA gf (npFromUDS $ fg detailA)
            linText = linearize gr lang tree
            linTree = showExpr tree
        return $ Text.pack (Text.unpack nameA ++ " AKA " ++ linText ++ "\n" ++ linTree ++ "\n")
      RegBreachA -> return "IT'S A BREACH >:( >:( >:("
      RegFulfilledA -> return "FULFILLED \\:D/ \\:D/ \\:D/"

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

parseFields :: NLGEnv -> Rule -> IO AnnotatedRule
parseFields env rl = case rl of
  Regulative {subj, rkeyword, who, cond, deontic, action, temporal, upon, given, having} -> do
    let gr = pgfGrammar $ udEnv env
    subjA <- gf . toUDS gr <$> bsp2gf env subj
    let keywordA = keyword2cid $ tokenOf rkeyword
    whoA <- mapM (bsr2gf env) who
    condA <- mapM (bsr2gf env) cond
    let deonticA = keyword2cid deontic
    actionA <- bsp2gf env action
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
    let superA = Just (dummyExpr "parseField : does not parse TypeSig yet ")
    enumsA <- mapM (parseParamText env) enums
    givenA <- mapM (parseParamText env) given
    uponA <- mapM (parseParamText env) upon
    return TypeDeclA {nameA, superA, enumsA, givenA, uponA}
  Scenario {scgiven, expect} -> do
    let fun = mkCId "RPis" -- TODO fix this properly
    scgivenA <- mapM (parseRP env fun) scgiven
    expectA <- mapM (parseExpect env fun) expect
    return ScenarioA {scgivenA, expectA}
  DefNameAlias { name, detail, nlhint } -> do
    nameA <- parseName env name
    detailA <- parseMulti env detail
    return DefNameAliasA {nameA, detailA, nlhintA=nlhint}
  RegFulfilled -> return RegFulfilledA
  RegBreach -> return RegBreachA
  _ -> return RegBreachA
  where
    parseHornClause :: NLGEnv -> CId -> HornClause2 -> IO Expr
    parseHornClause env fun (HC2 rp Nothing) = parseRP env fun rp
    parseHornClause env fun (HC2 rp (Just bsr)) = do
      extGrammar <- nlgExtPGF -- use extension grammar, because bsr2gf can return funs from UDExt
      db_is_NDB_UDFragment <- fg `fmap` parseRP env fun rp -- TODO: dangerous assumption, not all parseRPs return UDFragment
      db_occurred_UDS <- toUDS extGrammar `fmap` bsr2gf env bsr
      let hornclause = GHornClause2 db_is_NDB_UDFragment db_occurred_UDS
      return $ gf hornclause

    parseExpect :: NLGEnv -> CId -> Expect -> IO Expr
    parseExpect _env _f (ExpDeontic _) = error "NLG/parseExpect unimplemented for deontic rules"
    parseExpect  env  f (ExpRP rp) = parseRP env f rp

    -- TODO: switch to GUDS or GUDFragment? How can we know which type they return?
    -- Something a bit more typed than Expr would feel safer
    parseRP :: NLGEnv -> CId -> RelationalPredicate -> IO Expr
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
    parseMulti :: NLGEnv -> [Text.Text] -> IO Expr
    parseMulti env txt = gf `fmap` parseUD env (Text.unwords txt)

    parseName :: NLGEnv -> [Text.Text] -> IO Text.Text
    parseName _env txt = return (Text.unwords txt)

    parseParamText :: NLGEnv -> ParamText -> IO Expr
    parseParamText env pt = gf `fmap` (parseUD env $ pt2text pt)

    keyword2cid :: (Show a) => a -> CId
    keyword2cid = mkCId . show

    -- NB. we assume here only structure like "before 3 months", not "before the king sings"
    parseTemporal :: NLGEnv -> TemporalConstraint Text.Text -> IO Expr
    parseTemporal env (TemporalConstraint keyword time tunit) = do
      uds <- parseUD env $ kw2txt keyword <> time2txt time <> " " <> tunit
      let Just adv = advFromUDS uds
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
bsp2gf :: NLGEnv -> BoolStructP -> IO Expr
bsp2gf env bsp = case bsp of
  AA.Leaf (action :| mods) -> do
    actionExpr <- kvspair2gf env action  -- notify the PDPC
    modExprs <- mapM (kvspair2gf env) mods -- [by email, at latest at the deadline]
    return $ combineActionMods actionExpr modExprs
  _ -> error "bsp2gf: not supported yet"

-- | Takes the main action, a list of modifiers, and combines them into one Expr
combineActionMods :: (String,Expr) -> [(String, Expr)] -> Expr
combineActionMods (_, expr) [] = expr
combineActionMods ("VP",act) (("Adv",mod):rest) = combineActionMods advVP rest
  where
    advVP :: (String,Expr)
    advVP = ("VP", gf $ GAdvVP (fg act) (fg mod))
combineActionMods ("VP",act) (("RCl",mod):rest) = combineActionMods ("VP", resultVP) rest
  where
    -- Assumption: RCl doesn't modify the whole VP, but rather the object of the VP
    resultVP = gf $ case fg act of
      GComplVP vp np -> GComplVP vp (GRelNP np rs)
      vp             -> GComplVP vp (GRelNP dummyNP rs)
    rs = useRCl (fg mod)
combineActionMods (tAct,_) ((tMods,_):_) = error $ "combineActionMods: not supported yet " ++ tAct ++ "+" ++ tMods

-- | Takes a KVsPair, parses the fields, puts them together into GF Expr
kvspair2gf :: NLGEnv -> KVsPair -> IO (String, Expr)
kvspair2gf env (action,_) = case action of
  pred :| []     -> do
    predUDS <- parseUD env pred
    return $ case udsToTreeGroups predUDS of
      TG {gfAP=Just ap}   -> ("AP", gf ap)
      TG {gfAdv=Just adv} -> ("Adv", gf adv)
      TG {gfNP=Just np}   -> ("NP", gf np)
      TG {gfDet=Just det} -> ("Det", gf det)
      TG {gfCN=Just cn}   -> ("CN", gf cn)
      TG {gfPrep=Just pr} -> ("Prep", gf pr)
      TG {gfRP=Just rp}   -> ("RP", gf rp)
      TG {gfCl=Just cl}   -> ("Cl", gf cl)
      TG {gfVP=Just v}    -> ("VP", gf v)
      _ -> ("NP", dummyExpr $ "kvspair2gf: type of predicate not among " ++ acceptedRGLtypes)

  pred :| compls -> do
    predUDS <- parseUD env pred
    complUDS <- parseUD env (Text.unwords compls) -- TODO: or parse each item one by one?
    return $ combineExpr predUDS complUDS

-- | Takes two UDSs, puts them together, returns Expr and a string that tells which type the result is.
combineExpr :: GUDS -> GUDS -> (String, Expr)
combineExpr pred compl = result
  where
    predExpr = gf pred -- used for error msg
    complExpr = gf compl -- used for error msg
    predTyped = udsToTreeGroups pred
    complTyped = udsToTreeGroups compl
    result = case predTyped of
      TG {gfRP=Just for_which} ->
        ("RCl", case complTyped of
          TG {gfCl= Just you_work} -> gf $ GRelSlash for_which (GSlashCl you_work)
          TG {gfVP= Just works}    -> gf $ GRelVP for_which works
          _ -> error ("combineExpr: can't combine predicate " ++ showExpr predExpr ++ "with complement " ++ showExpr complExpr)
        )
      TG {gfPrep=Just under} ->
        case complTyped of
          TG {gfAP=Just haunted} -> ("Adv", gf $ GPrepNP under (GAdjAsNP haunted))
          -- TG {gfAdv=Just quickly} -> ???
          TG {gfNP=Just johnson} -> ("Adv", gf $ GPrepNP under johnson)
          TG {gfDet=Just my}     -> ("Adv", gf $ GPrepNP under (GDetNP my))
          TG {gfCN=Just car}     -> ("Adv", gf $ GPrepNP under (GMassNP car))
          TG {gfPrep=Just with}  -> ("Prep", gf $ GConjPrep andConj (GListPrep [under, with]))
          TG {gfRP=Just which}   -> ("RP", gf $ GPrepRP under which)
          TG {gfVP=Just haunt}   -> ("Adv", gf $ GPrepNP under (GGerundNP haunt))
          -- TG {gfCl=Just you_see} -> ???
          _ -> error ("combineExpr: can't combine predicate " ++ showExpr predExpr ++ "with complement " ++ showExpr complExpr)

      TG {gfVP=Just notify} ->
        ("VP", case complTyped of
          TG {gfAP=Just haunted} -> gf $ GComplVP notify (GAdjAsNP haunted)
          TG {gfAdv=Just quickly}-> gf $ GAdvVP   notify quickly
          TG {gfNP=Just johnson} -> gf $ GComplVP notify johnson
          TG {gfDet=Just my}     -> gf $ GComplVP notify (GDetNP my)
          TG {gfCN=Just car}     -> gf $ GComplVP notify (GMassNP car)
          TG {gfPrep=Just with}  -> gf $ GPrepVP notify with
          -- TG {gfRP=Just which}   -> ("RP", gf $ GPrepRP under which)
          -- TG {gfVP=Just haunt}   -> ("Adv", gf $ GPrepNP under (GGerundNP haunt))
          -- TG {gfCl=Just you_see} -> ???
          _ -> error ("combineExpr: can't combine predicate " ++ showExpr predExpr ++ "with complement " ++ showExpr complExpr)
        )
      TG {gfCN=Just house} ->
        ("CN", case complTyped of
          TG {gfCN=Just car}      -> gf $ GApposCN house (GMassNP car)
          TG {gfNP=Just johnson}  -> gf $ GApposCN house johnson
          TG {gfDet=Just my}      -> gf $ GApposCN house (GDetNP my)
          TG {gfAdv=Just quickly} -> gf $ GAdvCN  house quickly
          TG {gfAP=Just haunted}  -> gf $ GAdjCN  haunted house
          _ -> error ("combineExpr: can't combine predicate " ++ showExpr predExpr ++ "with complement " ++ showExpr complExpr)
        )
      TG {gfNP=Just the_customer} ->
        ("NP", case complTyped of
          TG {gfCN=Just house}    -> gf $ GGenModNP GNumSg the_customer house
          TG {gfNP=Just johnson}  -> gf $ GApposNP  the_customer johnson
          TG {gfDet=Just my}      -> gf $ GApposNP  the_customer (GDetNP my)
          TG {gfAdv=Just quickly} -> gf $ GAdvNP    the_customer quickly
          TG {gfAP=Just haunted}  -> gf $ GApposNP  the_customer (GAdjAsNP haunted)
          _ -> error ("combineExpr: can't combine predicate " ++ showExpr predExpr ++ "with complement " ++ showExpr complExpr)
        )
      TG {gfDet=Just any} -> case complTyped of
          TG {gfCN=Just house}   -> ("NP", gf $ GDetCN any house)
          TG {gfAP=Just haunted} -> ("DAP", gf $ GAdjDAP (GDetDAP any) haunted)
          _ -> error ("combineExpr: can't combine predicate " ++ showExpr predExpr ++ "with complement " ++ showExpr complExpr)
      TG {gfAdv=Just happily} -> case complTyped of
        TG {gfCN=Just house}   -> ("CN", gf $ GAdvCN house happily)
        TG {gfNP=Just johnson} -> ("NP", gf $ GAdvNP johnson happily)
        TG {gfAP=Just haunted} -> ("AP", gf $ GAdvAP haunted happily)
        _ -> error ("combineExpr: can't combine predicate " ++ showExpr predExpr ++ "with complement " ++ showExpr complExpr)
      TG {gfAP=Just happy} -> case complTyped of
        TG {gfCN=Just house}    -> ("CN", gf $ GAdjCN happy house)
        TG {gfNP=Just johnson}  -> ("NP", gf $ GApposNP (GAdjAsNP happy) johnson)
        TG {gfDet=Just my}      -> ("DAP", gf $ GAdjDAP (GDetDAP my) happy)
        TG {gfAdv=Just quickly} -> ("AP", gf $ GAdvAP happy quickly)
        TG {gfAP=Just (GPositA haunted)} -> ("AP", gf $ GAdvAP happy (GPositAdvAdj haunted))
        _ -> error ("combineExpr: can't combine predicate " ++ showExpr predExpr ++ "with complement " ++ showExpr complExpr)
      -- ("Cl", you_work) -> case complTyped of
      --   ("RP", for_which) -> ("RCl", gf $ GRelSlash (fg for_which) (GSlashCl (fg you_work)))
      --   _ -> error ("combineExpr: can't combine predicate " ++ showExpr predExpr ++ "with complement " ++ showExpr complExpr)
      tg -> error ("combineExpr: can't find type " ++ show tg ++ " for the predicate " ++ showExpr predExpr)


-- | Takes a UDS, peels off the UD layer, returns a pair ("RGL type", the peeled off Expr)
udsToTreeGroups :: GUDS -> TreeGroups
udsToTreeGroups uds = groupByRGLtype (LexConj "") [uds]

------------------------------------------------------------
-- Let's try to parse a BoolStructR into a GF list
-- First use case: "any unauthorised [access,use,…]  of personal data"

-- lookupMorpho :: Morpho -> String -> [(Lemma, Analysis)]
-- mkApp :: CId -> [Expr] -> Expr
parseLex :: NLGEnv -> String -> [Expr]
parseLex env str = {-- trace ("parseLex:" ++ show result)  --} result
  where
    lexicalCats :: [String]
    lexicalCats = words "N V A N2 N3 V2 A2 VA V2V VV V3 VS V2A V2S V2Q Adv AdV AdA AdN ACard CAdv Conj Interj PN Prep Pron Quant Det Card Text Predet Subj"
    result = nub [ mkApp cid []
              | (cid, _analy) <- lookupMorpho morpho str
              , let expr = mkApp cid []
              , findType parsingGrammar expr `elem` lexicalCats
            ]
    morpho = morphology $ udEnv env
    parsingGrammar = pgfGrammar $ udEnv env -- use the parsing grammar, not extension grammar

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

treePre conj contents pre =
  case map flattenGFTrees trees of
    []  -> dummyExpr $ "bsr2gf: failed parsing " ++ showExpr (gf pre)
    x:_ -> head x -- return the first one---TODO later figure out how to deal with different categories
    where trees = groupByRGLtype conj <$> [contents, [pre]]

treePrePost conj contents pre post =
  case groupByRGLtype conj <$> [contents, [pre], [post]] of
          [TG {gfCN=Just cn}, _, TG {gfAdv=Just (GPrepNP _of personal_data)}] ->
            let listcn = case cn of
                  GConjCN _ cns -> cns
                  _ -> GListCN [cn, cn]
            in constructTreeAPCNsOfNP listcn conj personal_data pre
          [TG {gfDet=Just det}, TG {gfCl=Just cl}, TG {gfCN=Just cn}] ->
            let obj = GDetCN det cn
            in case cl of
                  GPredVP np vp ->  gf $ GPredVP np (GComplVP vp obj)
                  GGenericCl vp ->  gf $ GComplVP vp obj
                  _ -> error $ "bsr2gf: can't handle the Cl " ++ showExpr (gf cl)
          _ -> dummyExpr $ "bsr2gf: can't handle the combination " ++ showExpr (gf pre) ++ "+" ++ showExpr (gf post)


-- Meant to be called from inside an Any or All, via parseAndDisambiguate
-- If we encounter another Any or All, fall back to bsr2gf
bsr2gfAmb :: NLGEnv -> BoolStructR -> IO [PGF.Expr]
bsr2gfAmb env bsr = case bsr of
  -- If it's a leaf, parse the contents
  AA.Leaf rp -> do
    let access = rp2text rp
    case Text.words access of
      -- If the leaf is a single word, do a lexicon lookup
      [_] -> return $ parseLex env (Text.unpack access)
      -- If the leaf is multiple words, parse with udpipe
      _ -> singletonList $ gf `fmap` parseUD env access

  -- If it's not a leaf, call bsr2gf
  _ -> singletonList $ bsr2gf env bsr
  where
    singletonList x = (:[]) `fmap` x

bsr2gf :: NLGEnv -> BoolStructR -> IO Expr
bsr2gf env bsr = case bsr of
  -- This happens only if the full BoolStructR is just a single Leaf
  -- In typical case, the BoolStructR is a list of other things, and in such case, bsr2gfAmb is called on the list
  AA.Leaf rp -> do
    let access = rp2text rp
    gf `fmap` parseUD env access  -- Always returns a UDS, don't check if it's a single word (no benefit because there's no context anyway)

  AA.Not rp -> do
    let not = bsr2text rp
    gf `fmap` parseUD env not

  AA.Any Nothing contents -> do
    contentsUDS <- parseAndDisambiguate env contents
    let existingTrees = groupByRGLtype orConj contentsUDS
    -- print ("qcl" :: [Char])
    -- putStrLn $ showExpr $ gf $ getQSFromTrees existingTrees
    -- gr <- nlgExtPGF
    -- print (linearize gr (head $ languages gr) $ gf $ getQSFromTrees existingTrees)
    return $ case flattenGFTrees existingTrees of
               []  -> dummyExpr $ "bsr2gf: failed parsing " ++ Text.unpack (bsr2text bsr)
               x:_ -> x -- return the first one---TODO later figure out how to deal with different categories

  AA.All Nothing contents -> do
    contentsUDS <- parseAndDisambiguate env contents
    let existingTrees = groupByRGLtype andConj contentsUDS
    return $ case flattenGFTrees existingTrees of
               []  -> dummyExpr $ "bsr2gf: failed parsing " ++ Text.unpack (bsr2text bsr)
               x:_ -> x

  AA.Any (Just (AA.PrePost any_unauthorised of_personal_data)) access_use_copying -> do
    contentsUDS <- parseAndDisambiguate env access_use_copying
    premodUDS <- parseUD env any_unauthorised
    postmodUDS <- parseUD env of_personal_data
    return $ treePrePost orConj contentsUDS premodUDS postmodUDS
    -- let tree = case groupByRGLtype orConj <$> [contentsUDS, [premodUDS], [postmodUDS]] of
    --       [TG {gfCN=Just cn}, _, TG {gfAdv=Just (GPrepNP _of personal_data)}] ->
    --         let listcn = case cn of
    --               GConjCN _ cns -> cns
    --               _ -> GListCN [cn, cn]
    --         in constructTreeAPCNsOfNP listcn orConj personal_data premodUDS
    --       [TG {gfDet=Just det}, TG {gfCl=Just cl}, TG {gfCN=Just cn}] ->
    --         let obj = GDetCN det cn
    --         in case cl of
    --               GPredVP np vp ->  gf $ GPredVP np (GComplVP vp obj)
    --               GGenericCl vp ->  gf $ GComplVP vp obj
    --               _ -> error $ "bsr2gf: can't handle the Cl " ++ showExpr (gf cl)
    --       _ -> dummyExpr $ "bsr2gf: can't handle the combination " ++ showExpr (gf premodUDS) ++ "+" ++ showExpr (gf postmodUDS)
    -- return tree

  AA.Any (Just (AA.Pre p1)) contents -> do
    contentsUDS <- parseAndDisambiguate env contents
    premodUDS <- parseUD env p1
    return $ treePre orConj contentsUDS premodUDS

  AA.All (Just (AA.Pre p1)) contents -> do
    contentsUDS <- parseAndDisambiguate env contents
    premodUDS <- parseUD env p1
    return $ treePre andConj contentsUDS premodUDS

  AA.All (Just (AA.PrePost p1 p2)) contents -> do
    contentsUDS <- parseAndDisambiguate env contents
    premodUDS <- parseUD env p1
    postmodUDS <- parseUD env p2
    return $ treePrePost andConj contentsUDS premodUDS postmodUDS

  _ -> return $ dummyExpr $ "bsr2gf: failed parsing " ++ Text.unpack (bsr2text bsr)

-- | A data structure for GF trees, which has different bins for different RGL categories.
--
-- A single UDS tree may become several of these; e.g. a root_nsubj sentence pattern could become
-- a Cl, "a breach occurs", but also a NP, "an occurring breach".
-- The different NLG functions make their decisions on how to combine phrases based on which fields are filled.
data TreeGroups = TG {
    gfAP   :: Maybe GAP
  , gfAdv  :: Maybe GAdv
  , gfNP   :: Maybe GNP
  , gfDet  :: Maybe GDet
  , gfCN   :: Maybe GCN
  , gfPrep :: Maybe GPrep
  , gfRP   :: Maybe GRP
  , gfVP   :: Maybe GVP
  , gfCl   :: Maybe GCl
   } deriving (Eq)

-- | for documentation: which RGL types are accepted currently
acceptedRGLtypes :: String
acceptedRGLtypes = "AP Adv NP Det CN Prep RP VP Cl"

instance Show TreeGroups where
  show = unlines . map showExpr . flattenGFTrees

-- | Workaround to flatten TreeGroups into a list of Exprs.
flattenGFTrees :: TreeGroups -> [Expr]
flattenGFTrees TG {gfAP, gfAdv, gfNP, gfDet, gfCN, gfPrep, gfRP, gfVP, gfCl} =
  gfAP <: gfAdv <: gfNP <: gfCN <: gfDet <: gfPrep <: gfRP <: gfVP <: gfCl <: []
  where
    infixr 5 <:
    (<:) :: (Gf a) => Maybe a -> [Expr] -> [Expr]
    Nothing <: exprs = exprs
    Just ap <: exprs = gf ap : exprs

--     ExistNPQS  : Temp -> Pol -> NP -> QS ;   -- was there a party
-- SQuestVPS  : NP   -> VPS -> QS ;
-- get GQS from Trees
--     ExistIPQS  : Temp -> Pol -> IP -> QS ;   -- what was there
--     QuestIAdv   : IAdv -> Cl -> QCl ;    -- why does John walk
--     ExistIP   : IP -> QCl ;       -- which houses are there

getQSFromTrees :: TreeGroups -> GQS
getQSFromTrees whichTG = case whichTG of
  TG {gfCl = Just cl} -> useQCl $ GQuestCl (makeSubjectDefinite cl)
  TG {gfNP = Just np} -> GExistNPQS (GTTAnt GTPres GASimul) GPPos (makeSubjectIndefinite np)
  TG {gfCN = Just cn} -> useQCl $ GQuestCl $ GExistCN cn
  TG {gfVP = Just vp} -> useQCl $ GQuestVP Gwhat_IP vp -- how to get what or who?
  TG {gfAP = Just ap} -> useQCl $ GQuestIComp (GICompAP ap) (GAdjAsNP ap)
  TG {gfDet = Just det} -> GExistNPQS (GTTAnt GTPres GASimul) GPPos $ GDetNP det
  TG {gfAdv = Just adv} -> useQCl $ GQuestCl (GImpersCl (GUseComp $ GCompAdv adv))
  _ -> useQCl $ GQuestCl dummyCl

  where
    makeSubjectDefinite :: GCl -> GCl
    makeSubjectDefinite cl = case cl of
      GPredVP (GMassNP cn) vp -> GPredVP (GDetCN (LexDet "theSg_Det") cn) vp
      _ -> cl

    makeSubjectIndefinite :: GNP -> GNP
    makeSubjectIndefinite np = case np of
      GAdvNP (GMassNP cn) adv -> GAdvNP (GDetCN (LexDet "aSg_Det") cn) adv
      _ -> np
-- checkIAdv :: GAdv -> GIAdv
-- checkIAdv adv
--   | adv `elem` [Galways_Adv, Gnever_Adv, Gsometimes_Adv] = Gwhen_IAdv
--   | adv `elem` [Geverywhere_Adv, Ghere_Adv, Gsomewhere_Adv, Gthere_Adv] = Gwhere_IAdv
--   | otherwise = Gwhy_IAdv


{- the ultimate refactoring goal
makeNPHaveArticle :: GDet -> GNP -> GNP
makeNPHaveArticle det ogNP = case ogNP of
  GMassNP cn -> GDetCN det cn
  _ -> ogNP

  -}

-- | Takes a list of UDS, and puts them into different bins according to their underlying RGL category.
groupByRGLtype :: GConj -> [GUDS] -> TreeGroups
groupByRGLtype conj contentsUDS = TG treeAP treeAdv treeNP treeDet treeCN treePrep treeRP treeVP treeCl
  -- TODO: what if they are different types?
  where
    treeAdv :: Maybe GAdv
    treeAdv = case mapMaybe advFromUDS contentsUDS :: [GAdv] of
                []    -> Nothing
                [adv] -> Just adv
                advs  -> Just $ GConjAdv conj (GListAdv advs)

    treeAP :: Maybe GAP
    treeAP = case mapMaybe apFromUDS contentsUDS :: [GAP] of
                []   -> Nothing
                [ap] -> Just ap
                aps  -> Just $ GConjAP conj (GListAP aps)

{- -- maybe too granular? reconsider this
    treePN :: Maybe Expr
    treePN = case mapMaybe pnFromUDS contentsUDS :: [GPN] of
                []   -> Nothing
                [pn] -> Just $ GUsePN pn
                pns  -> Just $ GConjNP conj (GListNP (map GUsePN pns))

    treePron :: Maybe Expr
    treePron = case mapMaybe pronFromUDS contentsUDS :: [GPron] of
                []   -> Nothing
                [pron] -> Just $ GUsePron pron
                prons  -> Just $ GConjNP conj (GListNP (map GUsePron prons))
-}
    -- Exclude MassNP here! MassNPs will be matched in treeCN
    treeNP :: Maybe GNP
    treeNP = case mapMaybe nonMassNpFromUDS contentsUDS :: [GNP] of
                []   -> Nothing
                [np] -> Just np
                nps  -> Just $ GConjNP conj (GListNP nps)

    -- All CNs will match NP, but we only match here if it's a MassNP
    treeCN :: Maybe GCN
    treeCN = case mapMaybe cnFromUDS contentsUDS :: [GCN] of
                []   -> Nothing
                [cn] -> Just cn
                cns  -> Just $ GConjCN conj (GListCN cns)

    treeDet :: Maybe GDet
    treeDet = case mapMaybe detFromUDS contentsUDS :: [GDet] of
                []    -> Nothing
                [det] -> Just det
                dets  -> Just $ GConjDet conj (GListDAP $ map GDetDAP dets)

    treeVP :: Maybe GVP
    treeVP = case mapMaybe verbFromUDS contentsUDS :: [GVP] of
                []    -> Nothing
                v:_  -> Just v --later: list instance for verbs?

    treePrep :: Maybe GPrep
    treePrep = case mapMaybe prepFromUDS contentsUDS :: [GPrep] of
                []    -> Nothing
                v:_  -> Just v

    treeRP :: Maybe GRP
    treeRP = case mapMaybe rpFromUDS contentsUDS :: [GRP] of
                []    -> Nothing
                r:_  -> Just r

    treeCl :: Maybe GCl
    treeCl = case mapMaybe clFromUDS contentsUDS :: [GCl] of
                []    -> Nothing
                c:_  -> Just c

parseAndDisambiguate :: NLGEnv -> [BoolStructR] -> IO [GUDS]
parseAndDisambiguate env text = do
  contentsAmb <- mapM (bsr2gfAmb env) text
  let parsingGrammar = pgfGrammar $ udEnv env -- here we use the parsing grammar, not extension grammar!
      contents = disambiguateList parsingGrammar contentsAmb
  return $ map (toUDS parsingGrammar) contents

constructTreeAPCNsOfNP :: GListCN -> GConj -> GNP -> GUDS -> Expr
constructTreeAPCNsOfNP cns conj nmod qualUDS = finalTree
  where
    amod = case getRoot qualUDS of
      [] -> dummyAP
      x:_ -> case x of
        (GrootA_ am) -> am
        (GrootDAP_ (GAdjDAP _d am)) -> am
        _ -> dummyAP
    cn = GCN_AP_Conj_CNs_of_NP amod conj cns nmod
    maybedet = case getRoot qualUDS of
      [] -> Nothing
      x:_ -> case x of
        (GrootDAP_ (GAdjDAP (GDetDAP d) _a)) -> Just d
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
  "Prep" -> Groot_only (GrootPrep_ (fg e))
  "RP" -> Groot_only (GrootRP_ (fg e))
  "RCl" -> case fg e :: GRCl of
             GRelVP _rp vp -> toUDS pgf (gf vp)
             GRelSlash _rp (GSlashCl cl) -> toUDS pgf (gf cl)
             _ -> fg $ dummyExpr ("unable to convert to UDS: " ++ showExpr e)
  "Cl" -> case fg e :: GCl of
            GPredVP np vp -> Groot_nsubj (GrootV_ vp) (Gnsubj_ np)
            GGenericCl vp -> toUDS pgf (gf vp)
            _ -> fg  $ dummyExpr ("unable to convert to UDS: " ++ showExpr e)
  _ -> fg $ dummyExpr $ "unable to convert to UDS: " ++ showExpr e

-----------------------------------------------------------------------------
-- Manipulating GF trees

dummyNP :: GNP
dummyNP = Gwhoever_NP

dummyAP :: GAP
dummyAP = GStrAP (GString [])

dummyAdv :: GAdv
dummyAdv = LexAdv "never_Adv"

dummyCl :: GCl
dummyCl = GExistsNP dummyNP

orConj, andConj :: GConj
orConj = LexConj "or_Conj"
andConj = LexConj "and_Conj"

peelNP :: Expr -> GNP
peelNP np = fromMaybe dummyNP (npFromUDS $ fg np)

useRCl :: GRCl -> GRS
useRCl = GUseRCl (GTTAnt GTPres GASimul) GPPos

useCl :: GCl -> GS
useCl = GUseCl (GTTAnt GTPres GASimul) GPPos

--     UseQCl   : Temp -> Pol -> QCl -> QS ;
useQCl :: GQCl -> GQS
useQCl = GUseQCl (GTTAnt GTPres GASimul) GPPos

-- Specialised version of npFromUDS: return Nothing if the NP is MassNP
nonMassNpFromUDS :: GUDS -> Maybe GNP
nonMassNpFromUDS x = case npFromUDS x of
  Just (GMassNP _) -> Nothing
  _ -> npFromUDS x

-- | Takes the RGL NP returned by npFromUDS, and extracts a CN out of it.
--
-- All UD-to-RGL work happens in npFromUDS, this is just peeling off the layers of the RGL functions.
cnFromUDS :: GUDS -> Maybe GCN
cnFromUDS x = np2cn =<< npFromUDS x

np2cn :: GNP -> Maybe GCN
np2cn np = case np of
  GMassNP   cn          -> Just cn
  GDetCN    _det cn     -> Just cn
  GGenModNP _num _np cn -> Just cn
  GExtAdvNP np   adv    -> fmap (`GAdvCN` adv) (np2cn np)
  GAdvNP    np   adv    -> fmap (`GAdvCN` adv) (np2cn np)
  GRelNP    np   rs     -> fmap (`GRelCN` rs) (np2cn np)
  GPredetNP _pre np     -> np2cn np
  _                     -> Nothing

-- | Constructs a RGL NP from a UDS.
-- If the UDS is like "kills a cat", the NP will be "a killed cat".
--
-- The functions cnFromUDS and nonMassNPfromUDS all rely on the UDS-matching done by npFromUDS.
npFromUDS :: GUDS -> Maybe GNP
npFromUDS x = case x of
  Groot_only (GrootN_ someNP) -> Just someNP
  Groot_only (GrootAdv_ (GPrepNP _ someNP)) -> Just someNP -- extract NP out of an Adv
  Groot_nsubj (GrootV_ someVP) (Gnsubj_ someNP) -> Just $ GSentNP someNP (GEmbedVP someVP)
  -- assessment (that sucks)
  Groot_aclRelcl (GrootN_ np) (GaclRelclUDS_ relcl) -> Just $ GRelNP np (udRelcl2rglRS relcl)
  -- the occurence at the beach
  Groot_nmod (GrootN_ rootNP) (Gnmod_ prep nmodNP) -> Just $ GAdvNP rootNP (GPrepNP prep nmodNP)
  -- service from the provider to the payer
  Groot_nmod_nmod (GrootN_ service_NP) (Gnmod_ from_Prep provider_NP) (Gnmod_ to_Prep payer_NP) -> Just $ GAdvNP (GAdvNP service_NP (GPrepNP from_Prep provider_NP)) (GPrepNP to_Prep payer_NP)
  -- great harm that she suffered
  Groot_acl (GrootN_ great_harm_NP) (GaclUDS_ (Groot_mark_nsubj (GrootV_ suffer_VP) _ (Gnsubj_ she_NP))) ->  Just $ GRelNP great_harm_NP (GRS_that_NP_VP she_NP suffer_VP)


  -- Groot_acl (GrootN_ (MassNP (AdjCN (PositA great_A) (UseN harm_N)))) (GaclUDS_ (Groot_mark_nsubj (GrootV_ (UseV suffer_V)) (Gmark_ that_Subj) (Gnsubj_ (UsePron she_Pron))))

  _ -> case getRoot x of -- TODO: fill in other cases
              GrootN_ np:_ -> Just np
              _            -> Nothing
-- | Constructs a RGL RS from a UDS.
udRelcl2rglRS :: GUDS -> GRS
udRelcl2rglRS uds = case uds of
  Groot_nsubj (GrootV_ vp) _ -> vp2rs vp -- TODO: check if nsubj contains something important
  _ -> maybe err vp2rs (verbFromUDS uds)
  where
    vp2rs vp = useRCl (GRelVP GIdRP vp)
    err = error ("udRelcl2rglRCl: doesn't handle yet " ++ showExpr (gf uds))

pnFromUDS :: GUDS -> Maybe GPN
pnFromUDS x = np2pn =<< npFromUDS x
  where
    np2pn :: GNP -> Maybe GPN
    np2pn np = case np of
      GUsePN pn -> Just pn
      _         -> Nothing

pronFromUDS :: GUDS -> Maybe GPron
pronFromUDS x = np2pron =<< npFromUDS x
  where
    np2pron :: GNP -> Maybe GPron
    np2pron np = case np of
      GUsePron pron -> Just pron
      _             -> Nothing

apFromUDS :: GUDS -> Maybe GAP
apFromUDS x = case x of
  Groot_only (GrootA_ ap) -> Just ap
  Groot_obl (GrootA_ ap) (Gobl_ adv) -> Just $ GAdvAP ap adv
  Groot_advmod (GrootA_ ap) (Gadvmod_ adv) -> Just $ GAdvAP ap adv
  Groot_advmod (GrootV_ v) (Gadvmod_ adv) -> Just $ GAdvAP (GPastPartAP v) adv
  Groot_ccomp (GrootA_ a1) (Gccomp_ (Groot_mark_nsubj_cop (GrootA_ a2) (Gmark_ m) (Gnsubj_ n) Gbe_cop)) -> Just $ GAdvAP a1 (GSubjS m (GPredVPS n (GUseComp (GCompAP a2))))
  _ -> case getRoot x of -- TODO: fill in other cases
              GrootA_ ap:_ -> Just ap
              _            -> Nothing

advFromUDS :: GUDS -> Maybe GAdv
advFromUDS x = case x of
  Groot_only (GrootAdv_ someAdv) -> Just someAdv
  Groot_obl (GrootAdv_ someAdv) (Gobl_ oblAdv) -> Just $ GAdvAdv someAdv oblAdv
  -- Groot_advcl (GrootAdv_ rt) (Gadvcl_ adv) -> Just $ GAdvAdv rt adv
  _ -> case getRoot x of -- TODO: fill in other cases
              GrootAdv_ adv:_ -> Just adv
              _               -> Nothing

detFromUDS :: GUDS -> Maybe GDet
detFromUDS x = case x of
  Groot_only (GrootDet_ someDet) -> Just someDet
  _ -> case getRoot x of -- TODO: fill in other cases
              GrootDet_ det:_ -> Just det
              _               -> Nothing

prepFromUDS :: GUDS -> Maybe GPrep
prepFromUDS x = case getRoot x of
  GrootPrep_ p:_ -> Just p
  _              -> Nothing

rpFromUDS :: GUDS -> Maybe GRP
rpFromUDS x = case getRoot x of
  GrootRP_ rp:_ -> Just rp
  _             -> Nothing

verbFromUDS :: GUDS -> Maybe GVP
verbFromUDS x = case getNsubj x of
  (_:_) -> Nothing  -- if the UDS has a subject, then it should be handled by clFromUDS instead
  [] -> case x of    -- no nsubj, move on to pattern match UDS constructors
    Groot_obl (GrootV_ vp) (Gobl_ adv) -> Just $ GAdvVP vp adv
    Groot_obl_obl (GrootV_ vp) (Gobl_ obl1) (Gobl_ obl2) -> Just $ GAdvVP (GAdvVP vp obl1) obl2
    Groot_obl_xcomp (GrootV_ vp) (Gobl_ obl) (GxcompAdv_ xc) -> Just $ GAdvVP (GAdvVP vp obl) xc
    Groot_xcomp (GrootV_ vp) (GxcompAdv_ adv) -> Just $ GAdvVP vp adv
    Groot_advmod (GrootV_ vp) (Gadvmod_ adv) -> Just $ GAdvVP vp adv

    {- -- version 1: explicit pattern match, brittle, specialised for 1 case
    Groot_acl_nmod (GrootN_ np) _                      (Gnmod_ nmod) ->
      Just $ GAdvVP (GUseComp (GCompNP np)) nmod
    Groot_acl_nmod (GrootA_ ap) _                      (Gnmod_ nmod) ->
      Just $ GAdvVP (GUseComp (GCompAP ap)) nmod
    Groot_acl_nmod (GrootV_ vp) _                      (Gnmod_ nmod) ->
      Just $ GAdvVP vp nmod -}

    -- version 2: general solution, recursion
    Groot_acl_nmod root         (GaclUDSgerund_ uds) (Gnmod_ prep np) -> do
      vp <- verbFromUDS (Groot_only root) -- recursively calling verbFromUDS, now with a UDS that is guaranteed to go to the _ case below, and getRoot will be called, and a VP will be constructed
      vpToBecomeGerund <- verbFromUDS uds -- :: GVP
      let gerundAdv = GGerundAdv vpToBecomeGerund -- :: GAdv
      let nmodAdv = GPrepNP prep np
      return $ GAdvVP (GAdvVP vp gerundAdv) nmodAdv

    _ -> case getRoot x of -- TODO: fill in other cases
                GrootV_ vp:_ -> Just vp
                GrootN_  np:_ -> Just $ GUseComp (GCompNP np)
                GrootA_  ap:_ -> Just $ GUseComp (GCompAP ap)
                GrootAdv_ a:_ -> Just $ GUseComp (GCompAdv a)
                -- TODO: add cases for
                -- GrootAdA_, GrootDet_ in the GF grammar, so we can add the cases here
                _            -> Nothing

-- TODO: use composOp to grab all (finite) UD labels and put them together nicely
clFromUDS :: GUDS -> Maybe GCl
clFromUDS x = case getNsubj x of
  [] -> Nothing  -- if the UDS doesn't have a subject, then it should be handled by vpFromUDS instead
  _ -> case x of
    Groot_nsubj root (Gnsubj_ np) -> GPredVP np <$> verbFromUDS (Groot_only root)
    Groot_nsubj_advmod root (Gnsubj_ np) (Gadvmod_ adv) -> do
      vp <- verbFromUDS (Groot_only root)
      Just $ GPredVP np (GAdvVP vp adv)
    Groot_nsubj_advmod_obj root (Gnsubj_ np) _ _ -> GPredVP np <$> verbFromUDS (Groot_only root)
    Groot_nsubj_aux_advmod root (Gnsubj_ np) _ _ -> GPredVP np <$> verbFromUDS (Groot_only root)
    Groot_nsubj_aux_advmod_obj_advcl root (Gnsubj_ np) _ _ _ _ -> GPredVP np <$> verbFromUDS (Groot_only root)
    Groot_nsubj_aux_obj root (Gnsubj_ np) _ _ -> GPredVP np <$> verbFromUDS (Groot_only root)
    Groot_nsubj_aux_obj_obl root (Gnsubj_ np) _ _ _ -> GPredVP np <$> verbFromUDS (Groot_only root)
    Groot_nsubj_aux_obj_obl_advmod_advcl root (Gnsubj_ np) _ _ _ _ _  -> GPredVP np <$> verbFromUDS (Groot_only root)
    Groot_nsubj_aux_obj_obl_obl root (Gnsubj_ np) _ _ _ _ -> GPredVP np <$> verbFromUDS (Groot_only root)
    Groot_nsubj_ccomp root (Gnsubj_ np) _ -> GPredVP np <$> verbFromUDS (Groot_only root)
    Groot_nsubj_cop root (Gnsubj_ np) _ -> GPredVP np <$> verbFromUDS (Groot_only root)
    Groot_nsubj_cop_aclRelcl root (Gnsubj_ np) _ _ -> GPredVP np <$> verbFromUDS (Groot_only root)
    Groot_nsubj_cop_advcl root (Gnsubj_ np) _ _ -> GPredVP np <$> verbFromUDS (Groot_only root)
    Groot_nsubj_cop_case_nmod_acl root (Gnsubj_ np) _ _ _ _  -> GPredVP np <$> verbFromUDS (Groot_only root)
    Groot_nsubj_cop_nmodPoss root (Gnsubj_ np) _ _ -> GPredVP np <$> verbFromUDS (Groot_only root)
    Groot_nsubj_obj root (Gnsubj_ np) _ -> GPredVP np <$> verbFromUDS (Groot_only root)
    Groot_nsubj_obj_xcomp root (Gnsubj_ np) _ _ -> GPredVP np <$> verbFromUDS (Groot_only root)
    Groot_nsubj_obl root (Gnsubj_ np) (Gobl_ adv) -> do
      vp <- verbFromUDS (Groot_only root)
      Just $ GPredVP np (GAdvVP vp adv)
    Groot_nsubj_obl_obl root (Gnsubj_ np) _ _ -> GPredVP np <$> verbFromUDS (Groot_only root)
    Groot_nsubj_xcomp root (Gnsubj_ np) _ -> GPredVP np <$> verbFromUDS (Groot_only root)
    Groot_nsubj_aux_obl root (Gnsubj_ np) _ _ -> GPredVP np <$> verbFromUDS (Groot_only root)
    Groot_nsubjPass_auxPass root (GnsubjPass_ np) _ -> GPredVP np <$> verbFromUDS (Groot_only root)
    Groot_nsubjPass_auxPass_advmod_advcl root (GnsubjPass_ np) _ _ _ -> GPredVP np <$> verbFromUDS (Groot_only root)
    Groot_nsubjPass_auxPass_advmod_xcomp root (GnsubjPass_ np) _ _ _ -> GPredVP np <$> verbFromUDS (Groot_only root)
    Groot_nsubjPass_auxPass_xcomp root (GnsubjPass_ np) _ _ -> GPredVP np <$> verbFromUDS (Groot_only root)
    Groot_nsubjPass_aux_auxPass root (GnsubjPass_ np) _ _ -> GPredVP np <$> verbFromUDS (Groot_only root)
    Groot_nsubjPass_aux_auxPass_obl_advmod root (GnsubjPass_ np) _ _ _ _ -> GPredVP np <$> verbFromUDS (Groot_only root)
    Groot_nsubjPass_aux_auxPass_obl_obl_advcl root (GnsubjPass_ np) _ _ _ _ _  -> GPredVP np <$> verbFromUDS (Groot_only root)
    Groot_nsubjPass_aux_auxPass_obl_obl_advmod root (GnsubjPass_ np) _ _ _ _ _  -> GPredVP np <$> verbFromUDS (Groot_only root)
    Groot_nsubj_cop_advmod root (Gnsubj_ np) _cop Gnot_advmod -> -- TODO: can we address the negation?
      GPredVP np <$> verbFromUDS (Groot_only root)

    _ -> case verbFromUDS x of -- TODO: fill in other cases
                Just vp -> Just $ GGenericCl vp
                _       -> Nothing

getRoot :: Tree a -> [Groot]
getRoot rt@(GrootA_ _) = [rt]
getRoot rt@(GrootN_ _) = [rt]
getRoot rt@(GrootV_ _) = [rt]
getRoot rt@(GrootDet_ _) = [rt]
getRoot rt@(GrootDAP_ _) = [rt]
getRoot rt@(GrootQuant_ _) = [rt]
getRoot rt@(GrootAdA_ _) = [rt]
getRoot rt@(GrootPrep_ _) = [rt]
getRoot rt@(GrootRP_ _) = [rt]
getRoot x = composOpMonoid getRoot x

getNsubj :: Tree a -> [Gnsubj]
getNsubj ns@(Gnsubj_ _) = [ns]
getNsubj x = composOpMonoid getNsubj x

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
