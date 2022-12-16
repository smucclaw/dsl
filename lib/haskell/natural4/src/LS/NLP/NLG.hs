{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, NamedFieldPuns, FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module LS.NLP.NLG where

import LS.NLP.UDExt
import LS.Types ( TemporalConstraint (..), TComparison(..),
      ParamText,
      Rule(..),
      BoolStructP, BoolStructR,
      RelationalPredicate(..), HornClause(..), RPRel(..), HasToken (tokenOf),
      Expect(..),
      rp2text, pt2text, bsr2text, KVsPair, HornClause2, BoolStructDTP)
import PGF ( readPGF, readLanguage, languages, CId, Expr, linearize, mkApp, mkCId, lookupMorpho, inferExpr, showType, ppTcError, PGF )
import qualified PGF
import UDAnnotations ( UDEnv(..), getEnv )
import qualified Data.Text as Text
import Data.Char (toLower, isUpper, toUpper, isDigit, isLower)
import UD2GF (getExprs)
import qualified AnyAll as AA
import Data.Maybe ( fromMaybe, catMaybes, mapMaybe )
import Data.List ( group, sort, sortOn, nub, intercalate )
import Data.List.Extra (groupOn, splitOn)
import Data.Either (partitionEithers)
import Debug.Trace (trace)
import qualified GF.Text.Pretty as GfPretty
import Data.List.NonEmpty (NonEmpty((:|)))
import UDPipe (loadModel, runPipeline, Model)
import Control.Monad (when, unless)
import System.Environment (lookupEnv)
import Control.Concurrent.Async (concurrently)
import Data.Set as Set (member, fromList)
import AnyAll.BoolStructTree
import qualified Data.Tree as DT


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
  mpn <- lookupEnv "MP_NLG"
  let verbose = maybe False (read :: String -> Bool) mpn
  (udEnv,udpipeModel) <- concurrently (
      getEnv (gfPath "UDApp") "Eng" "UDS"
    ) $ do
    when verbose $ putStrLn "\n-----------------------------\n\nLoading UDPipe model..."
    udpipeModel <- either error id <$> loadModel modelFilePath
    when verbose $ putStrLn "Loaded UDPipe model"
    pure udpipeModel
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
  unless (verbose env) $ -- when not verbose, just short output to reassure user we're doing something
    putStrLn ("    NLG.parseUD: parsing " <> "\"" <> Text.unpack txt <> "\"")
  lowerConll <- checkAllCapsIsWord txt
  -- when (verbose env) $ putStrLn ("\nconllu:\n" ++ lowerConll)
  expr <- either errorMsg pure (ud2gf lowerConll)
  -- let replaced = unwords $ swapBack (splitOn "propernoun" $ showExpr expr) nonWords
  -- when (verbose env) $ putStrLn ("The UDApp tree created by ud2gf:\n" ++ replaced)
  let uds = toUDS (pgfGrammar $ udEnv env) expr
  -- when (verbose env) $ putStrLn ("Converted into UDS:\n" ++ showExpr (gf uds))
  return uds
  where
    errorMsg msg = do
      putStrLn $ "parseUD: fail to parse " ++ Text.unpack txt ++ " with error message\n" ++ msg
      return $ dummyExpr ("not parsed: " ++ Text.unpack txt)

    udpipe :: Text.Text -> IO String
    udpipe txt = do
      let str = Text.unpack txt
      when (verbose env) $ putStrLn "Running UDPipe..."
      result <- runPipeline (udpipeModel env) str
      -- when (verbose env) $ putStrLn ("UDPipe result: " ++ show result) -- this is a bit extra verbose
      return $ either error id result

    ud2gf :: String -> Either String Expr
    ud2gf str = case getExprs ["no-backups"] (udEnv env) str of
      xs : _ -> case partitionEithers xs of
                  (_,  (r:_r)) -> Right r
                  ((l:_l), []) -> Left l
                  ([]  ,   []) -> Left "ud2gf: no results given for input"
      [] -> Left "ud2gf: tried parsing an empty input"

    swapBack :: [String] -> [String] -> [String]
    swapBack [] [] = []
    swapBack [] (_y:_ys) = []
    swapBack (x:xs) [] = x:xs
    swapBack (x:xs) (y:ys) = ((init x) ++ y) : swapBack xs ys

    checkIfChunk :: String -> Bool
    checkIfChunk x = checkDigit x || checkLower x || checkSymbol x
      where
        checkLower = not . any isLower
        checkDigit = any isDigit
        checkSymbol x = any (`Set.member` (Set.fromList ['#','§'])) x

    saveNonWords :: [String] -> [String] -> [[String]]
    saveNonWords [] _ls = []
    saveNonWords (x:xs) ls
      | checkIfChunk x = (x:ls) : saveNonWords xs ls
      | otherwise = saveNonWords xs ls

    swapChunk :: [String] -> [String]
    swapChunk [] = []
    swapChunk (x:xs)
      | checkIfChunk x = ("propernoun") : swapChunk xs
      | otherwise = x : swapChunk xs

    replaceChunks :: Text.Text -> [String]
    replaceChunks txt = swapChunk $ map Text.unpack $ Text.words txt
    -- Text.pack $ unwords $

    combinePROPERNOUN :: [[String]] ->[[String]]
    combinePROPERNOUN [] = []
    combinePROPERNOUN (x:xs)
      | head x == "propernoun" = [intercalate "_" x] : combinePROPERNOUN xs
      | otherwise = x : combinePROPERNOUN xs

    lowerButPreserveAllCaps :: Text.Text -> Text.Text
    lowerButPreserveAllCaps txt = Text.unwords
      [ if all isUpper (Text.unpack wd)
          then wd
          else Text.map toLower wd
      | wd <- Text.words txt
      ]

    checkAllCapsIsWord :: Text.Text -> IO String
    checkAllCapsIsWord txt = do
      lowerConll <- udpipe (Text.map toLower txt)
      defaultConll <- udpipe $ lowerButPreserveAllCaps txt
      let check | (map toLower lowerConll) == (map toLower defaultConll) = defaultConll
                | otherwise = lowerConll
      return check
-----------------------------------------------------------------------------

-- | rewrite statements into questions, for use by the Q&A web UI
-- 
-- +-----------------+-----------------------------------------------------+
-- | input           | the data breach, occurs on or after 1 Feb 2022      |
-- | output          | Did the data breach occur on or after 1 Feb 2022?   |
-- +-----------------+-----------------------------------------------------+
-- | input           | Organisation, NOT, is a Public Agency               |
-- | intermediate    | (AA.Not (...) :: BoolStructT                        |
-- | output          | Is the Organisation a Public Agency?                |
-- +-----------------+-----------------------------------------------------+
-- | input           | Claim Count <= 2                                    |
-- | intermediate    | RPConstraint (RPMT ["Claim Count"]) RelLTE          |
-- |                 |               (RPMT ["2"]) :: RelationalPredicate   |
-- | output          | Have there been more than two claims?               |
-- +-----------------+-----------------------------------------------------+

nlgQuestion :: NLGEnv -> Rule -> IO [Text.Text]
nlgQuestion env rl = do
  annotatedRule <- parseFields env rl
  -- TODO: here let's do some actual NLG
  gr <- nlgExtPGF
  let lang = head $ languages gr
  case annotatedRule of
    RegulativeA {subjA, whoA, condA, uponA} -> do
      let whoQuestions = concatMap (mkWhoQs gr lang subjA) $ catMaybes [whoA]
      -- print $ udsToTreeGroups (toUDS gr subj)
      -- print "flattened who"
      -- print $ map showExpr $ flattenGFTrees whoAsTG
          condQuestions = concatMap (mkCondQs gr lang subjA) $ catMaybes [condA, uponA]
      return $ map Text.pack $ whoQuestions ++ condQuestions
    HornlikeA {clausesA = cls} -> do
      let udfrags = map fg cls
          emptyExpr = gf (GString "")
          hcQuestions = concatMap (mkHCQs gr lang 0 (emptyExpr)) udfrags
      return $ map Text.pack hcQuestions
    _ -> do
      statement <- nlg env rl
      putStrLn ("nlgQuestion: no question to ask, but the regular NLG returns " ++ Text.unpack statement)
      return mempty --

  where
    mkHCQs :: PGF -> CId -> Int -> Expr -> GUDFragment -> [String]
    mkHCQs gr lang indentation emptE udfrag = case udfrag of
      GHornClause2 _ sent -> mkQs qsCond gr lang indentation emptE (sTG sent)
      GMeans _ uds -> mkQs qsCond gr lang indentation emptE (udsToTreeGroups uds)
      _ -> error $ "nlgQuestion.mkHCQs: unexpected argument " ++ showExpr (gf udfrag)

    mkWhoQs :: PGF -> CId -> Expr -> Expr -> [String]
    mkWhoQs gr lang subj e = trace ("whoA: " ++ showExpr e  ++ "\ntg: " ++ show tg) mkQs qsWho gr lang 2 subj tg
      where
        tg = case findType gr e of
              "VPS" -> vpTG $ fg e
              _     -> udsToTreeGroups (toUDS gr e)

    mkCondQs gr lang subj e = mkQs qsCond gr lang 2 subj (udsToTreeGroups (toUDS gr e))

    mkQs :: (Expr -> TreeGroups -> GQS) -> PGF -> CId -> Int -> Expr -> TreeGroups -> [String]
    mkQs qfun gr lang indentation s tg = case tg of
      TG {gfS = Just sent} -> case sent of
        GConjS _conj (GListS ss) -> concatMap (mkQs qfun gr lang (indentation+4) s) (sTG <$> ss)
        _ -> qnPunct $ lin indentation (qfun s $ sTG sent)
      TG {gfVP = Just vp} -> case vp of
        GConjVPS _conj (GListVPS vps) -> concatMap (mkQs qfun gr lang (indentation+4) s) (vpTG <$> vps)
        _ -> qnPunct $ lin indentation (qfun s $ vpTG vp)
      TG {gfNP = Just np} -> case np of
        GConjNP _conj (GListNP nps) -> concatMap (mkQs qfun gr lang (indentation+4) s) (npTG <$> nps)
        _ -> qnPunct $ lin indentation (qfun s $ npTG np)
      TG {gfCN = Just cn} -> case cn of
        GConjCN _conj (GListCN cns) -> concatMap (mkQs qfun gr lang (indentation+4) s) (cnTG <$> cns)
        _ -> qnPunct $ lin indentation (qfun s $ cnTG cn)
      TG {gfAP = Just ap} -> case ap of
        GConjAP _conj (GListAP aps) -> concatMap (mkQs qfun gr lang (indentation+4) s) (apTG <$> aps)
        _ -> qnPunct $ lin indentation (qfun s $ apTG ap)

      -- TG {gfDet = Just det} ->
      -- TG {gfAdv = Just adv} ->
      _ -> []

      where
        {-
        space :: Char
        space = ' '
        lin indentation x = [take indentation (repeat space) ++ linearize gr lang (gf x)]
        -}
        lin _indentation x = [linearize gr lang (gf x)]
        qnPunct :: [String] -> [String]
        qnPunct [] = []
        qnPunct [l] = [toUpper (head l) :( tail l ++ "?")]
        qnPunct (l:ls) = [toUpper (head l)] : tail l : concat ls : ["?"]

-- toHTML :: Text.Text -> String
-- toHTML str = Text.unpack $ either mempty id $ Pandoc.runPure $ Pandoc.writeHtml5String Pandoc.def =<< Pandoc.readMarkdown Pandoc.def str

-- toPDF :: Text.Text -> IO Byte.ByteString
-- toPDF str = do
--   template <- Pandoc.runIOorExplode $ Pandoc.getTemplate "template.tex"
--   Right pI guess the documentation emergesandTemplate <- Pandoc.compileTemplate "" template :: IO (Either String (Pandoc.Template Text.Text))
--   pdf <- Pandoc.runIOorExplode $ (Pandoc.makePDF "pdflatex" [] Pandoc.writeLaTeX (Pandoc.def {Pandoc.writerTemplate = Just pandTemplate}) =<< Pandoc.readMarkdown Pandoc.def str)
--   case pdf of
--     Left err -> do
--       Byte.hPutStrLn stderr err
--       exitFailure
--     Right bytePDF -> return bytePDF

nlg :: NLGEnv -> Rule -> IO Text.Text
nlg env rl = do
  --  print ("nlgQuestion")
  --  nlgquest <- nlgQuestion env rl
  --  print $ Text.unwords $ nlgquest
  --  print ("---")
   annotatedRule <- parseFields env rl
   -- TODO: here let's do some actual NLG
   gr <- nlgExtPGF
   let lang = head $ languages gr
   let Just eng = readLanguage "UDExtEng"
   let Just may = readLanguage "UDExtMay"
   case annotatedRule of
      RegulativeA {subjA, keywordA, whoA, condA, deonticA, actionA, temporalA, uponA, givenA} -> do
        let deonticAction = mkApp deonticA [gf $ toUDS gr actionA] -- TODO: or change type of DMust to take VP instead?
            subjWho = applyMaybe "Who" (gf . toUDS gr <$> whoA) (gf $ peelNP subjA)
            subj = mkApp keywordA [subjWho]
            king_may_sing = mkApp (mkCId "subjAction") [subj, deonticAction]
            existingQualifiers = [(name,expr) |
                                  (name,Just expr) <- [("Cond", gf . toUDS gr <$> condA),
                                                       ("Temporal", temporalA),
                                                       ("Upon", uponA),
                                                       ("Given", givenA)]]
            finalTree = doNLG existingQualifiers king_may_sing -- determine information structure based on which fields are Nothing
            linText = linearize gr eng finalTree
            linTree = showExpr finalTree
        return (Text.pack (linText ++ "\n" ++ linTree))
      HornlikeA {
        clausesA
      } -> do
        let linTrees_exprs = Text.unlines [
              Text.pack (linText ++ "\n" ++ linTree)
              | tree <- clausesA
              , let linText = unlines [
                                linearize gr eng tree
                              , linearize gr may tree ]
              , let linTree = showExpr tree ]

        return linTrees_exprs
      ConstitutiveA {nameA, condA, givenA} -> do
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
      DefNameAliasA { nameA, detailA } -> do
        let tree = maybe detailA gf (npFromUDS $ fg detailA)
            linText = linearize gr lang tree
            --linTree = showExpr tree
        return $ Text.pack (Text.unpack nameA ++ " AKA " ++ linText) -- ++ "\n" ++ linTree ++ "\n")
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
    parseHornClause env fun (HC rp Nothing) = parseRP env fun rp
    parseHornClause env fun (HC rp (Just bsr)) = do
      extGrammar <- nlgExtPGF -- use extension grammar, because bsr2gf can return funs from UDExt
      db_is_NDB_UDFragment <- parseRPforHC env fun rp
      db_occurred_S <- bsr2s env bsr
      let hornclause = GHornClause2 db_is_NDB_UDFragment db_occurred_S
      return $ gf hornclause

    bsr2s :: NLGEnv -> BoolStructR -> IO GS
    bsr2s env bsr = do
      expr <- bsr2gf env bsr
      extGrammar <- nlgExtPGF -- use extension grammar, because bsr2gf can return funs from UDExt
      return $ case findType extGrammar expr of
        "S" -> fg expr          -- S=[databreach occurred]
        "AP" -> ap2s $ fg expr  -- someone is AP=[happy]
        -- TODO: make other cats to S too
        _ -> error $ "bsr2s: expected S, got " ++ showExpr expr

    ap2s :: GAP -> GS
    ap2s ap = GPredVPS GSomeone (GMkVPS presSimul GPPos (GUseComp (GCompAP ap)))

    -- A wrapper for ensuring same return type for parseRP
    parseRPforHC :: NLGEnv -> CId -> RelationalPredicate -> IO GUDFragment
    parseRPforHC env _f (RPParamText pt) = (GUDS2Fragment . fg) <$> parseParamText env pt
    parseRPforHC env _f (RPMT txts) = (GUDS2Fragment . fg) <$> parseMulti env txts
    parseRPforHC env f rp = fg <$> parseRP env f rp

    parseExpect :: NLGEnv -> CId -> Expect -> IO Expr
    parseExpect _env _f (ExpDeontic _) = error "NLG/parseExpect unimplemented for deontic rules"
    parseExpect  env  f (ExpRP rp) = parseRP env f rp

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
    parseRP env fun (RPnary        _rprel rp) = parseRP env fun rp

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

bsp2gfDT :: NLGEnv -> BoolStructDTP -> IO Expr
bsp2gfDT env bsp = case bsp of
  (DT.Node (FAtom (action :| mods)) _    )  -> do
    actionExpr <- kvspair2gf env action  -- notify the PDPC
    modExprs <- mapM (kvspair2gf env) mods -- [by email, at latest at the deadline]
    return $ combineActionMods actionExpr modExprs
  _ -> error "bsp2gf: not supported yet"

-- | Takes the main action, a list of modifiers, and combines them into one Expr
combineActionMods :: (String,Expr) -> [(String, Expr)] -> Expr
combineActionMods (_, expr) [] = expr
combineActionMods ("CN",noun) (("RS",mod):rest) = combineActionMods ("CN", gf resultCN) rest
  where
    -- cnrs
    resultCN :: GCN
    resultCN = GRelCN (fg noun) (fg mod)
combineActionMods ("CN", noun) (("Adv",mod):rest) = combineActionMods ("CN", gf resultCN) rest
  where
    resultCN :: GCN
    resultCN = GAdvCN (fg noun) (fg mod)
combineActionMods ("CN", noun) (("NP", mod):rest) = combineActionMods ("CN", gf resultCN) rest
  where
    resultCN :: GCN
    resultCN = GPossNP (fg noun) (fg mod)
combineActionMods ("VPS",act) (("Adv",mod):rest) = combineActionMods ("VPS", gf resultVP) rest
  where
    resultVP :: GVPS
    resultVP = advVPS (fg act) (fg mod)

    advVPS :: GVPS -> GAdv -> GVPS
    advVPS vps adv = GMkVPS presSimul GPPos $ GAdvVP (vps2vp vps) adv

combineActionMods ("VPS",act) (("NP",mod):rest) = combineActionMods ("VPS", gf resultVP) rest
  where
    resultVP :: GVPS
    resultVP = npVPS (fg act) (fg mod)

    npVPS :: GVPS -> GNP -> GVPS
    npVPS vps np = GMkVPS presSimul GPPos $ GComplVP (vps2vp vps) np

combineActionMods ("VPS",act) (("RS",mod):rest) = combineActionMods ("VPS", gf resultVP) rest

  where
    -- Assumption: RCl doesn't modify the whole VP, but rather the object of the VP
    resultVP :: GVPS
    resultVP = rsVPS (fg act) (fg mod)

    rsVPS :: GVPS -> GRS -> GVPS
    rsVPS vps rs = case vps of
      GMkVPS t p vp -> case vp of
        GComplV   v  np -> GMkVPS t p $ GComplV v   (GRelNP np rs)
        GComplVP vp' np -> GMkVPS t p $ complVP vp' (GRelNP np rs)
        GUseComp (GCompNP np) -> GMkVPS t p $ GUseComp $ GCompNP (GRelNP np rs)
        _               -> GMkVPS t p $ complVP vp (GRelNP dummyNP rs)
      _ -> error $ "combineActionMods: expected VPS, got something else" -- ++ showExpr act

combineActionMods (tAct,_) ((tMods,_):_) = error $ "combineActionMods: not supported yet " ++ tAct ++ "+" ++ tMods

-- | Takes a KVsPair, parses the fields, puts them together into GF Expr
kvspair2gf :: NLGEnv -> KVsPair -> IO (String, Expr)
kvspair2gf env (action,_) = case action of
  pred :| []     -> do
    predUDS <- parseUD env pred
    return $ case udsToTreeGroups predUDS of
      TG {gfS=Just s}     -> ("S", gf s)
      TG {gfVP=Just v}    -> ("VP", gf v)
      TG {gfAdv=Just adv} -> ("Adv", gf adv)
      TG {gfAP=Just ap}   -> ("AP", gf ap)
      TG {gfNP=Just np}   -> ("NP", gf np)
      TG {gfDet=Just det} -> ("Det", gf det)
      TG {gfCN=Just cn}   -> ("CN", gf cn)
      TG {gfPrep=Just pr} -> ("Prep", gf pr)
      TG {gfRP=Just rp}   -> ("RP", gf rp)
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
      -- root_only (rootN_ (MassNP (UseN (StrN "propernoun"))))with complement root_nmod (rootDAP_ (DetDAP each_Det)) (nmod_ of_Prep (DetCN thePl_Det (AdjCN (PositA notifiable_A) (UseN individual_N))))

      -- TG {gfNP = Just propernoun} ->
      --   ("RS", case complTyped of
      --     TG {gfAdv = Just (Groot_nmod $ (GrootDAP_ (GDetDAP each)) (Gnmod_ of_Prep individuals))} -> gf $ GApposNP propernoun $ GAdvNP (GDetNP each) (GPrepNP of_Prep individuals)
      --   )
        -- each of the notifiable individuals

        -- root_nmod (rootDAP_ (DetDAP each_Det)) (nmod_ of_Prep (DetCN thePl_Det (AdjCN (PositA notifiable_A) (UseN individual_N))))

        -- root_only (rootN_ (MassNP (UseN (StrN "propernoun")))) with complement root_nmod (rootDAP_ (DetDAP each_Det)) (nmod_ of_Prep (DetCN thePl_Det (AdjCN (PositA notifiable_A) (UseN individual_N))))

      TG {gfRP=Just for_which} ->
        ("RS", case complTyped of
          TG {gfS= Just (GUseCl t p you_work)} -> gf $ GUseRCl t p $ GRelSlash for_which (GSlashCl you_work)
          TG {gfVP= Just (GMkVPS t p works)}   -> gf $ GUseRCl t p $ GRelVP for_which works
          _ -> error ("rp combineExpr: can't combine predicate " ++ showExpr predExpr ++ "with complement " ++ showExpr complExpr)
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
          TG {gfVP=Just (GMkVPS _t _p haunt)}   -> ("Adv", gf $ GPrepNP under (GGerundNP haunt))
          -- TG {gfS=Just you_see} -> ???
          _ -> error ("prep combineExpr: can't combine predicate " ++ showExpr predExpr ++ "with complement " ++ showExpr complExpr)

      TG {gfVP=Just (GMkVPS t p notify)} ->
        ("VPS", case complTyped of
          TG {gfS=Just you_see}  -> gf $ GMkVPS t p $ GComplSVP notify you_see
          TG {gfAP=Just haunted} -> gf $ GMkVPS t p $ complVP notify (GAdjAsNP haunted)
          TG {gfAdv=Just quickly}-> gf $ GMkVPS t p $ GAdvVP   notify quickly
          TG {gfNP=Just johnson} -> gf $ GMkVPS t p $ complVP notify johnson
          TG {gfDet=Just my}     -> gf $ GMkVPS t p $ complVP notify (GDetNP my)
          TG {gfCN=Just car}     -> gf $ GMkVPS t p $ complVP notify (GMassNP car)
          TG {gfPrep=Just with}  -> gf $ GMkVPS t p $ GPrepVP notify with
          -- TG {gfNP = Just (GDet each (GPrepNP of_Prep theindividual))} -> gf $ GMkVPS t p $ GAdvVP notify (GPrepNP of_Prep theindividual)
          -- TG {gfRP=Just which}   -> ("RP", gf $ GPrepRP under which)
          -- TG {gfVP=Just haunt}   -> ("Adv", gf $ GPrepNP under (GGerundNP haunt))

-- natural4-exe: vp combineExpr: can't combine predicate root_only (rootV_ (TTAnt TPres ASimul) PPos (UseV notify_V))with complement root_nmod (rootDAP_ (DetDAP each_Det)) (nmod_ of_Prep (DetCN thePl_Det (AdjCN (PositA notifiable_A) (UseN individual_N))))

          _ -> error ("vp combineExpr: can't combine predicate " ++ showExpr predExpr ++ "with complement " ++ showExpr complExpr ++ " pred " ++ show predTyped ++ " compl " ++ show complTyped)
        )
      TG {gfCN=Just house} ->
        ("CN", case complTyped of
          TG {gfCN=Just car}      -> gf $ GApposCN house (GMassNP car)
          TG {gfNP=Just johnson}  -> gf $ GApposCN house johnson
          TG {gfDet=Just my}      -> gf $ GApposCN house (GDetNP my)
          TG {gfAdv=Just quickly} -> gf $ GAdvCN  house quickly
          TG {gfAP=Just haunted}  -> gf $ GAdjCN  haunted house

          _ -> error ("cn combineExpr: can't combine predicate " ++ showExpr predExpr ++ "with complement " ++ showExpr complExpr)
        )
      TG {gfNP=Just the_customer} ->
        ("NP", case complTyped of
          TG {gfCN=Just house}    -> gf $ GGenModNP GNumSg the_customer house
          TG {gfNP=Just johnson}  -> gf $ GApposNP  the_customer johnson
          TG {gfDet=Just my}      -> gf $ GApposNP  the_customer (GDetNP my)
          TG {gfAdv=Just quickly} -> gf $ GAdvNP    the_customer quickly
          TG {gfAP=Just haunted}  -> gf $ GApposNP  the_customer (GAdjAsNP haunted)
          _ -> error ("np combineExpr: can't combine predicate " ++ showExpr predExpr ++ "with complement " ++ showExpr complExpr)
        )
      TG {gfDet=Just any} -> case complTyped of
          TG {gfCN=Just house}   -> ("NP", gf $ GDetCN any house)
          TG {gfAP=Just haunted} -> ("DAP", gf $ GAdjDAP (GDetDAP any) haunted)
          _ -> error ("det combineExpr: can't combine predicate " ++ showExpr predExpr ++ "with complement " ++ showExpr complExpr)
      TG {gfAdv=Just happily} -> case complTyped of
        TG {gfCN=Just house}   -> ("CN", gf $ GAdvCN house happily)
        TG {gfNP=Just johnson} -> ("NP", gf $ GAdvNP johnson happily)
        TG {gfAP=Just haunted} -> ("AP", gf $ GAdvAP haunted happily)
        _ -> error ("adv combineExpr: can't combine predicate " ++ showExpr predExpr ++ "with complement " ++ showExpr complExpr)
      TG {gfAP=Just happy} -> case complTyped of
        TG {gfCN=Just house}    -> ("CN", gf $ GAdjCN happy house)
        TG {gfNP=Just johnson}  -> ("NP", gf $ GApposNP (GAdjAsNP happy) johnson)
        TG {gfDet=Just my}      -> ("DAP", gf $ GAdjDAP (GDetDAP my) happy)
        TG {gfAdv=Just quickly} -> ("AP", gf $ GAdvAP happy quickly)
        TG {gfAP=Just (GPositA haunted)} -> ("AP", gf $ GAdvAP happy (GPositAdvAdj haunted))
        _ -> error ("ap combineExpr: can't combine predicate " ++ showExpr predExpr ++ "with complement " ++ showExpr complExpr)
      -- ("Cl", you_work) -> case complTyped of
      --   ("RP", for_which) -> ("RS", gf $ GRelSlash (fg for_which) (GSlashCl (fg you_work)))
      --   _ -> error ("combineExpr: can't combine predicate " ++ showExpr predExpr ++ "with complement " ++ showExpr complExpr)
      tg -> error ("all combineExpr: can't find type " ++ show tg ++ " for the predicate " ++ showExpr predExpr)


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
  Left te -> error $ "Tried to infer type of:\n\t* " ++ showExpr e  ++ "\nGot the error:\n\t* " ++ GfPretty.render (ppTcError te) -- gives string of error
  Right (_, typ) -> showType [] typ -- string of type

-- Given a list of ambiguous words like
-- [[access_V, access_N], [use_V, use_N], [copying_N, copy_V], [disclosure_N]]
-- return a disambiguated list: [access_N, use_N, copying_N, disclosure_N]
disambiguateList :: PGF -> [[PGF.Expr]] -> [PGF.Expr]
disambiguateList pgf access_use_copying_raw =
  if all isSingleton access_use_copying
    then concat access_use_copying
    else [ w | ws <- access_use_copying
         , w <- ws
         , findType pgf w == unambiguousType (map (map (findType pgf)) access_use_copying)
    ]
  where
    access_use_copying = map (map $ mkPhrasal pgf) access_use_copying_raw
    mkPhrasal :: PGF -> Expr -> Expr
    mkPhrasal pgf e = case findType pgf e of
      "N"  -> gf (GMassNP (GUseN (fg e)))
      "A"  -> gf (GPositA (fg e))
      "V"  -> gf (GUseV (fg e))
      _ -> e
    unambiguousType :: [[String]] -> String
    unambiguousType access_use_copying
      | not (any isSingleton access_use_copying) =
         -- all wordlists are either empty or >1: take the most frequent cat
         mostFrequentCat access_use_copying
      | otherwise =
         -- Some list has exactly 1 cat, use it to disambiguate the rest
         head $ head $ sortOn length access_use_copying

    mostFrequentCat :: [[String]] -> String
    mostFrequentCat cats = bestGuess
      where                                              -- cats = [["N","A"],["A","N","V"],["A","V","N"]]
        groups = Data.List.group $ Data.List.sort $ concat cats -- [["A","A","A"],["N","N","N"],["V","V"]]
        bestGuess = case head $ groupOn length groups of
          [(cat:_)] -> cat
          catss     -> preferCat "VP" catss -- TODO: instead of hardcoding some cat here, later put in NLGEnv the category we are currently parsing

    preferCat :: String -> [[String]] -> String
    preferCat _ [[]] = error $ "disambiguateList: failed with arguments=" ++ show access_use_copying
    preferCat heuristicBestCat frequentCats =
      if heuristicBestCat `elem` map head frequentCats
        then heuristicBestCat -- if there are equally many of the best guess & other cats, might as well choose our best guess
        else head $ head frequentCats


    isSingleton [_] = True
    isSingleton _ = False

-- this function is only to keep track of preferred order
-- if it can be parsed as S, use the S
-- if not, then try VP, if not, then try AP, etc.
treeContents :: GConj -> [GUDS] -> Expr
treeContents conj contents = case groupByRGLtype conj contents of
  TG {gfS    = Just x} -> gf x
--  TG {gfVP   = Just x, gfAP = Nothing , gfCN = Nothing , gfNP = Nothing, gfAdv = Nothing} -> gf x
  -- TODO match
  TG {gfVP   = Just x, gfAP = Just y} -> gf $ GConjVPS conj (GListVPS [x, GMkVPS presSimul GPPos (GUseComp (GCompAP y))])
  TG {gfVP   = Just (GMkVPS _ _ (GUseComp (GCompAP x)))} -> gf x
  TG {gfVP   = Just (GMkVPS _ _ (GUseComp (GCompNP x)))} -> gf x
  -- TG {gfVP   = Just (GMkVPS _ _ (GUseComp (GCompCN x)))} -> gf x
  TG {gfVP   = Just (GMkVPS _ _ (GUseComp (GCompAdv x)))} -> gf x
  TG {gfVP   = Just x} -> gf x
  TG {gfAdv  = Just x} -> gf x
  TG {gfAP   = Just x} -> gf x
  TG {gfNP   = Just x} -> gf x
  TG {gfCN   = Just x} -> gf x
  TG {gfDet  = Just x} -> gf x
  TG {gfRP   = Just x} -> gf x
  TG {gfPrep = Just x} -> gf x
  _                    -> error $ "treeContents: no contents"

treePre :: GConj -> [GUDS] -> GUDS -> Expr
treePre conj contents pre = case groupByRGLtype conj <$> [contents, [pre]] of
  [TG {gfCN=Just cn}, TG {gfAP=Just ap}] -> gf $ GAdjCN ap cn
  [TG {gfNP = Just np}, TG {gfVP = Just vp}] -> gf $ predVPS np vp
  _ -> trace ("bsr2gf: can't handle the combination pre=" ++ showExpr (gf pre) ++ "+ contents=" ++ showExpr (treeContents conj contents))
           $ treeContents conj contents

treePrePost :: GConj -> [GUDS] -> GUDS -> GUDS -> Expr
treePrePost conj contents pre post =
  case groupByRGLtype conj <$> [contents, [pre], [post]] of
          [TG {gfCN=Just cn}, _, TG {gfAdv=Just (GPrepNP _of personal_data)}] ->
            let listcn = case cn of
                  GConjCN _ cns -> cns
                  _ -> GListCN [cn, cn]
            in constructTreeAPCNsOfNP listcn conj personal_data pre
          [TG {gfDet=Just det}, TG {gfS=Just (GUseCl t p cl)}, TG {gfCN=Just cn}] ->
            let obj = GDetCN det cn
            in case cl of
                  GPredVP np vp -> gf $ GUseCl t p $ GPredVP np (complVP vp obj)
                  GGenericCl vp -> gf $ GUseCl t p $ GGenericCl (complVP vp obj)
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
    putStrLn ("bsr2gf: Any Nothing\n" ++ show existingTrees)
    return $ treeContents orConj contentsUDS

  AA.All Nothing contents -> do
    contentsUDS <- parseAndDisambiguate env contents
    -- let existingTrees = groupByRGLtype andConj contentsUDS
    --putStrLn ("bsr2gf: All Nothing\n" ++ show existingTrees)
    return $ treeContents andConj contentsUDS

  AA.Any (Just (AA.PrePost any_unauthorised of_personal_data)) access_use_copying -> do
    contentsUDS <- parseAndDisambiguate env access_use_copying
    premodUDS <- parseUD env any_unauthorised
    postmodUDS <- parseUD env of_personal_data
    return $ treePrePost orConj contentsUDS premodUDS postmodUDS

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

-- | A data structure for GF trees, which has different bins for different RGL categories.
--
-- A single UDS tree may become several of these; e.g. a root_nsubj sentence pattern could become
-- a S, "a breach occurs", but also a NP, "an occurring breach".
-- The different NLG functions make their decisions on how to combine phrases based on which fields are filled.
data TreeGroups = TG {
    gfAP   :: Maybe GAP  -- 1
  , gfAdv  :: Maybe GAdv -- 2
  , gfNP   :: Maybe GNP  -- 3
  , gfDet  :: Maybe GDet -- 4
  , gfCN   :: Maybe GCN  -- 5
  , gfPrep :: Maybe GPrep -- 6
  , gfRP   :: Maybe GRP  -- 7
  , gfVP   :: Maybe GVPS  -- 8
  , gfS    :: Maybe GS  -- 9
   } deriving (Eq)

emptyTG :: TreeGroups
emptyTG = TG Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

npTG :: GNP -> TreeGroups
npTG np = emptyTG {gfNP = Just np}

apTG :: GAP -> TreeGroups
apTG ap = emptyTG {gfAP = Just ap}

cnTG :: GCN -> TreeGroups
cnTG cn = emptyTG {gfCN = Just cn}

vpTG :: GVPS -> TreeGroups
vpTG vp = emptyTG {gfVP = Just vp}

sTG :: GS -> TreeGroups
sTG s = emptyTG {gfS = Just s}

-- | for documentation: which RGL types are accepted currently
acceptedRGLtypes :: String
acceptedRGLtypes = "AP Adv NP Det CN Prep RP VPS S"

instance Show TreeGroups where
  show tg = case flattenGFTrees tg of
             [] -> "the TreeGroups is empty"
             xs -> unlines $ map showExpr xs

-- | Workaround to flatten TreeGroups into a list of Exprs.
flattenGFTrees :: TreeGroups -> [Expr]
flattenGFTrees TG {gfAP, gfAdv, gfNP, gfDet, gfCN, gfPrep, gfRP, gfVP, gfS} =
  gfAP <: gfAdv <: gfNP <: gfCN <: gfDet <: gfPrep <: gfRP <: gfVP <: gfS <: []
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

qsWho :: Expr -> TreeGroups -> GQS
qsCond :: Expr -> TreeGroups -> GQS
qsHaving :: Expr -> TreeGroups -> GQS

qsWho subj whichTG = case whichTG of
  TG {gfS = Just (GUseCl t _p cl)} -> GUseQCl t GPPos $ GQuestCl (definiteNP cl) -- is the cat cute?
  TG {gfNP = Just np} -> useQCl $ GQuestCl $ GPredVP sub (GUseComp (GCompNP (indefiniteNP np))) -- are you the cat? (if it was originally MassNP, becomes "are you a cat")
  TG {gfCN = Just cn} -> useQCl $ GQuestCl $ GPredVP sub (GUseComp (GCompNP (GDetCN (LexDet "aSg_Det") cn))) -- are you a cat?
  TG {gfVP = Just (GMkVPS t _p vp)} -> GUseQCl t GPPos $ GQuestCl $ GPredVP sub vp -- do you eat cat food?
  TG {gfAP = Just ap} -> useQCl $ GQuestCl $ GPredVP sub (GUseComp (GCompAP ap))
  TG {gfDet = Just det} -> useQCl $ GQuestCl $ GPredVP sub (GUseComp (GCompNP (GDetNP det)))
  TG {gfAdv = Just adv} -> useQCl $ GQuestCl $ GPredVP sub (GUseComp $ GCompAdv adv)
  _ -> useQCl $ GQuestCl dummyCl
  where sub = definiteNP $ peelNP subj


qsCond _sub whichTG = case whichTG of
  TG {gfS = Just (GUseCl t _p cl)} -> GUseQCl t GPPos $ GQuestCl (definiteNP cl) -- is the cat cute?
  TG {gfNP = Just np} -> GExistNPQS presSimul GPPos (indefiniteNP np) -- is there a cat?
  TG {gfCN = Just cn} -> useQCl $ GQuestCl $ GExistCN cn -- is there a cat?
  TG {gfVP = Just (GMkVPS t _p vp)} -> GUseQCl t GPPos $ GQuestCl $ GPredVP GSomeone vp -- does someone eat cat food?
  TG {gfAP = Just ap} -> useQCl $ GQuestCl $ GExistsNP (GAdjAsNP ap) -- is there a green one?
  TG {gfDet = Just det} -> useQCl $ GQuestCl $ GExistsNP (GDetNP det) -- is there this?
  TG {gfAdv = Just adv} -> useQCl $ GQuestCl $ GPredVP GSomeone (GUseComp $ GCompAdv adv) -- is someone here?
  _ -> useQCl $ GQuestCl dummyCl

qsHaving = undefined

getQSFromTrees :: TreeGroups -> GQS
getQSFromTrees whichTG = case whichTG of
  TG {gfS = Just (GUseCl t p cl)} -> GUseQCl t p $ GQuestCl (definiteNP cl)
  TG {gfNP = Just np} -> GExistNPQS presSimul GPPos (indefiniteNP np)
  TG {gfCN = Just cn} -> useQCl $ GQuestCl $ GExistCN cn
  TG {gfVP = Just (GMkVPS t p vp)} -> GUseQCl t p $ GQuestCl $ GPredVP GYou vp -- how to get what or who?
  TG {gfAP = Just ap} -> useQCl $ GQuestIComp (GICompAP ap) (GAdjAsNP ap)
  TG {gfDet = Just det} -> GExistNPQS presSimul GPPos $ GDetNP det
  TG {gfAdv = Just adv} -> useQCl $ GQuestCl (GImpersCl (GUseComp $ GCompAdv adv))
  _ -> useQCl $ GQuestCl dummyCl

definiteNP :: forall a . Tree a -> Tree a
definiteNP np@(GDetCN (LexDet "theSg_Det") _) = np
definiteNP np@(GDetCN (LexDet "thePl_Det") _) = np
definiteNP t@(GComplV _ _) = t -- don't change objects
definiteNP (GDetCN _ cn) = GDetCN (LexDet "theSg_Det") cn
definiteNP (GMassNP cn) = GDetCN (LexDet "theSg_Det") cn
definiteNP x = composOp definiteNP x


indefiniteNP :: forall a . Tree a -> Tree a
indefiniteNP np@(GDetCN (LexDet "aSg_Det") _) = np
indefiniteNP np@(GDetCN (LexDet "aPl_Det") _) = np
indefiniteNP (GDetCN _ cn) = GDetCN (LexDet "aSg_Det") cn
indefiniteNP (GMassNP cn) = GDetCN (LexDet "aSg_Det") cn
indefiniteNP x = composOp indefiniteNP x

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
groupByRGLtype conj contentsUDS = TG treeAP treeAdv treeNP treeDet treeCN treePrep treeRP treeVP treeS
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

    treeVP :: Maybe GVPS
    treeVP = case mapMaybe verbFromUDS contentsUDS :: [GVPS] of
                []    -> Nothing
                [vp]  -> Just vp
                vps   -> Just $ GConjVPS conj (GListVPS vps)

    treePrep :: Maybe GPrep
    treePrep = case mapMaybe prepFromUDS contentsUDS :: [GPrep] of
                []     -> Nothing
                [prep] -> Just prep
                preps  -> Just $ GConjPrep conj (GListPrep preps)


    treeRP :: Maybe GRP
    treeRP = case mapMaybe rpFromUDS contentsUDS :: [GRP] of
                []    -> Nothing
                r:_  -> Just r

    treeS :: Maybe GS
    treeS = case mapMaybe sFromUDS contentsUDS :: [GS] of
                []  -> Nothing
                [s] -> Just s
                ss  -> Just $ GConjS conj (GListS ss)

parseAndDisambiguate :: NLGEnv -> [BoolStructR] -> IO [GUDS]
parseAndDisambiguate env text = do
  contentsAmb <- mapM (bsr2gfAmb env) text
  let parsingGrammar = pgfGrammar $ udEnv env -- here we use the parsing grammar, not extension grammar!
      contents = disambiguateList parsingGrammar contentsAmb
  -- putStrLn ("parseAndDisambiguate.contentsAmb = " ++ show (map (map showExpr) contentsAmb))
  -- putStrLn ("parseAndDisambiguate.contents = " ++ unwords (map showExpr contents))
  -- putStrLn ("parseAndDisambiguate: map toUDS contents = " ++ (unwords (map (showExpr . gf . toUDS parsingGrammar) contents)))
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
  "VP" -> Groot_only (GrootV_ presSimul GPPos (fg e))
  "VPS" -> vps2uds (fg e)
  "V"  -> Groot_only (GrootV_ presSimul GPPos (GUseV (fg e)))
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
             _ -> fg $ dummyExpr ("unable to convert to UDS rcl: " ++ showExpr e )
  "Cl" -> case fg e :: GCl of
            GPredVP np vp -> Groot_nsubj (GrootV_ presSimul GPPos vp) (Gnsubj_ np)
            GGenericCl vp -> toUDS pgf (gf vp)
            _ -> fg  $ dummyExpr ("unable to convert to UDS cl: " ++ showExpr e )
  "S" -> case fg e :: GS of
    GUseCl t p (GPredVP np vp) -> Groot_nsubj (GrootV_ t p vp) (Gnsubj_ np)
    _ -> fg  $ dummyExpr ("unable to convert to UDS S: " ++ showExpr e )
    -- vps2vp (GConjVPS c (GListVPS vps)) = GConjVP c (GListVP (map vps2vp vps))
  _ -> fg $ dummyExpr $ "unable to convert to UDS all: " ++ showExpr e
  where
    vps2uds :: GVPS -> GUDS
    vps2uds (GMkVPS t p vp) = Groot_only (GrootV_ t p vp)
    vps2uds vps = Groot_only (GrootV_ presSimul GPPos (vps2vp vps)) -- This causes trouble in other places TODO investigate

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
useRCl = GUseRCl presSimul GPPos

useCl :: GCl -> GS
useCl = GUseCl presSimul GPPos

--     UseQCl   : Temp -> Pol -> QCl -> QS ;
useQCl :: GQCl -> GQS
useQCl = GUseQCl presSimul GPPos

presSimul :: GTemp
presSimul = GTTAnt GTPres GASimul

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
  Groot_nsubj (GrootV_ _t _p vp) (Gnsubj_ someNP) -> Just $ GSentNP someNP (GEmbedVP vp)
  -- assessment (that sucks)
  Groot_aclRelcl (GrootN_ np) (GaclRelclUDSRP_ _rp relcl) -> Just $ GRelNP np (udRelcl2rglRS relcl)
  -- the occurence at the beach
  Groot_nmod (GrootN_ rootNP) (Gnmod_ prep nmodNP) -> Just $ GAdvNP rootNP (GPrepNP prep nmodNP)
  -- each of the notifiable individuals
  -- Groot_nmod (GrootDAP_ det_DAP) (Gnmod_ prep )
  -- service from the provider to the payer
  Groot_nmod_nmod (GrootN_ service_NP) (Gnmod_ from_Prep provider_NP) (Gnmod_ to_Prep payer_NP) -> Just $ GAdvNP (GAdvNP service_NP (GPrepNP from_Prep provider_NP)) (GPrepNP to_Prep payer_NP)
  -- great harm that she suffered
  Groot_acl (GrootN_ great_harm_NP) (GaclUDS_ (Groot_nsubj (GrootV_ _temp _pol suffer_VP) (Gnsubj_ she_NP))) -> Just $ GRelNP great_harm_NP (GRS_that_NP_VP she_NP suffer_VP)


  _ -> case getRoot x of -- TODO: fill in other cases
              GrootN_ np:_ -> Just np
              _            -> Nothing
-- | Constructs a RGL RS from a UDS.
udRelcl2rglRS :: GUDS -> GRS
udRelcl2rglRS uds = case uds of
  Groot_nsubj (GrootV_ t p vp) _ -> vp2rs (GMkVPS t p vp) -- TODO: check if nsubj contains something important
  _ -> maybe err vp2rs (verbFromUDS uds)
  where
    vp2rs :: GVPS -> GRS
    vp2rs (GMkVPS t p vp) = GUseRCl t p (GRelVP GIdRP vp)
    vp2rs vps = useRCl (GRelVP GIdRP (vps2vp vps))
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
  Groot_advmod (GrootV_ _ _ v) (Gadvmod_ adv) -> Just $ GAdvAP (GPastPartAP v) adv
  Groot_ccomp (GrootA_ ap) (GccompMarkUDS_ (Gmark_ subj) uds) -> do
    sent <- sFromUDS uds
    pure $ GAdvAP ap (GSubjS subj sent)
  _ -> case getRoot x of -- TODO: fill in other cases
              GrootA_ ap:_ -> Just ap
              _            -> Nothing

advFromUDS :: GUDS -> Maybe GAdv
advFromUDS x = case x of
  GaddMark (Gmark_ subj) uds -> do
    s <- sFromUDS uds
    pure $ GSubjS subj s
  Groot_only (GrootAdv_ someAdv) -> Just someAdv
  Groot_obl (GrootAdv_ someAdv) (Gobl_ oblAdv) -> Just $ GAdvAdv someAdv oblAdv
  -- very much overfitted to catch "unless we go where it's warm"
  Groot_nmod  (GrootDAP_ (GDetDAP det)) (Gnmod_ prep np) -> Just $ GPrepNP prep (GApposNP (GDetNP det) np)
  Groot_advcl (GrootAdv_ whereItsWarm) (GadvclMarkUDS_ (Gmark_ subj) uds) -> do
    weGo <- sFromUDS uds
    let weGoWarm = GPostAdvS weGo whereItsWarm
    pure $ GSubjS subj weGoWarm
  _ -> case [ adv | GrootAdv_ adv <- getRoot x] of
         adv:_ -> Just adv
         []    -> Nothing
{-         []    -> trace errorMsg Nothing
  where
    uds = showExpr (gf x)
    errorMsg = unlines $
      [ "advFromUDS: caught " ++ uds ++ ", couldn't turn it into an Adv."
      , "getRoot " ++ uds ++ " returns:"]
      ++ (showExpr . gf <$> getRoot x)
-}

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

verbFromUDS :: GUDS -> Maybe GVPS
verbFromUDS = verbFromUDS' False

verbFromUDS' :: Bool -> GUDS -> Maybe GVPS
verbFromUDS' verbose x = case getNsubj x of
  (_:_) ->
    if verbose
      then trace ("\n\n **** vpFromUDS: has a nsubj in " ++ showExpr (gf x)) Nothing
      else Nothing
--  (_:_) -> Nothing  -- if the UDS has a subject, then it should be handled by sFromUDS instead
  [] -> case x of    -- no nsubj, move on to pattern match UDS constructors
    Groot_obl (GrootV_ t p vp) (Gobl_ adv) -> Just $ GMkVPS t p $ GAdvVP vp adv
    Groot_obj (GrootV_ t p vp) (Gobj_ np) -> Just $ GMkVPS t p $ complVP vp np
    Groot_obl_obj (GrootV_ t p vp) (Gobl_ adv) (Gobj_ obj) -> Just $ GMkVPS t p $ GAdvVP (complVP vp obj) adv
    Groot_obj_obl (GrootV_ t p vp) (Gobj_ obj) (Gobl_ adv) -> Just $ GMkVPS t p $ GAdvVP (complVP vp obj) adv
    Groot_obl_obl (GrootV_ t p vp) (Gobl_ obl1) (Gobl_ obl2) -> Just $ GMkVPS t p $ GAdvVP (GAdvVP vp obl1) obl2
    Groot_obl_xcomp (GrootV_ t p vp) (Gobl_ obl) (GxcompAdv_ xc) -> Just $ GMkVPS t p $ GAdvVP (GAdvVP vp obl) xc
    Groot_xcomp (GrootV_ t p vp) (GxcompAdv_ adv) -> Just $ GMkVPS t p $ GAdvVP vp adv
    Groot_advmod (GrootV_ t p vp) (Gadvmod_ adv) ->
      Just $ GMkVPS t p $ GAdvVP vp adv
    Groot_acl_nmod root         (GaclUDSgerund_ uds) (Gnmod_ prep np) -> do
      GMkVPS t p vp <- verbFromUDS (Groot_only root) -- recursively calling verbFromUDS, now with a UDS that is guaranteed to go to the _ case below, and getRoot will be called, and a VP will be constructed
      GMkVPS _ _ vpToBecomeGerund <- verbFromUDS uds -- :: GVPS
      let gerundAdv = GGerundAdv vpToBecomeGerund -- :: GAdv
      let nmodAdv = GPrepNP prep np
      return $ GMkVPS t p $ GAdvVP (GAdvVP vp gerundAdv) nmodAdv

    _ -> case getRoot x of -- TODO: fill in other cases
                GrootV_ t p vp:_ -> Just $ GMkVPS t p vp
                GrootVaux_ t p aux vp:_ -> Just $ GComplAux aux t p vp ;
                -- Here we want only verby roots, for other root constructors we use root2vps!
                _ -> if verbose
                      then trace ("\n\n **** verbFromUDS: couldn't match " ++ showExpr (gf x)) Nothing
                      else Nothing

-- | Two first cases overlap with verbFromUDS: rootV_ and rootVaux_ always become VPS.
-- Rest don't, because this is called for any root ever that we want to turn into VPS.
root2vps :: Groot -> Maybe GVPS
root2vps root = case root of
  GrootV_ t p vp -> Just $ GMkVPS t p vp
  GrootVaux_ t p aux vp -> Just $ GComplAux aux t p vp ;
  GrootN_  np -> Just $ GMkVPS presSimul GPPos $ GUseComp (GCompNP np)
  GrootA_  ap -> Just $ GMkVPS presSimul GPPos $ GUseComp (GCompAP ap)
  GrootAdv_ a -> Just $ GMkVPS presSimul GPPos $ GUseComp (GCompAdv a)
  -- TODO: add cases fora
  -- GrootAdA_, GrootDet_ in the GF grammar, so we can add the cases here
  _            -> Nothing


scFromUDS :: GUDS -> Maybe GSC
scFromUDS x = case sFromUDS x of
  Just s -> pure $ GEmbedS s
  _ -> case verbFromUDS x of
    Just (GMkVPS _t _p vp) -> pure $ GEmbedVP vp
    _ -> error $ "scFromUDS: can't handle " ++ showExpr (gf x)

-- TODO: use composOp to grab all (finite) UD labels and put them together nicely
sFromUDS :: GUDS -> Maybe GS
sFromUDS x = case getNsubj x of
  --[] -> trace ("\n\n **** sFromUDS: no nsubj in " ++ showExpr (gf x)) Nothing  -- if the UDS doesn't have a subject, then it should be handled by vpFromUDS instead
  [] -> Nothing
  _ -> case x of
    Groot_expl_cop_csubj root _expl _cop csubj -> do
      GMkVPS t p vp <- (root2vps root)
      let pred = GAdvVP vp (Gcsubj2Adv csubj)
      pure $ GUseCl t p $ GImpersCl pred
    Groot_nsubj root (Gnsubj_ np) -> predVPS np <$> root2vps root
    Groot_csubj root (Gcsubj_ cs) -> do
      GMkVPS t p vp <- (root2vps root)
      sc <- scFromUDS cs
      pure $ GUseCl t p $ GPredSCVP sc vp
    Groot_nsubj_advmod root (Gnsubj_ np) (Gadvmod_ adv) -> do
      GMkVPS t p vp <- (root2vps root)
      pure $ GUseCl t p $ GPredVP np (GAdvVP vp adv)
    Groot_nsubj_obj_advcl root (Gnsubj_ subj) (Gobj_ obj) advcl -> do
      GMkVPS t p vp <- (root2vps root)
      let adv = Gadvcl2Adv advcl
          pred = GAdvVP (complVP vp obj) adv
      pure $ GUseCl t p $ GPredVP subj pred
    Groot_nsubj_advmod_obj root (Gnsubj_ np) _ _ -> predVPS np <$> root2vps root
    Groot_nsubj_aux_advmod root (Gnsubj_ np) _ _ -> predVPS np <$> root2vps root
    Groot_nsubj_aux_advmod_obj_advcl root (Gnsubj_ np) _ _ _ _ -> predVPS np <$> root2vps root
    Groot_nsubj_aux_cop_nmod root (Gnsubj_ np)_ _ _ -> predVPS np <$> root2vps root
    Groot_nsubj_aux_obj root (Gnsubj_ np) _ _ -> predVPS np <$> root2vps root
    Groot_nsubj_aux_obj_obl root (Gnsubj_ np) _ _ _ -> predVPS np <$> root2vps root
    Groot_nsubj_aux_obj_obl_advmod_advcl root (Gnsubj_ np) _ _ _ _ _  -> predVPS np <$> root2vps root
    Groot_nsubj_aux_obj_obl_obl root (Gnsubj_ np) _ _ _ _ -> predVPS np <$> root2vps root
    Groot_nsubj_ccomp root (Gnsubj_ np) _ -> predVPS np <$> root2vps root
    Groot_nsubj_cop root (Gnsubj_ np) _ -> predVPS np <$> root2vps root
    Groot_nsubj_cop_aclRelcl root (Gnsubj_ np) _ _ -> predVPS np <$> root2vps root
    Groot_nsubj_cop_advcl root (Gnsubj_ np) _ _ -> predVPS np <$> root2vps root
    Groot_nsubj_cop_case_nmod_acl root (Gnsubj_ np) _ _ _ _  -> predVPS np <$> root2vps root
    Groot_nsubj_cop_nmodPoss root (Gnsubj_ np) _ _ -> predVPS np <$> root2vps root
    Groot_nsubj_obj root (Gnsubj_ np) obj -> predVPS np <$> verbFromUDSVerbose (Groot_obj root obj)
    Groot_nsubj_obj_xcomp root (Gnsubj_ np) _ _ -> predVPS np <$> root2vps root
    Groot_nsubj_obl root (Gnsubj_ np) (Gobl_ adv) -> do
      GMkVPS t p vp <- (root2vps root)
      pure $ GUseCl t p $ GPredVP np (GAdvVP vp adv)
    Groot_nsubj_obl_obl root (Gnsubj_ np) _ _ -> predVPS np <$> root2vps root
    Groot_nsubj_xcomp root (Gnsubj_ np) _ -> predVPS np <$> root2vps root
    Groot_nsubj_aux_obl root (Gnsubj_ np) _ _ -> predVPS np <$> root2vps root
    Groot_obj_ccomp root (Gobj_ obj) _ -> predVPS obj <$> root2vps root
    Groot_xcomp root xcomp -> case xcomp of
      GxcompN_ np -> predVPS np <$> root2vps root
      GxcompToBeN_ _ _ np -> predVPS np <$> root2vps root
    -- todo: add other xcomps
    GaddMark (Gmark_ subj) (Groot_nsubj_cop root (Gnsubj_ nsubj) cop) -> do
      xcomp <- pure $ GxcompToBeN_ (Gmark_ subj) cop nsubj
      sFromUDS $ Groot_xcomp root xcomp
    Groot_ccomp root (Gccomp_ ccomp) -> do
      GMkVPS t p vp <- (root2vps root)
      sc <- GEmbedS <$> sFromUDS ccomp
      pure $ GUseCl t p $ GPredSCVP sc vp
    _ -> case verbFromUDSVerbose x of -- TODO: fill in other cases
                Just (GMkVPS t p vp) -> Just $ GUseCl t p $ GGenericCl vp
                --_       -> Nothing
                _    -> trace ("\n\n **** sFromUDS: couldn't match " ++ showExpr (gf x)) Nothing
    where
      verbFromUDSVerbose = verbFromUDS
      -- verbFromUDSVerbose = verbFromUDS' True -- uncomment when you want really verbose debug output

getRoot :: Tree a -> [Groot]
getRoot rt@(GrootA_ _) = [rt]
getRoot rt@(GrootN_ _) = [rt]
getRoot rt@(GrootV_ _ _ _) = [rt]
getRoot rt@(GrootVaux_ _ _ _ _) = [rt]
getRoot rt@(GrootDet_ _) = [rt]
getRoot rt@(GrootDAP_ _) = [rt]
getRoot rt@(GrootQuant_ _) = [rt]
getRoot rt@(GrootAdA_ _) = [rt]
getRoot rt@(GrootAdv_ _) = [rt]
getRoot rt@(GrootPrep_ _) = [rt]
getRoot rt@(GrootRP_ _) = [rt]
getRoot x = composOpMonoid getRoot x

getNsubj :: Tree a -> [Gnsubj]
getNsubj ns@(Gnsubj_ _) = [ns]
getNsubj (GadvclMarkUDS_ _ _) = []
getNsubj x = composOpMonoid getNsubj x

predVPS :: GNP -> GVPS -> GS
predVPS np (GMkVPS t p vp) = GUseCl t p (GPredVP np vp)
predVPS np vps = useCl $ GPredVP np $ vps2vp vps

vps2vp :: GVPS -> GVP
vps2vp (GMkVPS _t _p vp) = vp
vps2vp (GComplAux _a _t _p vp) = vp
vps2vp (GConjVPS c (GListVPS vps)) = GConjVP c (GListVP (map vps2vp vps))

complVP :: GVP -> GNP -> GVP
complVP (GUseV v) np = GComplV v np
complVP (GAdvVP vp adv) np = GAdvVP (complVP vp np) adv
complVP (GAdVVP adv vp) np = GAdVVP adv (complVP vp np)
complVP (GProgrVP vp) np = GProgrVP (complVP vp np)
complVP (GPassV v) np = GPassVAgent v np
complVP vp@(GUseComp _) np = GComplVP vp np -- last resort, probably something's misparsed somewhere
complVP vp _ = error $ "complVP: doesn't handle argument " ++ showExpr (gf vp)

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
