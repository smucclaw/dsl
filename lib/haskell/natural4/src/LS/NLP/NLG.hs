{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, NamedFieldPuns, FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module LS.NLP.NLG where

import LS.NLP.UDExt (GUDS, GUDFragment, gf, fg)
                          -- goal: only export startcat of the application grammar here,
import LS.NLP.TreeTransformations -- and details of the tree transformations are in this module.
import LS.Types ( TemporalConstraint (..), TComparison(..),
      ParamText,
      Rule(..),
      BoolStructP, BoolStructR,
      RelationalPredicate(..), HornClause(..), RPRel(..), HasToken (tokenOf),
      Expect(..),
      rp2text, pt2text, bsr2text, KVsPair, HornClause2, BoolStructDTP, MultiTerm)
import PGF ( readPGF, readLanguage, languages, CId, Expr, linearize, mkApp, mkCId, lookupMorpho, PGF, readExpr )
import UDAnnotations ( UDEnv(..), getEnv )
import qualified Data.Text as Text
import Data.Char (toLower, isDigit, isLower)
import UD2GF (getExprs)
import qualified AnyAll as AA
import Data.Maybe (fromMaybe, catMaybes)
import Data.List ( group, sort, sortOn, nub, intercalate, isInfixOf )
import Data.List.Extra (groupOn, splitOn)
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty((:|)))
import UDPipe (loadModel, runPipeline, Model)
import Control.Monad (when, unless)
import System.Environment (lookupEnv, getExecutablePath)
import Control.Concurrent.Async (concurrently)
import Data.Set as Set (member, fromList)
--import qualified Data.ByteString.Lazy.Char8 as Byte (ByteString, writeFile, hPutStrLn)
--import Control.Monad.Trans
--import System.IO (stderr)
--import System.Exit (exitFailure)
--import Data.Typeable
import Paths_natural4
import AnyAll.BoolStructTree
--import qualified AnyAll.Types as AA
import qualified Data.Tree as DT
import Data.Foldable as F


data NLGEnv = NLGEnv
  { udEnv :: UDEnv
  , udpipeModel :: Model
  , verbose :: Bool
  }

modelFilePath :: IO FilePath
modelFilePath = getDataFileName $ gfPath "english-ewt-ud-2.5-191206.udpipe"

myNLGEnv :: IO NLGEnv
myNLGEnv = do
  mpn <- lookupEnv "MP_NLG"
  let verbose = maybe False (read :: String -> Bool) mpn
  (udEnv,udpipeModel) <- concurrently (
      getEnv (gfPath "UDApp") "Eng" "UDS"
    ) $ do
    when verbose $ putStrLn "\n-----------------------------\n\nLoading UDPipe model..."
    udpath <- modelFilePath
    udpipeModel <- either error id <$> loadModel udpath
    when verbose $ putStrLn "Loaded UDPipe model"
    pure udpipeModel
  -- let
  --   parsingGrammar = pgfGrammar udEnv -- use the parsing grammar, not extension grammar
  --   lang = actLanguage udEnv
  --   gfMorpho = buildMorpho parsingGrammar lang
  return $ NLGEnv {udEnv, udpipeModel, verbose}

nlgExtPGF :: IO PGF
nlgExtPGF = do
  udext <- getDataFileName $ gfPath "UDExt.pgf"
  readPGF udext

gfPath :: String -> String
gfPath x = "grammars/" ++ x

-- | Parse text with udpipe via udpipe-hs, then convert the result into GF via gf-ud
parseUD :: NLGEnv -> Text.Text -> IO GUDS
parseUD env txt = do
  unless (verbose env) $ -- when not verbose, just short output to reassure user we're doing something
    putStrLn ("    NLG.parseUD: parsing " <> "\"" <> Text.unpack txt <> "\"")
  -- conll <- udpipe txt -- Initial parse
  let nonWords = concat $ saveNonWords (map Text.unpack $ Text.words txt) []
   -- check that it's not just a capitalised real word and replace if not
  lowerConll <- udpipe (Text.unwords $ map Text.pack $ checkAllCapsIsWord txt)
  -- when (verbose env) $ putStrLn ("\nconllu:\n" ++ lowerConll)
  -- let expr = case ud2gf lowerConll of
  --              Just e -> e
  --              Nothing -> fromMaybe errorMsg (ud2gf lowerConll)
  expr <- either errorMsg pure $ ud2gf lowerConll
  let swappedExpr = fromMaybe (dummyExpr ("no expr from udpipe: " ++ Text.unpack txt)) (readExpr $ swapBack nonWords $ showExpr expr)
  let uds = toUDS (pgfGrammar $ udEnv env) swappedExpr
  -- compare to just getting readExpr from original txt
  -- uds: "every Gra25 must notify the organization if the data's breach occurs on or after the date of commencement of PDPA_PDP(A)A_2020_\167\&13"
  -- unparsed original txt: Got the error: * Function Gra25 is not in scope
  when (verbose env) $ putStrLn ("Converted into UDS:\n" ++ showExpr (gf uds))
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
                  (_,    r:_) -> Right r
                  (l:_,   []) -> Left l
                  ([] ,   []) -> Left "ud2gf: no results given for input"
      [] -> Left "ud2gf: tried parsing an empty input"

    checkIfChunk :: String -> Bool
    checkIfChunk x = checkDigit x || checkLower x || checkSymbol x
      where
        checkLower = not . any isLower
        checkDigit = any isDigit
        checkSymbol = any (`Set.member` Set.fromList ['#','§'])

    saveNonWords :: [String] -> [String] -> [[String]]
    saveNonWords [] _ls = []
    saveNonWords (x:xs) ls
      | checkIfChunk x = (x:ls) : saveNonWords xs ls
      | otherwise = saveNonWords xs ls

    swapChunk :: [String] -> [String]
    swapChunk [] = []
    swapChunk (x:xs)
      | checkIfChunk x = "propernoun" : swapChunk xs
      | otherwise = x : swapChunk xs

    replaceChunks :: Text.Text -> [String]
    replaceChunks txt = swapChunk $ map Text.unpack $ Text.words txt
    -- Text.pack $ unwords $

    combinePROPERNOUN :: [String] -> [String]
    combinePROPERNOUN [] = []
    combinePROPERNOUN x
      | head x == "propernoun" = [intercalate "_" x]
      | otherwise = x

    {-
    lowerButPreserveAllCaps :: Text.Text -> Text.Text
    lowerButPreserveAllCaps txt = Text.unwords
      [ if all isUpper (Text.unpack wd)
          then wd
          else Text.map toLower wd
      | wd <- Text.words txt
      ] -}

    checkAllCapsIsWord :: Text.Text -> [String]
    checkAllCapsIsWord txt
      | isInfixOf "propernoun" (check (Text.map toLower txt)) = words $ check txt
      | otherwise = words $ check (Text.map toLower txt)
      where
        check tx = Text.unpack $ Text.unwords $ map Text.pack $ concatMap combinePROPERNOUN $ group $ replaceChunks tx

    swapBack :: [String] -> String -> String
    swapBack str txt
      | isInfixOf "propernoun" txt = concat $ concat $ swap str $ splitOn "propernoun" txt
      | otherwise = txt
      where
        swap a b = zipWith (++) (fst l) a : [snd l]
          where
            l = splitAt (length a) b
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

-- type BoolStructT  = AA.OptionallyLabeledBoolStruct Text.Text
-- type BoolStructP = AA.OptionallyLabeledBoolStruct ParamText
-- type BoolStructR = AA.OptionallyLabeledBoolStruct RelationalPredicate
    --  Expected: BoolStructT
    --     Actual: AA.BoolStruct (Maybe (AA.Label Text.Text)) (IO [String])

ruleQuestions :: NLGEnv -> Maybe (MultiTerm,MultiTerm) -> Rule -> IO [AA.OptionallyLabeledBoolStruct Text.Text]
ruleQuestions env alias rule = do
  gr <- nlgExtPGF
  [youExpr, orgExpr] <-
      case alias of
        Nothing        -> pure [dummySubj, dummySubj]
        Just (you,org) -> sequence [ do
                            uds <- parseMulti env mt
                            pure $ gf $ peelNP uds
                            | mt <- [you, org]]
  case rule of
    Regulative {subj,who,cond} -> do
      subjExpr <- bsp2gf env subj
      let aliasExpr = if subjExpr==orgExpr then youExpr else subjExpr
      whoBSR <- mapM (bsr2questions qsWho gr aliasExpr) who
      condBSR <- mapM (bsr2questions qsCond gr aliasExpr) cond
      pure $ concat $ catMaybes [whoBSR, condBSR]
    Constitutive {cond} -> do
      condBSR <- mapM (bsr2questions qsCond gr dummySubj) cond
      pure $ concat $ catMaybes [condBSR]
    Hornlike {keyword, clauses} -> do
      let kw = keyword2cid keyword
      parsedClauses <- mapM (parseHornClause2 env kw) clauses
      let statements = [s | [s] <- parsedClauses] -- TODO: eventually make a new function that parses RPs for HornClause that doesn't make its argument UDFragment
      let questions = concatMap (mkQ qsCond gr dummySubj) statements :: [AA.OptionallyLabeledBoolStruct Text.Text]
      pure questions
    DefNameAlias {} -> pure [] -- no questions needed to produce from DefNameAlias
    _ -> pure [AA.Leaf (Text.pack $ "ruleQuestions: doesn't work yet for " <> show rule)]

  where
    keepTextNegations :: AA.OptionallyLabeledBoolStruct [AA.OptionallyLabeledBoolStruct a] -> [AA.OptionallyLabeledBoolStruct a]
    keepTextNegations bsr = case bsr of
        AA.Leaf x -> x -- negation is in the value x!
        AA.All l xs -> [AA.All l (concatMap keepTextNegations xs)]
        AA.Any l xs -> [AA.Any l (concatMap keepTextNegations xs)]
        AA.Not x    -> [AA.Not bs | bs <- keepTextNegations x]

    bsr2questions :: QFun -> PGF -> Expr -> BoolStructR -> IO [AA.OptionallyLabeledBoolStruct Text.Text]
    bsr2questions qfun gr subj bsr = do
      bsrWithExprs <- mapM (parseRP env rpIs) bsr
      let bsrWithQuestions = mkQ qfun gr subj <$> bsrWithExprs -- this one returns a BSR inside a BSR, with the correct negations in the inner one
      pure $ keepTextNegations bsrWithQuestions -- keep the new inner layer with negations added from text

    rpIs = mkCId "RPis"
    Just eng = readLanguage "UDExtEng"
    dummySubj = gf dummyNP
    mkQ :: QFun -> PGF -> Expr -> Expr -> [AA.OptionallyLabeledBoolStruct Text.Text]
    mkQ qf gr subj e = fmap Text.pack <$> questionStrings
      where
        questionStrings :: [AA.OptionallyLabeledBoolStruct String]
        questionStrings = mkQs qf gr eng subj (expr2TreeGroups gr e)

nlgQuestion :: NLGEnv -> Rule -> IO [Text.Text]
nlgQuestion env rl = do
  rulesInABoolStruct <- ruleQuestions env Nothing rl -- TODO: the Nothing means there is no AKA
  pure $ concatMap F.toList rulesInABoolStruct

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
   annotatedRule <- parseFields env rl
   gr <- nlgExtPGF
   let lang = head $ languages gr
   let Just eng = readLanguage "UDExtEng"
   let Just swe = readLanguage "UDExtSwe"
   let toNP expr = gf $ peelNP $ gf $ toUDS gr expr -- TODO remove this before merging to main—just making sure subjA is still of type NP
   case annotatedRule of
      RegulativeA {subjA, keywordA, whoA, condA, deonticA, actionA, temporalA, uponA, givenA} -> do
        let deonticAction = mkApp deonticA [gf $ toUDS gr actionA] -- TODO: or change type of DMust to take VP instead?
            subjWho = applyMaybe "Who" (gf . toUDS gr <$> whoA) (toNP subjA)
            subj = mkApp keywordA [subjWho]
            king_may_sing = mkApp (mkCId "subjAction") [subj, deonticAction]
            existingQualifiers = [(name,expr) |
                                  (name,Just expr) <- [("Cond", gf . toUDS gr <$> condA),
                                                       ("Temporal", temporalA),
                                                       ("Upon", uponA),
                                                       ("Given", givenA)]]
            finalTree = doNLG existingQualifiers king_may_sing -- determine information structure based on which fields are Nothing
            linText = unlines [
                                linearize gr eng finalTree
                              , linearize gr swe finalTree ]
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
                              , linearize gr swe tree ]
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
    -- let gr = pgfGrammar $ udEnv env
    subjA <- bsp2gf env subj
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

parseHornClause :: NLGEnv -> CId -> HornClause2 -> IO Expr
parseHornClause env fun hc = do
  exprs <- parseHornClause2 env fun hc
  pure $ case exprs of
    [] -> error $ "parseHornClause: can't parse " <> show hc
    [expr] -> expr
    (udf:s:_) -> mkApp (mkCId "HornClause2") [udf,s]

parseHornClause2 :: NLGEnv -> CId -> HornClause2 -> IO [Expr]
parseHornClause2 env fun (HC rp Nothing) = do
  expr <- parseRP env fun rp
  pure [expr]
parseHornClause2 env fun (HC rp (Just bsr)) = do
  extGrammar <- nlgExtPGF -- use extension grammar, because bsr2gf can return funs from UDExt
  db_is_NDB_UDFragment <- parseRPforHC env fun rp
  db_occurred <- bsr2gf env bsr
  let db_occurred_S = case findType extGrammar db_occurred of
        "S" -> fg db_occurred          -- S=[databreach occurred]
        "AP" -> ap2s $ fg db_occurred  -- someone is AP=[happy]
        -- TODO: make other cats to S too
        _ -> error $ "parseHornClause2: expected S, got " ++ showExpr db_occurred
  pure [gf db_is_NDB_UDFragment, gf db_occurred_S]

-- A wrapper for ensuring same return type for parseRP
parseRPforHC :: NLGEnv -> CId -> RelationalPredicate -> IO GUDFragment
parseRPforHC env _f (RPParamText pt) = toFragment . fg <$> parseParamText env pt
parseRPforHC env _f (RPMT txts) = toFragment . fg <$> parseMulti env txts
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
  blueUDS <- gf . toUDS gr <$> bsr2gf env blue
  let skyNP = gf $ peelNP skyUDS -- TODO: make npFromUDS more robust for different sentence types
  let rprel = if is==RPis then fun else keyword2cid is
  return $ mkApp rprel [skyNP, blueUDS]
parseRP env fun (RPnary        _rprel rp) = parseRP env fun rp
parseRP _ _ rp = error $ "parseRP: doesn't handle yet " <> show rp

-- ConstitutiveName is [Text.Text]
parseMulti :: NLGEnv -> [Text.Text] -> IO Expr
parseMulti env txt = gf <$> parseUD env (Text.unwords txt)

parseName :: NLGEnv -> [Text.Text] -> IO Text.Text
parseName _env txt = return (Text.unwords txt)

parseParamText :: NLGEnv -> ParamText -> IO Expr
parseParamText env pt = gf <$> parseUD env (pt2text pt)

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
  AA.Any _ (x:_) -> bsp2gf env x -- TODO: handle others
  AA.All _ (x:_) -> bsp2gf env x -- TODO: handle others
  AA.Not x -> bsp2gf env x -- TODO: handle negation
  _ -> error "bsp2gf: not supported yet"

bsp2gfDT :: NLGEnv -> BoolStructDTP -> IO Expr
bsp2gfDT env bsp = case bsp of
  (DT.Node (FAtom (action :| mods)) _    )  -> do
    actionExpr <- kvspair2gf env action  -- notify the PDPC
    modExprs <- mapM (kvspair2gf env) mods -- [by email, at latest at the deadline]
    return $ combineActionMods actionExpr modExprs
  _ -> error "bsp2gf: not supported yet"


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


------------------------------------------------------------
-- Let's try to parse a BoolStructR into a GF list
-- First use case: "any unauthorised [access,use,…]  of personal data"

parseAndDisambiguate :: NLGEnv -> [BoolStructR] -> IO [GUDS]
parseAndDisambiguate env text = do
  contentsAmb <- mapM (bsr2gfAmb env) text
  let parsingGrammar = pgfGrammar $ udEnv env -- here we use the parsing grammar, not extension grammar!
      contents = disambiguateList parsingGrammar contentsAmb
  return $ map (toUDS parsingGrammar) contents

parseLex :: NLGEnv -> String -> [Expr]
parseLex env str = result
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

-- Given a list of ambiguous words like
-- [[access_V, access_N], [use_V, use_N], [copying_N, copy_V], [disclosure_N]]
-- return a disambiguated list: [access_N, use_N, copying_N, disclosure_N]
disambiguateList :: PGF -> [[Expr]] -> [Expr]
disambiguateList pgf access_use_copying_raw =
  if all isSingleton access_use_copying
    then concat access_use_copying
    else [ w | ws <- access_use_copying
         , w <- ws
         , findType pgf w == unambiguousType (map (map (findType pgf)) access_use_copying)
    ]
  where
    access_use_copying =  -- mkPhrasal: {N,A,V} to {NP,AP,VP}
      map (map $ mkPhrasal pgf) access_use_copying_raw

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
          [cat:_] -> cat
          catss     -> preferCat "VP" catss -- TODO: instead of hardcoding some cat here, later put in NLGEnv the category we are currently parsing

    preferCat :: String -> [[String]] -> String
    preferCat _ [[]] = error $ "disambiguateList: failed with arguments=" ++ show access_use_copying
    preferCat heuristicBestCat frequentCats =
      if heuristicBestCat `elem` map head frequentCats
        then heuristicBestCat -- if there are equally many of the best guess & other cats, might as well choose our best guess
        else head $ head frequentCats


    isSingleton [_] = True
    isSingleton _ = False


-- Meant to be called from inside an Any or All, via parseAndDisambiguate
-- If we encounter another Any or All, fall back to bsr2gf
bsr2gfAmb :: NLGEnv -> BoolStructR -> IO [Expr]
bsr2gfAmb env bsr = case bsr of
  -- If it's a leaf, parse the contents
  AA.Leaf rp -> do
    let access = rp2text rp
    case Text.words access of
      -- If the leaf is a single word, do a lexicon lookup
      [_] -> return $ parseLex env (Text.unpack access)
      -- If the leaf is multiple words, parse with udpipe
      _ -> singletonList $ gf <$> parseUD env access

  -- If it's not a leaf, call bsr2gf
  _ -> singletonList $ bsr2gf env bsr
  where
    singletonList x = (:[]) <$> x

-- TODO: deprecate in favour of AA.OptionallyLabeledBoolStruct Expr
bsr2gf :: NLGEnv -> BoolStructR -> IO Expr
bsr2gf env bsr = case bsr of
  -- This happens only if the full BoolStructR is just a single Leaf
  -- In typical case, the BoolStructR is a list of other things, and in such case, bsr2gfAmb is called on the list
  AA.Leaf rp -> do
    let access = rp2text rp
    gf <$> parseUD env access  -- Always returns a UDS, don't check if it's a single word (no benefit because there's no context anyway)

  AA.Not rp -> do
    let not = bsr2text rp
    gf <$> parseUD env not

  AA.Any Nothing contents -> do
    contentsUDS <- parseAndDisambiguate env contents
    let existingTrees = groupByRGLtype orConj contentsUDS
    putStrLn ("bsr2gf: Any Nothing\n" ++ show existingTrees)
    return $ treeContents orConj contentsUDS

  AA.All Nothing contents -> do
    contentsUDS <- parseAndDisambiguate env contents
    let existingTrees = groupByRGLtype andConj contentsUDS
    putStrLn ("bsr2gf: All Nothing\n" ++ show existingTrees)
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
