{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module LS.NLP.NLG
  ( NLGEnv (..),
    allLangs,
    expandRulesForNLG,
    expandRulesForNLGE,
    langEng,
    linBStext,
    mkConstraintText,
    myNLGEnv,
    nlg,
    nlgQuestion,
    parseSubj,
    printLangs,
    ruleQnTrees,
    ruleQuestions,
    ruleQuestionsNamed,
    textViaQaHorns,
  )
where

import AnyAll qualified as AA
import Control.Arrow ((>>>))
import Control.Monad (when)
import Data.Char qualified as Char (isDigit, toLower)
import Data.Foldable qualified as F
import Data.HashMap.Strict (elems, keys, lookup, toList)
import Data.HashMap.Strict qualified as Map
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, maybeToList)
import Data.Set qualified as Set
import Data.String.Interpolate as I (i, __i)
import Data.Text qualified as Text
import Data.Text.Read (decimal)
import Data.Traversable (for)
import Debug.Trace (trace)
import Flow ((|>))
import LS.Interpreter
  ( expandBSR,
    expandBSRM,
    expandClause,
    expandClauses,
    expandRP,
    qaHornsR,
  )
import LS.NLP.NL4
  ( GAction,
    GAdv,
    GCond,
    GConstraint,
    GDate,
    GDay,
    GDeontic,
    GDigits,
    GMonth,
    GNP,
    GPrePost,
    GS,
    GString,
    GTComparison,
    GTemporal,
    GText,
    GTimeUnit,
    GUpon,
    GVPS,
    GWho,
    GYear,
    Gf (fg, gf),
    Tree
      ( GAFTER,
        GAdvVP,
        GBEFORE,
        GBY,
        GDay_Unit,
        GIDig,
        GIIDig,
        GMAY,
        GMUST,
        GMkDate,
        GMkVPS,
        GMkYear,
        GMonth_Unit,
        GON,
        GPredVPS,
        GRPConstraint,
        GRPleafS,
        GRegulative,
        GSHANT,
        GString,
        GSubjWho,
        GTemporalConstraint,
        GTemporalConstraintNoDigits,
        GUPON,
        GUPONnp,
        GVAGUE,
        GWHEN,
        GYear_Unit,
        GYou,
        GadvUPON,
        GqCOND,
        GqCONSTR,
        GqPREPOST,
        GqUPON,
        GqWHO,
        GrecoverRPis,
        GrecoverRPmath,
        GrecoverUnparsedConstraint,
        GrecoverUnparsedTimeUnit,
        LexDay,
        LexDig,
        LexMonth,
        LexVP,
        LexYearComponent
      ),
  )
import LS.NLP.NL4Transformations
  ( BoolStructCond,
    BoolStructConstraint,
    BoolStructGText,
    BoolStructWho,
    aggregateBoolStruct,
    bsCond2gfCond,
    bsConstraint2gfConstraint,
    bsWho2gfWho,
    flipPolarity,
    introduceNP,
    isChinese,
    isMalay,
    mapBSLabel,
    pastTense,
    pushPrePostIntoMain,
    referNP,
  )
import LS.Rule (Interpreted (..), Rule (..), ruleConstructor, ruleLabelName, ruleName)
import LS.Types
  ( BoolStructP,
    BoolStructR,
    BoolStructT,
    Deontic (..),
    HornClause (hBody, hHead),
    HornClause2,
    MTExpr (MTT),
    MultiTerm,
    ParamText,
    RPRel (..),
    RelationalPredicate
      ( RPBoolStructR,
        RPConstraint,
        RPMT,
        RPParamText
      ),
    RuleName,
    TComparison (..),
    TemporalConstraint (..),
    bsp2text,
    bsr2text,
    mkLeafPT,
    mt2pt,
    mt2text,
    pt2text,
    rp2mt,
    rp2text,
  )
import LS.Utils ((|$>))
import LS.XPile.Logging
  ( XPileLog,
    XPileLogE,
    mutter,
    mutterd,
    mutterdhsf,
    mutters,
    pShowNoColorS,
    xpError,
    xpLog,
    xpReturn,
  )
import PGF
  ( CId,
    Expr,
    Language,
    PGF,
    Type,
    categories,
    languages,
    linearize,
    mkApp,
    mkCId,
    mkStr,
    mkType,
    parse,
    readLanguage,
    readPGF,
    readType,
    showExpr,
    showLanguage,
  )
import Paths_natural4 (getDataFileName)
import Prettyprinter.Interpolate (__di)
import System.Environment (lookupEnv)
import Text.Regex.PCRE.Heavy qualified as PCRE

data NLGEnv = NLGEnv
  { gfGrammar :: PGF
  , gfLang :: Language
  , gfParse :: Type -> Text.Text -> [Expr]
  , gfLin :: Expr -> Text.Text
  , verbose :: Bool
  , interpreted :: Interpreted
  }

allLangs :: IO [Language]
allLangs = do
  grammarFile <- getDataFileName $ gfPath "NL4.pgf"
  gr <- readPGF grammarFile
  pure $ languages gr

langEng :: IO (XPileLogE Language)
langEng = do
  grammarFile <- getDataFileName $ gfPath "NL4.pgf"
  gr <- readPGF grammarFile
  pure do
    mutter "*** langEng reading NL4.pgf, calling getLang NL4Eng"
    getLang "NL4Eng" gr

printLangs :: IO [Language] -> IO String
printLangs = fmap (intercalate "\", \"" . map (map Char.toLower . showLanguage))

getLang :: String -> PGF -> XPileLogE Language
getLang str gr = do
  mutter [i|*** getLang #{str}|]
  case (readLanguage str, languages gr) of
    (Just l, langs@(l':_))  -- Language looks valid, check if in grammar
      -> if l `elem` langs
           then xpReturn l
                -- Expected case: language looks valid and is in grammar
           else xpError [fallbackMsg $ show l']
                -- Language is valid but not in grammar, warn and fall back to another language
    (Nothing, l':_) -- Language not valid, warn and fall back to another language
      -> xpError [fallbackMsg $ show l']
    (_, []) -- The PGF has no languages, truly unexpected and fatal
      -> xpError ["NLG.getLang: the PGF has no languages, maybe you only compiled the abstract syntax?"]
  where
    fallbackMsg fblang = [i|language #{str} not found, falling back to #{fblang}|]

myNLGEnv :: Interpreted -> Language -> IO (XPileLogE NLGEnv)
myNLGEnv l4i lang = do
  mpn <- lookupEnv "MP_NLG"
  let verbose = maybe False (read :: String -> Bool) mpn
  grammarFile <- getDataFileName $ gfPath "NL4.pgf"
  gr <- readPGF grammarFile
  (eng, engErr) <- xpLog <$> langEng
  case eng of
    Left  engL -> return $ mutter "** myNLGEnv" >> mutters engErr >> xpError engL
    Right engR -> do
      let myParse typ txt = parse gr engR typ (Text.unpack txt)
          myLin = uncapKeywords . rmBIND . Text.pack . linearize gr lang
      return do
        mutter "** myNLGEnv"
        xpReturn $ NLGEnv gr lang myParse myLin verbose l4i

rmBIND :: Text.Text -> Text.Text
rmBIND = PCRE.gsub [PCRE.re|\s+&\+\s+|] ("" :: Text.Text)

uncapKeywords :: Text.Text -> Text.Text
uncapKeywords =
  PCRE.gsub [PCRE.re|(BEFORE|AFTER|IS)|]
    \(keyword:_) -> Text.toLower keyword

  -- Text.unwords . map (lowerWhole ["BEFORE","AFTER","IS"]) . Text.words
  -- where
  --   lowerWhole keywords word
  --     | word `elem` keywords = Text.toLower word
  --     | otherwise = word

gfPath :: String -> String
gfPath x = [i|grammars/#{x}|]

-----------------------------------------------------------------------------
-- Main

-- WIP: crude way of keeping track of whether we're in hence, lest or whatever
data RecursionLevel = TopLevel | MyHence Int | MyLest Int
  deriving (Eq,Ord,Show)

getLevel :: RecursionLevel -> Int
getLevel = \case
  TopLevel -> 2
  MyHence i -> i
  MyLest i -> i

debugNesting :: Language -> RecursionLevel -> (Text.Text, Text.Text)
debugNesting lang level = (getPrefix lang level, "")
  where
    getPrefix _ TopLevel = ""
    getPrefix lang (MyHence _)
      | isChinese lang = "在此之后，做:"
      | isMalay lang = "Tindakan seterusnya:"
      | otherwise = "Follow by:"
    getPrefix lang (MyLest _)
      | isChinese lang = Text.pack "万一失败，"
      | isMalay lang = Text.pack "Dalam kes kegagalan:"
      | otherwise = Text.pack "In case of failure:"

getIf :: Language -> Text.Text
getIf lang
  | isChinese lang = ". 如果 "
  | isMalay lang = ". Jika "
  | otherwise = ". If "

getWhen :: Language -> Text.Text
getWhen lang
  | isChinese lang = "于"
  | isMalay lang = "ketika"
  | otherwise = "when"

nlg :: NLGEnv -> Rule -> IO Text.Text
nlg = nlg' TopLevel

nlg' :: RecursionLevel -> NLGEnv -> Rule -> IO Text.Text
nlg' thl env = \case
  Regulative {subj,upon,temporal,cond,who,deontic,action,lest,hence} -> do
    let subjExpr = introduceNP $ parseSubj env subj
        deonticExpr = parseDeontic deontic
        actionExpr = parseAction env action
        whoSubjExpr = case who of
                      Just w -> GSubjWho subjExpr (bsWho2gfWho (parseWhoBS env w))
                      Nothing -> subjExpr
        ruleTree = gf $ GRegulative whoSubjExpr deonticExpr actionExpr
        ruleText :: Text.Text = gfLin env ruleTree
        uponText :: Text.Text = case upon of  -- TODO: doesn't work once we add another language
                    Just u ->
                      let uponExpr = gf $ GadvUPON $ parseUpon env u
                      in [I.i|#{gfLin env uponExpr}, |]
                    Nothing -> mempty
        tcText :: Text.Text = case temporal of
                    Just t -> [I.i| #{gfLin env (gf $ parseTemporal env t)}|]
                    Nothing -> mempty
        condText :: Text.Text = case cond of
                    Just c ->
                      let condExpr = gf $ pastTense $ bsCond2gfCond (parseCondBS env c)
                      in [I.i|#{getIf $ gfLang env}#{gfLin env condExpr}, |]
                    Nothing -> mempty
        ruleTextDebug :: Text.Text =
          [I.i|#{prefix} #{uponText}#{ruleText}#{tcText}#{condText} #{suffix}|]
    lestText <- henceLest2text MyLest lest
    henceText <- henceLest2text MyHence hence
    when (verbose env) do
      putStrLn [__i|
        nlg': regulative"
            #{showExpr [] ruleTree}
      |]
    pure $ Text.strip [__i|
      #{ruleTextDebug}
      #{henceText}
      #{lestText}
    |]
  Hornlike {clauses} -> do
    let headTrees = gf . parseConstraint env . hHead <$> clauses -- :: [GConstraint] -- this will not become a question
        headLins = gfLin env <$> headTrees
        parseBodyHC cl = case hBody cl of
          Just bs -> [gf $ bsConstraint2gfConstraint $ parseConstraintBS env bs]
          Nothing -> []
        bodyTrees = foldMap parseBodyHC clauses
        bodyLins = gfLin env <$> bodyTrees
    when (verbose env) do
      putStrLn "nlg': hornlike"
      putStrLn $ unlines $
        [[I.i|   head: #{showExpr [] t}|] | t <- headTrees]
          <> [[I.i|   body: #{showExpr [] t}|] | t <- bodyTrees]
    pure [__i|
      #{headLins}
      #{getWhen (gfLang env)}
      #{bodyLins}
    |]
  RuleAlias mt -> do
    let ruleText = gfLin env $ gf $ parseSubj env $ mkLeafPT $ mt2text mt
        ruleTextDebug = [I.i|#{prefix} #{ruleText} #{suffix}|]
    pure $ Text.strip ruleTextDebug
  DefNameAlias {} -> pure mempty
  DefTypically {} -> pure mempty
  rule -> pure [I.i|NLG.hs is under construction, we don't support yet #{rule}|]
  where
    (prefix,suffix) = debugNesting (gfLang env) thl
    i = getLevel thl + 2
    pad x = Text.replicate i " " <> x

    henceLest2text :: (Int -> RecursionLevel) -> Maybe Rule -> IO Text.Text
    henceLest2text myHenceMyLest =
      maybe (pure mempty) \r -> do
        rt <- nlg' (myHenceMyLest i) env r
        pure $ pad rt

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
--
-- there is some semantic difficulty here.
-- qaHorns* returns expanded boolstructs which could be the result of multiple rules.
-- but the ruleQuestions function here takes a rule as an argument.
-- so maybe a qaHorns approach doesn't want to use ruleQuestions directly, but should
-- instead call the underlying things like linBStext.

type Alias = Maybe (MultiTerm,MultiTerm)

ruleQuestions :: NLGEnv
              -> Alias
              -> Rule
              -> XPileLog [BoolStructT]
ruleQuestions env alias rule =
  case rule of
    Regulative {subj,who,cond,upon} -> do
      t <- text
      mutterdhsf 4 [i|ruleQuestions: regulative #{ruleLabelName rule}|]  pShowNoColorS t
      text
    Hornlike {clauses} -> do
      rqn <- ruleQnTrees env alias rule
      mutterdhsf 4 "ruleQuestions: horn; ruleQnTrees =" show rqn
      t <- text
      mutterdhsf 4 "ruleQuestions: horn; returning linBStext" show t
      text
    Constitutive {cond} -> do
      t <- text
      mutterdhsf 4 "ruleQuestions: constitutive; returning linBStext" show t
      text
    DefNameAlias {} -> pure [] -- no questions needed to produce from DefNameAlias
    DefTypically {} -> pure [] -- no questions needed to produce from DefTypically
    RuleGroup {} -> pure []
    _ -> pure [AA.Leaf [i|ruleQuestions: doesn't work yet for #{ruleConstructor rule}|]]
  -- [TODO] for our Logging exercise, see how to convert the _ case above to an xpError

    where
      text :: XPileLog [BoolStructT]
      text = do
        t1 <- ruleQnTrees env alias rule
        pure $ linBStext env <$> t1

ruleQuestionsNamed :: NLGEnv
                   -> Maybe (MultiTerm, MultiTerm)
                   -> Rule
                   -> XPileLog (RuleName, [BoolStructT])
ruleQuestionsNamed env alias rule = do
  let rn = ruleLabelName rule
  rq    <- ruleQuestions env alias rule
  pure (rn, rq)

-- | like ruleQuestions, this function is rule-oriented; it returns a list of
-- boolstructGTexts, which is defined in NL4.hs as a boolstruct of GTexts, which
-- in turn are trees of GText_s. Which takes us into PGF territory.

ruleQnTrees :: NLGEnv -> Maybe (MultiTerm,MultiTerm) -> Rule -> XPileLog [BoolStructGText]
ruleQnTrees env alias rule = do
  mutterd 3 "ruleQnTrees: running"
  let (youExpr, orgExpr) =
        case alias of
          Just (you,org) ->
              case parseSubj env . mkLeafPT . mt2text <$> [you, org] of
                [y,o] -> (y,o) -- both are parsed
                _ -> (GYou, GYou) -- dummy values
          Nothing -> (GYou, GYou) -- dummy values
  mutterdhsf 4 "ruleQnTrees: youExpr = " pShowNoColorS youExpr
  mutterdhsf 4 "ruleQnTrees: orgExpr = " pShowNoColorS orgExpr
  case rule of
    Regulative {subj,who,cond,upon} -> do
      let subjExpr = parseSubj env subj
          aliasExpr
            | subjExpr == orgExpr = youExpr
            | otherwise = referNP subjExpr
          qWhoTrees = mkWhoText env GqPREPOST (GqWHO aliasExpr) <$> who
          qCondTrees = mkCondText env GqPREPOST GqCOND <$> cond
          qUponTrees = mkUponText env aliasExpr GqUPON <$> upon
      mutterdhsf 4 "Regulative/subjExpr"   show subjExpr
      mutterdhsf 4 "Regulative/aliasExpr"  show aliasExpr
      mutterdhsf 4 "Regulative/qWhoTrees"  show qWhoTrees
      mutterdhsf 4 "Regulative/qCondTrees" show qCondTrees
      mutterdhsf 4 "Regulative/qUponTrees" show qUponTrees

      pure $ catMaybes [qWhoTrees, qCondTrees, qUponTrees]

    Hornlike {clauses} -> do
      let bodyTrees = fmap (mkConstraintText env GqPREPOST GqCONSTR) . hBody <$> clauses
      mutterdhsf 4 "Hornlike/bodyTrees" show bodyTrees
      pure $ catMaybes bodyTrees

    Constitutive {cond} -> do
      let qCondTrees = mkCondText env GqPREPOST GqCOND <$> cond
      mutterdhsf 4 "Constitutive/qCOndTrees" show qCondTrees
      pure $ maybeToList qCondTrees
    -- DefNameAlias {} -> return []

    _ -> pure []

-- | convert a BoolStructGText into a BoolStructT for `ruleQuestions`

----------------------------------------------------------------------

textViaQaHorns :: NLGEnv -> Alias -> Maybe GNP -> [([RuleName],  BoolStructT)]
textViaQaHorns env alias subj = [ (rn, linBStext env $ mkGFtext env alias (referNP <$> subj) bsr) | (rn, bsr) <- qaHornsR (interpreted env)]

mkGFtext :: NLGEnv -> Alias -> Maybe GNP -> BoolStructR -> BoolStructGText
mkGFtext env alias subj bsr = case (whoParses, condParses) of
  ([], []) -> mkConstraintText env GqPREPOST GqCONSTR bsr
  ([], _:_) -> mkCondText env GqPREPOST GqCOND bsr
  (_:_, _) -> case subj of
                Just s -> mkWhoText env GqPREPOST (GqWHO s) bsr
                Nothing -> mkConstraintText env GqPREPOST GqCONSTR bsr --- TODO
  where
    whoParses = parseWhoNoRecover env bsr
    condParses = parseCondNoRecover env bsr

parseTxtToGFNoRecover :: Gf a => NLGEnv -> String -> Text.Text -> [a]
parseTxtToGFNoRecover env cat = (fg <$>) . parseAnyNoRecover cat env

parseWhoNoRecover :: NLGEnv -> BoolStructR -> [BoolStructWho]
parseWhoNoRecover = parseWhoCondNoRecover "Who" 

parseCondNoRecover :: NLGEnv -> BoolStructR -> [BoolStructCond]
parseCondNoRecover= parseWhoCondNoRecover "Cond"

parseWhoCondNoRecover :: Gf a => String -> NLGEnv -> BoolStructR -> [AA.BoolStruct (Maybe (AA.Label GPrePost)) a]
parseWhoCondNoRecover whoCond env =
  sequenceA . mapBSLabel (parsePrePost env) parseS
  where
    parseS = parseTxtToGFNoRecover env whoCond . rp2text

----------------------------------------------------------------------

linBStext :: NLGEnv -> BoolStructGText -> BoolStructT
linBStext env = mapBSLabel (gfLin env . gf) (gfLin env . gf)

mkWhoText :: NLGEnv -> (GPrePost -> GText) -> (GWho -> GText) -> BoolStructR -> BoolStructGText
mkWhoText env f g bsr = pushPrePostIntoMain $ mapBSLabel f g $ aggregateBoolStruct (gfLang env) $ parseWhoBS env bsr

mkCondText :: NLGEnv -> (GPrePost -> GText) -> (GCond -> GText) -> BoolStructR -> BoolStructGText
mkCondText env f g bsr = mapBSLabel f g $ aggregateBoolStruct (gfLang env) $ parseCondBS env bsr

mkConstraintText :: NLGEnv -> (GPrePost -> GText) -> (GConstraint -> GText) -> BoolStructR -> BoolStructGText
mkConstraintText env f g bsr = mapBSLabel f g $ aggregateBoolStruct (gfLang env) $ parseConstraintBS env bsr

mkUponText :: NLGEnv -> GNP -> (GNP -> GUpon -> GText) -> ParamText -> BoolStructGText
mkUponText env alias f pt = AA.Leaf (f subj upon)
  where
    upon0 = parseUpon env pt
    (subj,upon) = case upon0 of
                    GUPONnp np vp -> (np, GUPON vp)
                    _ -> (alias, upon0)

nlgQuestion :: NLGEnv -> Rule -> XPileLog [Text.Text]
nlgQuestion env rl = do
  questionsInABoolStruct <- ruleQuestions env Nothing rl -- TODO: the Nothing means there is no AKA
  pure $ foldMap F.toList questionsInABoolStruct

-----------------------------------------------------------------------------
-- Parsing fields into GF categories – all typed, no PGF.Expr allowed

-- Special constructions for the fields that are BoolStructR
parseConstraintBS :: NLGEnv -> BoolStructR -> BoolStructConstraint
parseConstraintBS env = mapBSLabel (parsePrePost env) (parseConstraint env)

parseWhoBS :: NLGEnv -> BoolStructR -> BoolStructWho
parseWhoBS env = mapBSLabel (parsePrePost env) (parseWho env)

parseCondBS :: NLGEnv -> BoolStructR -> BoolStructCond
parseCondBS env = mapBSLabel (parsePrePost env) (parseCond env)

-- not really parsing, just converting nL4 constructors to GF constructors
parseDeontic :: Deontic -> GDeontic
parseDeontic DMust = GMUST
parseDeontic DMay = GMAY
parseDeontic DShant = GSHANT

parseTComparison :: TComparison -> GTComparison
parseTComparison TBefore = GBEFORE
parseTComparison TAfter = GAFTER
parseTComparison TBy = GBY
parseTComparison TOn = GON
parseTComparison TVague = GVAGUE

parseDate :: MultiTerm -> GDate
parseDate = \case
  (Text.words . mt2text -> [d, m, y]) ->
    GMkDate (tDay d) (tMonth m) (mkYear y)
  _ -> GMkDate (LexDay "Day1") (LexMonth "Jan")  dummyYear
  where
    dummyYear = mkYear "1970"

    mkYear :: Text.Text -> GYear
    mkYear (splitYear -> [y1, y2, y3, y4]) =
      GMkYear (LexYearComponent y1) (LexYearComponent y2) (LexYearComponent y3) (LexYearComponent y4)

    splitYear :: Text.Text -> [String]
    splitYear y = case [[i|Y#{d}|] | d <- Text.unpack y] of
      xs@[_, _, _, _] -> xs
      _ -> ["Y2", "Y0", "Y0", "Y0"]

    tDay :: Text.Text -> GDay
    tDay t = LexDay [i|Day#{t}|]

    tMonth :: Text.Text -> GMonth
    tMonth = LexMonth . Text.unpack

-- TODO: stop using *2text, instead use the internal structure
  -- "respond" :| []  -> respond : VP
  -- "demand" :| [ "an explanation for your inaction" ] -> demand : V2, NP complement, call ComplV2
  -- "assess" :| [ "if it is a Notifiable Data Breach" ] -> assess : VS, S complement, call ComplS2
parseAction :: NLGEnv -> BoolStructP -> GAction
parseAction env = parseTxtToGF "Action" env . bsp2text

parseSubj :: NLGEnv -> BoolStructP -> GNP
parseSubj env = parseTxtToGF "NP" env . bsp2text 

parseWho :: NLGEnv -> RelationalPredicate -> GWho
parseWho env = parseTxtToGF "Who" env . rp2text

parseCond :: NLGEnv -> RelationalPredicate -> GCond
parseCond env (RPConstraint c (RPTC t) d) = GRPConstraint cond tc date
  where
    cond = parseCond env $ RPMT c
    tc = parseTComparison t
    date = parseDate d

parseCond
  env
  ( RPConstraint
      (Text.strip . mt2text -> a)
      RPis
      (Text.strip . mt2text -> b)
    ) = case (nps, vps) of
    (np : _, (GMkVPS t p vp) : _) -> GWHEN np t p vp
    _ -> parseCond env $ RPMT [MTT [i|#{a} is #{b}|]]
    where
      nps :: [GNP]
      nps = fg <$> parseAnyNoRecover "NP" env a

      vps :: [GVPS]
      vps = fg <$> parseAnyNoRecover "VPS" env [i|is #{b}|]

parseCond env rp = fg tree
  where
    txt = rp2text rp
    tree :| _ = parseAny "Cond" env txt

parseUpon :: NLGEnv -> ParamText -> GUpon
parseUpon env pt = case (upons, nps) of
  (upon:_, _) -> upon
  (_, np:_) -> GUPONnp np $ LexVP "occur"
  (_, _) -> fg tree
  where
    txt = pt2text pt
    upons = go "Upon"
    nps = go "NP"
    tree :| _ = parseAny "Upon" env txt

    go :: Gf a => String -> [a]
    go txt' = fg <$> parseAnyNoRecover txt' env txt

parseTemporal :: NLGEnv -> TemporalConstraint Text.Text -> GTemporal
parseTemporal env (TemporalConstraint t (Just int) text) =
  GTemporalConstraint tc digits unit
  where
    tc = parseTComparison t
    digits = mkDigits int
    unit = parseTimeUnit text

    mkDigits :: Integer -> GDigits
    mkDigits x = case [LexDig [i|D_#{d}|] | d <- show x] of
      [] -> GIDig (LexDig "D_0") -- shouldn't happen, TODO alert user?
      [dig] -> GIDig dig
      xs -> foldr GIIDig (GIDig (last xs)) (init xs)

parseTemporal _ (TemporalConstraint t Nothing text) =
  GTemporalConstraintNoDigits tc unit
  where
    tc = parseTComparison t
    unit = parseTimeUnit text

parseTimeUnit :: Text.Text -> GTimeUnit
parseTimeUnit text = case Text.take 3 $ Text.toLower text of
  "day" -> GDay_Unit
  "mon" -> GMonth_Unit
  "yea" -> GYear_Unit
  _xs ->
    trace [i|NLG.hs: unrecognised time unit: #{text}|] $
      GrecoverUnparsedTimeUnit (tString text)

parseConstraint :: NLGEnv -> RelationalPredicate -> GConstraint
parseConstraint
  env
  ( RPBoolStructR
      (Text.strip . mt2text -> a)
      RPis
      (AA.Not (bsr2text -> b))
    ) = case (nps, vps) of
    (np : _, vp : _) -> GRPleafS (fg np) (flipPolarity $ fg vp)
    _ -> GrecoverRPis (tString a) (tString [i|not #{b}|])
    where
      nps = parseAnyNoRecover "NP" env a
      vps = parseAnyNoRecover "VPS" env [i|is #{b}|]

parseConstraint
  env
  ( RPConstraint
      (Text.strip . mt2text -> a)
      RPis
      (Text.strip . mt2text -> b)
    ) = case (nps, vps) of
    (np : _, vp : _) -> GRPleafS (fg np) (fg vp)
    _ -> GrecoverRPis (tString a) (tString b)
    where
      nps = parseAnyNoRecover "NP" env a
      vps = parseAnyNoRecover "VPS" env [i|is #{b}|]

parseConstraint
  env
  ( RPConstraint
      (Text.strip . mt2text -> a)
      (RPTC (gfLin env . gf . parseTComparison -> t))
      (Text.strip . mt2text -> b)
    ) = case (sents, advs) of
    (s : _, adv : _) -> case s of
      GPredVPS np (GMkVPS t p vp) -> GRPleafS np (GMkVPS t p (GAdvVP vp adv))
      _ -> trace [i|parseConstraint: unable to parse #{showExpr [] $ gf s}|] fallback
    x -> trace [i|parseConstraint: unable to parse #{x}#{t}|] fallback
    where
      sents :: [GS]
      sents = fg <$> parseAnyNoRecover "S" env a
      advs :: [GAdv]
      advs = fg <$> parseAnyNoRecover "Adv" env (Text.unwords [t, b])
      fallback = GrecoverUnparsedConstraint (tString $ Text.unwords [a, t, b])

parseConstraint
  env
  ( RPConstraint
      (Text.strip . mt2text -> a)
      op@((`elem` [RPlt, RPlte, RPgt, RPgte]) -> True)
      (Text.strip . mt2text -> b)
    ) =
    case (nps, vps) of
      (np : _, vp : _) -> GRPleafS (fg np) (fg vp)
      _ -> GrecoverRPmath (tString opTxt) (tString a') (tString b')
    where
      aPrefix = Text.stripSuffix "'s age" a
      a' = fromMaybe a aPrefix -- policy holder's age -> policy holder

      b' = case sequenceA [aPrefix, Text.stripSuffix "years" b] of
        Just _ -> [i|is #{opEng} #{splitDigits b} old|]
        Nothing -> [i|is #{opEng'} #{b}|]

      (opTxt, opEng :: Text.Text, opEng' :: Text.Text) = case op of
        RPlt -> ("<", "less than", "less than")
        RPlte -> ("<=", "at most", "at most")
        RPgt -> (">", "more than", "greater than")
        RPgte -> (">=", "at least", "at least")

      nps = parseAnyNoRecover "NP" env a'
      vps = parseAnyNoRecover "VPS" env b'

parseConstraint env (rp2text -> txt) = parseTxtToGF "Constraint" env txt

parsePrePost :: NLGEnv -> Text.Text -> GPrePost
parsePrePost = parseTxtToGF "PrePost"

parseTxtToGF :: Gf a => String -> NLGEnv -> Text.Text -> a
parseTxtToGF cat env txt = fg tree
  where
    tree :| _ = parseAny cat env txt

parseAny :: String -> NLGEnv -> Text.Text -> NonEmpty Expr
parseAny cat env = snd . parseAnyRecoverNoRecover cat env

parseAnyNoRecover :: String -> NLGEnv -> Text.Text -> [Expr]
parseAnyNoRecover cat env = fst . parseAnyRecoverNoRecover cat env 

parseAnyRecoverNoRecover :: String -> NLGEnv -> Text.Text -> ([Expr], NonEmpty Expr)
parseAnyRecoverNoRecover cat env txt = (toreturn, toreturnRecovered)
  where
    cats = categories $ gfGrammar env
    types = Set.fromList [mkType [] c [] | c <- cats]

    typ = case readType cat of
      Just t@((`elem` types) -> True) -> t
      _ -> typeError cat cats

    toreturn = gfParse env typ txt

    -- TODO: later if grammar is ambiguous, should we rank trees here?
    toreturnRecovered = case toreturn of
      -- [] -> parseError cat --- Alternative, if we don't want to use recoverUnparsedX
      [] -> pure $ mkApp (mkCId [i|recoverUnparsed#{cat}|]) [mkStr $ Text.unpack txt]
      x:xs -> x :| xs

-- parseError :: String -> Text.Text -> a
-- parseError cat txt = error $ unwords ["parse"<>cat, "failed to parse", Text.unpack txt]

typeError :: String -> [CId] -> a
typeError cat actualCats =
  error [i|category #{cat} not a valid GF cat, use one of these instead: #{actualCats}|]

tString :: Text.Text -> GString
tString = GString . Text.unpack

splitDigits :: Text.Text -> Text.Text
splitDigits =
  Text.words
    >>> map splitDigitsOfWord
    >>> Text.unwords
  where
    -- splitDigits' "123" == "1 &+ 2 &+ 3"
    splitDigitsOfWord str
      | isNumeric str =
          str
            |> Text.intersperse '&'
            |> PCRE.gsub [PCRE.re|&|] (" &+ " :: Text.Text)
      | otherwise = str

    -- Note that this is NOT the same as (Text.all Char.isDigit str)
    -- because that allows for the c ase when str is empty, but this does not.
    isNumeric (decimal -> Right (_, "")) = True
    isNumeric _ = False

    -- splitDigit d
    --   | Text.all Char.isDigit d =
    --       Text.intercalate " &+ " $ Text.groupBy (\_ _ -> False) d
    -- splitDigit d = d

-- tk i = Text.pack . tk' i . Text.unpack
-- dp i = Text.pack . dp' i . Text.unpack


-- tk', dp' :: Int -> String -> String
-- tk' i = reverse . drop i . reverse -- tk 2 "hello" == "hel"
-- dp' i = reverse . take i . reverse -- dp 2 "hello" == "lo"


-----------------------------------------------------------------------------
-- Expand a set of rules

expandRulesForNLGE :: NLGEnv -> [Rule] -> XPileLog [Rule]
expandRulesForNLGE = expandRulesForNLGE' 4

expandRulesForNLG :: NLGEnv -> [Rule] -> [Rule]
expandRulesForNLG env = fst . xpLog . expandRulesForNLGE' 0 env

expandRulesForNLGE' :: Int -> NLGEnv -> [Rule] -> XPileLog [Rule]
expandRulesForNLGE' depth env rules = do
  mutterdhsf depth "expandRulesForNLG() called with rules" pShowNoColorS rules
  toreturn <- for uniqrs $ expandRuleForNLGE l4i $ depth + 1
  mutterdhsf depth "expandRulesForNLG() returning" pShowNoColorS toreturn
  pure toreturn
  where
    l4i = interpreted env
    usedrules = getExpandedRuleNames l4i `foldMap` rules
    uniqrs = [r | r <- rules, ruleName r `notElem` usedrules]

getExpandedRuleNames :: Interpreted -> Rule -> [RuleName]
getExpandedRuleNames l4i = \case
  Regulative {who} -> go getNamesBSR `foldMap` who
  Hornlike {clauses} -> getNamesHC l4i `foldMap` clauses
  _ -> []
  where
    getNamesBSR :: Interpreted -> Int -> BoolStructR -> [RuleName]
    getNamesBSR l4i depth (AA.Leaf rp)  =
      case expandRP l4i (depth + 1) rp of
        RPBoolStructR mt1 RPis _bsr -> [mt1]
        _                           -> []
    getNamesBSR l4i depth (AA.Not item)   = getNamesBSR l4i (depth + 1) item
    getNamesBSR l4i depth (AA.All lbl xs) = getNamesBSR l4i (depth + 1) `foldMap` xs
    getNamesBSR l4i depth (AA.Any lbl xs) = getNamesBSR l4i depth $ AA.All lbl xs

    getNamesRP :: Interpreted -> Int -> RelationalPredicate -> [RuleName]
    getNamesRP l4i depth (RPConstraint  mt1 RPis mt2) = [mt2]
    getNamesRP l4i depth (RPBoolStructR mt1 RPis bsr) = getNamesBSR l4i depth bsr
    getNamesRP _l4i _depth _x                          = []

    getNamesHC :: Interpreted -> HornClause2 -> [RuleName]
    getNamesHC l4i clause = headNames <> bodyNames
      where
        headNames = go getNamesRP $ hHead clause
        bodyNames = clause |> hBody |> maybeToList |> foldMap (go getNamesBSR)

    go :: (Interpreted -> Int -> a -> [RuleName]) -> a -> [RuleName]
    go get = get l4i 1

expandRuleForNLGE :: Interpreted -> Int -> Rule -> XPileLog Rule
expandRuleForNLGE l4i depth rule@Regulative{who, cond, upon, hence, lest} = do
  mutterd depth "expandRuleForNLGE: running Regulative"
  -- Maybe (XPileLogE BoolStructR)
  -- XPileLogE (Maybe BoolStructR)
  who'   <- travExpandBSRM who
  cond'  <- travExpandBSRM cond
  hence' <- travExpandRule hence
  lest'  <- travExpandRule lest
  mutterd depth "running expandPT"
  let upon' = expandPT l4i depth <$> upon
  pure rule
    { who = who'
    , cond = cond'
    , upon = upon'
    , hence = hence'
    , lest = lest'
    }
  where
    travExpandBSRM = goTrav expandBSRM
    travExpandRule = goTrav expandRuleForNLGE

    goTrav :: (Interpreted -> Int -> a -> XPileLog a) -> Maybe a -> XPileLog (Maybe a)
    goTrav f = traverse $ f l4i depth

expandRuleForNLGE l4i depth rule@Hornlike {} = do
  mutterd 4 "expandRuleForNLGE: running Hornlike"
  pure rule {clauses = expandClauses l4i depth $ clauses rule}

expandRuleForNLGE l4i depth rule@Constitutive {} = do
  mutterd 4 "expandRuleForNLGE: running Constitutive"
  pure rule {cond = expandBSR l4i depth <$> cond rule}

expandRuleForNLGE _ _ rule = do
  mutterd 4 "expandRuleForNLGE: running some other rule"
  pure rule

-- This is used for creating questions from the rule, so we only expand
-- the fields that are used in ruleQuestions
expandRuleForNLG :: Interpreted -> Int -> Rule -> Rule
expandRuleForNLG l4i depth = fst . xpLog . expandRuleForNLGE l4i depth

-- I suspect that original intention was to not include expansions in UPON?
-- But in any case, here is a function that is applied in expandRuleForNLG to expand the UPON field.
-- There's a test case for this in NLGSpec ("test expandRulesForNLG for pdpa1 with added UPON expansion")
expandPT :: Interpreted -> Int -> ParamText -> ParamText
expandPT l4i depth pt = maybe pt ptFromRP expanded
  where
    ptAsMt = [MTT $ pt2text pt]
    fallbackPTfromRP = mt2pt . rp2mt
    ptFromRP (RPParamText pt)         = pt
    ptFromRP (RPMT mt)                = mt2pt mt
    ptFromRP (RPConstraint _ RPis mt) = mt2pt mt
    ptFromRP rp@(RPBoolStructR _ RPis bsr@(AA.Leaf _)) = mt2pt [MTT $ bsr2text bsr] -- Only works if the BSR is a leaf; otherwise we lose structure when trying to convert a BSR into ParamText
    ptFromRP rp = trace [i|ptFromRP: encountered #{rp}|] $ fallbackPTfromRP rp

    expanded = listToMaybe
                [ outrp
                | (_scopename, symtab) <- Map.toList (scopetable l4i)
                , (_mytype, cs) <- maybeToList $ Map.lookup ptAsMt symtab
                , c <- cs
                , let outs = expandClause l4i depth c
                , outrp <- outs
                ]
