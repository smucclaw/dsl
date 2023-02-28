{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, NamedFieldPuns, FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module LS.NLP.NLG where


import LS.NLP.NL4
import LS.NLP.NL4Transformations
import LS.Types
import LS.Rule (Rule(..))
import PGF
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import qualified AnyAll as AA
import System.Environment (lookupEnv)
import Paths_natural4
import Data.Foldable as F

data NLGEnv = NLGEnv
  { gfGrammar :: PGF
  , gfLang :: Language
  , gfParse :: Type -> Text.Text -> [Expr]
  , gfLin :: Expr -> Text.Text
  , verbose :: Bool
  }

allLangs :: IO [Language]
allLangs = do
  grammarFile <- getDataFileName $ gfPath "NL4.pgf"
  gr <- readPGF grammarFile
  pure $ languages gr

getLang :: String -> Language
getLang str = case readLanguage str of
    Nothing -> error $ "language " <> str <> " not found"
    Just l -> l

myNLGEnv :: Language -> IO NLGEnv
myNLGEnv lang = do
  mpn <- lookupEnv "MP_NLG"
  let verbose = maybe False (read :: String -> Bool) mpn
  grammarFile <- getDataFileName $ gfPath "NL4.pgf"
  gr <- readPGF grammarFile
  let eng = getLang "NL4Eng"
      myParse typ txt = parse gr eng typ (Text.unpack txt)
  print "lang"
  print $ showLanguage lang
  let myLin = Text.pack . linearize gr lang
  pure $ NLGEnv gr lang myParse myLin verbose
  -- map (lineariselang typ txt) (languages gr)

gfPath :: String -> String
gfPath x = "grammars/" ++ x

-- lineariselang typ txt lang gr = pure $ NLGEnv gr eng myParse myLin verbose
--   where myLin = parse gr lang typ (Text.unpack txt)


-----------------------------------------------------------------------------
-- Main

-- WIP: crude way of keeping track of whether we're in hence, lest or whatever
data RecursionLevel = TopLevel | MyHence Int | MyLest Int
  deriving (Eq,Ord,Show)

getLevel :: RecursionLevel -> Int
getLevel l = case l of
  TopLevel -> 2
  MyHence i -> i
  MyLest i -> i

debugNesting :: RecursionLevel -> (Text.Text, Text.Text)
debugNesting TopLevel = (Text.pack "", Text.pack "")
debugNesting (MyHence _) = (Text.pack "Follow by:", Text.pack "")
debugNesting (MyLest _) = (Text.pack "In case of failure:", Text.pack "")

nlg :: NLGEnv -> Rule -> IO Text.Text
nlg = nlg' TopLevel

nlg' :: RecursionLevel -> NLGEnv -> Rule -> IO Text.Text
nlg' thl env rule = case rule of
    Regulative {subj,upon,cond,who,deontic,action,lest,hence} -> do
      let subjExpr = introduceSubj $ parseSubj env subj
          deonticExpr = parseDeontic deontic
          actionExpr = parseAction env action
          whoSubjExpr = case who of
                        Just w -> GSubjWho subjExpr (bsWho2gfWho (parseWhoBS env w))
                        Nothing -> subjExpr
          ruleText = gfLin env $ gf $ GRegulative whoSubjExpr deonticExpr actionExpr
          uponText = case upon of  -- TODO: doesn't work once we add another language
                      Just u -> "Upon " <> pt2text u <> ", "
                      Nothing -> mempty
          condText = case cond of
                      Just c ->
                        let condExpr = gf $ pastTense $ bsCond2gfCond (parseCondBS env c)
                         in ". If " <> gfLin env condExpr <> ", "
                      Nothing -> mempty

          ruleTextDebug = Text.unwords [prefix, uponText <> ruleText <> condText, suffix]
      lestText <- case lest of
                    Just r -> do
                      rt <- nlg' (MyLest i) env r
                      pure $ pad rt
                    Nothing -> pure mempty
      henceText <- case hence of
                    Just r -> do
                      rt <- nlg' (MyHence i) env r
                      pure $ pad rt
                    Nothing -> pure mempty

--      pure $ Text.unlines [ruleText, henceText, lestText]
      pure $ Text.strip $ Text.unlines [ruleTextDebug, henceText, lestText]
    Hornlike {clauses} -> do
      let headLins = gfLin env . gf . parseConstraint env . hHead <$> clauses -- :: [GConstraint] -- this will not become a question
          parseBodyHC cl = case hBody cl of
            Just bs -> gfLin env $ gf $ bsConstraint2gfConstraint $ parseConstraintBS env bs
            Nothing -> mempty
          bodyLins = parseBodyHC <$> clauses
      pure $ Text.unlines $ headLins <> ["when"] <> bodyLins
    RuleAlias mt -> do
      let ruleText = gfLin env $ gf $ parseSubj env $ mkLeafPT $ mt2text mt
          ruleTextDebug = Text.unwords [prefix, ruleText, suffix]
      pure $ Text.strip ruleTextDebug
    DefNameAlias {} -> pure mempty
    _ -> pure $ "NLG.hs is under construction, we don't support yet " <> Text.pack (show rule)
  where
    (prefix,suffix) = debugNesting thl
    i = getLevel thl + 2
    pad x = Text.replicate i " " <> x


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


ruleQuestions :: NLGEnv -> Maybe (MultiTerm,MultiTerm) -> Rule -> IO [AA.OptionallyLabeledBoolStruct Text.Text]
ruleQuestions env alias rule = do
  case rule of
    Regulative {subj,who,cond,upon} -> text
    Hornlike {clauses} -> do
      print "---"
      print $ ruleQnTrees env alias rule
      print "---"
      text
    Constitutive {cond} -> text
    DefNameAlias {} -> pure [] -- no questions needed to produce from DefNameAlias
    _ -> pure [AA.Leaf $ Text.pack ("ruleQuestions: doesn't work yet for " <> show rule)]
    where
      text = pure $ fmap (linBStext env) (concat $ ruleQnTrees env alias rule)


ruleQnTrees :: NLGEnv -> Maybe (MultiTerm,MultiTerm) -> Rule -> [[BoolStructGText]]
ruleQnTrees env alias rule = do
  let (youExpr, orgExpr) =
        case alias of
          Just (you,org) ->
              case parseSubj env . mkLeafPT . mt2text <$> [you, org] of
                [y,o] -> (y,o) -- both are parsed
                _ -> (GYou, GYou) -- dummy values
          Nothing -> (GYou, GYou) -- dummy values
  case rule of
    Regulative {subj,who,cond,upon} -> do
      let subjExpr = parseSubj env subj
          aliasExpr = if subjExpr==orgExpr then youExpr else referSubj subjExpr
          qWhoTrees = mkWhoText env GqPREPOST (GqWHO aliasExpr) <$> who
          qCondTrees = mkCondText env GqPREPOST GqCOND <$> cond
          qUponTrees = mkUponText env (GqUPON aliasExpr) <$> upon
      return $ catMaybes [qWhoTrees, qCondTrees, qUponTrees]
    Hornlike {clauses} -> do
      let bodyTrees = fmap (mkConstraintText env GqPREPOST GqCONSTR) . hBody <$> clauses
      return $ catMaybes bodyTrees
    Constitutive {cond} -> do
      let qCondTrees = mkCondText env GqPREPOST GqCOND <$> cond
      return $ catMaybes [qCondTrees]
    DefNameAlias {} -> pure []

linBStext :: NLGEnv -> BoolStructGText -> AA.OptionallyLabeledBoolStruct Text.Text
linBStext env = mapBSLabel (gfLin env . gf) (gfLin env . gf)

mkWhoText :: NLGEnv -> (GPrePost -> GText) -> (GWho -> GText) -> BoolStructR -> BoolStructGText
mkWhoText env f g bsr = mapBSLabel f g $ aggregateBoolStruct $ parseWhoBS env bsr

mkCondText :: NLGEnv -> (GPrePost -> GText) -> (GCond -> GText) -> BoolStructR -> BoolStructGText
mkCondText env f g bsr = mapBSLabel f g $ aggregateBoolStruct $ parseCondBS env bsr

mkConstraintText :: NLGEnv -> (GPrePost -> GText) -> (GConstraint -> GText) -> BoolStructR -> BoolStructGText
mkConstraintText env f g bsr = mapBSLabel f g $ aggregateBoolStruct $ parseConstraintBS env bsr

mkUponText :: NLGEnv -> (GUpon -> GText) -> ParamText -> BoolStructGText
mkUponText env f pt = AA.Leaf  (f $ parseUpon env pt)

-- mkUponText :: NLGEnv -> (GUpon -> GText) -> ParamText -> AA.OptionallyLabeledBoolStruct Text.Text
-- mkUponText env f = AA.Leaf . gfLin env . gf . f . parseUpon env

nlgQuestion :: NLGEnv -> Rule -> IO [Text.Text]
nlgQuestion env rl = do
  questionsInABoolStruct <- ruleQuestions env Nothing rl -- TODO: the Nothing means there is no AKA
  pure $ concatMap F.toList questionsInABoolStruct

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
parseDate mt = case Text.words $ mt2text mt of
  [d, m, y] -> GMkDate (tInt d) (tMonth m) (tInt y)
  _ -> GMkDate (GInt 999) (LexMonth "Jan") (GInt 999)
 where
  tInt :: Text.Text -> GInt
  tInt = GInt . read . Text.unpack

  tMonth :: Text.Text -> GMonth
  tMonth = LexMonth . Text.unpack

-- TODO: stop using *2text, instead use the internal structure
  -- "respond" :| []  -> respond : VP
  -- "demand" :| [ "an explanation for your inaction" ] -> demand : V2, NP complement, call ComplV2
  -- "assess" :| [ "if it is a Notifiable Data Breach" ] -> assess : VS, S complement, call ComplS2
parseAction :: NLGEnv -> BoolStructP -> GAction
parseAction env bsp = let txt = bsp2text bsp in
  case parseAny "Action" env txt of
    [] -> error $ msg "Action" txt
    x:_ -> fg x

parseSubj :: NLGEnv -> BoolStructP -> GSubj
parseSubj env bsp = let txt = bsp2text bsp in
  case parseAny "Subj" env txt of
    [] -> error $ msg "Subj" txt
    x:_ -> fg x

parseWho :: NLGEnv -> RelationalPredicate -> GWho
parseWho env rp = let txt = rp2text rp in
  case parseAny "Who" env txt of
    [] -> error $ msg "Who" txt
    x:_ -> fg x

parseCond :: NLGEnv -> RelationalPredicate -> GCond
parseCond env (RPConstraint c (RPTC t) d) = GTemporalConstraint cond tc date
  where
    cond = parseCond env (RPMT c)
    tc = parseTComparison t
    date = parseDate d
parseCond env rp = let txt = rp2text rp in
    case parseAny "Cond" env txt of
      [] -> error $ msg "Cond" txt
      x:_ -> fg x

parseUpon :: NLGEnv -> ParamText -> GUpon
parseUpon env pt = let txt = pt2text pt in
  case parseAny "Upon" env txt of
    [] -> error $ msg "Upon" txt
    x:_ -> fg x

parseConstraint :: NLGEnv -> RelationalPredicate -> GConstraint
parseConstraint env (RPBoolStructR a RPis (AA.Not b)) = case (nps,vps) of
  (np:_, vp:_) -> GRPleafS (fg np) (flipPolarity $ fg vp)
  _ -> GrecoverRPis (tString aTxt) (tString $ Text.unwords ["not", bTxt])
  where
    aTxt = mt2text a
    bTxt = bsr2text b
    nps = parseAnyNoRecover "NP" env aTxt
    vps = parseAnyNoRecover "VPS" env $ Text.unwords ["is", bTxt]

    tString :: Text.Text -> GString
    tString = GString . read . Text.unpack
parseConstraint env (RPConstraint a RPis b) = case (nps,vps) of
  (np:_, vp:_) -> GRPleafS (fg np) (fg vp)
  _ -> GrecoverRPis (tString aTxt) (tString bTxt)
  where
    aTxt = mt2text a
    bTxt = mt2text b
    nps = parseAnyNoRecover "NP" env aTxt
    vps = parseAnyNoRecover "VPS" env $ Text.unwords ["is", bTxt]

    tString :: Text.Text -> GString
    tString = GString . read . Text.unpack

parseConstraint env rp = let txt = rp2text rp in
  case parseAny "Constraint" env txt of
    [] -> error $ msg "Constraint" txt
    x:_ -> fg x

parsePrePost :: NLGEnv -> Text.Text -> GPrePost
parsePrePost env txt =
  case parseAny "PrePost" env txt of
    [] -> GrecoverUnparsedPrePost $ GString $ Text.unpack txt
    x:_ -> fg x

-- TODO: later if grammar is ambiguous, should we rank trees here?
parseAny :: String -> NLGEnv -> Text.Text -> [Expr]
parseAny cat env txt = res
  where
    typ = case readType cat of
            Nothing -> error $ unwords ["category", cat, "not found among", show $ categories (gfGrammar env)]
            Just t -> t
    res = case gfParse env typ txt of
            [] -> [mkApp (mkCId $ "recoverUnparsed"<>cat) [mkStr $ Text.unpack txt]]
            xs -> xs

parseAnyNoRecover :: String -> NLGEnv -> Text.Text -> [Expr]
parseAnyNoRecover cat env = gfParse env typ
  where
    typ = case readType cat of
            Nothing -> error $ unwords ["category", cat, "not found among", show $ categories (gfGrammar env)]
            Just t -> t

msg :: String -> Text.Text -> String
msg typ txt = "parse" <> typ <> ": failed to parse " <> Text.unpack txt

-----------------------------------------------------------------------------