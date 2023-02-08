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

myNLGEnv :: IO NLGEnv
myNLGEnv = do
  mpn <- lookupEnv "MP_NLG"
  let verbose = maybe False (read :: String -> Bool) mpn
  grammarFile <- getDataFileName $ gfPath "NL4.pgf"
  gr <- readPGF grammarFile
  let lang = case readLanguage "NL4Eng" of 
        Nothing -> error $ "concrete language NL4Eng not found among " <> show (languages gr)
        Just l -> l
      myParse typ txt = parse gr lang typ (Text.unpack txt)
      myLin = Text.pack . linearize gr lang
  pure $ NLGEnv gr lang myParse myLin verbose

gfPath :: String -> String
gfPath x = "grammars/" ++ x

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
debugNesting (MyLest _) = (Text.pack "If you disobey, then", Text.pack "D:")
debugNesting (MyHence _) = (Text.pack "When that happens,", Text.pack "\\:D/")

nlg :: NLGEnv -> Rule -> IO Text.Text
nlg = nlg' TopLevel

nlg' :: RecursionLevel -> NLGEnv -> Rule -> IO Text.Text
nlg' thl env rule = case rule of 
    Regulative {subj,who,deontic,action,lest,hence} -> do
      let subjExpr = parseSubj env subj
          deonticExpr = parseDeontic deontic
          actionExpr = parseAction env action
          whoSubjExpr = case who of 
                        Just w -> GSubjWho subjExpr (bsWho2gfWho (parseWho env <$> w))
                        Nothing -> subjExpr
          ruleText = gfLin env $ gf $ GRegulative whoSubjExpr deonticExpr actionExpr
          ruleTextDebug = Text.unwords [prefix, ruleText, suffix]
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
    RuleAlias mt -> do
      let ruleText = gfLin env $ gf $ parseSubj env $ mkLeafPT $ mt2text mt
          ruleTextDebug = Text.unwords [prefix, ruleText, suffix]
      pure $ Text.strip $ ruleTextDebug
    DefNameAlias {} -> pure mempty
    _ -> pure "NLG.hs is under construction, we only support singing"
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

-- type BoolStructT  = AA.OptionallyLabeledBoolStruct Text.Text
-- type BoolStructP = AA.OptionallyLabeledBoolStruct ParamText
-- type BoolStructR = AA.OptionallyLabeledBoolStruct RelationalPredicate
    --  Expected: BoolStructT
    --     Actual: AA.BoolStruct (Maybe (AA.Label Text.Text)) (IO [String])

ruleQuestions :: NLGEnv -> Maybe (MultiTerm,MultiTerm) -> Rule -> IO [AA.OptionallyLabeledBoolStruct Text.Text]
-- ruleQuestions env alias rule = pure [AA.Leaf (Text.pack "NLG.hs is under construction, this will work again shortly")]
ruleQuestions env alias rule = do
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
          aliasExpr = if subjExpr==orgExpr then youExpr else subjExpr
          mkWhoQ = gfLin env . gf . GqWHO aliasExpr . parseWho env -- :: RelationalPredicate -> Text
          mkCondQ = gfLin env . gf . GqCOND . parseCond env
          mkUponQ = gfLin env . gf . GqUPON aliasExpr . parseUpon env -- :: ParamText -> Text
          qWhoBS = fmap (mkWhoQ <$>) who -- fmap is for Maybe, <$> for BoolStruct
          qCondBS = fmap (mkCondQ <$>) cond
          qUponBS = case upon of 
                      Just u -> Just $ AA.Leaf $ mkUponQ u
                      Nothing -> Nothing
      pure $ catMaybes [qWhoBS, qCondBS, qUponBS]
    -- Constitutive {cond} -> do
    --   condBSR <- mapM (bsr2questions qsCond gr dummySubj) cond
    --   pure $ concat $ catMaybes [condBSR]
    -- Hornlike {keyword, clauses} -> do
    --   let kw = keyword2cid keyword
    --   parsedClauses <- mapM (parseHornClause2 env kw) clauses
    --   let statements = [s | [s] <- parsedClauses] -- TODO: eventually make a new function that parses RPs for HornClause that doesn't make its argument UDFragment
    --   let questions = concatMap (mkQ qsCond gr dummySubj) statements :: [AA.OptionallyLabeledBoolStruct Text.Text]
    --   pure questions
    DefNameAlias {} -> pure [] -- no questions needed to produce from DefNameAlias
    _ -> pure [AA.Leaf (Text.pack $ "ruleQuestions: doesn't work yet for " <> show rule)]
{-
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
-}

nlgQuestion :: NLGEnv -> Rule -> IO [Text.Text]
nlgQuestion env rl = do
  rulesInABoolStruct <- ruleQuestions env Nothing rl -- TODO: the Nothing means there is no AKA
  pure $ concatMap F.toList rulesInABoolStruct

-----------------------------------------------------------------------------
-- Parsing fields into GF categories – all typed, no PGF.Expr allowed

parseActions :: NLGEnv -> Text.Text -> [GAction]
parseUpons :: NLGEnv -> Text.Text -> [GUpon]
parseSubjs :: NLGEnv -> Text.Text -> [GSubj]
parseConds :: NLGEnv -> Text.Text -> [GCond]
parseWhos :: NLGEnv -> Text.Text -> [GWho]

parseActions e t = fg <$> parseAny "Action" e t
parseUpons e t =  fg <$> parseAny "Upon" e t
parseSubjs e t = fg <$> parseAny "Subj" e t
parseConds e t = fg <$> parseAny "Cond" e t
parseWhos e t = fg <$> parseAny "Who" e t

-- TODO: stop using *2text, instead use the internal structure
  -- "respond" :| []  -> respond : VP 
  -- "demand" :| [ "an explanation for your inaction" ] -> demand : V2, NP complement, call ComplV2
  -- "assess" :| [ "if it is a Notifiable Data Breach" ] -> assess : VS, S complement, call ComplS2
parseAction :: NLGEnv -> BoolStructP -> GAction
parseAction env action = case parseActions env $ bsp2text action of 
                       [] -> error $ "no parse for " <> Text.unpack (bsp2text action)
                       x:_ -> x

parseSubj :: NLGEnv -> BoolStructP -> GSubj
parseSubj env subj = case parseSubjs env $ bsp2text subj of 
                       [] -> error $ "no parse for " <> Text.unpack (bsp2text subj)
                       x:_ -> x

parseWho :: NLGEnv -> RelationalPredicate -> GWho
parseWho env rp = case parseWhos env (rp2text rp) of
                    x:_ -> x
                    [] -> error $ "parseWho: failed to parse " <> Text.unpack (rp2text rp)

parseCond :: NLGEnv -> RelationalPredicate -> GCond
parseCond env rp = case parseConds env (rp2text rp) of
                    x:_ -> x
                    [] -> error $ "parseCond: failed to parse " <> Text.unpack (rp2text rp)

parseUpon :: NLGEnv -> ParamText -> GUpon
parseUpon env pt = case parseUpons env (pt2text pt) of
                    x:_ -> x
                    [] -> error $ "parseUpon: failed to parse " <> Text.unpack (pt2text pt)

parseDeontic :: Deontic -> GDeontic
parseDeontic DMust = GMUST
parseDeontic DMay = GMAY
parseDeontic DShant = GSHANT

parseAny :: String -> NLGEnv -> Text.Text -> [Expr] 
parseAny cat env = gfParse env typ 
  where
    typ = case readType cat of 
            Nothing -> error $ unwords ["category", cat, "not found among", show $ categories (gfGrammar env)]
            Just t -> t


-----------------------------------------------------------------------------