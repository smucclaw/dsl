{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

{-| transpiler to SVG visualization of the AnyAll and/or trees.

Largely a wrapper. Most of the functionality is in the anyall lib.

-}

module LS.XPile.Purescript where

import AnyAll qualified as AA
import AnyAll.BoolStruct (alwaysLabeled)
import Control.Applicative (liftA2)
import Control.Monad (guard, join, liftM, unless, when)
import Data.Bifunctor (Bifunctor (..), first, second)
import Data.Char qualified as Char
import Data.Either (lefts, rights)
import Data.Foldable (for_)
import Data.HashMap.Strict ((!))
import Data.HashMap.Strict qualified as Map
import Data.List (sortOn)
import Data.List qualified as DL
import Data.List.Split (chunk)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ord qualified
import Data.String.Interpolate (i, __i)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Traversable (for)
import Flow ((|>))
import LS.Interpreter (getMarkings, qaHornsT, QAHorn(..))
import LS.NLP.NL4Transformations ()
import LS.NLP.NLG
  ( NLGEnv (..),
    expandRulesForNLG,
    expandRulesForNLGE,
    ruleQuestions,
    ruleQuestionsNamed,
    parseSubj,
    textViaQaHorns
  )
import LS.Rule (Interpreted (..), Rule (..), ruleLabelName)
import LS.Types
  ( BoolStructT,
    RuleName,
    defaultInterpreterOptions,
    mt2text,
  )
import LS.Utils ((|$>))
import LS.XPile.Logging
  ( XPileLog,
    XPileLogE,
    mutter,
    mutterd,
    mutterd1,
    mutterd2,
    mutterdhs,
    mutterdhsf,
    mutters,
    xpError,
    xpReturn, XPileLogW,
    pShowNoColorS
  )
import PGF (showLanguage)
import Text.Pretty.Simple (pShowNoColor)

-- | extract the tree-structured rules from Interpreter
-- currently: construct a Data.Map of rulenames to exposed decision root expanded BSR
-- in future: also ship out a Marking which represents the TYPICALLY values
-- far future: construct a JSON with everything in it, and get the Purescript to read the JSON, so we are more interoperable with non-FP languages


-- | shim for Purescript tuples which use slightly different syntax
data Tuple a b = Tuple a b
  deriving (Show, Eq, Ord)

-- | output Haskell tuples to Purescript
toTuple :: (a,b) -> Tuple a b
toTuple (x,y) = Tuple x y

-- | RuleName to text multiterm
textMT :: [RuleName] -> [T.Text]
textMT = map mt2text

slashNames :: [RuleName] -> String
slashNames names = T.unpack (T.intercalate " / " (mt2text <$> names))

mutterRuleNameAndBS ::          [([RuleName], [BoolStructT])]
                    -> XPileLog [([RuleName], [BoolStructT])]
mutterRuleNameAndBS rnbss = do
  mutterd 3 "rulename, bs pairs:"
  for_ rnbss $ \(names, bs) -> do
    mutterdhsf 4 (slashNames names)
      pShowNoColorS bs
  return rnbss

-- two boolstructT: one question and one phrase
namesAndStruct :: Interpreted -> [Rule] -> XPileLog [([RuleName], [BoolStructT])]
namesAndStruct l4i rl = do
  mutter $ "*** namesAndStruct: running on " ++ show (length rl) ++ " rules"
  mutter "calling qaHornsT against l4i"
  mutterdhsf 3 "we know qaHornsT returns" pShowNoColorS (qaHornsT l4i)
  mutterRuleNameAndBS [ (names, [bs]) | QAHorn names qahead bs <- qaHornsT l4i]

-- | for each rule, construct the questions for that rule;
-- and then jam them together with all the names for all the rules???
namesAndQ :: NLGEnv -> [Rule] -> XPileLog [([RuleName], [BoolStructT])]
namesAndQ env rl = do
  mutterdhsf 3 "namesAndQ: name" show name
  mutterdhsf 3 "namesAndQ: about to call ruleQuestions with alias=" show alias
  expandedRules <- expandRulesForNLGE env rl
  questStruct <- traverse (ruleQuestions env alias) expandedRules
  mutterdhsf 3 "namesAndQ: back from ruleQuestions, questStruct =" pShowNoColorS questStruct
  let wut = concat [ [ (name, q) -- [TODO] this is probably the source of bugs.
                     | q' <- q ]
                   | q <- questStruct ]
  mutter $ "*** wut the heck are we returning? like, " ++ show (length wut) ++ " things."
  sequence_ [ mutterdhsf 4 (show n) pShowNoColorS w | (n,w) <- zip [1..] wut ]
  return wut
  where
    name = map ruleLabelName rl
    alias = listToMaybe [ (you,org) | DefNameAlias you org _ _ <- rl]
    -- [AA.OptionallyLabeledBoolStruct Text.Text]

-- | not sure why this is throwing away information
combine :: [([RuleName], [BoolStructT])]
        -> [([RuleName], [BoolStructT])]
        -> XPileLog [([RuleName], [BoolStructT])]
combine = combine' 3

combine' :: Int -- ^ depth
         -> [([RuleName], [BoolStructT])]
         -> [([RuleName], [BoolStructT])]
         -> XPileLog [([RuleName], [BoolStructT])]

combine' d [] []     = mutter "*** combine: case 1, nil" >> pure []
combine' d (b:bs) [] = mutter "*** combine: case 2, nil" >> pure []
combine' d [] (q:qs) = mutter "*** combine: case 3, nil" >> pure []
combine' d (b:bs) (q:qs) = do
  mutterd  d "combine: case 4, non-nil"
  mutterd1 d "input"
  mutterdhsf (d+2) "fst b"    pShowNoColorS (fst b)
  mutterdhsf (d+2) "snd b ++" pShowNoColorS (snd b)
  mutterdhsf (d+2) "snd q"    pShowNoColorS (snd q)
  (:) <$> pure (fst b, snd b <> snd q) <*> combine' (d+1) bs qs


-- [TODO] shouldn't this recurse down into the All and Any structures?
-- something like fixNot AA.Any k xs = AA.Any k (fixNot <$> xs)
fixNot :: BoolStructT -> BoolStructT
fixNot (AA.Leaf x) = AA.Leaf x
fixNot (AA.Not (AA.Leaf x)) = AA.Leaf x
fixNot y = y

-- | this throws away the first argument, in favour of the second. Not sure about this ...
justQuestions :: BoolStructT -> [BoolStructT] -> BoolStructT
justQuestions (AA.All Nothing a) q = AA.All Nothing q
justQuestions (AA.Any Nothing a) q = AA.Any Nothing q
justQuestions xs y = xs

justStatements :: BoolStructT -> [BoolStructT] -> BoolStructT
justStatements (AA.All Nothing a) q = AA.All Nothing a
justStatements (AA.Any Nothing a) q = AA.Any Nothing a
justStatements xs y = xs

labelQs :: [AA.OptionallyLabeledBoolStruct T.Text] -> [AA.BoolStruct (AA.Label T.Text) T.Text]
labelQs = map alwaysLabeled

biggestQ :: NLGEnv -> [Rule] -> XPileLog [BoolStructT]
biggestQ env rl = do
  mutter $ "*** biggestQ: running"
  let alias = listToMaybe [ (you,org) | DefNameAlias{name = you, detail = org} <- rl]
  q <- traverse (ruleQuestionsNamed env alias) $ expandRulesForNLG env rl
  let flattened = q |$> second (AA.extractLeaves <$>) -- \(x,ys) -> (x, [ AA.extractLeaves y | y <- ys])
      onlyqs = Map.fromList q
      sorted = sortOn (Data.Ord.Down . DL.length) flattened
  case (null sorted, fst (DL.head sorted) `Map.lookup` onlyqs) of
    (True, _) -> pure []
    (_, Nothing) -> do
      mutter [i|biggestQ didn't work, couldn't find #{fst $ DL.head sorted} in dict|]
      pure []
    (_, Just x) -> pure x

biggestS :: NLGEnv -> [Rule] -> XPileLog [BoolStructT]
biggestS env rl = do
  mutter $ "*** biggestS running"
  q <- join $ combine <$> namesAndStruct (interpreted env) rl <*> namesAndQ env rl
  let flattened = q |$> second (AA.extractLeaves <$>) -- \(x,ys) -> (x, [ AA.extractLeaves y | y <- ys])

      onlys = Map.fromList
        [ (x, justStatements yh (map fixNot yt))
        | (x,y) <- q
        , let Just (yh, yt) = DL.uncons y ]

      sorted = sortOn (Data.Ord.Down . DL.length) flattened
  return $
    if null sorted
      then []
      else pure $ onlys ! fst (DL.head sorted)

asPurescript :: NLGEnv -> [Rule] -> XPileLogE String
asPurescript env rl = do
  let nlgEnvStr = env |> gfLang |> showLanguage
      l4i       = env |> interpreted
  mutter [i|** asPurescript running for gfLang=#{nlgEnvStr}|]

  mutterd 3 "building namesAndStruct"
  nAS <- namesAndStruct l4i rl
  mutterdhsf 3 "built namesAndStruct" pShowNoColorS nAS

  mutterd 3 "building namesAndQ"
  nAQ <- namesAndQ      env rl
  mutterdhsf 3 "built namesAndQ" pShowNoColorS nAQ

  mutterd 3 "combining nAS and nAQ to form c'"
  c'  <- combine nAS nAQ
  mutterdhsf 3 "c' =" pShowNoColorS c'

  guts <- sequence [
    do
      mutterdhsf 3 "names: " show ( mt2text <$> names )
      mutterdhsf 4 "hbs = head boolstruct" show hbs
      mutterdhsf 4 "tbs = tail boolstruct" show tbs
      mutterdhsf 4 "fixedNot" show fixedNot
      mutterdhsf 4 "jq" show jq
      mutterdhsf 4 "labeled" show labeled
      -- return as an Either
      xpReturn $ toTuple ( T.intercalate " / " (mt2text <$> names) , labeled)

    | (names,bs) <- c'
    , let Just (hbs, tbs) = DL.uncons bs
          fixedNot = map fixNot tbs
          jq       = justQuestions hbs fixedNot
          labeled  = alwaysLabeled jq
    ]

  let nlgEnvStrLower = Char.toLower <$> nlgEnvStr
      listOfMarkings = Map.toList . AA.getMarking $ getMarkings l4i
      gutsRights = rights guts
      gutsLefts  = lefts  guts

  mutterdhsf 3 "Guts, Lefts (fatal errors)"        pShowNoColorS gutsLefts
  mutterdhsf 3 "Guts, Rights (successful results)" pShowNoColorS gutsRights

  mutter "*** Markings"
  mutters [
    [__i|
      **** #{fst m}
      #{snd m}
    |]
    | m <- listOfMarkings
    ]

  xpReturn [__i|
    #{nlgEnvStrLower} :: Object.Object (Item String)
    #{nlgEnvStrLower} = Object.fromFoldable
      #{pShowNoColor gutsRights}
    #{nlgEnvStrLower}Marking :: Marking
    #{nlgEnvStrLower}Marking = Marking $ Map.fromFoldable
      #{TL.replace "False" "false"
        . TL.replace "True" "true"
        . pShowNoColor $
            fmap toTuple listOfMarkings}
  |]
          -- #{pretty $ showLanguage $ gfLang env}Statements :: Object.Object (Item String)
          -- , (pretty $ showLanguage $ gfLang env) <> "Statements = Object.fromFoldable " <>
          --   (pretty $ TL.unpack (
          --       pShowNoColor
          --         [ toTuple ( T.intercalate " / " (mt2text <$> names)
          --                 , alwaysLabeled (justStatements (head bs) (map fixNot (tail bs))))
          --         | (names,bs) <- (combine (namesAndStruct env rl) (namesAndQ env rl))
          --         ]
          --       )
          --   )

translate2PS :: [NLGEnv] -> NLGEnv -> [Rule] -> XPileLogE String
translate2PS nlgEnvs eng rules = do
  mutter [__i|** translate2PS: running against #{length rules} rules|]
  mutter [i|*** nlgEnvs has #{length nlgEnvs} elements|]
  mutter [i|*** eng.gfLang = #{gfLang eng}|]

  -------------------------------------------------------------
  -- topBit
  -------------------------------------------------------------
  mutter $ "** calling biggestQ"
  bigQ <- biggestQ eng rules
  mutter $ "** got back bigQ"
  mutter $ show bigQ
  let topBit =
        bigQ
          |$> alwaysLabeled
          |> pShowNoColor
          |> TL.init
          |> TL.tail
          |> interviewRulesRHS2topBit
  mutterdhsf 2 "topBit =" pShowNoColorS topBit

  -------------------------------------------------------------
  -- New bottomBit
  -------------------------------------------------------------
  mutterd 2 "trying the new approach based on qaHornsT"
  qaHornsAllLangs :: [Either XPileLogW String] <-
    for nlgEnvs $ \nlgEnv@(NLGEnv {gfLang}) -> do
      let nlgEnvStrLower = gfLang |> showLanguage |$> Char.toLower
          l4i       = interpreted nlgEnv
          listOfMarkings = l4i |> getMarkings |> AA.getMarking |> Map.toList

      -- The Right may contain duplicates, so we need to nub later.
      hornByLang :: Either XPileLogW [Tuple String (AA.BoolStruct (AA.Label T.Text) T.Text)] <-
        qaHornsByLang rules nlgEnv

      case hornByLang of
        Left err -> xpError err
        Right hornByLang -> xpReturn [__i|
          #{nlgEnvStrLower} :: Object.Object (Item String)
          #{nlgEnvStrLower} = Object.fromFoldable
            #{pShowNoColor $ DL.nub hornByLang}

          #{nlgEnvStrLower}Marking :: Marking
          #{nlgEnvStrLower}Marking = Marking $ Map.fromFoldable
            #{TL.replace "False" "false"
              . TL.replace "True" "true"
              . pShowNoColor $
                  fmap toTuple listOfMarkings}
        |]
  -- mutterdhsf 2 "qaHornsAllLangs" pShowNoColorS qaHornsRights

  -------------------------------------------------------------
  -- bottomBit
  -------------------------------------------------------------
  -- mutterd 2 "constructing bottomBit by calling asPurescript over rules"
  -- bottomBit <- traverse (`asPurescript` rules) nlgEnvs
  -- mutterdhsf 2 "bottomBit without running rights" pShowNoColorS bottomBit
  -- mutterdhsf 2 "actual bottomBit output" pShowNoColorS (rights bottomBit)

  -- Stitch the top, middle and bottom bits together.

  -- interviewRules2 :: Map.Map String (Item String)
  -- interviewRules2 = Map.fromList #{qaHornsRights}
  let x <.> y = x <> "\n\n" <> y
  xpReturn [__i|
    #{topBit}

    #{foldr (<.>) mempty $ rights qaHornsAllLangs}

  |]
--    #{unlines $ rights bottomBit}



qaHornsByLang :: [Rule] -> NLGEnv -> XPileLogE [Tuple String (AA.BoolStruct (AA.Label T.Text) T.Text)]
qaHornsByLang rules langEnv = do
  mutterd 3 ("qaHornsByLang for language " ++ show (gfLang langEnv))
  let alias = listToMaybe [ (you,org) | DefNameAlias{name = you, detail = org} <- rules]
      subject = listToMaybe [ parseSubj langEnv person | Regulative{subj = person} <- rules]
      qaHT = textViaQaHorns langEnv alias subject
      qaHornNames = foldMap qaRulename qaHT
      -- qaHT = qaHornsT $ interpreted langEnv -- [ (names, bs) | (names, bs) <- qaHornsT (interpreted langEnv)]
      d = 4
  mutterdhsf d "qaHT fsts" show (qaRulename <$> qaHT)
  mutterdhsf d "all qaHT" pShowNoColorS qaHT
  mutterdhsf d "qaHornNames" show qaHornNames
  mutterd d "traversing ruleQuestionsNamed"
  allRQs <- traverse (ruleQuestionsNamed langEnv alias) $ expandRulesForNLG langEnv rules
  -- first we see which of these actually returned anything useful
  mutterd d "all rulequestionsNamed returned"

  measuredRQs <- for allRQs $ \(rn, asqn) -> do
    mutterdhsf (d+1) (show rn) pShowNoColorS asqn
    mutterd (d+1) [i|size of [BoolStruct] = #{length asqn}|]
    case compare (length asqn) 1 of
      GT -> xpReturn (rn, AA.All Nothing asqn)
      EQ -> xpReturn (rn, head asqn)
      _ -> xpError [[i|ruleQuestion not of interest: #{rn}|]]

  mutterdhsf d "measured RQs, rights (successes) ->" show (rights measuredRQs)
  mutterdhsf d "measured RQs, lefts (failures) ->"   show (lefts  measuredRQs)

  -- now we filter for only those bits of questStruct whose names match the names from qaHorns.
  wantedRQs <- for (rights measuredRQs) $ \case
    (rn@((`elem` qaHornNames) -> True), asqn) -> xpReturn (rn, asqn)
    (rn, _) -> xpError [[i| #{rn} not named in qaHorns"|]]

  mutterd d "wanted RQs, rights (successes) ->"
  for_ (rights wantedRQs) (\(rn, asqn) -> mutterdhsf (d+1) (show rn) pShowNoColorS asqn)
  mutterdhsf d "wanted RQs, lefts (failures) ->"   show (lefts  wantedRQs)

  let rqMap = Map.fromList (rights wantedRQs)

  let qaHornsWithQuestions = concatMap catMaybes
        [ [ if Map.member n rqMap then Just (names, rqMap Map.! n) else Nothing
          | n <- names ]
        | names <- qaRulename <$> qaHT ]

  mutterdhsf d "qaHornsWithQuestions" pShowNoColorS qaHornsWithQuestions

  let qaHTBit = qaHornsWithQuestions
                |$> bimap slashNames alwaysLabeled
                |$> toTuple

  mutterdhsf d "qaHTBit =" pShowNoColorS qaHTBit
  xpReturn qaHTBit

interviewRulesRHS2topBit :: TL.Text -> String
interviewRulesRHS2topBit interviewRulesRHS =
  let interviewRulesRHS' = case interviewRulesRHS of
        (TL.null -> True) -> [i|Leaf ""|]
        _ -> interviewRulesRHS
  in [__i|
    -- This file was automatically generated by natural4.
    -- Do not edit by hand.
    -- Instead, revise the toolchain starting at smucclaw/dsl/lib/haskell/natural4/app/Main.hs

    module RuleLib.Interview where

    import Prelude
    import Data.Either
    import Data.Maybe
    import Data.Tuple
    import Data.Map as Map
    import Foreign.Object as Object

    import AnyAll.Types

    interviewRules :: Item String
    interviewRules = #{interviewRulesRHS'}

    interviewRules_nl :: NLDict
    interviewRules_nl =
      Map.fromFoldable
        [ ]

  |]

