{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- [TODO] export list

-- | transpiler to SVG visualization of the AnyAll and/or trees.
--
-- Largely a wrapper. Most of the functionality is in the anyall lib.
module LS.XPile.AaJson
  ( -- * These are the top-level entry points for the Purescript transpiler
    translate2AaJson,
  )
where

import AnyAll qualified as AA
import AnyAll.BoolStruct (alwaysLabeled, BoolStructLT)
import Control.Monad (join)
import Data.Bifunctor (Bifunctor (..), second)
import Data.Char qualified as Char
import Data.Either (rights)
import Data.HashMap.Strict ((!))
import Data.HashMap.Strict qualified as Map
import Data.List (sortOn)
import Data.List qualified as DL
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ord qualified
import Data.String.Interpolate (i, __i)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Traversable (for)
import Flow ((|>))
import LS.Interpreter (Interpreted (..), getMarkings, qaHornsT)
import LS.NLP.NLG
  ( NLGEnv (..),
    expandRulesForNLG,
    expandRulesForNLGE,
    parseSubj,
    ruleQuestions,
    ruleQuestionsNamed,
    textViaQaHorns,
  )
import LS.Rule (Rule (..), ruleLabelName)
import LS.Types
  ( BoolStructT,
    RuleName,
    mt2text,
  )
import LS.Utils ((|$>))
import LS.XPile.Logging
  ( XPileLog,
    XPileLogE,
    XPileLogW,
    xpError,
    xpReturn,
  )
import PGF (showLanguage)
import Data.Aeson (ToJSON(toJSON), object, (.=),Value)
import Data.Aeson.Key (fromString)
import Data.Aeson.Encode.Pretty (encodePretty)


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


labelToAaJson :: AA.Label T.Text -> Value
labelToAaJson (AA.Pre a) = object [ "Pre" .= a ]
labelToAaJson (AA.PrePost a b) = object [ "PrePost" .= [a,b] ]
labelToAaJson (AA.Metadata a) = object [ "Metadata" .= a ]

bsToAaJson :: BoolStructLT -> Value
bsToAaJson (AA.All l bs) = object [ "All" .= object["label" .= labelToAaJson l, "children" .= [bsToAaJson c | c <- bs] ]]
bsToAaJson (AA.Any l bs) = object [ "Any" .= object["label" .= labelToAaJson l, "children" .=  [bsToAaJson c | c <- bs] ]]
bsToAaJson (AA.Leaf a) = object [ "Leaf" .= a ]
bsToAaJson (AA.Not bs) = object [ "Not" .= bsToAaJson bs]

instance ToJSON (Tuple String BoolStructLT) where
 toJSON (Tuple a b) =
    object [ fromString a .= bsToAaJson b]

-- | RuleName to text multiterm
textMT :: [RuleName] -> [T.Text]
textMT = map mt2text

slashNames :: [RuleName] -> String
slashNames names = T.unpack (T.intercalate " / " (mt2text <$> names))

-- two boolstructT: one question and one phrase
namesAndStruct :: Interpreted -> [Rule] -> XPileLog [([RuleName], [BoolStructT])]
namesAndStruct l4i rl = do
  pure [ (names, [bs]) | (names, bs) <- qaHornsT l4i]

-- | for each rule, construct the questions for that rule;
-- and then jam them together with all the names for all the rules???
namesAndQ :: NLGEnv -> Interpreted -> [Rule] -> XPileLog [([RuleName], [BoolStructT])]
namesAndQ env l4i rl = do
  expandedRules <- expandRulesForNLGE l4i rl
  questStruct <- traverse (ruleQuestions env alias) expandedRules
  let wut = concat [ [ (name, q) -- [TODO] this is probably the source of bugs.
                     | q' <- q ]
                   | q <- questStruct ]
  return wut
  where
    name = map ruleLabelName rl
    alias = listToMaybe [ (you,org) | DefNameAlias you org _ _ <- rl]

-- | not sure why this is throwing away information
combine :: [([RuleName], [BoolStructT])]
        -> [([RuleName], [BoolStructT])]
        -> XPileLog [([RuleName], [BoolStructT])]
combine = combine' 3

combine' :: Int -- ^ depth
         -> [([RuleName], [BoolStructT])]
         -> [([RuleName], [BoolStructT])]
         -> XPileLog [([RuleName], [BoolStructT])]

combine' d [] []     = pure []
combine' d (b:bs) [] = pure []
combine' d [] (q:qs) = pure []
combine' d (b:bs) (q:qs) = do
  (:) (fst b, snd b <> snd q) <$> combine' (d+1) bs qs


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

biggestQ :: NLGEnv -> Interpreted -> [Rule] -> XPileLog [BoolStructT]
biggestQ env l4i rl = do
  let alias = listToMaybe [ (you,org) | DefNameAlias{name = you, detail = org} <- rl]
  q <- ruleQuestionsNamed env alias `traverse` expandRulesForNLG l4i  rl
  let flattened = q |$> second (AA.extractLeaves <$>)
      onlyqs = Map.fromList q
      sorted = sortOn (Data.Ord.Down . DL.length) flattened
  case (null sorted, fst (DL.head sorted) `Map.lookup` onlyqs) of
    (True, _) -> pure []
    (_, Nothing) -> do
      pure []
    (_, Just x) -> pure x

biggestS :: NLGEnv -> Interpreted -> [Rule] -> XPileLog [BoolStructT]
biggestS env l4i rl = do
  q <- join $ combine <$> namesAndStruct l4i rl <*> namesAndQ env l4i rl
  let flattened = q |$> second (AA.extractLeaves <$>)

      onlys = Map.fromList
        [ (x, justStatements yh (map fixNot yt))
        | (x,y) <- q
        , let Just (yh, yt) = DL.uncons y ]

      sorted = sortOn (Data.Ord.Down . DL.length) flattened
  return $
    if null sorted
      then []
      else pure $ onlys ! fst (DL.head sorted)

translate2AaJson :: [NLGEnv] -> NLGEnv -> Interpreted -> XPileLogE String
translate2AaJson nlgEnvs eng l4i = do
  let rules = origrules l4i

  bigQ <- biggestQ eng l4i rules

  qaHornsAllLangs :: [Either XPileLogW String] <-
    for nlgEnvs \nlgEnv@(NLGEnv {gfLang}) -> do
      let nlgEnvStrLower = gfLang |> showLanguage |$> Char.toLower
          listOfMarkings = l4i |> getMarkings |> AA.getMarking |> Map.toList

      hornByLang :: Either XPileLogW [Tuple String (AA.BoolStruct (AA.Label T.Text) T.Text)] <-
        qaHornsByLang rules nlgEnv l4i

      case hornByLang of
        Left err -> xpError err
        Right hornByLang -> xpReturn [__i|
            #{encodePretty $ toJSON $ DL.nub hornByLang}
        |]

  let qaHornsStrings = rights qaHornsAllLangs
  xpReturn [__i|
    [
      #{DL.intercalate ",\n" qaHornsStrings}
    ]
  |]


qaHornsByLang :: [Rule] -> NLGEnv -> Interpreted -> XPileLogE [Tuple String (AA.BoolStruct (AA.Label T.Text) T.Text)]
qaHornsByLang rules langEnv l4i = do
  let alias = listToMaybe [ (you,org) | DefNameAlias{name = you, detail = org} <- rules]
      subject = listToMaybe [ parseSubj langEnv person | Regulative{subj = person} <- rules]
      qaHT = textViaQaHorns langEnv l4i subject
      qaHornNames = foldMap fst qaHT
      d = 4
  allRQs <- ruleQuestionsNamed langEnv alias `traverse` expandRulesForNLG l4i rules

  measuredRQs <- for allRQs \(rn, asqn) -> do
    case compare (length asqn) 1 of
      GT -> xpReturn (rn, AA.All Nothing asqn)
      EQ -> xpReturn (rn, head asqn)
      _ -> xpError [[i|ruleQuestion not of interest: #{rn}|]]

  -- now we filter for only those bits of questStruct whose names match the names from qaHorns.
  wantedRQs <- for (rights measuredRQs) \case
    (rn@((`elem` qaHornNames) -> True), asqn) -> xpReturn (rn, asqn)
    (rn, _) -> xpError [[i| #{rn} not named in qaHorns"|]]

  let rqMap = Map.fromList $ rights wantedRQs

  let qaHornsWithQuestions = catMaybes do
        ruleNames <- fst <$> qaHT
        ruleName <- ruleNames
        let rq :: Maybe (AA.BoolStruct (Maybe (AA.Label T.Text)) T.Text) =
              rqMap Map.!? ruleName
        pure $ (ruleNames,) <$> rq

  let qaHTBit = qaHornsWithQuestions
                  |$> bimap slashNames alwaysLabeled
                  |$> toTuple

  xpReturn qaHTBit

interviewRulesRHS2topBit :: TL.Text -> String
interviewRulesRHS2topBit interviewRulesRHS = ""