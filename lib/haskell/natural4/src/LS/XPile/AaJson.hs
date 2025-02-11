{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

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


labelToAaJson :: AA.Label T.Text -> Value
labelToAaJson (AA.Pre a) = object [ "Pre" .= a ]
labelToAaJson (AA.PrePost a b) = object [ "Pre" .= a, "Post" .= b ]
labelToAaJson (AA.Metadata a) = object [ "Metadata" .= a ]

bsToAaJson :: BoolStructLT -> Value
bsToAaJson (AA.All l bs) = object [ "All" .= object["label" .= labelToAaJson l, "children" .= [bsToAaJson c | c <- bs] ]]
bsToAaJson (AA.Any l bs) = object [ "Any" .= object["label" .= labelToAaJson l, "children" .=  [bsToAaJson c | c <- bs] ]]
bsToAaJson (AA.Leaf a) = object [ "Leaf" .= a ]
bsToAaJson (AA.Not bs) = object [ "Not" .= bsToAaJson bs]

toAaJson :: (String, BoolStructLT) -> Value
toAaJson (a, b) = object [ fromString a .= bsToAaJson b]

slashNames :: [RuleName] -> String
slashNames names = T.unpack (T.intercalate " / " (mt2text <$> names))


translate2AaJson :: [NLGEnv] -> Interpreted -> XPileLogE String
translate2AaJson nlgEnvs l4i = do
  let rules = origrules l4i

  qaHornsAllLangs :: [Either XPileLogW String] <-
    for nlgEnvs \nlgEnv@(NLGEnv {gfLang}) -> do
      let nlgEnvStrLower = gfLang |> showLanguage |$> Char.toLower

      hornByLang :: Either XPileLogW [(String, BoolStructLT)] <-
        qaHornsByLang rules nlgEnv l4i

      case hornByLang of
        Left err -> xpError err
        Right haveHorn -> xpReturn [__i|
           "#{nlgEnvStrLower}" : #{encodePretty $ toAaJson <$> (DL.nub haveHorn)}
        |]

  let qaHornsStrings = rights qaHornsAllLangs
  xpReturn [__i|
    {
      #{DL.intercalate ",\n" qaHornsStrings}
    }
  |]

qaHornsByLang :: [Rule] -> NLGEnv -> Interpreted -> XPileLogE [(String, BoolStructLT)]
qaHornsByLang rules langEnv l4i = do
  let alias = listToMaybe [ (you,org) | DefNameAlias{name = you, detail = org} <- rules]
      subject = listToMaybe [ parseSubj langEnv person | Regulative{subj = person} <- rules]
      qaHT = textViaQaHorns langEnv l4i subject
      qaHornNames = foldMap fst qaHT
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

  xpReturn qaHTBit
