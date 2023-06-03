{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

{-| transpiler to SVG visualization of the AnyAll and/or trees.

Largely a wrapper. Most of the functionality is in the anyall lib.

-}

module LS.XPile.Purescript where

import LS
import AnyAll.BoolStruct (alwaysLabeled)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Prettyprinter
import Text.Pretty.Simple (pShowNoColor)
import qualified AnyAll as AA
import qualified Data.Map as Map
import LS.NLP.NLG
import LS.NLP.NL4Transformations
import LS.Interpreter
import Control.Monad (guard)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.List as DL
import Data.Map ((!))
import Data.Bifunctor (second)
import Data.Maybe (listToMaybe)
import Data.List.Split (chunk)
import PGF
import qualified Data.Char as Char

import qualified Text.RawString.QQ as QQ

-- | extract the tree-structured rules from Interpreter
-- currently: construct a Data.Map of rulenames to exposed decision root expanded BSR
-- in future: also ship out a Marking which represents the TYPICALLY values
-- far future: construct a JSON with everything in it, and get the Purescript to read the JSON, so we are more interoperable with non-FP languages

data Tuple a b = Tuple a b
  deriving (Show, Eq, Ord)

toTuple :: (a,b) -> Tuple a b
toTuple (x,y) = Tuple x y

textMT :: [RuleName] -> [T.Text]
textMT rl = map mt2text rl

-- two boolstructT: one question and one phrase
namesAndStruct :: [Rule] -> [([RuleName], [BoolStructT])]
namesAndStruct rl =
  [ (names, [bs]) | (names, bs) <- qaHornsT interp]
  where
    interp = l4interpret defaultInterpreterOptions rl

namesAndQ :: NLGEnv -> [Rule] -> [([RuleName], [BoolStructT])]
namesAndQ env rl =
  [ (name, unsafePerformIO q) | q <- questStruct]
  where
    name = map ruleLabelName rl
    alias = listToMaybe [(you,org) | DefNameAlias you org _ _ <- rl]
    questStruct = map (ruleQuestions env alias) (expandRulesForNLG env rl) -- [AA.OptionallyLabeledBoolStruct Text.Text]

combine :: [([RuleName], [BoolStructT])] -> [([RuleName], [BoolStructT])] -> [([RuleName], [BoolStructT])]
combine [] [] = []
combine (b:bs) [] = []
combine [] (q:qs) = []
combine (b:bs) (q:qs) =
  ((fst b), (snd b) ++ (snd q)) : combine bs qs


fixNot :: BoolStructT -> BoolStructT
fixNot (AA.Leaf x) = AA.Leaf x
fixNot (AA.Not (AA.Leaf x)) = AA.Leaf x
fixNot y = y

justQuestions :: BoolStructT -> [BoolStructT] -> BoolStructT
justQuestions (AA.All Nothing a) q = (AA.All Nothing (q))
justQuestions (AA.Any Nothing a) q = (AA.Any Nothing (q))
justQuestions xs y = xs

justStatements :: BoolStructT -> [BoolStructT] -> BoolStructT
justStatements (AA.All Nothing a) q = (AA.All Nothing (a))
justStatements (AA.Any Nothing a) q = (AA.Any Nothing (a))
justStatements xs y = xs

labelQs :: [AA.OptionallyLabeledBoolStruct T.Text] -> [AA.BoolStruct (AA.Label T.Text) T.Text]
labelQs x = map alwaysLabeled x

biggestQ :: NLGEnv -> [Rule] -> [BoolStructT]
biggestQ env rl = do
  let q = combine (namesAndStruct rl) (namesAndQ env rl)
      flattened = (\(x,ys) ->
        (x, [AA.extractLeaves y | y <- ys])) <$> q
      onlyqs = (\(x, y) -> (x, (justQuestions (head y) (map fixNot $ tail y)))) <$> q
      sorted = DL.reverse $ DL.sortOn (DL.length) (flattened)
  guard (not $ null sorted)
  return ((Map.fromList (onlyqs)) ! (fst $ DL.head sorted))

biggestS :: NLGEnv -> [Rule] -> [BoolStructT]
biggestS env rl = do
  let q = combine (namesAndStruct rl) (namesAndQ env rl)
      flattened = (\(x,ys) ->
        (x, [AA.extractLeaves y | y <- ys])) <$> q
      onlys = (\(x, y) -> (x, (justStatements (head y) (map fixNot $ tail y)))) <$> q
      sorted = DL.reverse $ DL.sortOn (DL.length) (flattened)
  guard (not $ null sorted)
  return ((Map.fromList (onlys)) ! (fst $ DL.head sorted))

asPurescript :: NLGEnv -> [Rule] -> String
asPurescript env rl =
     show (vsep
           [ (pretty $ map Char.toLower $ showLanguage $ gfLang env) <> " :: " <> "Object.Object (Item String)"
           , (pretty $ map Char.toLower $ showLanguage $ gfLang env) <> " = " <> "Object.fromFoldable " <>
             (pretty $ TL.unpack (
                 pShowNoColor
                   [ toTuple ( T.intercalate " / " (mt2text <$> names)
                            , alwaysLabeled (justQuestions (head bs) (map fixNot (tail bs))))
                   | (names,bs) <- (combine (namesAndStruct rl) (namesAndQ env rl))
                   ]
                 )
             )
           , (pretty $ map Char.toLower $ showLanguage $ gfLang env) <> "Marking :: Marking"
           , (pretty $ map Char.toLower $ showLanguage $ gfLang env) <>  "Marking = Marking $ Map.fromFoldable " <>
             (pretty . TL.unpack
              . TL.replace "False" "false"
              . TL.replace "True" "true"
              . pShowNoColor $
              fmap toTuple . Map.toList . AA.getMarking $
              getMarkings (l4interpret defaultInterpreterOptions rl)
             )
          -- , (pretty $ showLanguage $ gfLang env) <> "Statements :: Object.Object (Item String)"
          -- , (pretty $ showLanguage $ gfLang env) <> "Statements = Object.fromFoldable " <>
          --   (pretty $ TL.unpack (
          --       pShowNoColor
          --         [ toTuple ( T.intercalate " / " (mt2text <$> names)
          --                 , alwaysLabeled (justStatements (head bs) (map fixNot (tail bs))))
          --         | (names,bs) <- (combine (namesAndStruct env rl) (namesAndQ env rl))
          --         ]
          --       )
          --   )
           ]
          )

translate2PS :: [NLGEnv] -> NLGEnv -> [Rule] -> String
translate2PS nlgEnv eng rules =
  psPrefix
    <> ((tail . init) (TL.unpack ((pShowNoColor . map alwaysLabeled) (biggestQ eng rules))))
    <> "\n\n"
    <> psSuffix
    <> "\n\n"
    <> (DL.intercalate "\n\n" $ [asPurescript l rules | l <- nlgEnv])

psPrefix :: String -- the stuff at the top of the purescript output
psPrefix = [QQ.r|

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
interviewRules =

  |]


psSuffix :: String -- at the bottom of the purescript output
psSuffix = [QQ.r|
interviewRules_nl :: NLDict
interviewRules_nl =
  Map.fromFoldable
    [ ]
    |]
