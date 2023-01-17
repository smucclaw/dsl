{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import LS.Interpreter
import Control.Monad (guard)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.List as DL
import Data.Map ((!))
import Data.Bifunctor (second)
import Data.Maybe (listToMaybe)
import Data.List.Split (chunk)


-- | extract the tree-structured rules from Interpreter
-- currently: construct a Data.Map of rulenames to exposed decision root expanded BSR
-- in future: also ship out a Marking which represents the TYPICALLY values
-- far future: construct a JSON with everything in it, and get the Purescript to read the JSON, so we are more interoperable with non-FP languages

data Tuple a b = Tuple a b
  deriving (Show, Eq, Ord)

toTuple :: (a,b) -> Tuple a b
toTuple (x,y) = Tuple x y

-- startWithRule :: InterpreterOptions -> NLGEnv -> RunConfig -> [Rule] ->

-- two boolstructT: one question and one phrase
namesAndStruct :: NLGEnv -> [Rule] -> [([RuleName], [BoolStructT])]
namesAndStruct env rl =
  [ (names, [bs]) | (names, bs) <- qaHornsT interp]
  where
    interp = l4interpret defaultInterpreterOptions rl

namesAndQ :: NLGEnv -> [Rule] -> [([RuleName], [BoolStructT])]
namesAndQ env rl =
  [ (name, unsafePerformIO q) | q <- questStruct]
  where
    name = map ruleLabelName rl
    alias = listToMaybe [(you,org) | DefNameAlias you org _ _ <- rl]
    questStruct = map (ruleQuestions env alias) rl -- [AA.OptionallyLabeledBoolStruct Text.Text]

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
  let q = combine (namesAndStruct env rl) (namesAndQ env rl)
      flattened = (\(x,ys) ->
        (x, [AA.extractLeaves y | y <- ys])) <$> q
      onlyqs = (\(x, y) -> (x, (justQuestions (head y) (map fixNot $ tail y)))) <$> q
      sorted = DL.reverse $ DL.sortOn (DL.length) (flattened)
  guard (not $ null sorted)
  return ((Map.fromList (onlyqs)) ! (fst $ DL.head sorted))

biggestS :: NLGEnv -> [Rule] -> [BoolStructT]
biggestS env rl = do
  let q = combine (namesAndStruct env rl) (namesAndQ env rl)
      flattened = (\(x,ys) ->
        (x, [AA.extractLeaves y | y <- ys])) <$> q
      onlys = (\(x, y) -> (x, (justStatements (head y) (map fixNot $ tail y)))) <$> q
      sorted = DL.reverse $ DL.sortOn (DL.length) (flattened)
  guard (not $ null sorted)
  return ((Map.fromList (onlys)) ! (fst $ DL.head sorted))

-- (namesAndStruct env rl)

asPurescript :: NLGEnv -> [Rule] -> String
asPurescript env rl =
     show (vsep
           [ "toplevelDecisions :: Map.Map (String) (Item String)"
           , "toplevelDecisions = Map.fromFoldable " <>
             (pretty $ TL.unpack (
                 pShowNoColor
                   [ toTuple ( T.intercalate " / " (T.unwords <$> names)
                            , alwaysLabeled (justStatements (head bs) (map fixNot (tail bs))))
                   | (names,bs) <- (combine (namesAndStruct env rl) (namesAndQ env rl))
                   ]
                 )
             )
           , "toplevelDefaultMarking :: Marking"
           , "toplevelDefaultMarking = Marking $ Map.fromFoldable " <>
             (pretty . TL.unpack
              . TL.replace "False" "false"
              . TL.replace "True" "true"
              . pShowNoColor $
              fmap toTuple . Map.toList . AA.getMarking $
              getMarkings (l4interpret defaultInterpreterOptions rl)
             )
           ]
          )
