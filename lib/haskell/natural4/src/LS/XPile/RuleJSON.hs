{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

{-| transpiler to SVG visualization of the AnyAll and/or trees.

Largely a wrapper. Most of the functionality is in the anyall lib.

-}

module LS.XPile.RuleJSON where

import LS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Prettyprinter hiding (space)
import Text.Pretty.Simple (pShowNoColor, pString)
import qualified AnyAll.BoolStruct as AA
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
import Data.Maybe (listToMaybe, fromMaybe)
import Data.List.Split (chunk)
import qualified Data.Char as Char
import Data.List (intercalate, stripPrefix)
import Control.Applicative ((<|>))
import Data.Map.Strict (Map, fromList)
import Text.Read (readMaybe)
import Data.List.Split (splitOn)
import Text.ParserCombinators.ReadP
import Data.Functor.Identity (runIdentity)
import qualified Text.RawString.QQ as QQ

-- | extract the tree-structured rules from Interpreter
-- currently: construct a Data.Map of rulenames to exposed decision root expanded BSR
-- in future: also ship out a Marking which represents the TYPICALLY values
-- -- far future: construct a JSON with everything in it, and get the Purescript to read the JSON, so we are more interoperable with non-FP languages

-- ruleQuestions :: NLGEnv -> Maybe (MultiTerm,MultiTerm) -> Rule -> IO [AA.OptionallyLabeledBoolStruct Text.Text]
-- ruleQuestions env alias rule

-- namesAndQ :: NLGEnv -> [Rule] -> [[BoolStructT]]
-- namesAndQ env rl =
--   [ (name, unsafePerformIO q) | q <- questStruct]
--   where
--     name = map ruleLabelName rl
--     alias = listToMaybe [(you,org) | DefNameAlias you org _ _ <- rl]
--     que



rlToBST :: NLGEnv -> [Rule] ->  [([RuleName], [BoolStructT])]
rlToBST env rl = [(name, unsafePerformIO q) | q <- quest]
  where
    name = map ruleLabelName rl
    alias = listToMaybe [(you,org) | DefNameAlias you org _ _ <- rl]
    quest = map (ruleQuestions env alias) rl

bsToJSON :: AA.OptionallyLabeledBoolStruct T.Text -> AA.BoolStruct (AA.Label T.Text) T.Text
bsToJSON (AA.Leaf b) = AA.Leaf (b)
bsToJSON (AA.All Nothing items) = AA.All (AA.Pre "all") (map bsToJSON items)
bsToJSON (AA.All (Just pre@(AA.Pre _)) items) = AA.All pre (map bsToJSON items)
bsToJSON (AA.All (Just pp@(AA.PrePost _ _)) items) = AA.All pp (map bsToJSON items)
bsToJSON (AA.Any Nothing items) = AA.Any (AA.Pre "any") (map bsToJSON items)
bsToJSON (AA.Any (Just pre@(AA.Pre _)) items) = AA.Any pre (map bsToJSON items)
bsToJSON (AA.Any (Just pp@(AA.PrePost _ _)) items) = AA.Any pp (map bsToJSON items)
bsToJSON (AA.Not item) = AA.Not (bsToJSON item)

ruleToRuleJSON :: NLGEnv -> [Rule] -> [Map.Map String (AA.BoolStruct (AA.Label T.Text) T.Text)]
ruleToRuleJSON env rl = [Map.fromList [(T.unpack $ mt2text rn, bsToJSON bst)] | ([rn],[bst]) <- rlToBST env rl]

-- my shitty parser
convertString str = convert (words str)
  where
    convert :: [String] -> String
    convert ("{\"Person\",All":rest) = "{\"Person\":{\"All\":[" ++ (convertAll rest) ++ "]}}"
    convert ("{\"Person\",Any":rest) = "{\"Person\":{\"Any\":[" ++ convertAll rest
    convert (word:rest) = word ++ convert rest
    convert [] = ""

    convertAll :: [String] -> String
    convertAll ("{Pre":"\"all\"":all:rest) = "{\"All\":" ++ show all ++ "}" ++ convertAll rest
    convertAll ("[Leaf":rest) = "{\"Leaf\":" ++ recurseUntilComma rest
    convertAll ("Any":rest) = ",{\"Any\":[" ++ convertAll rest ++ "]}" ++ convertAll (drop 1 $ dropWhile (/= "]") rest)
    convertAll ("All":rest) = ",{\"All\":[" ++ convertAll rest ++ "]}" ++ convertAll (drop 1 $ dropWhile (/= "]") rest)
    convertAll ("]":_) = "]"
    convertAll (_:rest) = convertAll rest
    convertAll [] = ""

    recurseUntilComma :: [String] -> String
    recurseUntilComma (x:xs)
      | elem ',' x = (head (splitOn "," x) ++ "}") ++ convertAll ((unwords $ tail (splitOn "," x)) : xs)
      | otherwise = (x ++ " ") ++ recurseUntilComma xs
    recurseUntilComma [] = []


convertChar :: Char -> String
convertChar '(' = "{"
convertChar ')' = "}"
convertChar '[' = "["
convertChar ']' = "]"
convertChar '"' = "\""
convertChar ',' = ","
convertChar c   = [c]

convertList :: [String] -> String
convertList = intercalate ","

--

rlsToJSON :: NLGEnv -> [Rule] -> String
rlsToJSON env rs = convertString $ tail $ init $ concatMap convertChar $ unwords $ tail $ words $ show $ mconcat $ ruleToRuleJSON env rs
    -- "([[MTT \"Person\"]],[All Nothing [Leaf \"does the person walk?\",Any Nothing [Leaf \"does the person eat?\",Leaf \"does the person drink?\"]]])"


    -- <> "\n\n"
    -- <> psSuffix
    -- <> "\n\n"
    -- <> ([asPurescript l rules | l <- nlgEnv])

-- psPrefix :: String -- the stuff at the top of the purescript output
-- psPrefix = [QQ.r|

-- -- This file was automatically generated by natural4.
-- -- Do not edit by hand.
-- -- Instead, revise the toolchain starting at smucclaw/dsl/lib/haskell/natural4/app/Main.hs

-- module RuleLib.PDPADBNO where

-- import Prelude
-- import Data.Either
-- import Data.Maybe
-- import Data.Tuple
-- import Data.Map as Map
-- import Foreign.Object as Object

-- import AnyAll.Types

-- schedule1_part1 :: Item String
-- schedule1_part1 =

--   |]


-- psSuffix :: String -- at the bottom of the purescript output
-- psSuffix = [QQ.r|
-- schedule1_part1_nl :: NLDict
-- schedule1_part1_nl =
--   Map.fromFoldable
--     [ ]
--     |]

-- type RuleJSON = Map.Map String (AA.BoolStruct (AA.Label T.Text) T.Text)

