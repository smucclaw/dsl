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
import qualified Data.Text.Lazy.Encoding as TLE
import Prettyprinter
import Text.Pretty.Simple (pShowNoColor)
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
import Data.Maybe (listToMaybe)
import Data.List.Split (chunk)
import qualified Data.Char as Char
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), object, toJSON, Value)

import Data.List (foldl')

import qualified Text.RawString.QQ as QQ


rlToBST :: NLGEnv -> [Rule] -> [([RuleName], [BoolStructT])]
rlToBST env rl = [(name, unsafePerformIO q) | q <- quest]
  where
    name = map ruleLabelName rl
    alias = listToMaybe [(you,org) | DefNameAlias you org _ _ <- rl]
    quest = map (ruleQuestions env alias) rl

bsToJSON :: AA.OptionallyLabeledBoolStruct T.Text -> AA.BoolStruct (AA.Label T.Text) T.Text
bsToJSON (AA.Leaf b) = AA.mkLeaf b
bsToJSON (AA.All Nothing items) = AA.mkAll (AA.Pre "all of the following") (map bsToJSON items)
bsToJSON (AA.All (Just pre@(AA.Pre _)) items) = AA.All pre (map bsToJSON items)
bsToJSON (AA.All (Just pp@(AA.PrePost _ _)) items) = AA.All pp (map bsToJSON items)
bsToJSON (AA.Any Nothing items) = AA.mkAny (AA.Pre "any of the following") (map bsToJSON items)
bsToJSON (AA.Any (Just pre@(AA.Pre _)) items) = AA.mkAny pre (map bsToJSON items)
bsToJSON (AA.Any (Just pp@(AA.PrePost _ _)) items) = AA.mkAny pp (map bsToJSON items)
bsToJSON (AA.Not item) = AA.mkNot (bsToJSON item)

ruleToRuleJSON :: NLGEnv -> [Rule] -> [(String, AA.BoolStruct (AA.Label T.Text) T.Text)]
ruleToRuleJSON env rl = [(T.unpack $ mt2text rn, bsToJSON bst) | ([rn], [bst]) <- rlToBST env rl]

data Tree = Leaf T.Text | Any T.Text [Tree] | All T.Text [Tree]
  deriving Show
newtype TreeList = TreeList [Tree]

instance Aeson.ToJSON Tree where
  toJSON (Leaf leaf) = object ["Leaf" .= leaf]
  toJSON (Any label trees) = object ["Any" .= toJSON trees]
  toJSON (All label trees) = object ["All" .= toJSON trees]

instance Read Tree where
  readsPrec _ input = [(fromString input, "")]


fromString :: String -> Tree
fromString input =
  case reads input of
    [(tree, "")] -> tree
    _ -> error "Invalid input"

toNestedMap :: [(String, AA.BoolStruct (AA.Label T.Text) T.Text)] -> Map.Map T.Text (Map.Map T.Text Value)
toNestedMap pairs = Map.singleton "root" $ Map.fromList $ map toMap pairs
  where
    toMap :: (String, AA.BoolStruct (AA.Label T.Text) T.Text) -> (T.Text, Value)
    toMap (key, value) = (T.pack key, toJSON value)

mapToString :: Map.Map T.Text (Map.Map T.Text Value) -> String
mapToString = TL.unpack . TL.replace "\"" "'" . pShowNoColor

rlsToJSON :: NLGEnv -> [Rule] -> String
rlsToJSON env rs = mapToString $ toNestedMap $ ruleToRuleJSON env rs


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



-- rlToBST :: NLGEnv -> [Rule] ->  [([RuleName], [BoolStructT])]
-- rlToBST env rl = [(name, unsafePerformIO q) | q <- quest]
--   where
--     name = map ruleLabelName rl
--     alias = listToMaybe [(you,org) | DefNameAlias you org _ _ <- rl]
--     quest = map (ruleQuestions env alias) rl

-- bsToJSON :: AA.OptionallyLabeledBoolStruct T.Text -> AA.BoolStruct (AA.Label T.Text) T.Text
-- bsToJSON (AA.Leaf b) = AA.mkLeaf (b)
-- bsToJSON (AA.All Nothing items) = AA.mkAll (AA.Pre "all of the following") (map bsToJSON items)
-- bsToJSON (AA.All (Just pre@(AA.Pre _)) items) = AA.All pre (map bsToJSON items)
-- bsToJSON (AA.All (Just pp@(AA.PrePost _ _)) items) = AA.All pp (map bsToJSON items)
-- bsToJSON (AA.Any Nothing items) = AA.mkAny (AA.Pre "any of the following") (map bsToJSON items)
-- bsToJSON (AA.Any (Just pre@(AA.Pre _)) items) = AA.mkAny pre (map bsToJSON items)
-- bsToJSON (AA.Any (Just pp@(AA.PrePost _ _)) items) = AA.mkAny pp (map bsToJSON items)
-- bsToJSON (AA.Not item) = AA.mkNot (bsToJSON item)

-- ruleToRuleJSON :: NLGEnv -> [Rule] -> [(String, AA.BoolStruct (AA.Label T.Text) T.Text)]
-- ruleToRuleJSON env rl = [(T.unpack $ mt2text rn, bsToJSON bst) | ([rn], [bst]) <- rlToBST env rl]

-- data Tree = Leaf T.Text | Any T.Text [Tree] | All T.Text [Tree]
--   deriving Show

-- instance Read Tree where
--   readsPrec _ input = [(fromString input, "")]

-- fromString :: String -> Tree
-- fromString input =
--   case reads input of
--     [(tree, "")] -> tree
--     _ -> error "Invalid input"

-- newtype TreeList = TreeList [Tree]

-- instance ToJSON Tree where
--   toJSON (Leaf leaf) = object ["Leaf" .= leaf]
--   toJSON (Any label trees) = object ["Any" .= toJSON trees]
--   toJSON (All label trees) = object ["All" .= toJSON trees]

-- instance ToJSON TreeList where
--   toJSON (TreeList trees) = toJSON trees

-- convertToJSON :: String -> Value
-- convertToJSON tree = toJSON $ fromString tree

-- toNestedMap :: [(String, AA.BoolStruct (AA.Label T.Text) T.Text)] -> Map.Map T.Text (Map.Map T.Text Value)
-- toNestedMap pairs = Map.singleton "root" $ Map.fromList $ map toMap pairs
--   where
--     toMap (key, value) = (T.pack key, toJSON value)

-- mapToString :: Map.Map T.Text (Map.Map T.Text Value) -> String
-- mapToString = TL.unpack . TL.replace "\"" "'" . pShowNoColor

-- rlsToJSON :: NLGEnv -> [Rule] -> String
-- rlsToJSON env rs = mapToString $ toNestedMap $ ruleToRuleJSON env rs