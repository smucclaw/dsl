module AnyAll.Relevance where

import AnyAll.Types
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))
import Data.List (any, all)
import Debug.Trace
import Control.Monad (when, guard)
import Data.Maybe (isJust)

-- given
-- - a marking indicating which values are known;
-- - an and/or tree,
-- return a marking indicating which nodes in the tree are still of interest.
relevant :: (Ord a, Show a) => Marking a -> ShouldView -> Item a -> ShouldAsk a
relevant marking optimism (Leaf x) = case marking ! x of
                                       Right _ -> Map.singleton x View
                                       Left  _ -> Map.singleton x optimism
relevant marking optimism (All label items) = let dis = dispositive marking (All label items)
                                              in if -- trace ("is All " ++ show label ++ " " ++ show items ++ " dispositive? " ++ show dis)
                                                    dis
                                                 then Map.unions (relevant marking Hide <$> items)
                                                 else Map.unions (relevant marking optimism <$> items)
relevant marking optimism (Any label items) = if dispositive marking (Any label items)
                                              then Map.unions (relevant marking Hide <$> items)
                                              else Map.unions (relevant marking optimism <$> items)

-- basically logical shortcut: given a marking, is this item dispositive?
dispositive :: (Ord a, Show a) => Marking a -> Item a -> Bool
dispositive marking x = isJust $ childrenValue marking x

-- well, it depends on what values the children have.
childrenValue :: (Ord a, Show a) => Marking a -> Item a -> Maybe Bool
childrenValue marking (Leaf x) = case marking ! x of
                                   Right (Just x) -> Just x
                                   _              -> Nothing
childrenValue marking (All label items)
  | all (== Just True) (childrenValue marking <$> items) = Just True
  | Just False `elem`  (childrenValue marking <$> items) = Just False
  | otherwise = Nothing
childrenValue marking (Any label items)
  | all (== Just False) (childrenValue marking <$> items) = Just False
  | Just True `elem`    (childrenValue marking <$> items) = Just True
  | otherwise = Nothing


