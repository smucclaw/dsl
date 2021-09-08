module AnyAll.Relevance where

import AnyAll.Types
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))
import Data.List (any, all)

-- given
-- - a marking indicating which values are known;
-- - an and/or tree,
-- return a marking indicating which nodes in the tree are still of interest.
relevant :: Ord a => Marking a -> ShouldView -> Item a -> ShouldAsk a
relevant marking optimism (Leaf x) = case marking ! x of
                                       Right _ -> Map.singleton x View
                                       Left  _ -> Map.singleton x optimism
relevant marking optimism (All label items) = if dispositive marking (All label items) then Map.unions (relevant marking Hide <$> items) else Map.unions (relevant marking optimism <$> items)
relevant marking optimism (Any label items) = if dispositive marking (Any label items) then Map.unions (relevant marking Hide <$> items) else Map.unions (relevant marking optimism <$> items)

-- basically logical shortcut: given a marking, is this item dispositive?
dispositive :: Ord a => Marking a -> Item a -> Bool
dispositive marking (Leaf x) = case marking ! x of
                                 Right _ -> True
                                 _       -> False
dispositive marking (All label items) = any (confirmed marking False) items || all (confirmed marking True) items
dispositive marking (Any label items) = any (confirmed marking True ) items || all (confirmed marking False) items

-- given a marking and a desired value, does this item evaluate to the desired value?
confirmed :: Ord a => Marking a -> Bool -> Item a -> Bool
confirmed marking tf (Leaf x) = marking ! x == Right (Just tf)
confirmed marking tf (All label items) = all (confirmed marking tf) items
confirmed marking tf (Any label items) = any (confirmed marking tf) items
