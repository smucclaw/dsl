{-# LANGUAGE MultiWayIf #-}

module AnyAll.Relevance where

import AnyAll.Types
import qualified Data.Map.Strict as Map
import Data.Map.Strict (lookup)
import Data.List (any, all)
import Debug.Trace
import Control.Monad (when, guard)
import Data.Maybe (isJust)
import Data.Either
import Data.Tree

-- paint a tree as View, Hide, or Ask, depending on the dispositivity of the current node and its children.
relevant :: (Ord a, Show a) => Hardness -> DisplayPref -> Marking a -> Maybe Bool -> Item a -> Tree (Q a)
relevant sh dp marking parentValue self =
  let selfValue = evaluate sh marking self
      initVis   = if | isJust parentValue -> if | parentValue == selfValue              -> View
                                                | otherwise                             -> Hide
                     | otherwise          -> if | isJust (evaluate Hard marking self)   -> View
                                                | otherwise                             -> Ask
      -- we are able to compute the initial visibility of the subtree; TODO we can modify it according to our display preference
      paintedChildren = relevant sh dp marking selfValue <$> getChildren self
      -- if i am myself hidden, then convert all my descendants' Ask to Hide
      repaintedChildren = if initVis /= Hide then paintedChildren
                          else fmap ask2hide <$> paintedChildren
  in -- convert to a QTree for output
  case self of
             Leaf x -> case Map.lookup x marking of
                         Just (Right b) -> Node (Q View                                     (Simply x) Nothing (Right b)) []
                         Just (Left  b) -> Node (Q (if initVis /= Hide then Ask else Hide)  (Simply x) Nothing (Left  b)) []
                         Nothing          -> Node (Q (if initVis /= Hide then Ask else Hide)  (Simply x) Nothing (Left Nothing)) []
             Any label items -> Node (Q initVis  Or (Just label) (Left selfValue)) repaintedChildren
             All label items -> Node (Q initVis And (Just label) (Left selfValue)) repaintedChildren
  where
    getChildren (Leaf _) = []
    getChildren (Any _ c) = c
    getChildren (All _ c) = c

    ask2hide :: Q a -> Q a
    ask2hide (Q Ask x y z) = Q Hide x y z
    ask2hide x = x
    
-- which of my descendants are dispositive? i.e. contribute to the final result.
-- TODO: this probably needs to be pruned some

dispositive :: (Ord a, Show a) => Hardness -> Marking a -> Item a -> [Item a]
dispositive sh marking self =
  let selfValue  = evaluate sh marking self
      recurse cs = concatMap (dispositive sh marking) (filter ((selfValue ==) . evaluate sh marking) cs)
  in case self of
       Leaf x          -> if isJust selfValue then return self else mempty
       Any label items -> recurse items
       All label items -> recurse items

-- well, it depends on what values the children have. and that depends on whether we're assessing them in soft or hard mode.
evaluate :: (Ord a, Show a) => Hardness -> Marking a -> Item a -> Maybe Bool
evaluate Soft marking (Leaf x) = case Map.lookup x marking of
                                   Just (Right (Just x)) -> Just x
                                   Just (Left  (Just x)) -> Just x
                                   _              -> Nothing
evaluate Hard marking (Leaf x) = case Map.lookup x marking of
                                   Just (Right (Just x)) -> Just x
                                   _              -> Nothing
evaluate sh marking (Any label items)
  | Just True `elem`    (evaluate sh marking <$> items) = Just True
  | all (== Just False) (evaluate sh marking <$> items) = Just False
  | otherwise = Nothing
evaluate sh marking (All label items)
  | all (== Just True) (evaluate sh marking <$> items) = Just True
  | Just False `elem`  (evaluate sh marking <$> items) = Just False
  | otherwise = Nothing


