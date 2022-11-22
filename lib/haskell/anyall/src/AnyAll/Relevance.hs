{-# LANGUAGE MultiWayIf #-}

module AnyAll.Relevance where

import AnyAll.Types
import AnyAll.BoolStruct
import qualified Data.Map.Strict as Map
import Data.Map.Strict (lookup)
import Data.List (any, all)
import Debug.Trace
import Control.Monad (when, guard)
import Data.Maybe (isJust)
import Data.Either
import Data.Tree
import qualified Data.Text       as T

-- paint a tree as View, Hide, or Ask, depending on the dispositivity of the current node and its children.
relevant :: Hardness -> Marking T.Text -> Maybe Bool -> OptionallyLabeledBoolStruct T.Text-> Tree (Q T.Text)
relevant sh  marking parentValue self =
  let
    repaintedChildren = if initVis == Hide
                          then (ask2hide <$>) <$> paintedChildren
                          else paintedChildren
  in -- convert to a QTree for output
  case self of
             Leaf x -> case Map.lookup x (getMarking marking) of
                         Just (Default (Right b)) -> Node (Q View                                     (Simply x) Nothing (Default $ Right b)) []
                         Just (Default (Left  b)) -> Node (Q (if initVis /= Hide then Ask else Hide)  (Simply x) Nothing (Default $ Left  b)) []
                         Nothing          -> Node (Q (if initVis /= Hide then Ask else Hide)  (Simply x) Nothing (Default $ Left Nothing)) []
             Any label items -> Node (ask2view (Q initVis  Or label   (Default $ Left selfValue))) repaintedChildren
             All label items -> Node (ask2view (Q initVis And label   (Default $ Left selfValue))) repaintedChildren
             Not       item  -> Node (ask2view (Q initVis Neg Nothing (Default $ Left selfValue))) repaintedChildren
  where
    selfValue = evaluate sh marking self
    selfValueHard = evaluate Hard marking self
    initVis = initVisFn parentValue selfValue selfValueHard
    paintedChildren = relevant sh marking selfValue <$> boolStructChildren self

initVisFn :: Maybe Bool -> Maybe Bool -> Maybe Bool -> ShouldView
initVisFn parentValue selfValue selfValueHard
  | isJust parentValue = if parentValue == selfValue then View else Hide
  | isJust selfValueHard = View
  | otherwise = Ask

-- which of my descendants are dispositive? i.e. contribute to the final result.
-- TODO: this probably needs to be pruned some

dispositive :: (Ord a, Show a) => Hardness -> Marking a -> BoolStruct l a -> [BoolStruct l a]
dispositive sh marking self =
  let selfValue  = evaluate sh marking self
      recurse cs = concatMap (dispositive sh marking) (filter ((selfValue ==) . evaluate sh marking) cs)
  in case self of
       Leaf x          -> if isJust selfValue then return self else mempty
       Any label items -> recurse items
       All label items -> recurse items
       Not       item  -> recurse [item]

-- well, it depends on what values the children have. and that depends on whether we're assessing them in soft or hard mode.
evaluate :: (Ord a, Show a) => Hardness -> Marking a -> BoolStruct l a -> Maybe Bool
evaluate Soft (Marking marking) (Leaf x) = case Map.lookup x marking of
                                             Just (Default (Right (Just x))) -> Just x
                                             Just (Default (Left  (Just x))) -> Just x
                                             _                               -> Nothing
evaluate Hard (Marking marking) (Leaf x) = case Map.lookup x marking of
                                             Just (Default (Right (Just x))) -> Just x
                                             _                               -> Nothing
evaluate sh   marking           (Not x)  = not <$> evaluate sh marking x
evaluate sh marking (Any label items)
  | Just True `elem`    (evaluate sh marking <$> items) = Just True
  | all (== Just False) (evaluate sh marking <$> items) = Just False
  | otherwise = Nothing
evaluate sh marking (All label items)
  | all (== Just True) (evaluate sh marking <$> items) = Just True
  | Just False `elem`  (evaluate sh marking <$> items) = Just False
  | otherwise = Nothing


