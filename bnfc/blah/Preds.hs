module Preds where

  allPreds :: Foldable t => t (a -> Bool) -> a -> Bool
  allPreds preds value = all (flip ($) value) preds

  anyPreds :: Foldable t => t (a -> Bool) -> a -> Bool
  anyPreds preds value = any (flip ($) value) preds

  numPreds ::                [ a -> Bool ] -> a -> Int
  numPreds preds value = length (filter (flip ($) value) preds)

  xorPreds ::                [ a -> Bool ] -> a -> Bool
  xorPreds preds value = 1 == numPreds preds value
