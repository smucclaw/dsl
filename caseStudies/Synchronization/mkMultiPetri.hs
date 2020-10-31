#!/usr/bin/env stack
-- stack --resolver lts-16.20 script --package split 
{-# LANGUAGE OverloadedStrings #-}
-- convert a single untimed-petri dot file to success frames of an animation
-- mkPetriAnim < untimed-petri.dot
-- "you can write FORTRAN^H^H^H^H^H^H^HPerl in any language"

import Control.Monad (forM_)
import Data.List.Split
import System.Environment

main = do
  inlines <- lines <$> getContents
  forM_ [ ("anim1", "a_form a_fee > b_review > c_in pa > c_out1")
        , ("anim2", "a_form a_fee > b_review > c_in pa > c_out2")
        , ("anim3", "a_form a_fee > b_review > c_in pa > c_out3 > a_form") ] (
    \(outfile, tokenSpecs) -> do
      forM_ (zip [1..] (splitOn " > " tokenSpecs)) (
        \(n,actives) ->
          writeFile (outfile++"-"++(show n)++".dot") $ unlines $
          (flip map) inlines (
          \inline -> let ws  = words  inline
                         len = length inline
                     in if   length ws > 0 && head ws `elem` (words actives) && "\\n\\n\"];" == (drop (len - 7) inline)
                        then take (len - 3) inline ++ "&bull;" ++ drop (len -3) inline
                        else inline
          )
        )
    )
