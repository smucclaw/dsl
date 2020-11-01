#!/usr/bin/env stack
-- stack --resolver lts-16.20 script --package split 
{-# LANGUAGE Haskell2010 #-}
-- stack ./mkMultiPetri.hs < untimed-petri.dot
-- convert a single untimed-petri dot file to frames of an animation
-- "you can write FORTRAN^H^H^H^H^H^H^HPerl in any language"
-- the control block is as seen in untimed-petri.dot, commented out at the bottom

import Control.Monad (forM_)
import Data.Functor ((<&>))
import Data.List.Split
import Data.List
import System.Environment

main = do
  inlines <- lines <$> getContents
  let controlBlock = [ (label, specs) | eachline <- inlines
                                      , "//" `isPrefixOf` eachline
                                      , let commented = foldl (\str ch -> dropWhile (==ch) str) eachline " / "
                                      , " : " `isInfixOf` commented
                                      , let label = head $        splitOn " : " commented
                                      , let specs = head $ tail $ splitOn " : " commented ]
  forM_ controlBlock (
    \(outfile, tokenSpecs) -> do
      forM_ (zip [1..] (splitOn " > " tokenSpecs)) (
        \(frameNum,actives) ->
          writeFile (outfile++"-"++(show frameNum)++".dot") $ unlines $
          inlines <&> (
          \inline -> let ws  = words  inline
                         len = length inline
                         insert str = take (len - 3) inline ++ str ++ drop (len - 3) inline
                     in if        "\\n\\n\"];" == (drop (len - 7) inline)
                        then if   length ws > 0 && head ws `elem` (words actives)
                             then insert "&bull;"
                             else insert "\\n"
                        else id inline
          )
        )
    )
