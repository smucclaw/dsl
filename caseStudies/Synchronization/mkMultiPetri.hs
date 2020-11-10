#!/usr/bin/env stack
-- stack --resolver lts-16.20 script --package split 
{-# LANGUAGE Haskell2010 #-}
-- stack ./mkMultiPetri.hs < untimed-petri.dot
-- convert a single untimed-petri dot file to frames of an animation
-- "you can write FORTRAN^H^H^H^H^H^H^HPerl in any language"
-- the control block is as seen in untimed-petri.dot, commented out at the bottom

import Text.Printf (printf)
import Control.Monad (forM_)
import Data.Functor ((<&>))
import Data.List.Split
import Data.List
import System.Environment

main = do
  inlines <- lines <$> getContents
  let controlBlock = [ (label, specs)
                     | eachline <- inlines
                     , "//" `isPrefixOf` eachline
                     , let commented = words $ foldl (\str ch -> dropWhile (==ch) str) eachline " / "
                     , ":" `elem` commented
                     , let label = head   $ commented
                           specs = drop 2 $ commented
                     ]
  forM_ controlBlock (
    \(outfile, tokenSpecs) -> do
      forM_ (zip [1..] (splitOn [">"] tokenSpecs)) (
        \(frameNum,actives) ->
          writeFile (printf "%s-%02d.dot" outfile (frameNum :: Int)) $ unlines $
          inlines <&> (
          \inline -> let ws  = words  inline
                         len = length inline
                         insertl = insert 3 -- before the "];
                         insertr = insert 2 -- before the  ];
                         insert n str = take (len - n) inline ++ str ++ drop (len - n) inline
                     in if "label=" `isInfixOf` inline
                        then if not ("\\n\\n\"];" `isSuffixOf` inline)
                             then if head ws `elem` actives -- transition
                                  then insertr ",fontcolor=black,fillcolor=yellow" -- highlight
                                  else id inline                                   -- noop
                             else if head ws `elem` actives -- place
                                  then insertl "&bull;"     ---- active gets a bullet to mark token
                                  else insertl "\\n"        ---- inactive gets a newline for formatting
                        else id inline
          )
        )
    )
