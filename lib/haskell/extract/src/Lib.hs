{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( orgMain
    ) where

import Data.Maybe

import Data.OrgMode
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

import Data.List (intercalate)
import Data.List.Split

orgMain :: IO ()
orgMain = do
  input <- orgFile <$> getContents
  let processed = processNode <$> odNodes input
  mapM_ putStrLn $ showNode <$> processed

processNode :: Node -> Node
processNode n = n { nChildren = processNodeChild <$> nChildren n }
  where
    processNodeChild :: NodeChild -> NodeChild
    processNodeChild (ChildText tl@(TextLine tli tlt tll)) =
      let asl4 = example <$> tl2l4 tlt
      in case asl4 of
        Nothing -> ChildText tl
        Just ex -> 
          ChildNode (Node { nDepth = nDepth n + 1
                          , nPrefix = pure $ Prefix "TODO"
                          , nTags = []
                          , nChildren = [ChildText (TextLine tli ex tll)]
                          , nTopic = take 20 tlt
                          , nLine = tl
                          })

    processNodeChild (ChildNode cn) = ChildNode $ processNode cn
    processNodeChild x = x

    example x = unlines [ "#+BEGIN_SRC l4 :tangle out.l4", x, "#+END_SRC" ]

-- | TextLine to L4 text
tl2l4 :: String -> Maybe String
tl2l4 tlt
  | "means" `elem` words tlt = do
      (lhs, rhs) <- parseMaybe ((,)
                                <$> someTill anySingle (string " means ")
                                <*> (some anySingle :: Parsec Void String String)
                               ) tlt
      pure (intercalate "," [ ";;\n", "", trimStars lhs ++ "\n", "MEANS", rhs ])

  | length (sentences tlt) > 1
    && catMaybes (tl2l4 <$> sentences tlt) /= [] =
      pure $ unlines (catMaybes (tl2l4 <$> sentences tlt))

  | otherwise = Nothing

  where
    trimStars x = reverse (dropWhile (== '*') (reverse (dropWhile (== '*') x)))
    sentences xs = let splitted = splitOn ". " xs
                   in (("."++) <$> init splitted) ++ [last splitted]


-- The T1 must be T2 within T3 days of T4, or else T5.
-- We will T1 only if T2 happens within T3 from T4.


showNode :: Node -> String
showNode n = unlines $ tlText <$> getTextLines n




