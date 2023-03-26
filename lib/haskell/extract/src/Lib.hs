{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-| This library is intended for parsing and extracting, or at least reformatting, natural language input, in the direction of L4.
-}

module Lib where

import Data.Maybe ( maybeToList )
import Data.OrgMode
import Text.Megaparsec
    ( anySingle, parseMaybe, some, someTill, Parsec )
import Text.Megaparsec.Char ( string )
import Data.Void ( Void )

import Data.List (intercalate, partition)
import Data.List.Split ( splitOn )

-- | top level function called by Main: extract, transform, load!
orgMain :: IO ()
orgMain = do
  input <- orgFile <$> getContents
  let processed = updateNode processNode <$> odNodes input
  mapM_ putStrLn $ showNode <$> processed

-- | add a text paragraph to represent our analysis of a given org section
processNode :: Node -> Maybe Node
processNode n = pure $
                n { nTags = ["moo"]
                  , nChildren = ChildText (TextLine { tlIndent = 0
                                                    , tlText = srcL4 "moo"
                                                    , tlLineNum = Nothing
                                                    } )
                                `afterPropertiesDrawer`
                                nChildren n
                  }

  where
    srcL4 x = unlines [ "#+BEGIN_SRC l4 :tangle out.l4", x, "#+END_SRC" ]
    afterPropertiesDrawer :: NodeChild -> [NodeChild] -> [NodeChild]
    afterPropertiesDrawer new olds =
      let (drawers, notdrawers) =
            partition (\case ChildDrawer _ -> True
                             _             -> False) olds
      in drawers ++ new : notdrawers

-- | we annotate rules using tags
data MyTag
  = TDeontic           -- ^ deontic, duty, regulative rule
  | TDisplay           -- ^ verbatim output
  | TSummarizeChildren -- ^ signposting giving outline of what is coming up next
  | TPleaseRefer       -- ^ signposting to elsewhere in this document
  | TMeans             -- ^ "X Means Y"
  | TDecide            -- ^ "Decide ... "
  | TDefine            -- ^ variable definition
  | TDeclare           -- ^ type declaration
  deriving (Eq, Show)

-- | TextLine to L4 text
tl2l4 :: String -> [(MyTag, String)]
tl2l4 tlt
  -- recurse to process multiple sentences within a single input
  | length (sentences tlt) > 1
    && concatMap tl2l4 (sentences tlt) /= [] =
       concatMap tl2l4 (sentences tlt)

  -- a MEANS rule
  | "means" `elem` words tlt = maybeToList $ do
      (lhs, rhs) <- parseMaybe ((,)
                                <$> someTill anySingle (string " means ")
                                <*> (some anySingle :: Parsec Void String String)
                               ) tlt
      pure (TMeans, intercalate "," [ ";;\n", "", trimStars lhs ++ "\n", "MEANS", rhs ])

  -- untransformed
  | otherwise = []

  where
    trimStars x = reverse (dropWhile (== '*') (reverse (dropWhile (== '*') x)))
    sentences xs = let splitted = splitOn ". " xs
                   in (("."++) <$> init splitted) ++ [last splitted]


-- The T1 must be T2 within T3 days of T4, or else T5.
-- We will T1 only if T2 happens within T3 from T4.


showNode :: Node -> String
showNode n = unlines $ tlText <$> getTextLines n




