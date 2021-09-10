{-# LANGUAGE OverloadedStrings #-}

module Main where

import AnyAll
import Control.Monad (forM_)
import Data.Tree
import Data.Map.Strict as Map
import Prettyprinter
import Prettyprinter.Render.Util.SimpleDocTree
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB

simpleQ1 :: Marking TL.Text -> QTree TL.Text
simpleQ1 m = relevant Hard DPNormal m Nothing mustSing

ppline = Prettyprinter.line

svwrap View = angles
svwrap Hide = parens
svwrap Ask  = brackets

markbox (Right (Just True )) sv = svwrap sv "YES"
markbox (Right (Just False)) sv = svwrap sv "NO "
markbox (Right  Nothing    ) sv = svwrap sv "?  "
markbox (Left  (Just True )) sv = svwrap sv "yes"
markbox (Left  (Just False)) sv = svwrap sv "no "
markbox (Left   Nothing    ) sv = svwrap sv "   "
                                                                 
docQ1 :: Marking TL.Text -> Tree (Q TL.Text) -> Doc ann
docQ1 m (Node (Q sv (Simply a)        pp              v) _) = markbox v sv <+> pretty a
docQ1 m (Node (Q sv  And       (Just (Pre     p1   )) v) c) = markbox v sv <+> pretty p1 <> ":" <> nest 3 (ppline <> vsep ((\i -> "&-" <+> docQ1 m i) <$> c))
docQ1 m (Node (Q sv  And       (Just (PrePost p1 p2)) v) c) = markbox v sv <+> pretty p1 <> ":" <> nest 3 (ppline <> vsep ((\i -> "&-" <+> docQ1 m i) <$> c)) <> ppline <> pretty p2
docQ1 m (Node (Q sv  Or        (Just (Pre     p1   )) v) c) = markbox v sv <+> pretty p1 <> ":" <> nest 3 (ppline <> vsep ((\i -> "|-" <+> docQ1 m i) <$> c))
docQ1 m (Node (Q sv  Or        (Just (PrePost p1 p2)) v) c) = markbox v sv <+> pretty p1 <> ":" <> nest 3 (ppline <> vsep ((\i -> "|-" <+> docQ1 m i) <$> c)) <> ppline <> pretty p2

mustSing :: Item TL.Text
mustSing =
  All (Pre "both")
  [ Leaf "walk"
  , Any (Pre "either")
    [ Leaf "eat"
    , Leaf "drink" ] ]

main :: IO ()
main = forM_
  [ Map.empty
  , Map.fromList [("walk",  Right $ Just True)
                 ,("run",   Right $ Just True)
                 ,("eat",   Right $ Just True)
                 ,("drink", Right $ Just True)]
  ] (\m -> do
        print $ "*" <+> viaShow m
        print $ "<=" <+> docQ1 m (simpleQ1 m)
    )

