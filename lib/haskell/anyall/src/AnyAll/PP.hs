{-# LANGUAGE OverloadedStrings #-}

module AnyAll.PP where

import AnyAll.Types
import AnyAll.Relevance
import Control.Monad (forM_)
import Data.Tree
import Data.Map.Strict as Map
import Prettyprinter
import Prettyprinter.Render.Util.SimpleDocTree
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB

ppline = Prettyprinter.line

svwrap View = angles
svwrap Hide = parens
svwrap Ask  = brackets

markbox (Right (Just True )) sv = svwrap sv "YES"
markbox (Right (Just False)) sv = svwrap sv " NO"
markbox (Right  Nothing    ) sv = svwrap sv "  ?"
markbox (Left  (Just True )) sv = svwrap sv "yes"
markbox (Left  (Just False)) sv = svwrap sv " no"
markbox (Left   Nothing    ) sv = svwrap sv "   "
                                                                 
hardnormal :: Marking TL.Text -> Item TL.Text -> QTree TL.Text
hardnormal m = relevant Hard DPNormal m Nothing

softnormal :: Marking TL.Text -> Item TL.Text -> QTree TL.Text
softnormal m = relevant Soft DPNormal m Nothing

docQ1 :: Marking TL.Text -> Tree (Q TL.Text) -> Doc ann
docQ1 m (Node (Q sv (Simply a)        pp              v) _) = markbox v sv <+> pretty a
docQ1 m (Node (Q sv  And       (Just (Pre     p1   )) v) c) = markbox v sv <+> pretty p1 <> ":" <> nest 3 (ppline <> vsep ((\i -> "&-" <+> docQ1 m i) <$> c))
docQ1 m (Node (Q sv  And       (Just (PrePost p1 p2)) v) c) = markbox v sv <+> pretty p1 <> ":" <> nest 3 (ppline <> vsep ((\i -> "&-" <+> docQ1 m i) <$> c)) <> ppline <> pretty p2
docQ1 m (Node (Q sv  Or        (Just (Pre     p1   )) v) c) = markbox v sv <+> pretty p1 <> ":" <> nest 3 (ppline <> vsep ((\i -> "|-" <+> docQ1 m i) <$> c))
docQ1 m (Node (Q sv  Or        (Just (PrePost p1 p2)) v) c) = markbox v sv <+> pretty p1 <> ":" <> nest 3 (ppline <> vsep ((\i -> "|-" <+> docQ1 m i) <$> c)) <> ppline <> pretty p2

ppQTree :: Item TL.Text -> Marking TL.Text -> IO ()
ppQTree i m = do
  print $ "*"  <+> "Marking:" <+> (pretty $ Prelude.drop 9 $ show m)
  print $ "**"  <+> "hard mode (ignoring defaults, considering user input only)"
  print $ nest 4 $ "    <=" <+> docQ1 m (hardnormal m i) <> ppline
  print $ "**"  <+> "soft mode (considering defaults)"
  print $ nest 4 $ "    <~" <+> docQ1 m (softnormal m i) <> ppline



