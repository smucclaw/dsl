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
docQ1 m (Node (Q sv  And       (Just (Pre     p1   )) v) c) = markbox v sv <+> pretty p1 <> ":" <> nest 2 (ppline <> vsep ((\i -> "&" <+> docQ1 m i) <$> c))
docQ1 m (Node (Q sv  And       (Just (PrePost p1 p2)) v) c) = markbox v sv <+> pretty p1 <> ":" <> nest 2 (ppline <> vsep ((\i -> "&" <+> docQ1 m i) <$> c)) <> ppline <> pretty p2
docQ1 m (Node (Q sv  Or        (Just (Pre     p1   )) v) c) = markbox v sv <+> pretty p1 <> ":" <> nest 2 (ppline <> vsep ((\i -> "|" <+> docQ1 m i) <$> c))
docQ1 m (Node (Q sv  Or        (Just (PrePost p1 p2)) v) c) = markbox v sv <+> pretty p1 <> ":" <> nest 2 (ppline <> vsep ((\i -> "|" <+> docQ1 m i) <$> c)) <> ppline <> pretty p2

ppQTree :: Item TL.Text -> Marking TL.Text -> IO ()
ppQTree i m = do
  let hardresult = hardnormal m i
      softresult = softnormal m i
  print $ "*"  <+> "Marking:" <+> (pretty $ Prelude.drop 9 $ show m) <> ppline
  print $ "**" <+> "soft result =" <+> markbox (mark (rootLabel softresult)) View
  print $ "**" <+> "hard result =" <+> markbox (mark (rootLabel hardresult)) View
  print $ nest 3 $ "   =" <+> docQ1 m hardresult <> ppline


-- next steps:
-- - output an HTML version QTree
-- - output a JSON version of QTree
-- - accept a JSON input of both the Item tree and the Marking store. use Aeson to parse


