{-# LANGUAGE OverloadedStrings #-}

module AnyAll.PP (ppQTree, hardnormal) where

import AnyAll.Types hiding ((<>))
import AnyAll.Relevance
import Control.Monad (forM_)
import Data.Maybe
import Data.Tree
import Data.String (IsString)
import Data.Map.Strict as Map
import Prettyprinter
import Prettyprinter.Render.Util.SimpleDocTree
import qualified Data.ByteString.Lazy   as B
import qualified Data.Text       as T
import Data.Aeson.Types

ppline = Prettyprinter.line

svwrap View = angles
svwrap Hide = parens
svwrap Ask  = brackets

markbox (Default (Right (Just True ))) sv = svwrap sv "YES"
markbox (Default (Right (Just False))) sv = svwrap sv " NO"
markbox (Default (Right  Nothing    )) sv = svwrap sv "  ?"
markbox (Default (Left  (Just True ))) sv = svwrap sv "yes"
markbox (Default (Left  (Just False))) sv = svwrap sv " no"
markbox (Default (Left   Nothing    )) sv = svwrap sv "   "
                                                                 
hardnormal, softnormal :: Marking T.Text -> Item T.Text -> QTree T.Text
hardnormal m = relevant Hard DPNormal m Nothing

softnormal m = relevant Soft DPNormal m Nothing

docQ1 :: (IsString a, Ord a, Show a, Pretty a) => Marking a -> Tree (Q a) -> Doc ann
docQ1 m (Node (Q sv  Neg              pp              v) c) = markbox v sv <+>              ": !" <+> nest 2 (hsep $ docQ1 m <$> c)
docQ1 m (Node (Q sv (Simply a)        pp              v) _) = markbox v sv <+> pretty a
docQ1 m (Node (Q sv  And       (Just (Pre     p1   )) v) c) = markbox v sv <+> pretty p1 <> ":" <> nest 2 (ppline <> vsep ((\i -> "&" <+> docQ1 m i) <$> c))
docQ1 m (Node (Q sv  And       (Just (PrePost p1 p2)) v) c) = markbox v sv <+> pretty p1 <> ":" <> nest 2 (ppline <> vsep ((\i -> "&" <+> docQ1 m i) <$> c)) <> ppline <> pretty p2
docQ1 m (Node (Q sv  And       Nothing                v) c) = markbox v sv <+> "all of:"        <> nest 2 (ppline <> vsep ((\i -> "&" <+> docQ1 m i) <$> c))
docQ1 m (Node (Q sv  Or        Nothing                v) c) = markbox v sv <+> "any of:"        <> nest 2 (ppline <> vsep ((\i -> "|" <+> docQ1 m i) <$> c))
docQ1 m (Node (Q sv  Or        (Just (Pre     p1   )) v) c) = markbox v sv <+> pretty p1 <> ":" <> nest 2 (ppline <> vsep ((\i -> "|" <+> docQ1 m i) <$> c))
docQ1 m (Node (Q sv  Or        (Just (PrePost p1 p2)) v) c) = markbox v sv <+> pretty p1 <> ":" <> nest 2 (ppline <> vsep ((\i -> "|" <+> docQ1 m i) <$> c)) <> ppline <> pretty p2

ppQTree :: Item T.Text -> Map.Map T.Text (Either (Maybe Bool) (Maybe Bool)) -> IO ()
ppQTree i mm = do
  let m = Marking (Default <$> mm)
      hardresult = hardnormal m i
      softresult = softnormal m i
  print $ "*"  <+> "Marking:" <+> (pretty $ Prelude.drop 9 $ show m) <> ppline
  print $ "**" <+> "soft result =" <+> markbox (mark (rootLabel softresult)) View
  print $ "**" <+> "hard result =" <+> markbox (mark (rootLabel hardresult)) View
  print $ nest 3 $ "   =" <+> docQ1 m hardresult <> ppline

  print $ "**" <+> "JSON:"
  B.putStr $ asJSON hardresult
  print ppline

  print $ "**" <+> "For UI:"
  B.putStr $ getForUI hardresult
  print ppline
  
{-
    {
      "ask": {
        "run": {
          "Left": false
        }
      },
      "view": {
        "drink": {
          "Right": true
        },
        "eat": {
          "Right": true
        },
        "walk": {
          "Right": true
        }
      }
    }
-}

-- next steps:
-- - output an HTML version QTree
-- - accept a JSON input of both the Item tree and the Marking store. use Aeson to parse





