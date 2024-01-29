{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module AnyAll.PP
  ( ppQTree,
    hardnormal,
    softnormal,
    cStyle,
    haskellStyle,
  )
where

import AnyAll.BoolStruct
  ( BoolStruct (..),
    OptionallyLabeledBoolStruct,
    alwaysLabeled,
  )
import AnyAll.Relevance (relevant)
import AnyAll.Types
  ( AndOr (And, Neg, Or, Simply),
    Default (..),
    Hardness (Hard, Soft),
    Label (..),
    Marking (..),
    Q (Q, mark),
    QTree,
    ShouldView (Ask, Hide, View),
    asJSON,
    getDefault,
    getForUI,
  )
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Coerce (coerce)
import Data.HashMap.Strict as Map (HashMap)
import Data.List (intersperse)
import Data.String (IsString)
import Data.Text qualified as T
import Data.Tree (Tree (Node, rootLabel))
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    angles,
    brackets,
    hsep,
    line,
    nest,
    parens,
    vsep,
    (<+>),
  )
import Prettyprinter.Interpolate (di, __di)
import Text.Pretty.Simple (pPrint)

data Style ann = Style
                 { s_parens :: Doc ann -> Doc ann
                 , s_not :: String
                 , s_and :: String
                 , s_or  :: String
                 }

cStyle, haskellStyle, pythonStyle :: (Pretty txt) => OptionallyLabeledBoolStruct txt -> Doc ann

-- | render an BoolStruct to a C-style syntax
cStyle       = mystyle (Style parens "!"   "&&"  "||")

-- | render an BoolStruct to Haskell-style syntax
haskellStyle = mystyle (Style parens "not" "&&"  "||")

-- | render an BoolStruct to Python-style syntax
pythonStyle  = mystyle (Style parens "not" "and" "or")

mystyle :: (Pretty txt) => Style ann -> OptionallyLabeledBoolStruct txt -> Doc ann
mystyle _ (Leaf x)     = pretty x
mystyle s (All lbl xs) = parens (hsep (intersperse (pretty $ s_and s) (mystyle s <$> xs)))
mystyle s (Any lbl xs) = parens (hsep (intersperse (pretty $ s_or  s) (mystyle s <$> xs)))
mystyle s (Not     x ) = pretty (s_not s) <+> mystyle s x

ppline :: Doc ann
ppline = Prettyprinter.line

svwrap :: ShouldView -> Doc ann -> Doc ann
svwrap View = angles
svwrap Hide = parens
svwrap Ask  = brackets

markbox :: Default Bool -> ShouldView -> Doc ann
markbox (getDefault -> bool) sv = svwrap sv case bool of
  Right (Just True) -> "YES"
  Right (Just False) -> " NO"
  Right Nothing -> "  ?"
  Left (Just True) -> "yes"
  Left (Just False) -> " no"
  Left Nothing -> "   "

hardnormal :: Marking T.Text -> OptionallyLabeledBoolStruct T.Text -> QTree T.Text
hardnormal m = relevant Hard m Nothing

softnormal :: Marking T.Text -> OptionallyLabeledBoolStruct T.Text -> QTree T.Text
softnormal m = relevant Soft m Nothing

docQ1 :: (IsString a, Ord a, Show a, Pretty a) => Marking a -> Tree (Q a) -> Doc ann
docQ1 m (Node (Q sv  Neg              pp              v) c) = markbox v sv <+>              ": !" <+> nest 2 (hsep $ docQ1 m <$> c)
docQ1 m (Node (Q sv (Simply a)        pp              v) _) = markbox v sv <+> pretty a
docQ1 m (Node (Q sv  And       (Just (Pre     p1   )) v) c) = markbox v sv <+> pretty p1 <> ":" <> nest 2 (ppline <> vsep ((\i -> "&" <+> docQ1 m i) <$> c))
docQ1 m (Node (Q sv  And       (Just (PrePost p1 p2)) v) c) = markbox v sv <+> pretty p1 <> ":" <> nest 2 (ppline <> vsep ((\i -> "&" <+> docQ1 m i) <$> c)) <> ppline <> pretty p2
docQ1 m (Node (Q sv  And       Nothing                v) c) = markbox v sv <+> "all of:"        <> nest 2 (ppline <> vsep ((\i -> "&" <+> docQ1 m i) <$> c))
docQ1 m (Node (Q sv  Or        Nothing                v) c) = markbox v sv <+> "any of:"        <> nest 2 (ppline <> vsep ((\i -> "|" <+> docQ1 m i) <$> c))
docQ1 m (Node (Q sv  Or        (Just (Pre     p1   )) v) c) = markbox v sv <+> pretty p1 <> ":" <> nest 2 (ppline <> vsep ((\i -> "|" <+> docQ1 m i) <$> c))
docQ1 m (Node (Q sv  Or        (Just (PrePost p1 p2)) v) c) = markbox v sv <+> pretty p1 <> ":" <> nest 2 (ppline <> vsep ((\i -> "|" <+> docQ1 m i) <$> c)) <> ppline <> pretty p2

ppQTree :: OptionallyLabeledBoolStruct T.Text -> Map.HashMap T.Text (Either (Maybe Bool) (Maybe Bool)) -> IO ()
ppQTree i (coerce -> m) = do
  let hardresult = hardnormal m i
      softresult = softnormal m i
  print [__di|
    * Marking: #{drop 9 $ show m}
    ** soft result = #{markbox (mark (rootLabel softresult)) View} 
    ** hard result = #{markbox (mark (rootLabel hardresult)) View} 
          = #{docQ1 m hardresult}
    ** JSON: #{asJSON hardresult}
    ** For UI: #{getForUI hardresult}
    ** C-style: #{cStyle i}
    ** show of the BoolStruct: #{i}
    ** JSON of the BoolStruct: #{encodePretty i}
    ** show of the (DefaultLabeled) BoolStruct: #{alwaysLabeled i}
    ** JSON of the (DefaultLabeled) BoolStruct: #{encodePretty $ alwaysLabeled i}
  |]

instance (IsString t, Pretty t, Pretty a) => Pretty (BoolStruct (Maybe (Label t)) a) where
  pretty (Leaf a)            = pretty a
  pretty (All Nothing    xs)             = pretty (All (Just (Pre "All of the following:")) xs)
  pretty (All (Just (Pre     p1   )) xs) = nest 4 (vsep $ pretty p1 : (pretty <$> xs))
  pretty (All (Just (PrePost p1 p2)) xs) = nest 4 (vsep $ pretty p1 : (pretty <$> xs)) <> line <> pretty p2
  pretty (Any Nothing    xs)             = pretty (Any (Just (Pre "Any of the following:")) xs)
  pretty (Any (Just (Pre     p1   )) xs) = nest 4 (vsep $ pretty p1 : (pretty <$> xs))
  pretty (Any (Just (PrePost p1 p2)) xs) = nest 4 (vsep $ pretty p1 : (pretty <$> xs)) <> line <> pretty p2
  pretty (Not            x ) = "not" <+> pretty x

instance (Pretty a) => Pretty (Label a) where
  pretty (Pre     p1)    = pretty p1
  pretty (PrePost p1 p2) = [di|#{pretty p1} ... #{pretty p2}|]
