{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- the job of this module is to create orphan instances

module LS.PrettyPrinter where

-- This module instantiates a number of LS types into the Pretty typeclass used by Prettyprinter.
-- This is similar to instantiating into Show, but it all happens within Prettyprinter's "Doc ann" rather than String.

import qualified Data.Text as T
import LS.Types
    ( RelationalPredicate(..),
      HornClause2(HC2),
      BoolStructR,
      RPRel,
      ParamText,
      TypedMulti,
      TypeSig(..),
      ParamType(..),
      pt2text,
      rel2txt,
      rel2op )
import qualified AnyAll as AA
import Prettyprinter
import Data.List (intersperse)
import Data.List.NonEmpty as NE ( NonEmpty((:|)), toList, head, tail )

-- | Pretty RelationalPredicate: recurse
instance Pretty RelationalPredicate where
  pretty (RPParamText   pt)            = pretty $ pt2text pt
  pretty (RPMT          mt)            = snake_case mt
  pretty (RPConstraint  mt1 rprel mt2) = hsep [ snake_case mt1, pretty (rel2op rprel), snake_case mt2 ]
  pretty (RPBoolStructR mt1 rprel bsr) = hsep [ snake_case mt1, pretty rprel, pretty bsr ]

snake_case :: [T.Text] -> Doc ann
snake_case xs = hsep (snake_inner <$> xs)

snake_inner :: T.Text -> Doc ann
snake_inner = pretty . T.replace " " "_"

instance Pretty RPRel where
  pretty rpr = pretty $ rel2txt rpr

newtype ParamText2 = PT2 ParamText

-- | ParamText2: the first line appears on the first line, subsequent lines appear on subsequent lines
-- if you want different ways of rendering a ParamText, go ahead and create newtypes for it.
-- because a ParamText is a type alias.
-- so it makes sense to marshal to a newtype PT2, PT3, PT4 etc that has the appropriate rendering instance.
instance Pretty ParamText2 where
  pretty (PT2 (x1 :| xs)) = nest 4 ( vsep ( hsep (pretty <$> NE.toList ( fst x1) )
                                           : (hsep . fmap pretty . toList . fst <$> xs) ) )

-- | ParamText3 is used by the CoreL4 transpiler to produce colon-annotated paramtexts.
newtype ParamText3 = PT3 ParamText
instance Pretty ParamText3 where
  pretty (PT3 pt) = hcat (intersperse ", " (toList $ typedOrNot <$> pt))

-- | ParamText4 is used to approximate a recursive record. currently we can only go 2 deep.
newtype ParamText4 = PT4 ParamText
instance Pretty ParamText4 where
  pretty (PT4 (line1 :| line2s))
    | line2s == [] = word1 line1 <> colon <+> dquotes (lrest line1)
    | otherwise    = word1 line1 <> colon <+> lbrace <+> "--" <+> lrest line1 <> Prettyprinter.line
                     <> nest 2 (vsep [ word1 l2 <+> colon <+> dquotes (lrest l2) <> comma | l2 <- line2s ])
                     <> Prettyprinter.line
                     <> rbrace
    where word1 l = typedOrNot       ((NE.head . fst $ l) :| [], snd l)
          lrest :: TypedMulti -> Doc ann
          lrest l = hsep $ pretty <$> (NE.tail . fst $ l)

typedOrNot :: TypedMulti -> Doc ann
typedOrNot (multitext, Nothing)                        = snake_case (toList multitext)
typedOrNot (multitext, Just (SimpleType TOne      s1)) = snake_case (toList multitext) <> ":"  <+> pretty s1
typedOrNot (multitext, Just (SimpleType TOptional s1)) = snake_case (toList multitext) <> ":?" <+> pretty s1
typedOrNot (multitext, Just (SimpleType TList0    s1)) = snake_case (toList multitext) <> ":"  <+> brackets (pretty s1)
typedOrNot (multitext, Just (SimpleType TList1    s1)) = snake_case (toList multitext) <> ":"  <+> brackets (pretty s1)
typedOrNot (multitext, Just (InlineEnum pt1       s1)) = snake_case (toList multitext) <> ":"  <+>
  "InlineEnum unsupported:" <+> viaShow pt1 <+> parens (pretty $ PT2 s1)

prettySimpleType :: (T.Text -> Doc ann) -> TypeSig -> Doc ann
prettySimpleType prty (SimpleType TOne      s1) = prty s1
prettySimpleType prty (SimpleType TOptional s1) = prty s1 <> "?"
prettySimpleType prty (SimpleType TList0    s1) = brackets (prty s1)
prettySimpleType prty (SimpleType TList1    s1) = brackets (prty s1)
prettySimpleType prty (InlineEnum pt1       s1) = "InlineEnum unsupported:" <+> viaShow pt1 <+> parens (pretty $ PT2 s1)

prettyMaybeType :: (T.Text -> Doc ann) -> (Maybe TypeSig) -> Doc ann
prettyMaybeType inner Nothing   = ""
prettyMaybeType inner (Just ts) = colon <+> prettySimpleType inner ts

-- At this time, none of the preconditions should be found in the head, so we ignore that.
hc2preds :: HornClause2 -> BoolStructR
hc2preds (HC2 _headRP Nothing) = AA.Leaf (RPMT ["TRUE"])
hc2preds (HC2 _headRP (Just bsr)) = bsr

  
