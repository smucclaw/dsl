{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- the job of this module is to create orphan instances

module LS.PrettyPrinter where

-- This module instantiates a number of LS types into the Pretty typeclass used by Prettyprinter.
-- This is similar to instantiating into Show, but it all happens within Prettyprinter's "Doc ann" rather than String.

import qualified Data.Text as T
import LS.Types
import qualified AnyAll as AA
import Prettyprinter
import Data.List (intersperse)
import qualified Data.Map as Map
import Data.List.NonEmpty as NE ( NonEmpty((:|)), toList, head, tail )
import Debug.Trace

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
-- in future we will have to upgrade ParamText to a full Tree type which can nest arbitrarily deep.
data ParamText4 = PT4 ParamText Interpreted -- VarPath
  deriving (Eq, Show)
instance Pretty ParamText4 where
  pretty (PT4 (line1 :| line2s) l4i) -- varpath)
    | line2s == [] = word1 line1 <> colon <+> quoteBoT line1
    | otherwise    = word1 line1 <> colon <+> lbrace <+> "--" <+> quoteBoT line1 <> Prettyprinter.line
                     <> nest 2 (vsep [ word1 l2 <+> colon <+> dquotes (lrest l2) <> comma | l2 <- line2s ])
                     <> Prettyprinter.line
                     <> rbrace
    where
      word1,lrest :: TypedMulti -> Doc ann
      word1 l = typedOrNot       ((NE.head . fst $ l) :| [], snd l)
      lrest l = hsep $ pretty . T.replace "\n" "\\n" <$> (NE.tail . fst $ l)
      -- quote based on type.
      quoteBoT :: TypedMulti -> Doc ann
      quoteBoT l@(net, mts) =
        let unquoted = hsep $ pretty <$> NE.tail net
        in case typeOfTerm l4i {-varpath-} l of
          Just (SimpleType _ s1)  -> case s1 of
                                       "string" -> dquotes $ lrest l
                                       "number" -> unquoted
                                       "num"    -> unquoted
                                       _        -> snake_case (NE.tail net)
          Just (InlineEnum _p _s) -> dquotes $ lrest l
          Nothing                 -> unquoted

-- what is the (L4) type of a given term?
-- the term in question is given as a TypedMulti, which often works out to be a key/value together in a TypedMulti.
-- if the TypedMulti already contains an explicit type, we'll return that.
-- otherwise, we need to interrogate the class hierarchy and the "varpath" breadcrumb by which we arrived at the current term.
-- for example, if we know that a Corporation extends Party with a representative : Natural Person, and we are now considering
-- a representative: Bob inside of a const Company : US_Company which extends Corporation, we have enough information to know that
-- the RHS Bob is a variable name, not a string, so we shoudn't double-quote it.

typeOfTerm :: Interpreted {- -> VarPath -} -> TypedMulti -> Maybe TypeSig
typeOfTerm l4i tm =
  let (ct@(CT ch), scopetabs) = (classtable l4i, scopetable l4i)
  in
    Nothing

-- -- we are given a long varpath representing the traversal through class (anonymous) attribute space -- i.e. multiple levels of HAS
-- walk :: Interpreted -> Maybe ClsTab -> VarPath -> TypedMulti -> Maybe TypeSig
-- walk l4i Nothing xs     tm = trace ("walk 1: Maybe ClsTab argument is Nothing") $ Nothing
-- walk l4i _       []     tm = trace ("walk 2: varpath exhausted") $ Nothing
-- walk l4i (Just ct0) (x:xs) ot@(l:|r,mts) =
--   trace ("walk 3: x = " ++ show x ++ "; xs = " ++ show xs ++ "; l = " ++ show l ++ "; r = " ++ show r) $
--       case mts of
--         Just ts -> case getUnderlyingType ts of
--                      Right t1 -> case Map.lookup t1 (unCT $ classtable l4i) of
--                                    Nothing -> trace ("underlying type " ++ show t1 ++ " not found in toplevel classtable!") $
--                                               Just ts
--                                    Just (its, ct1) -> trace ("underlying type " ++ show t1 ++ " with inferrable typesig " ++ show its ++ " found in toplevel classtable, walking") $
--                                                       trace ("attempting to obtain extended attributes first tho") $
--                                                       let ct2 = extendedAttributes ct1 t1
--                                                       in 
--                                                       walk l4i ct2 xs ot
--                      Left err -> trace ("underlying type returned error: " ++ err) $
--                                  Nothing
--         Nothing -> trace ("no explicit type given, so attempting to walk current clstab " ++ show ct0) $
--                    trace ("does the current clstab have an attribute matching our varname " ++ show l ++ "?") $
--                    case Map.lookup l (unCT ct0) of
--                      Nothing -> trace ("no it does not.") $
--                                 Nothing
--                      Just (its, ct1) -> trace ("yes it does! we got back ct2 " ++ show ct1 ++ " so we will recurse, dropping " ++ show x) $
--                                         walk l4i (Just ct1) xs ot

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

  
