{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module LS.PrettyPrinter.TypeSig (module LS.PrettyPrinter.TypeSig) where

import LS.Types (ParamText, TypedMulti, TypeSig (..), ParamType (..), MTExpr (..), mtexpr2text)
import LS.Interpreter (Interpreted, typeOfTerm)
import LS.PrettyPrinter
import Data.List.NonEmpty (NonEmpty((:|)))
import Prettyprinter
import Prettyprinter.Interpolate (di, __di)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Data.Foldable (toList)
import Data.List (intersperse)

newtype ParamText2 = PT2 ParamText

-- | ParamText2: the first line appears on the first line, subsequent lines appear on subsequent lines
-- if you want different ways of rendering a ParamText, go ahead and create newtypes for it.
-- because a ParamText is a type alias.
-- so it makes sense to marshal to a newtype PT2, PT3, PT4 etc that has the appropriate rendering instance.
instance Pretty ParamText2 where
  pretty (PT2 (x1 :| xs)) = nest 4 ( vsep ( hsep (pretty <$> toList ( fst x1) )
                                           : (hsep . fmap pretty . toList . fst <$> xs) ) )

-- | ParamText3 is used by the CoreL4 transpiler to produce colon-annotated paramtexts.
newtype ParamText3 = PT3 ParamText
instance Pretty ParamText3 where
  pretty (PT3 pt) = hcat $ intersperse ", " $ toList $ typedOrNot "_" <$> pt

-- | ParamText4 is used to approximate a recursive record. currently we can only go 2 deep.
-- in future we will have to upgrade ParamText to a full Tree type which can nest arbitrarily deep.
data ParamText4 = PT4 ParamText Interpreted -- VarPath
                | PT5 ParamText Interpreted
  deriving (Eq, Show)

instance Pretty ParamText4 where
  pretty (PT5 (line1 :| line2s) l4i)
    | null line2s = [di|#{word1 line1} = #{quoteBoT l4i line1}|]
    | otherwise   = [di|#{word1 line1} : #{quoteBoT l4i line1}|]

  pretty (PT4 orig@(line1 :| line2s) l4i) -- varpath)
    | null line2s = -- "//" <+> "208:" <+> viaShow line1 <//>
                     [di|#{quoteRHS line1} : #{pretty (MT1 (NE.head (fst line1)))}|]
                     -- we should be in a DEFINE, printing a value; if we're not, we may be dumping values with the wrong order, so we need to create a PT5.
                     --    | line2s == [] = "-- " <> viaShow line1 <//> "199: " <> word1 line1 <> colon <+> quoteBoT line1

    | otherwise    = [di|// 213: #{orig}|] <//> -- [TODO] need to fix this -- test by considering a DEFINE with nested records.
                     [__di|
                      #{quoteRHS line1} = { -- #{quoteBoT l4i line1}
                        #{vsep [ word1 l2 <+> colon <+> dquotes (lrest l2) <> comma | l2 <- line2s ]}
                      }
                     |]

-- quote based on type.
quoteBoT :: Interpreted -> TypedMulti -> Doc ann
quoteBoT l4i l@(net, _mts) =
  let unquoted = hsep $ pretty <$> NE.tail net
  in case typeOfTerm l4i {-varpath-} l of
    Just (SimpleType _ s1)  -> case s1 of
                                 "string" -> dquotes $ lrest l
                                 "number" -> unquoted
                                 "num"    -> unquoted
                                 _        -> snake_case $ NE.tail net
    Just (InlineEnum _p _s) -> dquotes $ lrest l
    Nothing                 -> unquoted

typedOrNot :: String -> TypedMulti -> Doc ann
typedOrNot        _ (multiterm, Nothing)                        = [di|#{snake_case (toList multiterm)}: Object|]
typedOrNot        _ (multiterm, Just (SimpleType TOne      s1)) = [di|#{snake_case (toList multiterm)}: #{snake_case [MTT s1]}|]
typedOrNot "corel4" (multiterm, Just (SimpleType TOptional s1)) = [di|#{snake_case (toList multiterm)}: #{[MTT s1]}|]
typedOrNot        _ (multiterm, Just (SimpleType TOptional s1)) = [di|#{snake_case (toList multiterm)}:? #{snake_case [MTT s1]}|]
typedOrNot        _ (multiterm, Just (SimpleType TList0    s1)) = [di|#{snake_case (toList multiterm)}: #{brackets (pretty s1)}|]
typedOrNot        _ (multiterm, Just (SimpleType TList1    s1)) = [di|#{snake_case (toList multiterm)}: #{brackets (pretty s1)}|]
typedOrNot        _ (multiterm, Just (SimpleType TSet0     s1)) = [di|#{snake_case (toList multiterm)}: #{brackets (pretty s1)}|]
typedOrNot        _ (multiterm, Just (SimpleType TSet1     s1)) = [di|#{snake_case (toList multiterm)}: #{brackets (pretty s1)}|]
typedOrNot        _ (multiterm, Just (InlineEnum pt1       s1)) =
  [di|#{snake_case (toList multiterm)}\# : InlineEnum unsupported: #{pt1} #{parens (pretty $ PT2 s1)}|]

prettySimpleType :: String -> (T.Text -> Doc ann) -> TypeSig -> Doc ann
prettySimpleType _        prty (SimpleType TOne      s1) = prty s1
prettySimpleType "corel4" prty (SimpleType TOptional s1) = prty s1
prettySimpleType "ts"     prty (SimpleType TOptional s1) = prty s1
prettySimpleType _        prty (SimpleType TOptional s1) = [di|#{prty s1}?|]
prettySimpleType "ts"     prty (SimpleType TList0    s1) = [di|#{prty s1}[]|]
prettySimpleType "ts"     prty (SimpleType TList1    s1) = [di|#{prty s1}[]|]
prettySimpleType "ts"     prty (SimpleType TSet0     s1) = [di|#{prty s1}[]|]
prettySimpleType "ts"     prty (SimpleType TSet1     s1) = [di|#{prty s1}[]|]
prettySimpleType _        prty (SimpleType TList0    s1) = [di|[#{prty s1}]|]
prettySimpleType _        prty (SimpleType TList1    s1) = [di|[#{prty s1}]|]
prettySimpleType _        prty (SimpleType TSet0     s1) = [di|[#{prty s1}]|]
prettySimpleType _        prty (SimpleType TSet1     s1) = [di|[#{prty s1}]|]
prettySimpleType _       _prty (InlineEnum pt1       s1) =
  [di|\# InlineEnum unsupported: #{pt1} #{parens (pretty $ PT2 s1)}|]

prettyMaybeType :: String -> (T.Text -> Doc ann) -> Maybe TypeSig -> Doc ann
prettyMaybeType _ _inner Nothing   = ""
prettyMaybeType t inner (Just ts) = [di|: #{prettySimpleType t inner ts}|]

word1,lrest :: TypedMulti -> Doc ann
word1 l = typedOrNot "_"      ((NE.head . fst $ l) :| [], snd l)
lrest l = hsep $ pretty . T.replace "\n" "\\n" <$> (NE.tail . fmap mtexpr2text . fst $ l)
