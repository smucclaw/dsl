{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- the job of this module is to create orphan instances

module LS.PrettyPrinter where

-- This module instantiates a number of LS types into the Pretty typeclass used by Prettyprinter.
-- This is similar to instantiating into Show, but it all happens within Prettyprinter's "Doc ann" rather than String.

import qualified Data.Text as T
import LS.Types
import qualified AnyAll as AA
import Prettyprinter
import Data.List ( intersperse )
import Data.List.NonEmpty

-- | Pretty RelationalPredicate: recurse
instance Pretty RelationalPredicate where
  pretty (RPParamText   pt)            = pretty $ pt2text pt
  pretty (RPMT          mt)            = snakeCase mt
  pretty (RPConstraint  mt1 rprel mt2) = hsep [ snakeCase mt1, pretty (rel2op rprel), snakeCase mt2 ]
  pretty (RPBoolStructR mt1 rprel bsr) = hsep [ snakeCase mt1, pretty rprel, pretty bsr ]

tr :: Char -> Char -> String -> String
tr _ _ [] = []
tr x y (z:zs)
  | x == z    = y : tr x y zs
  | otherwise = z : tr x y zs
  
snakeCase :: [T.Text] -> Doc ann
snakeCase xs = hsep (pretty . tr ' ' '_' . T.unpack <$> xs)

instance Pretty RPRel where
  pretty rpr = pretty $ rel2txt rpr

-- | ParamText: the first line appears on the first line, subsequent lines appear on subsequent lines
instance Pretty ParamText2 where
  pretty (PT2 (x1 :| xs)) = nest 4 ( vsep ( hsep (pretty <$> toList ( fst x1) )
                                           : (hsep . fmap pretty . toList . fst <$> xs) ) )

-- | get the AnyAll tree out of a HornClause2.
-- At this time, none of the preconditions should be found in the head, so we ignore that.
hc2preds :: HornClause2 -> BoolStructR
hc2preds (HC2 _headRP Nothing) = AA.Leaf (RPMT ["TRUE"])
hc2preds (HC2 _headRP (Just bsr)) = bsr

  
