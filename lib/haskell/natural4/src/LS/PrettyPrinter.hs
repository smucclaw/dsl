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
      ParamText2(..),
      pt2text,
      rel2txt,
      rel2op )
import qualified AnyAll as AA
import Prettyprinter
import Data.List.NonEmpty

-- | Pretty RelationalPredicate: recurse
instance Pretty RelationalPredicate where
  pretty (RPParamText   pt)            = pretty $ pt2text pt
  pretty (RPMT          mt)            = snake_case mt
  pretty (RPConstraint  mt1 rprel mt2) = hsep [ snake_case mt1, pretty (rel2op rprel), snake_case mt2 ]
  pretty (RPBoolStructR mt1 rprel bsr) = hsep [ snake_case mt1, pretty rprel, pretty bsr ]

snake_case :: [T.Text] -> Doc ann
snake_case xs = hsep (pretty . T.replace " " "_" <$> xs)

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

  
