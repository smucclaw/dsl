{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE RankNTypes, GADTs #-}

module LS.NLP.NL4Transformations where

import LS.NLP.NL4
import qualified AnyAll as AA


flipPolarity :: forall a . Tree a -> Tree a
flipPolarity (GMkVPS temp GPOS vp) = GMkVPS temp GNEG vp
flipPolarity (GMkVPS temp GNEG vp) = GMkVPS temp GPOS vp
flipPolarity x = composOp flipPolarity x


type BoolStructWho = AA.OptionallyLabeledBoolStruct GWho

bsNeg2textNeg :: BoolStructWho -> BoolStructWho
bsNeg2textNeg bs = case bs of
  AA.Leaf x -> AA.Leaf x
  AA.All l xs -> AA.All l (fmap bsNeg2textNeg xs)
  AA.Any l xs -> AA.Any l (fmap bsNeg2textNeg xs)
  AA.Not (AA.Leaf x)    -> AA.Leaf (flipPolarity x)
  AA.Not (AA.All l xs)  -> AA.All l (fmap bsNeg2textNeg xs)
  AA.Not (AA.Any l xs)  -> AA.Any l (fmap bsNeg2textNeg xs)
  AA.Not (AA.Not x)     -> bsNeg2textNeg x

-- inverse:
-- textNeg2bsNeg :: BoolStructWho -> BoolStructWho

bsWho2gfWho :: BoolStructWho -> GWho
bsWho2gfWho bs = case bs' of
    AA.Leaf x -> x
    AA.Any _ xs -> GConjWho GOR $ GListWho $ bsWho2gfWho <$> xs
    AA.All _ xs -> GConjWho GAND $ GListWho $ bsWho2gfWho <$> xs
    AA.Not _ -> error $ "bsWho2gfWho: not expecting NOT in " <> show bs'
  where 
    bs' = bsNeg2textNeg bs