{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE GADTs, FlexibleInstances, FlexibleContexts, UndecidableInstances, KindSignatures, RankNTypes #-}

module LS.NLP.NL4Transformations where

import LS.NLP.NL4
import qualified AnyAll as AA


flipPolarity :: forall a . Tree a -> Tree a
flipPolarity (GMkVPS temp GPOS vp) = GMkVPS temp GNEG vp
flipPolarity (GMkVPS temp GNEG vp) = GMkVPS temp GPOS vp
flipPolarity x = composOp flipPolarity x

type BoolStructWho = AA.OptionallyLabeledBoolStruct GWho
type BoolStructCond = AA.OptionallyLabeledBoolStruct GCond
--type BoolStructConstraint = AA.OptionallyLabeledBoolStruct GConstraint

-- TODO: parse BoolStructR into this structure, where even the Pre and Post are parsed into GF types
type BoolStructConstraint = AA.BoolStruct (Maybe GPre) GConstraint

bsNeg2textNeg :: (Gf (Tree a)) => AA.BoolStruct b (Tree a) -> AA.BoolStruct b (Tree a)
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

bsCond2gfCond :: BoolStructCond -> GCond
bsCond2gfCond bs = case bs' of
    AA.Leaf x -> x
    AA.Any _ xs -> GConjCond GOR $ GListCond $ bsCond2gfCond <$> xs
    AA.All _ xs -> GConjCond GAND $ GListCond $ bsCond2gfCond <$> xs
    AA.Not _ -> error $ "bsCond2gfCond: not expecting NOT in " <> show bs'
  where 
    bs' = bsNeg2textNeg bs

bsConstraint2gfConstraint :: BoolStructConstraint -> GConstraint
bsConstraint2gfConstraint bs = case bs' of
    AA.Leaf x -> x
    AA.Any Nothing xs -> GConjConstraint GOR $ GListConstraint $ bsConstraint2gfConstraint <$> xs
    AA.All Nothing xs -> GConjConstraint GAND $ GListConstraint $ bsConstraint2gfConstraint <$> xs
    AA.Any (Just pre) xs -> GConjPreConstraint pre GOR $ GListConstraint $ bsConstraint2gfConstraint <$> xs
    AA.All (Just pre) xs -> GConjPreConstraint pre GAND $ GListConstraint $ bsConstraint2gfConstraint <$> xs
    AA.Not _ -> error $ "bsConstraint2gfConstraint: not expecting NOT in " <> show bs'
  where 
    bs' = bsNeg2textNeg bs

mapBSLabel :: (a -> b) -> (c -> d) -> AA.BoolStruct (Maybe a) c ->  AA.BoolStruct (Maybe b) d
mapBSLabel f g bs = case bs of 
    AA.Leaf x -> AA.Leaf $ g x
    AA.Any Nothing xs -> AA.Any Nothing (mapBSLabel f g <$> xs)
    AA.All Nothing xs -> AA.All Nothing (mapBSLabel f g <$> xs)
    AA.Any (Just pre) xs -> AA.Any (Just $ f pre) (mapBSLabel f g <$> xs)
    AA.All (Just pre) xs -> AA.All (Just $ f pre) (mapBSLabel f g <$> xs)
    AA.Not x -> AA.Not $ mapBSLabel f g x

bsConstraint2questions :: BoolStructConstraint -> BoolStructConstraint
bsConstraint2questions = mapBSLabel GqPRE GqCONSTR
