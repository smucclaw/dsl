{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE GADTs, FlexibleInstances, FlexibleContexts, UndecidableInstances, KindSignatures, RankNTypes #-}

module LS.NLP.NL4Transformations where

import LS.NLP.NL4
import qualified AnyAll as AA


flipPolarity :: forall a . Tree a -> Tree a
flipPolarity (GMkVPS temp GPOS vp) = GMkVPS temp GNEG vp
flipPolarity (GMkVPS temp GNEG vp) = GMkVPS temp GPOS vp
flipPolarity x = composOp flipPolarity x

type BoolStructGF a = AA.BoolStruct (Maybe (AA.Label GPre)) a

type BoolStructWho = BoolStructGF GWho
type BoolStructCond = BoolStructGF GCond
type BoolStructConstraint = BoolStructGF GConstraint

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
    AA.Any (Just (AA.Pre pre)) xs -> GConjPreConstraint pre GOR $ GListConstraint $ bsConstraint2gfConstraint <$> xs
    AA.All (Just (AA.Pre pre)) xs -> GConjPreConstraint pre GAND $ GListConstraint $ bsConstraint2gfConstraint <$> xs
    AA.Any (Just (AA.PrePost pre post)) xs -> GConjPrePostConstraint pre post GOR $ GListConstraint $ bsConstraint2gfConstraint <$> xs
    AA.All (Just (AA.PrePost pre post)) xs -> GConjPrePostConstraint pre post GAND $ GListConstraint $ bsConstraint2gfConstraint <$> xs
    AA.Not _ -> error $ "bsConstraint2gfConstraint: not expecting NOT in " <> show bs'
  where 
    bs' = bsNeg2textNeg bs

mapBSLabel :: (a -> b) -> (c -> d) -> AA.BoolStruct (Maybe (AA.Label a)) c ->  AA.BoolStruct (Maybe (AA.Label b)) d
mapBSLabel f g bs = case bs of 
    AA.Leaf x -> AA.Leaf $ g x
    AA.Any pre xs -> AA.Any (applyLabel f <$> pre) (mapBSLabel f g <$> xs)
    AA.All pre xs -> AA.All (applyLabel f <$> pre) (mapBSLabel f g <$> xs)
    AA.Not x -> AA.Not $ mapBSLabel f g x

bsConstraint2questions :: BoolStructConstraint -> BoolStructConstraint
bsConstraint2questions = mapBSLabel GqPRE GqCONSTR

applyLabel :: (a -> b) -> AA.Label a -> AA.Label b
applyLabel f (AA.Pre a) = AA.Pre (f a)
applyLabel f (AA.PrePost a a') = AA.PrePost (f a) (f a')

-- could do this technically?
-- instance Functor AA.Label where
--     fmap = applyLabel