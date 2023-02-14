{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE GADTs, FlexibleInstances, FlexibleContexts, UndecidableInstances, KindSignatures, RankNTypes #-}

module LS.NLP.NL4Transformations where

import LS.NLP.NL4
import qualified AnyAll as AA


flipPolarity :: forall a . Tree a -> Tree a
flipPolarity (GMkVPS temp GPOS vp) = GMkVPS temp GNEG vp
flipPolarity (GMkVPS temp GNEG vp) = GMkVPS temp GPOS vp
flipPolarity x = composOp flipPolarity x


type BoolStructGF a = AA.BoolStruct (Maybe (AA.Label GPrePost)) (Tree a)

type BoolStructWho = BoolStructGF GWho_  -- have to use underscore versions because of flipPolarity
type BoolStructCond = BoolStructGF GCond_
type BoolStructConstraint = BoolStructGF GConstraint_

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

-----------------------------------------------------------------------------
-- This is rather hard to read, but the alternative is to duplicate bs2gf for every single GF category

type ConjFun list single = GConj -> Tree list -> Tree single
type ConjPreFun list single = GPrePost -> GConj -> Tree list -> Tree single
type ConjPrePostFun list single = GPrePost -> GPrePost -> GConj -> Tree list -> Tree single
type ListFun single list = [Tree single] -> Tree list

bs2gf :: (Gf (Tree s)) => ConjFun l s -> ConjPreFun l s -> ConjPrePostFun l s -> ListFun s l -> BoolStructGF s -> Tree s
bs2gf conj conjPre conjPrePost mkList bs = case bs' of
    AA.Leaf x -> x
    AA.Any Nothing xs -> conj GOR $ mkList $ f <$> xs
    AA.All Nothing xs -> conj GAND $ mkList $ f <$> xs
    AA.Any (Just (AA.Pre pre)) xs -> conjPre pre GOR $ mkList $ f <$> xs
    AA.All (Just (AA.Pre pre)) xs -> conjPre pre GAND $ mkList $ f <$> xs
    AA.Any (Just (AA.PrePost pre post)) xs -> conjPrePost pre post GOR $ mkList $ f <$> xs
    AA.All (Just (AA.PrePost pre post)) xs -> conjPrePost pre post GAND $ mkList $ f <$> xs
    AA.Not _ -> error $ "bs2gf: not expecting NOT in " <> show bs'
  where 
    f = bs2gf conj conjPre conjPrePost mkList
    bs' = bsNeg2textNeg bs

bsWho2gfWho :: BoolStructWho -> GWho
bsWho2gfWho = bs2gf GConjWho GConjPreWho GConjPrePostWho GListWho

bsCond2gfCond :: BoolStructCond -> GCond
bsCond2gfCond = bs2gf GConjCond GConjPreCond GConjPrePostCond GListCond 

bsConstraint2gfConstraint :: BoolStructConstraint -> GConstraint
bsConstraint2gfConstraint = bs2gf GConjConstraint GConjPreConstraint GConjPrePostConstraint GListConstraint 

-----------------------------------------------------------------------------

mapBSLabel :: (a -> b) -> (c -> d) -> AA.BoolStruct (Maybe (AA.Label a)) c ->  AA.BoolStruct (Maybe (AA.Label b)) d
mapBSLabel f g bs = case bs of 
    AA.Leaf x -> AA.Leaf $ g x
    AA.Any pre xs -> AA.Any (applyLabel f <$> pre) (mapBSLabel f g <$> xs)
    AA.All pre xs -> AA.All (applyLabel f <$> pre) (mapBSLabel f g <$> xs)
    AA.Not x -> AA.Not $ mapBSLabel f g x

bsConstraint2questions :: BoolStructConstraint -> BoolStructConstraint
bsConstraint2questions = mapBSLabel GqPREPOST GqCONSTR

applyLabel :: (a -> b) -> AA.Label a -> AA.Label b
applyLabel f (AA.Pre a) = AA.Pre (f a)
applyLabel f (AA.PrePost a a') = AA.PrePost (f a) (f a')

-- could do this technically?
-- instance Functor AA.Label where
--     fmap = applyLabel

-----------------------------------------------------------------------------
-- Generic useful transformations

introduceSubj :: forall a . Tree a -> Tree a
introduceSubj (GEVERY x) = GAN x
introduceSubj (GPARTY x) = GAN x
introduceSubj x = composOp introduceSubj x

referSubj :: forall a . Tree a -> Tree a
referSubj (GEVERY x) = GTHE x
referSubj (GPARTY x) = GTHE x
referSubj (GAN x) = GTHE x
referSubj x = composOp referSubj x

pastTense :: forall a . Tree a -> Tree a
pastTense (GMkVPS _ pol vp) = GMkVPS GpastSimul pol vp
pastTense x = composOp pastTense x

-- TODO: generalise to other list constructors + lists longer than 2
squeezeRedundant :: forall a . Tree a -> Tree a
squeezeRedundant (GConjCond conj (GListCond
  [ GTemporalConstraint cond1 tc1 date1
  , GTemporalConstraint cond2 tc2 date2]))
  | cond1==cond2
  , date1==date2 = 
     GTemporalConstraint cond1 conjTC date1
  where 
    conjTC :: GTComparison
    conjTC = GConjTComparison conj (GListTComparison [tc1, tc2])
squeezeRedundant x = composOp squeezeRedundant x

-- handle e.g. this
-- ConjConstraint AND (BaseConstraint (ConjPreConstraint (NP_caused_by_PrePost Loss_or_Damage) OR (ConsConstraint (RPleafNP rodents) (ConsConstraint (RPleafNP insects) (BaseConstraint (RPleafNP vermin) (RPleafNP birds))))) (ConjConstraint OR (BaseConstraint (ConjConstraint AND (BaseConstraint (RPleafS Loss_or_Damage (MkVPS presSimul POS (UseComp (CompAdv (PrepNP to_Prep Contents))))) (RPleafS Loss_or_Damage (MkVPS presSimul POS (UseComp (CompAP (caused_by birds))))))) (ConjConstraint AND (ConsConstraint (RPleafS Loss_or_Damage (MkVPS presSimul POS (UseComp (CompAP (ensuing (MassNP loss)))))) (BaseConstraint (RPleafS Loss_or_Damage (MkVPS presSimul POS (UseComp (CompAP covered)))) (ConjConstraint OR (BaseConstraint (RPleafS any_other_exclusion (MkVPS presSimul POS apply)) (ConjPreConstraint (NP_caused_NP_to_VP_Prep_PrePost animal water escape from_Prep) OR (ConsConstraint (RPleafNP household_appliance) (BaseConstraint (RPleafNP swimming_pool) (RPleafNP plumbing_heating_or_AC))))))))))))