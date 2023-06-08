{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE GADTs, FlexibleInstances, FlexibleContexts, UndecidableInstances, KindSignatures, RankNTypes #-}

module LS.NLP.NL4Transformations where
import LS.NLP.NL4
import PGF (Language, mkCId)
import qualified AnyAll as AA
import Data.Maybe (fromMaybe)
import Data.Foldable (toList)
import Debug.Trace (trace)

flipPolarity :: forall a . Tree a -> Tree a
flipPolarity GPOS = GNEG
flipPolarity GNEG = GPOS
flipPolarity x = composOp flipPolarity x

type BoolStructGF a = AA.BoolStruct (Maybe (AA.Label GPrePost)) (Tree a)

type BoolStructGText = AA.BoolStruct (Maybe (AA.Label GText)) GText

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
    AA.Any Nothing xs -> mergeConj $ conj GOR $ mkList $ f <$> xs
    AA.All Nothing xs -> mergeConj $ conj GAND $ mkList $ f <$> xs
    AA.Any (Just (AA.Pre pre)) xs -> conjPre pre GOR $ mkList $ f <$> xs
    AA.All (Just (AA.Pre pre)) xs -> conjPre pre GAND $ mkList $ f <$> xs
    AA.Any (Just (AA.PrePost pre post)) xs -> conjPrePost pre post GOR $ mkList $ f <$> xs
    AA.All (Just (AA.PrePost pre post)) xs -> conjPrePost pre post GAND $ mkList $ f <$> xs
    AA.Not unexpectedBS -> trace unexpectedNegationMsg $ bs2gf conj conjPre conjPrePost mkList unexpectedBS
--    AA.Not _ -> error unexpectedNegationMsg
  where
    f = bs2gf conj conjPre conjPrePost mkList
    bs' = bsNeg2textNeg bs
    unexpectedNegationMsg = "bs2gf: not expecting NOT in " <> show bs'

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

applyLabel :: (a -> b) -> AA.Label a -> AA.Label b
applyLabel f (AA.Pre a) = AA.Pre (f a)
applyLabel f (AA.PrePost a a') = AA.PrePost (f a) (f a')

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

-----------------------------------------------------------------------------
-- db happens ON x or db happens AFTER x ==> db happens ON or AFTER x

mergeConj :: forall a . Tree a -> Tree a
mergeConj og@(GConjCond conj (GListCond cs)) = fromMaybe og $ squeezeTrees conj cs
mergeConj og@(GConjConstraint conj (GListConstraint cs)) = fromMaybe og $ squeezeTrees conj cs
mergeConj x = composOp mergeConj x


-- The function that does all the repetitive work
-- TODO: check if viewpatterns help?
squeezeTrees :: forall a . GConj -> [Tree a] -> Maybe (Tree a)
squeezeTrees conj [
    GRPConstraint cond1 tc1 date1
  , GRPConstraint cond2 tc2 date2]
  | cond1==cond2
  , date1==date2 = pure $ GRPConstraint cond1 conjTC date1
  where
    conjTC :: GTComparison
    conjTC = GConjTComparison conj (GListTComparison [tc1, tc2])

-- TODO: how to make this work without lots of copy and paste?
-- squeezeTrees conj [GCompNP np1, GCompNP np2] = pure $ GCompNP (GConjNP conj (GListNP [np1, np2]))
-- squeezeTrees conj [GCompAP ap1, GCompAP ap2] = pure $ GCompAP (GConjAP conj (GListAP [ap1, ap2]))
-- squeezeTrees conj [GCompAdv adv1, GCompAdv adv2] = pure $ GCompAdv (GConjAdv conj (GListAdv [adv1, adv2]))
-- squeezeTrees conj [
--     GRPleafS subj1 (GMkVPS temp1 pol1 (GUseComp comp1))
--   , GRPleafS subj2 (GMkVPS temp2 pol2 (GUseComp comp2))]
--   | subj1==subj2, temp1==temp2, pol1==pol2 = do
--     newComp <- squeezeTrees conj [comp1, comp2]
--     pure $ GRPleafS subj1 (GMkVPS temp1 pol1 (GUseComp newComp))

squeezeTrees conj [
    GRPleafS subj1 vps1
  , GRPleafS subj2 vps2]
  | subj1==subj2 = pure $ GRPleafS subj1 (GConjVPS conj (GListVPS [vps1, vps2]))

squeezeTrees _ _ = Nothing

isChinese :: Language -> Bool
isChinese l = l == mkCId "NL4Chi"

isMalay :: Language -> Bool
isMalay l = l == mkCId "NL4May"

aggregateBoolStruct :: forall a . Language -> BoolStructGF a ->  BoolStructGF a
aggregateBoolStruct l bs =
  if False -- isChinese l
    then bs
    else
      (case bs of
        AA.Any _ xs -> maybe bs AA.Leaf $ squeezeTrees GOR $ concatMap toList xs
        AA.All _ xs -> maybe bs AA.Leaf $ squeezeTrees GAND $ concatMap toList xs
        _ -> bs)