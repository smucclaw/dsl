{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module LS.NLP.NL4Transformations where

import AnyAll (BoolStruct (..), Label (..))
import AnyAll qualified as AA
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Debug.Trace (trace)
import LS.NLP.NL4
  ( GAP,
    GCond,
    GCond_,
    GConj,
    GConstraint,
    GConstraint_,
    GNP,
    GPol,
    GPrePost,
    GString,
    GTComparison,
    GTemp,
    GText,
    GV2,
    GVP,
    GWho,
    GWho_,
    Gf,
    Tree
      ( GAPWho,
        GAdjCN,
        GAdvVP,
        GAdvWho,
        GComplV2,
        GConjCond,
        GConjConstraint,
        GConjPreCond,
        GConjPreConstraint,
        GConjPrePostCond,
        GConjPrePostConstraint,
        GConjPrePostWho,
        GConjPreWho,
        GConjTComparison,
        GConjVPS,
        GConjWho,
        GDetCN,
        GEVERY,
        GListCond,
        GListConstraint,
        GListTComparison,
        GListVPS,
        GListWho,
        GMassNP,
        GMkVPS,
        GNEG,
        GNP_PrePost,
        GPOS,
        GRPConstraint,
        GRPleafS,
        GUseN,
        GV2_PrePost,
        GWHO,
        GaSg,
        GpastSimul,
        GqPREPOST,
        GqWHO,
        GrecoverUnparsedAdv,
        GrecoverUnparsedPrePost,
        GtheSg,
        LexConj
      ),
    composOp,
  )
import PGF (Language, mkCId)

flipPolarity :: Tree a -> Tree a
flipPolarity GPOS = GNEG
flipPolarity GNEG = GPOS
flipPolarity x = composOp flipPolarity x


pushPrePostIntoMain :: BoolStructGText -> BoolStructGText
pushPrePostIntoMain = \case
  Leaf x -> Leaf x
  All l xs -> tryTransformWhole (All l (pushPrePostIntoMain <$> xs))
  Any l xs -> tryTransformWhole (Any l (pushPrePostIntoMain <$> xs))
  Not x -> Not (pushPrePostIntoMain x)

  where
    hackStrVP :: GString -> GVP -> GVP
    hackStrVP in_part vp = GAdvVP vp $ GrecoverUnparsedAdv in_part

    transformWho :: GTemp -> GPol -> GV2 -> GNP -> GText -> GText
    transformWho t p consume beverage (GqWHO person (GAPWho alcoholic)) =
      GqWHO (referNP person) (GWHO t p (GComplV2 consume (introduceNP (insertAP alcoholic beverage))))
    transformWho t p consume beverage (GqWHO person (GAdvWho in_part)) =
      GqWHO (referNP person) (GWHO t p (GAdvVP (GComplV2 consume (referNP beverage)) in_part))

    tryTransformWhole :: BoolStructGText -> BoolStructGText
    tryTransformWhole = \case
      All pp
          ( Any
              ( Just ( PrePost (GqPREPOST ( GV2_PrePost t p consume ) )
                               (GqPREPOST ( GNP_PrePost beverage))))
              alcoholic_nonalcoholic
          :  Any
            ( Just ( Pre (GqPREPOST ( GrecoverUnparsedPrePost whether ))))
            inpart_inwhole
          : restOfInnerRules ) ->
        All pp
            ( Any
                Nothing (
                   (transformWho t p consume beverage `mapBS`) <$> alcoholic_nonalcoholic)
            : Any
                Nothing (
                  (transformWho t p consume beverage `mapBS`) <$> inpart_inwhole)
            : restOfInnerRules )

      Any pp
          ( All
              ( Just ( PrePost (GqPREPOST ( GV2_PrePost t p consume ) )
                               (GqPREPOST ( GNP_PrePost beverage))))
              alcoholic_nonalcoholic
          :  All
            ( Just ( Pre (GqPREPOST ( GrecoverUnparsedPrePost whether ))))
            inpart_inwhole
          : restOfInnerRules ) ->
        Any pp
            ( All
                Nothing (
                   (transformWho t p consume beverage `mapBS`) <$> alcoholic_nonalcoholic)
            : All
                Nothing (
                  (transformWho t p consume beverage `mapBS`) <$> inpart_inwhole)
            : restOfInnerRules )
      bs -> bs


type BoolStructGF a = AA.BoolStruct (Maybe (AA.Label GPrePost)) (Tree a)

type BoolStructGText = AA.BoolStruct (Maybe (AA.Label GText)) GText

type BoolStructWho = BoolStructGF GWho_  -- have to use underscore versions because of flipPolarity
type BoolStructCond = BoolStructGF GCond_
type BoolStructConstraint = BoolStructGF GConstraint_

bsNeg2textNeg :: (Gf (Tree a)) => AA.BoolStruct b (Tree a) -> AA.BoolStruct b (Tree a)
bsNeg2textNeg = \case
  AA.Leaf x -> AA.Leaf x
  AA.All l xs -> AA.All l $ bsNeg2textNeg <$> xs
  AA.Any l xs -> AA.Any l $ bsNeg2textNeg <$> xs
  AA.Not (AA.Leaf x)    -> AA.Leaf $ flipPolarity x
  AA.Not (AA.All l xs)  -> AA.All l $ bsNeg2textNeg <$> xs
  AA.Not (AA.Any l xs)  -> AA.Any l $ bsNeg2textNeg <$> xs
  AA.Not (AA.Not x)     -> bsNeg2textNeg x

-- inverse:
-- textNeg2bsNeg :: BoolStructWho -> BoolStructWho

-----------------------------------------------------------------------------
-- This is rather hard to read, but the alternative is to duplicate bs2gf for every single GF cate(LexConj "OR")y

type ConjFun list single = GConj -> Tree list -> Tree single
type ConjPreFun list single = GPrePost -> GConj -> Tree list -> Tree single
type ConjPrePostFun list single = GPrePost -> GPrePost -> GConj -> Tree list -> Tree single
type ListFun single list = [Tree single] -> Tree list

bs2gf :: (Gf (Tree s)) => ConjFun l s -> ConjPreFun l s -> ConjPrePostFun l s -> ListFun s l -> BoolStructGF s -> Tree s
bs2gf conj conjPre conjPrePost mkList bs = case bs' of
    AA.Leaf x -> x
    AA.Any Nothing xs -> mergeConj $ conj (LexConj "OR") $ mkList $ f <$> xs
    AA.All Nothing xs -> mergeConj $ conj (LexConj "AND") $ mkList $ f <$> xs
    AA.Any (Just (AA.Pre pre)) xs -> conjPre pre (LexConj "OR") $ mkList $ f <$> xs
    AA.All (Just (AA.Pre pre)) xs -> conjPre pre (LexConj "AND") $ mkList $ f <$> xs
    AA.Any (Just (AA.PrePost pre post)) xs -> conjPrePost pre post (LexConj "OR") $ mkList $ f <$> xs
    AA.All (Just (AA.PrePost pre post)) xs -> conjPrePost pre post (LexConj "AND") $ mkList $ f <$> xs
    AA.Not unexpectedBS -> trace unexpectedNegationMsg $ bs2gf conj conjPre conjPrePost mkList unexpectedBS
--    AA.Not _ -> error unexpectedNegationMsg
  where
    f = bs2gf conj conjPre conjPrePost mkList
    bs' = bsNeg2textNeg bs
    unexpectedNegationMsg = [i|bs2gf: not expecting NOT in #{bs'}|]

bsWho2gfWho :: BoolStructWho -> GWho
bsWho2gfWho = bs2gf GConjWho GConjPreWho GConjPrePostWho GListWho

bsCond2gfCond :: BoolStructCond -> GCond
bsCond2gfCond = bs2gf GConjCond GConjPreCond GConjPrePostCond GListCond

bsConstraint2gfConstraint :: BoolStructConstraint -> GConstraint
bsConstraint2gfConstraint = bs2gf GConjConstraint GConjPreConstraint GConjPrePostConstraint GListConstraint

-----------------------------------------------------------------------------

mapBSLabel :: (a -> b) -> (c -> d) -> AA.BoolStruct (Maybe (AA.Label a)) c ->  AA.BoolStruct (Maybe (AA.Label b)) d
mapBSLabel f g = \case
  AA.Leaf x -> AA.Leaf $ g x
  AA.Any pre xs -> AA.Any (applyLabel f <$> pre) (mapBSLabel f g <$> xs)
  AA.All pre xs -> AA.All (applyLabel f <$> pre) (mapBSLabel f g <$> xs)
  AA.Not x -> AA.Not $ mapBSLabel f g x

applyLabel :: (a -> b) -> AA.Label a -> AA.Label b
applyLabel f (AA.Pre a) = AA.Pre $ f a
applyLabel f (AA.PrePost a a') = AA.PrePost (f a) (f a')

mapBS :: (a -> b) -> AA.BoolStruct c a ->  AA.BoolStruct c b
mapBS f = \case
  AA.Leaf x -> AA.Leaf $ f x
  AA.Any lbl xs -> AA.Any lbl $ mapBS f <$> xs
  AA.All lbl xs -> AA.All lbl $ mapBS f <$> xs
  AA.Not x -> AA.Not $ mapBS f x
-----------------------------------------------------------------------------
-- Generic useful transformations
-- for NP

introduceNP :: Tree a -> Tree a
introduceNP (GEVERY x) = GDetCN GaSg x
introduceNP (GMassNP x) = GDetCN GaSg x
introduceNP (GDetCN _ x) = GDetCN GaSg x
introduceNP x = composOp introduceNP x

referNP :: Tree a -> Tree a
referNP (GEVERY x) = GDetCN GtheSg x
referNP (GMassNP x) = GDetCN GtheSg x
referNP (GDetCN GaSg x) = GDetCN GtheSg x
--referNP (GDetCN GaPl x) = GDetCN GthePl x
referNP x = composOp referNP x

insertAP :: GAP -> Tree a -> Tree a
insertAP ap = go
  where
    go :: Tree b -> Tree b
    go (GMassNP cn) = GMassNP $ GAdjCN ap cn
    go cn@(GUseN _) = GAdjCN ap cn
    go x = composOp go x

pastTense :: Tree a -> Tree a
pastTense (GMkVPS _ pol vp) = GMkVPS GpastSimul pol vp
pastTense x = composOp pastTense x

-----------------------------------------------------------------------------
-- db happens ON x or db happens AFTER x ==> db happens ON or AFTER x

mergeConj :: Tree a -> Tree a
mergeConj og@(GConjCond conj (GListCond cs)) = fromMaybe og $ squeezeTrees conj cs
mergeConj og@(GConjConstraint conj (GListConstraint cs)) = fromMaybe og $ squeezeTrees conj cs
mergeConj x = composOp mergeConj x


-- The function that does all the repetitive work
-- TODO: check if viewpatterns help?
squeezeTrees :: GConj -> [Tree a] -> Maybe (Tree a)
squeezeTrees conj [
    GRPConstraint cond tc1 date
  , GRPConstraint ((== cond) -> True) tc2 ((== date) -> True)]
  = pure $ GRPConstraint cond conjTC date
  where
    conjTC :: GTComparison
    conjTC = GConjTComparison conj $ GListTComparison [tc1, tc2]

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
    GRPleafS subj vps1
  , GRPleafS ((== subj) -> True) vps2]
  = pure $ GRPleafS subj (GConjVPS conj (GListVPS [vps1, vps2]))

squeezeTrees _ _ = Nothing

isChinese :: Language -> Bool
isChinese = (== mkCId "NL4Chi")

isMalay :: Language -> Bool
isMalay = (== mkCId "NL4May")

aggregateBoolStruct :: Language -> BoolStructGF a ->  BoolStructGF a
aggregateBoolStruct l bs
  | False = -- isChinese l
    bs
  | otherwise = case bs of
    AA.Any _ xs -> maybe bs AA.Leaf $ squeezeTrees (LexConj "OR") $ foldMap toList xs
    AA.All _ xs -> maybe bs AA.Leaf $ squeezeTrees (LexConj "AND") $ foldMap toList xs
    _ -> bs
