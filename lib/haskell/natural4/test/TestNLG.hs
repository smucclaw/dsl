{-# LANGUAGE OverloadedStrings #-}

module TestNLG where

import PGF hiding (showExpr)
import Test.Hspec
import LS.NLP.ToPredicate
import LS.NLP.NLG
import LS.NLP.TreeTransformations (showExpr)
import LS.Types hiding (And)
import LS.Rule
import Data.Maybe
import qualified AnyAll as AA
import qualified Data.Text as Text
import Data.Text.Arbitrary
import LS.NLP.WordNet
import Test.QuickCheck
import Data.List.NonEmpty (NonEmpty ((:|)), toList, fromList)
import LS.NLP.UDExt
import AnyAll.BoolStructTree (mkLeafDT)
import LS.NLP.NLG (bsp2gfDT)

nlgTests :: NLGEnv -> Spec
nlgTests env = do
  describe "test bsr2gf" $ do
    it "Should return an adverbial" $ do
        treeAdv <- bsr2gf env testAdvBSR
        showExpr treeAdv `shouldBe` "ConjAdv or_Conj (BaseAdv today_Adv tomorrow_Adv)"

    it "Should return a complex adverbial" $ do
        treeAdvComplex <- bsr2gf env testAdvComplexBSR
        showExpr treeAdvComplex `shouldBe` "ConjAdv or_Conj (BaseAdv (AdvAdv once_Adv (PrepNP upon_Prep (DetCN aSg_Det (UseN time_N)))) (PrepNP over_Prep (DetCN theSg_Det (UseN (CompoundN monochrome_N rainbow_N)))))"

    it "Should return a det" $ do
        treeDet <- bsr2gf env testDetBSR
        showExpr treeDet `shouldBe` "ConjDet or_Conj (BaseDAP (DetDAP (DetQuant this_Quant NumSg)) (DetDAP (DetQuant that_Quant NumSg)))"

    it "Should return an adjective phrase" $ do
        treeAP <- bsr2gf env testAPBSR
        showExpr treeAP `shouldBe` "ConjAP or_Conj (BaseAP (AdvAP (PositA harmful_A) (PrepNP to_Prep (DetCN theSg_Det (AdjCN (PastPartAP (UseV affect_V)) (UseN individual_N))))) (PositA significant_A))"

    -- TODO fix this test
    xit "Should return a complex adjective phrase" $ do
        treeAPComplex <- bsr2gf env testAPComplexBSR
        showExpr treeAPComplex `shouldBe` "ConjAP or_Conj (ConsAP (AdvAP (AdvAP (PositA lethal_A) (PrepNP to_Prep (DetCN theSg_Det (AdjCN (PastPartAP (UseV afflict_V)) (UseN individual_N))))) (PositAdvAdj dangerous_A)) (ConsAP (AdvAP (PastPartAP (UseV disturb_V)) (PositAdvAdj grave_A)) (ConsAP (AdvAP (PositA happy_A) (SubjS that_Subj (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetCN theSg_Det (UseN sky_N)) (UseComp (CompAP (PositA blue_A))))))) (BaseAP (AdvAP (ConjAP and_Conj (BaseAP (PositA pulpy_A) (PositA tentacled_A))) (PrepNP from_Prep (DetCN (DetQuant (PossPron it_Pron) NumSg) (AdvCN (UseN head_N) (PrepNP to_Prep (DetCN (DetQuant (PossPron it_Pron) NumPl) (AdjCN (PositA rudimentary_A) (UseN wing_N)))))))) (PositA diseased_A)))))"

    it "Should return a common noun" $ do
        treeCN <- bsr2gf env testCNBSR
        showExpr treeCN `shouldBe` "ConjCN or_Conj (BaseCN (UseN occurrence_N) (UseN assessment_N))"

    xit "Should return a complex common noun" $ do
        treeCNComplex <- bsr2gf env testCNComplexBSR
        showExpr treeCNComplex `shouldBe` "ConjNP or_Conj (BaseNP (AdvNP (AdvNP (MassNP (UseN service_N)) (PrepNP from_Prep (DetCN theSg_Det (UseN provider_N)))) (PrepNP to_Prep (DetCN theSg_Det (UseN payer_N)))) (RelNP (MassNP (AdjCN (PositA great_A) (UseN harm_N))) (RS_that_NP_VP (UsePron she_Pron) (UseV suffer_V))))"

    it "Should return a simple noun phrase" $ do
        treeNP <- bsr2gf env testNPBSR
        showExpr treeNP `shouldBe` "ConjNP or_Conj (BaseNP (PredetNP all_Predet (DetCN aPl_Det (UseN occurrence_N))) (DetCN (DetQuant this_Quant NumSg) (UseN assessment_N)))"

    xit "Should return a adverbial for root_nmod_acl phrase" $ do
        testNPRootNmodAcl <- bsr2gf env testNPRootNmodAclBSR
        showExpr testNPRootNmodAcl `shouldBe` "ConjCN or_Conj (BaseCN (UseN none_N) (UseN policy_N))" -- incomplete TODO but suffice for now until UDApp develops more

    xit "Should return a adverbial for root_nmod_aclRelcl phrase" $ do
        testNPRootNmodAclRelcl <- bsr2gf env testNPRootNmodAclRelclBSR
        showExpr testNPRootNmodAclRelcl `shouldBe` "TODO"

    it "Should return a complex noun phrase" $ do
        treeNPComplex <- bsr2gf env testNPComplexBSR
        showExpr treeNPComplex `shouldBe` "ConjNP or_Conj (BaseNP (AdvNP (DetCN theSg_Det (UseN occurrence_N)) (PrepNP at_Prep (DetCN theSg_Det (UseN beach_N)))) (RelNP (DetCN (DetQuant this_Quant NumSg) (UseN assessment_N)) (UseRCl (TTAnt TPres ASimul) PPos (RelVP IdRP (UseV suck_V)))))"

    xit "Should return a det as det" $ do
        treeDetsDet <- bsr2gf env testDetsAsDet
        showExpr treeDetsDet `shouldBe` "UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron i_Pron) (ComplV like_V (DetCN (ConjDet or_Conj (BaseDAP (DetDAP (DetQuant this_Quant NumSg)) (DetDAP (DetQuant that_Quant NumSg)))) (UseN cat_N))))"

  describe "test bsp2gf" $ do
    xit "Should return an adverbial" $ do
        treeAdv <- bsp2gf env $ testBSP ["today", "tomorrow"]
        showExpr treeAdv `shouldBe` "ApposCN (UseN today_N) (MassNP (UseN tomorrow_N))"

    xit "Should return a complex adverbial" $ do
        treeAdvComplex <- bsp2gf env $ testBSP ["once upon a time", "over the monochrome rainbow"]
        showExpr treeAdvComplex `shouldBe` "AdvCN (UseN (CompoundN monochrome_N rainbow_N)) (AdvAdv once_Adv (PrepNP upon_Prep (DetCN aSg_Det (UseN time_N))))"

    xit "Should return a det" $ do
        treeDet <- bsp2gf env $ testBSP ["this", "that"]
        showExpr treeDet `shouldBe` "ApposNP (DetNP (DetQuant this_Quant NumSg)) (DetNP (DetQuant that_Quant NumSg))"

    it "Should return an adjective phrase" $ do
        treeAP <- bsp2gf env $ testBSP ["harmful to the affected individual", "significant"]
        showExpr treeAP `shouldBe` "AdvAP (AdvAP (PositA harmful_A) (PrepNP to_Prep (DetCN theSg_Det (AdjCN (PastPartAP (UseV affect_V)) (UseN individual_N))))) (PositAdvAdj significant_A)"

    xit "Should return a common noun" $ do
        treeCN <- bsp2gf env $ testBSP ["occurrence", "assessment"]
        showExpr treeCN `shouldBe` "ApposCN (UseN occurrence_N) (MassNP (UseN assessment_N))"

    xit "Should return a complex common noun" $ do
        treeCNComplex <- bsp2gf env $ testBSP ["service from the provider to the payer", "great harm that she suffered" ]
        showExpr treeCNComplex `shouldBe` "ApposCN (AdvCN (AdvCN (UseN service_N) (PrepNP from_Prep (DetCN theSg_Det (UseN provider_N)))) (PrepNP to_Prep (DetCN theSg_Det (UseN payer_N)))) (MassNP (AdjCN (PositA great_A) (UseN harm_N)))"

    xit "Should return a simple noun phrase" $ do
        treeNP <- bsp2gf env $ testBSP ["all occurrences", "this assessment"]
        showExpr treeNP `shouldBe` "ApposCN (UseN occurrence_N) (MassNP (UseN assessment_N))"

    xit "Should return a complex noun phrase" $ do
        treeNPComplex <- bsp2gf env $ testBSP ["the occurrence at the beach", "this assessment that sucks"]
        showExpr treeNPComplex `shouldBe` "ApposCN (AdvCN (UseN occurrence_N) (PrepNP at_Prep (DetCN theSg_Det (UseN beach_N)))) (MassNP (RelCN (UseN assessment_N) (UseRCl (TTAnt TPres ASimul) PPos (RelVP IdRP (UseV suck_V)))))"

  describe "test bsp2gfDT" $ do
    xit "Should return an adverbial" $ do
        treeAdv <- bsp2gfDT env $ testBSPDT ["today", "tomorrow"]
        showExpr treeAdv `shouldBe` "ApposCN (UseN today_N) (MassNP (UseN tomorrow_N))"

    xit "Should return a complex adverbial" $ do
        treeAdvComplex <- bsp2gfDT env $ testBSPDT ["once upon a time", "over the monochrome rainbow"]
        showExpr treeAdvComplex `shouldBe` "AdvCN (UseN (CompoundN monochrome_N rainbow_N)) (AdvAdv once_Adv (PrepNP upon_Prep (DetCN aSg_Det (UseN time_N))))"

    xit "Should return a det" $ do
        treeDet <- bsp2gfDT env $ testBSPDT ["this", "that"]
        showExpr treeDet `shouldBe` "ApposNP (DetNP (DetQuant this_Quant NumSg)) (DetNP (DetQuant that_Quant NumSg))"

    it "Should return an adjective phrase" $ do
        treeAP <- bsp2gfDT env $ testBSPDT ["harmful to the affected individual", "significant"]
        showExpr treeAP `shouldBe` "AdvAP (AdvAP (PositA harmful_A) (PrepNP to_Prep (DetCN theSg_Det (AdjCN (PastPartAP (UseV affect_V)) (UseN individual_N))))) (PositAdvAdj significant_A)"

    xit "Should return a common noun" $ do
        treeCN <- bsp2gfDT env $ testBSPDT ["occurrence", "assessment"]
        showExpr treeCN `shouldBe` "ApposCN (UseN occurrence_N) (MassNP (UseN assessment_N))"

    xit "Should return a complex common noun" $ do
        treeCNComplex <- bsp2gfDT env $ testBSPDT ["service from the provider to the payer", "great harm that she suffered" ]
        showExpr treeCNComplex `shouldBe` "ApposCN (AdvCN (AdvCN (UseN service_N) (PrepNP from_Prep (DetCN theSg_Det (UseN provider_N)))) (PrepNP to_Prep (DetCN theSg_Det (UseN payer_N)))) (MassNP (AdjCN (PositA great_A) (UseN harm_N)))"

    xit "Should return a simple noun phrase" $ do
        treeNP <- bsp2gfDT env $ testBSPDT ["all occurrences", "this assessment"]
        showExpr treeNP `shouldBe` "ApposCN (UseN occurrence_N) (MassNP (UseN assessment_N))"

    xit "Should return a complex noun phrase" $ do
        treeNPComplex <- bsp2gfDT env $ testBSPDT ["the occurrence at the beach", "this assessment that sucks"]
        showExpr treeNPComplex `shouldBe` "ApposCN (AdvCN (UseN occurrence_N) (PrepNP at_Prep (DetCN theSg_Det (UseN beach_N)))) (MassNP (RelCN (UseN assessment_N) (UseRCl (TTAnt TPres ASimul) PPos (RelVP IdRP (UseV suck_V)))))"

nlgTests2 :: Spec
nlgTests2 = do
  describe "Convert to predicate" $ do
    -- "organization"
    let Just org1 = readExpr "root_only (rootN_ (MassNP (UseN organization_N)))"
    it "Should convert single noun to a unary predicate" $ do
      convertToPredicate org1 `shouldBe` Unary "organization"

    -- "the organization"
    let Just org2 = readExpr "root_only (rootN_ (DetCN theSg_Det (UseN organization_N)))"
    it "Should convert Det+N to a unary predicate" $ do
      convertToPredicate org2 `shouldBe` Unary "organization"

    -- "public agency"
    let Just pub = readExpr "root_only (rootN_ (MassNP (AdjCN (PositA public_A) (UseN agency_N))))"
    it "Should convert Adj+N to a unary predicate" $ do
      convertToPredicate pub `shouldBe` Unary "publicAgency"

    -- "is not a public agency"
    let Just notPub = readExpr "root_only (rootV_ (TTAnt TPres ASimul) PNeg (UseComp (CompNP (DetCN aSg_Det (AdjCN (PositA public_A) (UseN agency_N))))))"
    it "Should convert 'is not a public agency' to a negated unary predicate" $ do
      convertToPredicate notPub `shouldBe` Not (Unary "publicAgency")

    -- "a data breach occurs"
    let Just occurs = readExpr "root_nsubj (rootV_ (TTAnt TPres ASimul) PPos (UseV occur_V)) (nsubj_ (DetCN aSg_Det (UseN (CompoundN data_N breach_N))))"
    it "Should convert root_mark_nsubj to a unary predicate" $ do
      convertToPredicate occurs `shouldBe` Unary "occur"

     -- "become aware"
    --   let Just aware = readExpr "root_xcomp (rootV_ (TTAnt TPres ASimul) PPos (UseV become_V)) (xcompA_ (PositA aware_A))"
    --   it "Should convert root_xcomp to a unary predicate" $ do
    --     convertToPredicate aware `shouldBe` Unary "becomeAware"

      -- "become aware that a data breach occurs"
    let Just becomingAware = readExpr "root_xcomp_ccomp (rootV_ (TTAnt TPres ASimul) PPos (UseV become_V)) (xcompA_ (PositA aware_A)) (ccomp_ (root_nsubj (rootV_ (TTAnt TPres ASimul) PPos (UseV occur_V)) (nsubj_ (DetCN aSg_Det (UseN (CompoundN data_N breach_N))))))"
    it "Should convert root_*_ccomp into a binary predicate" $ do
      convertToPredicate becomingAware `shouldBe` Binary "becomeAwareOccur" "dataBreach"

    it "Should convert a whole Rule into a Formula" $ do
      convertToFormula defaultRule `shouldBe` And [Unary "organization"]
      convertToFormula whoRule `shouldBe` And [Unary "organization", Not $ Unary "publicAgency"]
      convertToFormula ndbRule `shouldBe` And [Unary "organization", Not $ Unary "publicAgency",Binary "becomeAwareOccur" "dataBreach"]
      convertToFormula ndbRule_correctXcomp `shouldBe` And [Unary "organization", Not $ Unary "publicAgency",Binary "becomeAwareOccur" "dataBreach"]

    it "Should apply a Formula to an argument" $ do
      applyFormula (convertToFormula defaultRule) "org" `shouldBe` "\\forall org . organization(org)"
      applyFormula (convertToFormula whoRule) "org" `shouldBe` "\\forall org . organization(org) && !publicAgency(org)"
      applyFormula (convertToFormula ndbRule) "org" `shouldBe` "\\forall org . organization(org) && !publicAgency(org) && becomeAwareOccur(org, dataBreach)"
      applyFormula (convertToFormula ndbRule_correctXcomp) "org" `shouldBe` "\\forall org . organization(org) && !publicAgency(org) && becomeAwareOccur(org, dataBreach)"

    it "Should handle nested ccomps" $ do
      convertToPredicate (fromJust $ uponA nestedCcompRule) `shouldBe` Ternary "becomeAwareKnowOccur" "lawyer" "dataBreach"
      applyFormula (convertToFormula nestedCcompRule) "org" `shouldBe` "\\forall org . organization(org) && becomeAwareKnowOccur(org, lawyer, dataBreach)"

  describe "mkGerund" $ do
    it "behaves like gfmkGerund" $ do
      property prop_gerundcheck

testDetsAsDet :: BoolStructR
testDetsAsDet =
    AA.Any (Just (AA.PrePost "I like" "cat"))
            [ AA.Leaf ( RPMT [ "this" ] )
            , AA.Leaf ( RPMT [ "that" ] ) ]

testBSR :: [String] -> BoolStructR
testBSR strs = AA.Any Nothing [ AA.Leaf (RPMT [MTT $ Text.pack str]) | str <- strs ]

testBSP :: [String] -> BoolStructP
testBSP strs =  AA.Leaf $ ( qq, Nothing) :| []
  where qq = fromList $ Text.pack <$> strs

testBSPDT :: [String] -> BoolStructDTP
testBSPDT strs =  mkLeafDT $ ( qq, Nothing) :| []
  where qq = fromList $ Text.pack <$> strs

testDetBSR :: BoolStructR
testDetBSR = testBSR ["this", "that"]

testAdvBSR :: BoolStructR
testAdvBSR = testBSR ["today", "tomorrow"]

testAdvComplexBSR :: BoolStructR
testAdvComplexBSR = testBSR ["once upon a time",
 -- "unless we go where it is warm",
 "over the monochrome rainbow"]
-- "beyond the edge of forever"]

testAPBSR :: BoolStructR
testAPBSR = testBSR ["harmful to the affected individual", "significant"]

testAPComplexBSR :: BoolStructR
testAPComplexBSR = testBSR ["dangerously lethal to the afflicted individual", "gravely disturbed", "happy that the sky is blue", "pulpy and tentacled from its head to its rudimentary wings", "hopelessly diseased while spitting angrily"]

testCNBSR :: BoolStructR
testCNBSR = testBSR ["occurrence", "assessment"]

testCNComplexBSR :: BoolStructR
testCNComplexBSR = testBSR ["service from the provider to the payer", "great harm that she suffered" ]

testNPBSR :: BoolStructR
testNPBSR = testBSR ["all occurrences", "this assessment"]

testNPComplexBSR :: BoolStructR
testNPComplexBSR = testBSR ["the occurrence at the beach", "this assessment that sucks"]

testNPRootNmodAclBSR :: BoolStructR
testNPRootNmodAclBSR  = testBSR [
  "none of the submitted proposals focusing on this topic"
  , "policy (called the applicable policy) of the company"
  ]

testNPRootNmodAclRelclBSR :: BoolStructR
testNPRootNmodAclRelclBSR = testBSR [
  --"any manner in these circumstances that is reasonable", "any motive of the suspect which was ruled out", "a kind word from her friend she relies on"
  "every suggestion on this matter that is plausible"]

defaultRule :: AnnotatedRule
defaultRule = RegulativeA {
    subjA = fromJust $ readExpr "root_only (rootN_ (MassNP (UseN organization_N)))",
    keywordA = mkCId "Every",
    whoA = Nothing,
    condA = Nothing,
    deonticA = mkCId "dummy",
    actionA = fromJust $ readExpr "root_only rootV_ (TTAnt TPres ASimul) PPos (UseV sing_V))",
    temporalA = Nothing,
    havingA = Nothing,
    uponA = Nothing,
    givenA = Nothing
    }

whoRule :: AnnotatedRule
whoRule = defaultRule {
    whoA = readExpr "root_only (rootV_ (TTAnt TPres ASimul) PNeg (UseComp (CompNP (DetCN aSg_Det (AdjCN (PositA public_A) (UseN agency_N))))))"
    }

ndbRule :: AnnotatedRule
ndbRule = whoRule {
    uponA = readExpr "root_xcomp_ccomp (rootV_ (TTAnt TPres ASimul) PPos (UseV become_V)) (xcompA_ (PositA aware_A)) (ccomp_ (root_nsubj (rootV_ (TTAnt TPres ASimul) PPos (UseV occur_V)) (nsubj_ (DetCN aSg_Det (UseN (CompoundN data_N breach_N))))))"
    }

ndbRule_correctXcomp :: AnnotatedRule
ndbRule_correctXcomp = whoRule {
    uponA = readExpr "root_xcomp (rootV_ (TTAnt TPres ASimul) PPos (UseV become_V)) (xcompA_ccomp_ (PositA aware_A) (ccomp_ (root_nsubj (rootV_ (TTAnt TPres ASimul) PPos (UseV occur_V))  (nsubj_ (DetCN aSg_Det (UseN (CompoundN data_N breach_N)))))))"
}

nestedCcompRule :: AnnotatedRule
nestedCcompRule = defaultRule {
    uponA = readExpr "root_xcomp_ccomp (rootV_ (TTAnt TPres ASimul) PPos (UseV become_V)) (xcompA_ (PositA aware_A)) (ccomp_ (root_nsubj_ccomp (rootV_ (TTAnt TPres ASimul) PPos (UseV know_V)) (nsubj_ (MassNP (UseN lawyer_N))) (ccomp_ (root_nsubj (rootV_ (TTAnt TPres ASimul) PPos (UseV occur_V)) (nsubj_ (DetCN aSg_Det (UseN (CompoundN data_N breach_N))))))))"
    }

everyOrgNotPublicAg ::  Rule
everyOrgNotPublicAg = defaultReg
  { subj = mkLeafPT "organization"
  , rkeyword = REvery
  , who = Just $ multiterm2bsr' ["is not a public agency"]
  , cond = Nothing
  , deontic = DMust
  , action = mkLeafPT "sings"
  }

prop_gerundcheck :: Text.Text -> Bool
prop_gerundcheck string = let str = Text.unpack string in
  gfmkGerund str == mkGerund str
