{-# LANGUAGE OverloadedStrings #-}

module TestNLG where

import PGF hiding (showExpr)
import Test.Hspec
import LS.NLP.ToPredicate
import LS.NLP.NLG
import LS.Types hiding (And)
import Data.Maybe
import qualified AnyAll as AA
import Data.List.NonEmpty (NonEmpty((:|)))
import UDAnnotations ( UDEnv(..), getEnv )
import System.IO.Unsafe
import qualified Data.Text.Lazy as Text

nlgTests :: Spec
nlgTests = do
   let env = unsafePerformIO myUDEnv
   describe "test bsr2gf" $ do
    let treeAdv = unsafePerformIO (bsr2gf env testAdvBSR)
    it "Should return an adverbial" $ do
        showExpr treeAdv `shouldBe` "ConjAdv or_Conj (BaseAdv today_Adv tomorrow_Adv)"

    let treeDet = unsafePerformIO (bsr2gf env testDetBSR)
    it "Should return a det" $ do
        showExpr treeDet `shouldBe` "ConjNP or_Conj (BaseNP (DetNP (DetQuant this_Quant NumSg)) (DetNP (DetQuant that_Quant NumSg)))"

    let treeAP = unsafePerformIO (bsr2gf env testAPBSR)
    it "Should return an adjective phrase" $ do
        showExpr treeAP `shouldBe` "ConjAP or_Conj (BaseAP (AdvAP (PositA harmful_A) (PrepNP to_Prep (DetCN (DetQuant DefArt NumSg) (AdjCN (PastPartAP (UseV affect_V)) (UseN individual_N))))) (PositA significant_A))"

    let treeAPComplex = unsafePerformIO (bsr2gf env testAPComplexBSR)
    it "Should return a complex adjective phrase" $ do
        showExpr treeAPComplex `shouldBe` "ConjAP or_Conj (ConsAP (AdvAP (AdvAP (PositA lethal_A) (PrepNP to_Prep (DetCN (DetQuant DefArt NumSg) (AdjCN (PastPartAP (UseV afflict_V)) (UseN individual_N))))) (PositAdvAdj dangerous_A)) (ConsAP (AdvAP (PastPartAP (UseV disturb_V)) (PositAdvAdj grave_A)) (BaseAP (AdvAP (PositA happy_A) (SubjS that_Subj (PredVPS (DetCN (DetQuant DefArt NumSg) (UseN sky_N)) (UseComp (CompAP (PositA blue_A)))))) (AdvAP (ConjAP and_Conj (BaseAP (PositA pulpy_A) (PositA tentacled_A))) (PrepNP from_Prep (DetCN (DetQuant DefArt NumSg) (AdvCN (UseN head_N) (PrepNP to_Prep (DetCN (DetQuant DefArt NumPl) (AdjCN (PositA rudimentary_A) (UseN wing_N)))))))))))"

    let treeCN = unsafePerformIO (bsr2gf env testCNBSR)
    it "Should return a common noun" $ do
        showExpr treeCN `shouldBe` "ConjCN or_Conj (BaseCN (UseN occurrence_N) (UseN assessment_N))"

    let treeCNComplex = unsafePerformIO (bsr2gf env testCNComplexBSR)
    it "Should return a complex common noun" $ do
        showExpr treeCNComplex `shouldBe` "AdvNP (AdvNP (MassNP (UseN service_N)) (PrepNP from_Prep (DetCN (DetQuant DefArt NumSg) (UseN provider_N)))) (PrepNP to_Prep (DetCN (DetQuant DefArt NumSg) (UseN payer_N)))"


    let treeNP = unsafePerformIO (bsr2gf env testNPBSR)
    it "Should return a noun phrase" $ do
        showExpr treeNP `shouldBe` "ConjNP or_Conj (BaseNP (PredetNP all_Predet (DetCN aPl_Det (UseN occurrence_N))) (DetCN (DetQuant this_Quant NumSg) (UseN assessment_N)))"

    let treeNPComplex = unsafePerformIO (bsr2gf env testNPComplexBSR)
    it "Should return a noun phrase" $ do
        showExpr treeNPComplex `shouldBe` "ConjNP or_Conj (BaseNP (AdvNP (DetCN (DetQuant DefArt NumSg) (UseN occurrence_N)) (PrepNP at_Prep (DetCN (DetQuant DefArt NumSg) (UseN beach_N)))) (RelNP (DetCN (DetQuant this_Quant NumSg) (UseN assessment_N)) (UseRCl (TTAnt TPres ASimul) PPos (RelVP IdRP (UseV suck_V)))))"

    let treeDetsDet = unsafePerformIO (bsr2gf env testDetsAsDet)
    it "Should return a det as det" $ do
        showExpr treeDetsDet `shouldBe` "foo"

    describe "Convert to predicate" $ do
      -- "organization"
      let Just org = readExpr "root_only (rootN_ (MassNP (UseN organization_N)))"
      it "Should convert single noun to a unary predicate" $ do
        convertToPredicate org `shouldBe` Unary "organization"

      -- "the organization"
      let Just org = readExpr "root_only (rootN_ (DetCN theSg_Det (UseN organization_N)))"
      it "Should convert Det+N to a unary predicate" $ do
        convertToPredicate org `shouldBe` Unary "organization"

      -- "public agency"
      let Just pub = readExpr "root_only (rootN_ (MassNP (AdjCN (PositA public_A) (UseN agency_N))))"
      it "Should convert Adj+N to a unary predicate" $ do
        convertToPredicate pub `shouldBe` Unary "publicAgency"

      -- "is not a public agency"
      let Just notPub = readExpr "root_cop_advmod (rootN_ (DetCN (DetQuant IndefArt NumSg) (AdjCN (PositA public_A) (UseN agency_N)))) be_cop not_advmod"
      it "Should convert root_cop_advmod to a negated unary predicate" $ do
        convertToPredicate notPub `shouldBe` Not (Unary "publicAgency")

      -- "a data breach occurs"
      let Just occurs = readExpr "root_mark_nsubj (rootV_ (UseV occur_V)) (mark_ that_Subj) (nsubj_ (DetCN (DetQuant IndefArt NumSg) (UseN (CompoundN data_N breach_N))))"
      it "Should convert root_mark_nsubj to a unary predicate" $ do
        convertToPredicate occurs `shouldBe` Unary "occur"

     -- "become aware"
    --   let Just aware = readExpr "root_xcomp (rootV_ (UseV become_V)) (xcompA_ (PositA aware_A))"
    --   it "Should convert root_xcomp to a unary predicate" $ do
    --     convertToPredicate aware `shouldBe` Unary "becomeAware"

      -- "become aware that a data breach occurs"
      let Just becomingAware = readExpr "root_xcomp_ccomp (rootV_ (UseV become_V)) (xcompA_ (PositA aware_A)) (ccomp_ (root_mark_nsubj (rootV_ (UseV occur_V)) (mark_ that_Subj) (nsubj_ (DetCN (DetQuant IndefArt NumSg) (UseN (CompoundN data_N breach_N))))))"
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

testDetsAsDet :: BoolStructR
testDetsAsDet =
    AA.Any (Just (AA.PrePost "I like" "cat"))
            [ AA.Leaf ( RPMT [ "this" ] )
            , AA.Leaf ( RPMT [ "that" ] ) ]

testBSR :: [String] -> BoolStructR
testBSR strs = AA.Any Nothing [ AA.Leaf (RPMT [Text.pack str]) | str <- strs ]

testDetBSR :: BoolStructR
testDetBSR = testBSR ["this", "that"]

testAdvBSR :: BoolStructR
testAdvBSR = testBSR ["today", "tomorrow"]

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

defaultRule :: AnnotatedRule
defaultRule = RegulativeA {
    subjA = fromJust $ readExpr "root_only (rootN_ (MassNP (UseN organization_N)))",
    keywordA = mkCId "Every",
    whoA = Nothing,
    condA = Nothing,
    deonticA = mkCId "dummy",
    actionA = fromJust $ readExpr "root_only (rootV_ (UseV sing_V))",
    temporalA = Nothing,
    havingA = Nothing,
    uponA = Nothing,
    givenA = Nothing
    }

whoRule :: AnnotatedRule
whoRule = defaultRule {
    whoA = readExpr "root_cop_advmod (rootN_ (DetCN (DetQuant IndefArt NumSg) (AdjCN (PositA public_A) (UseN agency_N)))) be_cop not_advmod"
    }

ndbRule :: AnnotatedRule
ndbRule = whoRule {
    uponA = readExpr "root_xcomp_ccomp (rootV_ (UseV become_V)) (xcompA_ (PositA aware_A)) (ccomp_ (root_mark_nsubj (rootV_ (UseV occur_V)) (mark_ that_Subj) (nsubj_ (DetCN (DetQuant IndefArt NumSg) (UseN (CompoundN data_N breach_N))))))"
    }

ndbRule_correctXcomp :: AnnotatedRule
ndbRule_correctXcomp = whoRule {
    uponA = readExpr "root_xcomp (rootV_ (UseV become_V)) (xcompA_ccomp_ (PositA aware_A) (ccomp_ (root_mark_nsubj (rootV_ (UseV occur_V)) (mark_ that_Subj) (nsubj_ (DetCN (DetQuant IndefArt NumSg) (UseN (CompoundN data_N breach_N)))))))"
}

nestedCcompRule :: AnnotatedRule
nestedCcompRule = defaultRule {
    uponA = readExpr "root_xcomp_ccomp (rootV_ (UseV become_V)) (xcompA_ (PositA aware_A)) (ccomp_ (root_nsubj_ccomp (rootV_ (UseV know_V)) (nsubj_ (MassNP (UseN lawyer_N))) (ccomp_ (root_nsubj (rootV_ (UseV occur_V)) (nsubj_ (DetCN (DetQuant IndefArt NumSg) (UseN (CompoundN data_N breach_N))))))))"
    }

everyOrgNotPublicAg ::  Rule
everyOrgNotPublicAg = Regulative
  { subj = mkLeaf "organization"
  , rkeyword = REvery
  , who = Just $ multiterm2bsr' ["is not a public agency"]
  , cond = Nothing
  , deontic = DMust
  , action = mkLeaf "sings"
  , temporal = Nothing
  , hence = Nothing
  , lest = Nothing
  , rlabel = Nothing
  , lsource = Nothing
  , srcref = Nothing
  , upon = Nothing
  , given = Nothing
  , having = Nothing
  }
