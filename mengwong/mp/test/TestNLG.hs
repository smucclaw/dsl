{-# LANGUAGE OverloadedStrings #-}

module TestNLG where

import PGF
import Test.Hspec
import LS.ToPredicate
import LS.NLG
import LS.Types hiding (And)
import Data.Maybe

nlgTests :: Spec
nlgTests = do
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

      it "Should apply a Formula to an argument" $ do
        applyFormula (convertToFormula defaultRule) "org" `shouldBe` "\\forall org . organization(org)"
        applyFormula (convertToFormula whoRule) "org" `shouldBe` "\\forall org . organization(org) && !publicAgency(org)"
        applyFormula (convertToFormula ndbRule) "org" `shouldBe` "\\forall org . organization(org) && !publicAgency(org) && becomeAwareOccur(org, dataBreach)"

      it "Should handle nested ccomps" $ do
        convertToPredicate (fromJust $ uponA nestedCcompRule) `shouldBe` Ternary "becomeAwareKnowOccur" "lawyer" "dataBreach"
        applyFormula (convertToFormula nestedCcompRule) "org" `shouldBe` "\\forall org . organization(org) && becomeAwareKnowOccur(org, lawyer, dataBreach)"


defaultRule :: AnnotatedRule
defaultRule = RegulativeA {
    subjA = fromJust $ readExpr "root_only (rootN_ (MassNP (UseN organization_N)))",
    whoA = Nothing,
    condA = Nothing,
    deonticA = mkCId "dummy",
    actionA = fromJust $ readExpr "root_only (rootV_ (UseV sing_V))",
    temporalA = Nothing,
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

nestedCcompRule :: AnnotatedRule
nestedCcompRule = defaultRule {
    uponA = readExpr "root_xcomp_ccomp (rootV_ (UseV become_V)) (xcompA_ (PositA aware_A)) (ccomp_ (root_nsubj_ccomp (rootV_ (UseV know_V)) (nsubj_ (MassNP (UseN lawyer_N))) (ccomp_ (root_nsubj (rootV_ (UseV occur_V)) (nsubj_ (DetCN (DetQuant IndefArt NumSg) (UseN (CompoundN data_N breach_N))))))))"
    }

everyOrgNotPublicAg ::  Rule
everyOrgNotPublicAg = Regulative
  { subj = mkLeaf "organization"
  , keyword = Every
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
