module TestNLG where

import PGF
import Test.Hspec
import LS.ToPredicate

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
        convertToPredicate becomingAware `shouldBe` Binary "becomeAwareOccur"
