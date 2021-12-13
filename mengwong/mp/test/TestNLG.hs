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

