{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LS.XPile.JSONSchemaSpec where

import Control.Applicative (liftA2)
import Data.Text qualified as T
import LS.Rule (Rule (..))
import LS.Types
  ( EntityType,
    MTExpr (MTT),
    ParamType (..),
    TypeSig (..),
  )
import LS.XPile.ExportTypes
  ( FieldType (..),
    rulesToJsonSchema,
    showTypesJson,
    typeDeclSuperToFieldType,
  )
import Test.Hspec (Spec, describe, it, shouldBe, xit)
import Test.QuickCheck
import Text.Regex.PCRE.Heavy qualified as PCRE

instance Arbitrary ParamType where
    arbitrary = oneof $ pure <$> [TOne, TOptional, TList0, TList1, TSet0, TSet1]

instance Arbitrary TypeSig where
    arbitrary = liftA2 SimpleType arbitrary arbitrary

instance Arbitrary MTExpr where
    arbitrary = MTT . T.pack . (:[]) <$> arbitrary

instance Arbitrary EntityType where
    arbitrary = T.pack <$> arbitrary

instance Arbitrary Rule where
  arbitrary = do
    ruleName <- arbitrary
    maybeTypeSig <- arbitrary
    hasRules <- arbitrary
    pure $ TypeDecl {
            name = ruleName,
            super = maybeTypeSig,
            has = hasRules,
            enums   = Nothing,
            given   = Nothing,
            upon    = Nothing,
            rlabel  = Nothing,
            lsource = Nothing,
            srcref  = Nothing,
            defaults = [],
            symtab   = []
            }

processTopLvlNameTextForJsonSchema :: T.Text -> T.Text
processTopLvlNameTextForJsonSchema = PCRE.gsub [PCRE.re|\s+|] ("_" :: T.Text)

isNormalised :: T.Text -> Bool
isNormalised s = s == processTopLvlNameTextForJsonSchema s

prop_RuleNormalisedAsJson :: Rule -> Bool
prop_RuleNormalisedAsJson r = isNormalised $ T.pack $ rulesToJsonSchema [r]

spec :: Spec
spec = do
  -- Primitive types into JSON primitives
  describe "primitive list" do
    it "should turn primitive L4 list into primitive JSON array" do
      let
        typeSig = Just (SimpleType TList1 "String")
      typeDeclSuperToFieldType typeSig `shouldBe` FTList FTString

  -- Custom types into JSON references
  describe "custom list" do
    it "should turn custom L4 list into reference in JSON array" do
      let
        typeSig = Just (SimpleType TList1 "Address")
      typeDeclSuperToFieldType typeSig `shouldBe` FTList (FTRef "Address")

   -- Tests for prettyprinting

  describe "Prettyprinting: internal, spaces" do
    it "L4 name with spaces should still be with spaces in the internal representation" do
      let
        typeSig = Just (SimpleType TList1 "Name With Spaces")
      typeDeclSuperToFieldType typeSig `shouldBe` FTList (FTRef "Name With Spaces")

  describe "Prettyprinting: JSON, spaces" do
    it "Spaces should be replaced with _ when internal representation is prettyprinted" do
      let
        typeSig = Just (SimpleType TList1 "Name With Spaces")
        fieldType = typeDeclSuperToFieldType typeSig
      show (showTypesJson fieldType) `shouldBe` "\"type\": \"array\",\"items\": {\"$ref\": \"#/$defs/Name_With_Spaces\"}"

   -- Tests for prettyprinting
  describe "Prettyprinting: internal, no spaces" do
    it "L4 name without spaces unchanged internal representation" do
      let
        typeSig = Just (SimpleType TList1 "NameWithoutSpaces")
      typeDeclSuperToFieldType typeSig `shouldBe` FTList (FTRef "NameWithoutSpaces")

  describe "Prettyprinting: JSON, no spaces" do
    it "No changed to L4 name when there are no spaces" do
      let
        typeSig = Just (SimpleType TList1 "NameWithoutSpaces")
        fieldType = typeDeclSuperToFieldType typeSig
      show (showTypesJson fieldType) `shouldBe` "\"type\": \"array\",\"items\": {\"$ref\": \"#/$defs/NameWithoutSpaces\"}"

  -- TODO: this test gets stuck, why ???
  describe "Prettyprinting: property-based testing" do
    xit "Applying rulesToJsonSchema to arbitrary rules, all should be normalised" do
        property prop_RuleNormalisedAsJson