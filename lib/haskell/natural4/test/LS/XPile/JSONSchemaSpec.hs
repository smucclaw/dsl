{-# LANGUAGE OverloadedStrings #-}

module LS.XPile.JSONSchemaSpec where

import Test.Hspec ( describe, it, xit, shouldBe, Spec )
import LS.Types (TypeSig(..), ParamType(..))
import LS.XPile.ExportTypes

spec :: Spec
spec = do
  -- Primitive types into JSON primitives
  describe "primitive list" $ do
    it "should turn primitive L4 list into primitive JSON array" $ do
      let
        typeSig = Just (SimpleType TList1 "String")
      typeDeclSuperToFieldType typeSig `shouldBe` FTList FTString

  -- Custom types into JSON references
  describe "custom list" $ do
    it "should turn custom L4 list into reference in JSON array" $ do
      let
        typeSig = Just (SimpleType TList1 "Address")
      typeDeclSuperToFieldType typeSig `shouldBe` FTList (FTRef "Address")

   -- Tests for prettyprinting

  -- TODO: this one doesn't work yet, will do a separate PR to get that done
  describe "Prettyprinting: internal, spaces" $ do
    xit "L4 name with spaces should still be with spaces in the internal representation" $ do
      let
        typeSig = Just (SimpleType TList1 "Name With Spaces")
      typeDeclSuperToFieldType typeSig `shouldBe` FTList (FTRef "Name With Spaces")

  describe "Prettyprinting: JSON, spaces" $ do
    it "Spaces should be replaced with _ when internal representation is prettyprinted" $ do
      let
        typeSig = Just (SimpleType TList1 "Name With Spaces")
        fieldType = typeDeclSuperToFieldType typeSig
      show (showTypesJson fieldType)  `shouldBe` "\"type\": \"array\",\"items\": {\"$ref\": \"#/$defs/Name_With_Spaces\"}"

   -- Tests for prettyprinting
  describe "Prettyprinting: internal, no spaces" $ do
    it "L4 name without spaces unchanged internal representation" $ do
      let
        typeSig = Just (SimpleType TList1 "NameWithoutSpaces")
      typeDeclSuperToFieldType typeSig `shouldBe` FTList (FTRef "NameWithoutSpaces")

  describe "Prettyprinting: JSON, no spaces" $ do
    it "No changed to L4 name when there are no spaces" $ do
      let
        typeSig = Just (SimpleType TList1 "NameWithoutSpaces")
        fieldType = typeDeclSuperToFieldType typeSig
      show (showTypesJson fieldType)  `shouldBe` "\"type\": \"array\",\"items\": {\"$ref\": \"#/$defs/NameWithoutSpaces\"}"