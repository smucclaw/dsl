{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module LS.InterpreterSpec (spec) where

import AnyAll (mkAll, mkAny, mkLeaf, mkNot)
import Data.HashMap.Strict qualified as Map
import LS.Interpreter (expandBSR')
import LS.Rule
  ( Interpreted (L4I, classtable, origrules, scopetable),
  )
import LS.Types
  ( ClsTab (CT),
    MTExpr (MTT),
    RPRel (RPhas, RPis),
    RelationalPredicate (RPBoolStructR),
    mkRpmtLeaf,
  )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "expandBSR'" do
    it "expand Leaf" do
      let
        emptyInt = L4I {classtable = CT Map.empty, scopetable = Map.empty, origrules = []}
        bsr = mkRpmtLeaf ["it is a Notifiable Data Breach"]
      expandBSR' emptyInt 0 bsr `shouldBe` bsr
    
    it "expand Leaf RPBoolStructR is" do
      let
        emptyInt = L4I {classtable = CT Map.empty, scopetable = Map.empty, origrules = []}
        l2 = mkRpmtLeaf ["sky", "is", "blue"]
        l1 = mkLeaf (RPBoolStructR (MTT <$> ["head2"]) RPis l2)
        bsr = mkLeaf (RPBoolStructR (MTT <$> ["head1"]) RPis l1)
      expandBSR' emptyInt 0 bsr `shouldBe` l2

    it "expand Leaf RPBoolStructR has" do
      let
        emptyInt = L4I {classtable = CT Map.empty, scopetable = Map.empty, origrules = []}
        l = mkRpmtLeaf ["rose", "has", "red"]
        bsr = mkLeaf (RPBoolStructR (MTT <$> ["head"]) RPhas l)
      expandBSR' emptyInt 0 bsr `shouldBe` bsr

    it "expand Not RPBoolStructR " do
      let
        emptyInt = L4I {classtable = CT Map.empty, scopetable = Map.empty, origrules = []}
        l = mkRpmtLeaf ["rose", "has", "red"]
        bsr = mkNot l
      expandBSR' emptyInt 0 bsr `shouldBe` bsr

    it "expand All RPBoolStructR is" do
      let
        emptyInt = L4I {classtable = CT Map.empty, scopetable = Map.empty, origrules = []}
        l2 = mkRpmtLeaf ["sky", "is", "blue"]
        l1 = mkRpmtLeaf ["rose", "is", "red"]
        bsr = mkAll Nothing [l1 , l2]
      expandBSR' emptyInt 0 bsr `shouldBe` bsr

    it "expand Any RPBoolStructR is" do
      let
        emptyInt = L4I {classtable = CT Map.empty, scopetable = Map.empty, origrules = []}
        l2 = mkRpmtLeaf ["sky", "is", "blue"]
        l1 = mkRpmtLeaf ["rose", "is", "red"]
        bsr = mkAny Nothing [l1 , l2]
      expandBSR' emptyInt 0 bsr `shouldBe` bsr
