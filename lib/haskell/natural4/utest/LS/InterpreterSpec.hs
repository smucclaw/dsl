{-# LANGUAGE OverloadedStrings #-}
module LS.InterpreterSpec where

import Test.Hspec
import LS.Types
import AnyAll
import qualified Data.Map as Map
import LS.Interpreter
import AnyAll.BoolStructTree

spec :: Spec
spec = do
  describe "expandBSR'" $ do
    it "expand Leaf" $ do
      let
        emptyInt = L4I {classtable = CT Map.empty, scopetable = Map.empty, origrules = []}
        bsr = mkLeaf (RPMT ["it is a Notifiable Data Breach"])
      expandBSR' emptyInt 0 bsr `shouldBe` bsr
    
    it "expand Leaf RPBoolStructR is" $ do
      let
        emptyInt = L4I {classtable = CT Map.empty, scopetable = Map.empty, origrules = []}
        l2 = mkLeaf $ RPMT ["sky", "is", "blue"]
        l1 = mkLeaf (RPBoolStructR ["head2"] RPis l2)
        bsr = mkLeaf (RPBoolStructR ["head1"] RPis l1)
      expandBSR' emptyInt 0 bsr `shouldBe` l2

    it "expand Leaf RPBoolStructR has" $ do
      let
        emptyInt = L4I {classtable = CT Map.empty, scopetable = Map.empty, origrules = []}
        l = mkLeaf $ RPMT ["rose", "has", "red"]
        bsr = mkLeaf (RPBoolStructR ["head"] RPhas l)
      expandBSR' emptyInt 0 bsr `shouldBe` bsr

    it "expand Not RPBoolStructR " $ do
      let
        emptyInt = L4I {classtable = CT Map.empty, scopetable = Map.empty, origrules = []}
        l = mkLeaf $ RPMT ["rose", "has", "red"]
        bsr = mkNot l
      expandBSR' emptyInt 0 bsr `shouldBe` bsr

    it "expand All RPBoolStructR is" $ do
      let
        emptyInt = L4I {classtable = CT Map.empty, scopetable = Map.empty, origrules = []}
        l2 = mkLeaf $ RPMT ["sky", "is", "blue"]
        l1 = mkLeaf $ RPMT ["rose", "is", "red"]
        bsr = mkAll Nothing [l1 , l2]
      expandBSR' emptyInt 0 bsr `shouldBe` bsr

    it "expand Any RPBoolStructR is" $ do
      let
        emptyInt = L4I {classtable = CT Map.empty, scopetable = Map.empty, origrules = []}
        l2 = mkLeaf $ RPMT ["sky", "is", "blue"]
        l1 = mkLeaf $ RPMT ["rose", "is", "red"]
        bsr = mkAny Nothing [l1 , l2]
      expandBSR' emptyInt 0 bsr `shouldBe` bsr

  describe "expandBSRDT'" $ do
    it "expand Leaf" $ do
      let
        emptyInt = L4I {classtable = CT Map.empty, scopetable = Map.empty, origrules = []}
        bsr = mkLeafDT (RPMT ["it is a Notifiable Data Breach"])
      expandBSRDT' emptyInt 0 bsr `shouldBe` bsr
    
    it "expand Leaf RPBoolStructR is" $ do
      let
        emptyInt = L4I {classtable = CT Map.empty, scopetable = Map.empty, origrules = []}
        l2 = mkLeafDT $ RPMT ["sky", "is", "blue"]
        l1 = mkLeafDT (RPBoolStructDTR ["head2"] RPis l2)
        bsr = mkLeafDT (RPBoolStructDTR ["head1"] RPis l1)
      expandBSRDT' emptyInt 0 bsr `shouldBe` l2

    it "expand Leaf RPBoolStructR has" $ do
      let
        emptyInt = L4I {classtable = CT Map.empty, scopetable = Map.empty, origrules = []}
        l = mkLeafDT $ RPMT ["rose", "has", "red"]
        bsr = mkLeafDT (RPBoolStructDTR ["head"] RPhas l)
      expandBSRDT' emptyInt 0 bsr `shouldBe` bsr

    it "expand Not RPBoolStructR " $ do
      let
        emptyInt = L4I {classtable = CT Map.empty, scopetable = Map.empty, origrules = []}
        l = mkLeafDT $ RPMT ["rose", "has", "red"]
        bsr = mkNotDT l
      expandBSRDT' emptyInt 0 bsr `shouldBe` bsr

    it "expand All RPBoolStructR is" $ do
      let
        emptyInt = L4I {classtable = CT Map.empty, scopetable = Map.empty, origrules = []}
        l2 = mkLeafDT $ RPMT ["sky", "is", "blue"]
        l1 = mkLeafDT $ RPMT ["rose", "is", "red"]
        bsr = mkAllDT Nothing [l1 , l2]
      expandBSRDT' emptyInt 0 bsr `shouldBe` bsr

    it "expand Any RPBoolStructR is" $ do
      let
        emptyInt = L4I {classtable = CT Map.empty, scopetable = Map.empty, origrules = []}
        l2 = mkLeafDT $ RPMT ["sky", "is", "blue"]
        l1 = mkLeafDT $ RPMT ["rose", "is", "red"]
        bsr = mkAnyDT Nothing [l1 , l2]
      expandBSRDT' emptyInt 0 bsr `shouldBe` bsr

