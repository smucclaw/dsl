{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LS.InterpreterSpec (spec) where

import AnyAll (mkAll, mkAny, mkLeaf, mkNot)
import Data.HashMap.Strict qualified as Map
import Data.String.Interpolate (i)
import LS.Interpreter (expandBSR')
import LS.Rule
  ( Interpreted (L4I, classtable, origrules, scopetable),
  )
import LS.Types
  ( ClsTab (CT),
    MTExpr (MTT),
    RPRel (RPhas, RPis),
    RelationalPredicate (RPBoolStructR),
    mkCT,
    mkRpmtLeaf,
  )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "expandBSR'" do
    it "expand Leaf" do
      let bsr = mkRpmtLeaf ["it is a Notifiable Data Breach"]
      expandBSR'' bsr `shouldBe` bsr
    
    it "expand Leaf RPBoolStructR is" do
      let l2 = mkRpmtLeaf ["sky", "is", "blue"]
          l1 = mkLeaf (RPBoolStructR (MTT <$> ["head2"]) RPis l2)
          bsr = mkLeaf (RPBoolStructR (MTT <$> ["head1"]) RPis l1)
      expandBSR'' bsr `shouldBe` l2

    it "expand Leaf RPBoolStructR has" do
      let l = mkRpmtLeaf ["rose", "has", "red"]
          bsr = mkLeaf (RPBoolStructR (MTT <$> ["head"]) RPhas l)
      expandBSR'' bsr `shouldBe` bsr

    it "expand Not RPBoolStructR " do
      let l = mkRpmtLeaf ["rose", "has", "red"]
          bsr = mkNot l
      expandBSR'' bsr `shouldBe` bsr

    testExpandAnyAll "All" mkAll
    testExpandAnyAll "Any" mkAny
    where
      emptyInt =
        L4I
          { classtable = mkCT Map.empty,
            scopetable = Map.empty,
            origrules = []
          }

      expandBSR'' = expandBSR' emptyInt 0

      testExpandAnyAll (txt :: String) ctor =
        it [i|expand #{txt} RPBoolStructR is|] $
          expandBSR'' bsr `shouldBe` bsr
        where
          l2 = mkRpmtLeaf ["sky", "is", "blue"]
          l1 = mkRpmtLeaf ["rose", "is", "red"]
          bsr = ctor Nothing [l1, l2]
