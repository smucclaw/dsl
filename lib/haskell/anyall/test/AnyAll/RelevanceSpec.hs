{-# LANGUAGE OverloadedStrings #-}
module AnyAll.RelevanceSpec where

import Data.Text
import Data.Tree
import AnyAll.BoolStructTree
import Test.Hspec
import AnyAll.Types
import AnyAll.Relevance (relevant)
import qualified Data.Map as Map
import AnyAll.BoolStruct

markingMap :: Either (Maybe Bool) (Maybe Bool) -> Map.Map Text (Default Bool)
markingMap payload = Map.singleton "key" (Default payload)

type WireBoolStruct = BoolStruct (Maybe (Label Text)) Text

spec :: Spec
spec = do
  describe "relevant" $ do
    let
      ma =
        Map.fromList
          [ ("key1", Default $ Right $ Just True),
            ("key2", Default $ Right $ Just True)
          ]

      m =  Marking {getMarking = ma}
      qc1 = Q {shouldView = View, andOr = Simply "key1", prePost = Nothing, mark = Default (Right (Just True))}
      qc2 = Q {shouldView = View, andOr = Simply "key2", prePost = Nothing, mark = Default (Right (Just True))}

    describe "leaf" $ do
      describe "leaf marking=(Right True)" $ do
        it "Hard DPNormal (Right True) (Just True) (leaf key)" $ do
          relevant Hard DPNormal m (Just True) (mkLeaf "key1")
            `shouldBe` 
            Node qc1 []

        it "Hard DPNormal (Right True) (Just False) (leaf key)" $ do
          relevant Hard DPNormal m (Just False) (mkLeaf "key1")
            `shouldBe` 
            Node qc1 []

        it "Hard DPNormal (Right True) (Nothing) (leaf key)" $ do
          relevant Hard DPNormal m Nothing (mkLeaf "key1")
            `shouldBe` 
            Node qc1 []
      
        it "Hard DPNormal (Right True) (Just True) (leaf missing)" $ do
          relevant Hard DPNormal m (Just True) (mkLeaf "missing")
            `shouldBe` 
            Node qc1 {shouldView = Hide, andOr = Simply "missing", mark = Default (Left Nothing)} []

        it "Hard DPNormal (Right True) (Just False) (leaf missing)" $ do
          relevant Hard DPNormal m (Just False) (mkLeaf "missing")
            `shouldBe` 
            Node qc1 {shouldView = Hide, andOr = Simply "missing", mark = Default (Left Nothing)} []

        it "Hard DPNormal (Right True) (Nothing) (leaf missing)" $ do
          relevant Hard DPNormal m Nothing (mkLeaf "missing")
            `shouldBe` 
            Node qc1 {shouldView = Ask, andOr = Simply "missing", mark = Default (Left Nothing)} []

      describe "leaf marking=(Right False)" $ do
        let
          mrf =  Marking {getMarking = Map.singleton "key1" (Default $ Right $ Just False)}
        it "Hard DPNormal (Right False) (Just True) (leaf key)" $ do
          relevant Hard DPNormal mrf (Just True) (mkLeaf "key1")
            `shouldBe` 
            Node qc1 {mark = Default (Right (Just False))} []

        it "Hard DPNormal (Right False) (Just False) (leaf key)" $ do
          relevant Hard DPNormal mrf (Just False) (mkLeaf "key1")
            `shouldBe` 
            Node qc1 {mark = Default (Right (Just False))} []

        it "Hard DPNormal (Right False) (Nothing) (leaf key)" $ do
          relevant Hard DPNormal mrf Nothing (mkLeaf "key1")
            `shouldBe` 
            Node qc1 {mark = Default (Right (Just False))} []
      
        it "Hard DPNormal (Right False) (Just True) (leaf missing)" $ do
          relevant Hard DPNormal mrf (Just True) (mkLeaf "missing")
            `shouldBe` 
            Node qc1 {shouldView = Hide, andOr = Simply "missing", mark = Default (Left Nothing)} []

        it "Hard DPNormal (Right False) (Just False) (leaf missing)" $ do
          relevant Hard DPNormal mrf (Just False) (mkLeaf "missing")
            `shouldBe` 
            Node qc1 {shouldView = Hide, andOr = Simply "missing", mark = Default (Left Nothing)} []

        it "Hard DPNormal (Right False) (Nothing) (leaf missing)" $ do
          relevant Hard DPNormal mrf Nothing (mkLeaf "missing")
            `shouldBe` 
            Node qc1 {shouldView = Ask, andOr = Simply "missing", mark = Default (Left Nothing)} []

    it "Hard DPNormal (Right True) (Just True) (not key)" $ do
      let
        qp = Q {shouldView = Hide, andOr = Neg, prePost = Nothing, mark = Default (Left (Just False))}
      relevant Hard DPNormal m (Just True) (mkNot (mkLeaf "key1"))
        `shouldBe`
        Node qp [Node qc1 []]

    it "Hard DPNormal (Right True) (Just True) (not key)" $ do
      let
        qp = Q {shouldView = Hide, andOr = Neg, prePost = Nothing, mark = Default (Left (Just False))}
      relevant Hard DPNormal m (Just True) (mkNot (mkLeaf "key1"))
        `shouldBe`
        Node qp [Node qc1 []]

    it "Hard DPNormal (Right True) (Just True) (and key1 key2)" $ do
      let 
        qp = Q {shouldView = View, andOr = And, prePost = Nothing, mark = Default (Left (Just True))}
      relevant Hard DPNormal m (Just True) (mkAll Nothing [mkLeaf "key1", mkLeaf "key2"])
        `shouldBe`
        Node qp [Node qc1 [],Node qc2 []]

    it "Hard DPNormal (Right True) (Just True) (or key1 key2)" $ do
      let 
        qp = Q {shouldView = View, andOr = Or, prePost = Nothing, mark = Default (Left (Just True))}
      relevant Hard DPNormal m (Just True) (mkAny Nothing [mkLeaf "key1", mkLeaf "key2"])
        `shouldBe`
        Node qp [Node qc1 [],Node qc2 []]