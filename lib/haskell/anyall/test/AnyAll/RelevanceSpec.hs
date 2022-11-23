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
      let
        mrf =  Marking {getMarking = Map.singleton "key1" (Default $ Right $ Just False)}
        mlf =  Marking {getMarking = Map.singleton "key1" (Default $ Left $ Just False)}
        mlt =  Marking {getMarking = Map.singleton "key1" (Default $ Left $ Just True)}
      it "Hard (Right True) (Just True) (leaf key)" $ do
        relevant Hard m (Just True) (mkLeaf "key1")
          `shouldBe`
          Node qc1 []
      
      it "Hard (Right True) (Just True) (leaf key)" $ do
        relevant Hard m (Just False) (mkLeaf "key1")
          `shouldBe`
          Node qc1 []

      it "Soft (Left True) (Just True) (leaf key)" $ do
        relevant Soft mlt (Just True) (mkLeaf "key1")
          `shouldBe`
          Node qc1{shouldView = Ask, mark = Default (Left (Just True))} []
      
      it "Soft (Left True) (Just True) (leaf key)" $ do
        relevant Soft mlt (Just False) (mkLeaf "key1")
          `shouldBe`
          Node qc1{shouldView = Hide, mark = Default (Left (Just True))} []

      it "Soft (Left True) (Just True) (leaf key)" $ do
        relevant Soft mlt Nothing (mkLeaf "key1")
          `shouldBe`
          Node qc1{shouldView = Ask, mark = Default (Left (Just True))} []

      it "Soft (Left True) (Just True) (leaf key)" $ do
        relevant Hard mlt (Just True) (mkLeaf "miss")
          `shouldBe`
          Node qc1{shouldView = Hide, andOr = Simply "miss", mark = Default (Left Nothing)} []

      it "Soft (Left True) (Just True) (leaf key)" $ do
        relevant Hard mlt Nothing (mkLeaf "miss")
          `shouldBe`
          Node qc1{shouldView = Ask, andOr = Simply "miss", mark = Default (Left Nothing)} []

    it "Hard DPNormal (Right True) (Just True) (not key)" $ do
      let
        qp = Q {shouldView = Hide, andOr = Neg, prePost = Nothing, mark = Default (Left (Just False))}
      relevant Hard m (Just True) (mkNot (mkLeaf "key1"))
        `shouldBe`
        Node qp [Node qc1 []]

    it "Hard DPNormal (Right True) (Just True) (not key)" $ do
      let
        qp = Q {shouldView = Hide, andOr = Neg, prePost = Nothing, mark = Default (Left (Just False))}
      relevant Hard m (Just True) (mkNot (mkLeaf "key1"))
        `shouldBe`
        Node qp [Node qc1 []]

    it "Hard DPNormal (Right True) (Just True) (and key1 key2)" $ do
      let 
        qp = Q {shouldView = View, andOr = And, prePost = Nothing, mark = Default (Left (Just True))}
      relevant Hard m (Just True) (mkAll Nothing [mkLeaf "key1", mkLeaf "key2"])
        `shouldBe`
        Node qp [Node qc1 [],Node qc2 []]

    it "Hard DPNormal (Right True) (Just True) (or key1 key2)" $ do
      let 
        qp = Q {shouldView = View, andOr = Or, prePost = Nothing, mark = Default (Left (Just True))}
      relevant Hard m (Just True) (mkAny Nothing [mkLeaf "key1", mkLeaf "key2"])
        `shouldBe`
        Node qp [Node qc1 [],Node qc2 []]

