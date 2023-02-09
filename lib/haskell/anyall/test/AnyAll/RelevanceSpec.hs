{-# LANGUAGE OverloadedStrings #-}
module AnyAll.RelevanceSpec where

import qualified Data.Text as T
import Data.Tree
import Test.Hspec
import AnyAll.Types
import AnyAll.Relevance
import qualified Data.Map as Map
import AnyAll.BoolStruct
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Instances.Text

markingMap :: Either (Maybe Bool) (Maybe Bool) -> Map.Map T.Text (Default Bool)
markingMap payload = Map.singleton "key" (Default payload)

type WireBoolStruct = BoolStruct (Maybe (Label T.Text)) T.Text

sumAssoc :: Int -> [Int] -> Expectation
sumAssoc n ints = sum ints `shouldBe` sum qa + sum qb
  where (qa, qb) = splitAt (n `mod` (1 + length ints)) ints

evalAnyAssoc :: Marking T.Text -> Int -> [BoolStruct T.Text T.Text] -> Expectation
evalAnyAssoc m n ints = evaluate Hard m (mkAny "" ints) `shouldBe` evaluate Hard m (mkAny "" [mkAny "" qa, mkAny "" qb])
  where (qa, qb) = splitAt (n `mod` (1 + length ints)) ints

mkTextLeaf :: a -> BoolStruct T.Text a
mkTextLeaf = Leaf

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

  describe "evaluate" $ do
    let
      ma =
        Map.fromList
          [ ("key1", Default $ Right $ Just True),
            ("key2", Default $ Right $ Just True)
          ]

      m =  Marking {getMarking = ma}
      qc1 = Q {shouldView = View, andOr = Simply "key1", prePost = Nothing, mark = Default (Right (Just True))}
      qc2 = Q {shouldView = View, andOr = Simply "key2", prePost = Nothing, mark = Default (Right (Just True))}
      mrt =  Marking {getMarking = Map.singleton "key1" (Default $ Right $ Just True)}
      mrf =  Marking {getMarking = Map.singleton "key1" (Default $ Right $ Just False)}
      mlf =  Marking {getMarking = Map.singleton "key1" (Default $ Left $ Just False)}
      mlt =  Marking {getMarking = Map.singleton "key1" (Default $ Left $ Just True)}

      leafNode = mkLeaf "key1"

    describe "leaf" $ do
      it "Hard (Right True) (leaf key)" $ do
        evaluate Hard mrt leafNode `shouldBe` Just True

      it "Hard (Right False) (leaf key)" $ do
        evaluate Hard mrf leafNode `shouldBe` Just False

      it "Hard (Left True) (leaf key)" $ do
        evaluate Hard mlt leafNode `shouldBe` Nothing

      it "Hard (Left False) (leaf key)" $ do
        evaluate Hard mlf leafNode `shouldBe` Nothing

      it "Hard (Nothing) (leaf key)" $ do
        evaluate Hard mrf (mkLeaf "missing") `shouldBe` Nothing

      it "Soft (Right True) (leaf key)" $ do
        evaluate Soft mrt leafNode `shouldBe` Just True

      it "Soft (Right False) (leaf key)" $ do
        evaluate Soft mrf leafNode `shouldBe` Just False

      it "Soft (Left True) (leaf key)" $ do
        evaluate Soft mlt leafNode `shouldBe` Just True

      it "Soft (Left False) (leaf key)" $ do
        evaluate Soft mlf leafNode `shouldBe` Just False

      it "Soft (Nothing) (leaf key)" $ do
        evaluate Soft mrf (mkLeaf "missing") `shouldBe` Nothing

    describe "not" $ do
      it "Not Just True" $ do
        evaluate Hard mrt (mkNot leafNode) `shouldBe` Just False

      it "Not Just False" $ do
        evaluate Hard mrf (mkNot leafNode) `shouldBe` Just True

      it "Not Nothing" $ do
        evaluate Hard mlt (mkNot leafNode) `shouldBe` Nothing

    describe "Any" $ do
      it "Any (Just True, Nothing)" $ do
        evaluate Hard mrt (mkAny "" [leafNode, mkLeaf "missing"]) `shouldBe` Just True

      it "Any (Just False, Nothing)" $ do
        evaluate Hard mrf (mkAny "" [leafNode, mkLeaf "missing"]) `shouldBe` Nothing

      it "Any (Nothing, Nothing)" $ do
        evaluate Hard mrt (mkAny "" [mkLeaf "missing1", mkLeaf "missing2"]) `shouldBe` Nothing
    
      prop "eval is assoaciative" (evalAnyAssoc mrf)

    describe "All" $ do
      it "All (Just True, Nothing)" $ do
        evaluate Hard mrt (mkAll "" [leafNode, mkLeaf "missing"]) `shouldBe` Nothing

      it "All (Just False, Nothing)" $ do
        evaluate Hard mrf (mkAll "" [leafNode, mkLeaf "missing"]) `shouldBe` Just False

      it "All (Nothing, Nothing)" $ do
        evaluate Hard mrt (mkAll "" [mkLeaf "missing1", mkLeaf "missing2"]) `shouldBe` Nothing
 
  describe "dispositive" $ do
    let
      ma =
        Map.fromList
          [ ("key1", Default $ Right $ Just True),
            ("key2", Default $ Right $ Just True)
          ]

      m =  Marking {getMarking = ma}
      qc1 = Q {shouldView = View, andOr = Simply "key1", prePost = Nothing, mark = Default (Right (Just True))}
      qc2 = Q {shouldView = View, andOr = Simply "key2", prePost = Nothing, mark = Default (Right (Just True))}
      mrt =  Marking {getMarking = Map.singleton "key1" (Default $ Right $ Just True)}
      mrf =  Marking {getMarking = Map.singleton "key1" (Default $ Right $ Just False)}
      mlf =  Marking {getMarking = Map.singleton "key1" (Default $ Left $ Just False)}
      mlt =  Marking {getMarking = Map.singleton "key1" (Default $ Left $ Just True)}

      leafNode = mkLeaf "key1"::BoolStruct T.Text T.Text

    describe "leaf" $ do
      it "Hard (Right True) (leaf key)" $ do
        dispositive Hard mrt leafNode `shouldBe` [leafNode]

      it "Hard (Right False) (leaf key)" $ do
        dispositive Hard mrf leafNode `shouldBe` [leafNode]

      it "Hard (Left True) (leaf key)" $ do
        dispositive Hard mlt leafNode `shouldBe` []

      it "Hard (Left False) (leaf key)" $ do
        dispositive Hard mlf leafNode `shouldBe` []

      it "Hard (Nothing) (leaf key)" $ do
        dispositive Hard mrf (mkTextLeaf "missing") `shouldBe` []

      it "Soft (Right True) (leaf key)" $ do
        dispositive Soft mrt leafNode `shouldBe` [leafNode]

      it "Soft (Right False) (leaf key)" $ do
        dispositive Soft mrf leafNode `shouldBe` [leafNode]

      it "Soft (Left True) (leaf key)" $ do
        dispositive Soft mlt leafNode `shouldBe` [leafNode]

      it "Soft (Left False) (leaf key)" $ do
        dispositive Soft mlf leafNode `shouldBe` [leafNode]

      it "Soft (Nothing) (leaf key)" $ do
        dispositive Soft mrf (mkTextLeaf "missing") `shouldBe` []

    describe "not" $ do
      xit "Not Just True" $ do
        dispositive Hard mrt (mkNot leafNode) `shouldBe` [leafNode]

      xit "Not Just False" $ do
        dispositive Hard mrf (mkNot leafNode) `shouldBe` [leafNode]

      it "Not Nothing" $ do
        dispositive Hard mlt (mkNot leafNode) `shouldBe` []

    describe "Any" $ do
      it "Any (Just True, Nothing)" $ do
        dispositive Hard mrt (mkAny "" [leafNode, mkLeaf "missing"]) `shouldBe` [leafNode]

      it "Any (Just False, Nothing)" $ do
        dispositive Hard mrf (mkAny "" [leafNode, mkLeaf "missing"]) `shouldBe` []

      it "Any (Nothing, Nothing)" $ do
        dispositive Hard mrt (mkAny "" [mkLeaf "missing1", mkLeaf "missing2"]) `shouldBe` []

    describe "All" $ do
      it "All (Just True, Nothing)" $ do
        dispositive Hard mrt (mkAll "" [leafNode, mkLeaf "missing"]) `shouldBe` []

      it "All (Just False, Nothing)" $ do
        dispositive Hard mrf (mkAll "" [leafNode, mkLeaf "missing"]) `shouldBe` [leafNode]

      it "All (Nothing, Nothing)" $ do
        dispositive Hard mrt (mkAll "" [mkLeaf "missing1", mkLeaf "missing2"]) `shouldBe` []
