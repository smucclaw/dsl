
import Test.Hspec
import AnyAll.Types
import AnyAll.Relevance
import qualified Data.Map.Strict as Map
import Data.Tree

main :: IO ()
main = hspec $ do
  let rlv item marking = relevant Hard DPNormal marking Nothing item
  describe "with mustSing," $ do
    it "should ask for confirmation of assumptions, even if true" $ do
      rlv mustSing (Map.fromList [("walk",  Left $ Just True)
                                 ,("eat",   Left $ Just True)
                                 ,("drink", Left $ Just True)])
        `shouldBe`
        Node (Q Ask And (Just $ Pre "both"))
        [ Node (Q Ask (Simply "walk") Nothing) []
        , Node (Q Ask Or (Just $ Pre "either"))
          [ Node (Q Ask (Simply "eat") Nothing) []
          , Node (Q Ask (Simply "drink") Nothing) []
          ]
        ]

    it "should ask for confirmation of assumptions, even if false" $ do
      rlv mustSing (Map.fromList [("walk",  Left $ Just False)
                                 ,("eat",   Left $ Just True)
                                 ,("drink", Left $ Just True)])
        `shouldBe`
        Node (Q Ask And (Just $ Pre "both"))
        [ Node (Q Ask (Simply "walk") Nothing) []
        , Node (Q Ask Or (Just $ Pre "either"))
          [ Node (Q Ask (Simply "eat") Nothing) []
          , Node (Q Ask (Simply "drink") Nothing) []
          ]
        ]

    it "should ask for confirmation of assumptions, when Nothing" $ do
      rlv mustSing (Map.fromList [("walk",  Left   Nothing)
                                 ,("eat",   Left $ Just True)
                                 ,("drink", Left $ Just True)])
        `shouldBe`
        Node (Q Ask And (Just $ Pre "both"))
        [ Node (Q Ask (Simply "walk") Nothing) []
        , Node (Q Ask Or (Just $ Pre "either"))
          [ Node (Q Ask (Simply "eat") Nothing) []
          , Node (Q Ask (Simply "drink") Nothing) [] ] ]

    it "should consider a Walk=R False to be dispositive" $ do
      flip (dispositive Hard) mustSing (Map.fromList [("walk",  Right $ Just False)
                                                     ,("eat",   Left $ Just True)
                                                     ,("drink", Left $ Just True)])
        `shouldBe` [Leaf "walk"]

    it "should consider a Walk=R True, Eat=R True to be dispositive" $ do
      flip (dispositive Hard) mustSing (Map.fromList [("walk",  Right $ Just True)
                                              ,("eat",   Right $ Just True)
                                              ,("drink", Left $ Just True)])
        `shouldBe` [Leaf "walk", Leaf "eat"]

    it "should short-circuit a confirmed False in an And list" $ do
      rlv mustSing (Map.fromList [("walk",  Right $ Just False)
                                 ,("eat",   Left $ Just True)
                                 ,("drink", Left $ Just True)])
        `shouldBe`
        Node (Q View And (Just $ Pre "both"))
        [ Node (Q View (Simply "walk") Nothing) []
        , Node (Q Hide Or (Just $ Pre "either"))
          [ Node (Q Hide (Simply "eat") Nothing) []
          , Node (Q Hide (Simply "drink") Nothing) [] ] ]

    it "given a confirmed True in an And list, should recurse into the eat/drink limb" $ do
      rlv mustSing (Map.fromList [("walk",  Right $ Just True)
                                 ,("eat",   Left $ Just True)
                                 ,("drink", Left $ Just True)])
        `shouldBe`
        Node (Q Ask And (Just $ Pre "both"))
        [ Node (Q View (Simply "walk") Nothing) []
        , Node (Q Ask Or (Just $ Pre "either"))
          [ Node (Q Ask (Simply "eat") Nothing) []
          , Node (Q Ask (Simply "drink") Nothing) [] ] ]

    it "should stop given a confirmed Eat  =True in an Or list" $ do
      rlv mustSing (Map.fromList [("walk",  Right $ Just True)
                                 ,("eat",   Right $ Just True)
                                 ,("drink", Left $ Just True)])
        `shouldBe`
        Node (Q View And (Just $ Pre "both"))
        [ Node (Q View (Simply "walk") Nothing) []
        , Node (Q View Or (Just $ Pre "either"))
          [ Node (Q View (Simply "eat") Nothing) []
          , Node (Q Hide (Simply "drink") Nothing) [] ] ]

    it "should stop given a confirmed Drink=True in an Or list" $ do
      rlv mustSing (Map.fromList [("walk",  Right $ Just True)
                                 ,("eat",   Left  $ Just True)
                                 ,("drink", Right $ Just True)])
        `shouldBe`
        Node (Q View And (Just $ Pre "both"))
        [ Node (Q View (Simply "walk") Nothing) []
        , Node (Q View Or (Just $ Pre "either"))
          [ Node (Q Hide (Simply "eat") Nothing) []
          , Node (Q View (Simply "drink") Nothing) [] ] ]

    it "should demand walk even when Drink=True" $ do
      rlv mustSing (Map.fromList [("walk",  Left  $ Just True)
                                 ,("eat",   Left  $ Just True)
                                 ,("drink", Right $ Just True)])
        `shouldBe`
        Node (Q Ask And (Just $ Pre "both"))
        [ Node (Q Ask (Simply "walk") Nothing) []
        , Node (Q View Or (Just $ Pre "either"))
          [ Node (Q Hide (Simply "eat") Nothing) []
          , Node (Q View (Simply "drink") Nothing) [] ] ]

  describe "with mustDance," $ do
    it "should ask for everything when nothing is known" $ do
      rlv mustDance (Map.fromList [("walk",  Left Nothing)
                                  ,("run",   Left Nothing)
                                  ,("eat",   Left Nothing)
                                  ,("drink", Left Nothing)])
        `shouldBe`
        Node (Q Ask And (Just $ Pre "three of:"))
        [ Node (Q Ask And (Just $ Pre "both"))
          [ Node (Q Ask (Simply "walk") Nothing) []
          , Node (Q Ask (Simply "run")  Nothing) []
          ]
        , Node (Q Ask Or (Just $ Pre "either"))
          [ Node (Q Ask (Simply "eat") Nothing) []
          , Node (Q Ask (Simply "drink") Nothing) [] ] ]

    it "should ask for everything when everything is assumed true" $ do
      rlv mustDance (Map.fromList [("walk",  Left (Just True))
                                  ,("run",   Left (Just True))
                                  ,("eat",   Left (Just True))
                                  ,("drink", Left (Just True))])
        `shouldBe`
        Node (Q Ask And (Just $ Pre "three of:"))
        [ Node (Q Ask And (Just $ Pre "both"))
          [ Node (Q Ask (Simply "walk") Nothing) []
          , Node (Q Ask (Simply "run")  Nothing) []
          ]
        , Node (Q Ask Or (Just $ Pre "either"))
          [ Node (Q Ask (Simply "eat") Nothing) []
          , Node (Q Ask (Simply "drink") Nothing) [] ] ]

    it "should ask for everything when everything is assumed false" $ do
      rlv mustDance (Map.fromList [("walk",  Left (Just False))
                                  ,("run",   Left (Just False))
                                  ,("eat",   Left (Just False))
                                  ,("drink", Left (Just False))])
        `shouldBe`
        Node (Q Ask And (Just $ Pre "three of:"))
        [ Node (Q Ask And (Just $ Pre "both"))
          [ Node (Q Ask (Simply "walk") Nothing) []
          , Node (Q Ask (Simply "run")  Nothing) []
          ]
        , Node (Q Ask Or (Just $ Pre "either"))
          [ Node (Q Ask (Simply "eat") Nothing) []
          , Node (Q Ask (Simply "drink") Nothing) [] ] ]

    it "should handle a Walk=False by stopping" $ do
      rlv mustDance (Map.fromList [("walk",  Right (Just False))
                                  ,("run",   Left (Just False))
                                  ,("eat",   Left (Just False))
                                  ,("drink", Left (Just False))])
        `shouldBe`
        Node (Q View And (Just $ Pre "three of:"))
        [ Node (Q View And (Just $ Pre "both"))
          [ Node (Q View (Simply "walk") Nothing) []
          , Node (Q Hide (Simply "run")  Nothing) []
          ]
        , Node (Q Hide Or (Just $ Pre "either"))
          [ Node (Q Hide (Simply "eat") Nothing) []
          , Node (Q Hide (Simply "drink") Nothing) [] ] ]

    it "should handle a Run=False by stopping (in future this will depend on displaypref" $ do
      rlv mustDance (Map.fromList [("walk",  Left (Just False))
                                  ,("run",   Right (Just False))
                                  ,("eat",   Right (Just True))
                                  ,("drink", Right (Just True))])
        `shouldBe`
        Node (Q View And (Just $ Pre "three of:"))
        [ Node (Q View And (Just $ Pre "both"))
          [ Node (Q Hide (Simply "walk") Nothing) []
          , Node (Q View (Simply "run")  Nothing) []
          ]
        , Node (Q Hide Or (Just $ Pre "either"))
          [ Node (Q View (Simply "eat") Nothing) []
          , Node (Q View (Simply "drink") Nothing) [] ] ]

    it "should handle a Walk=True by remaining curious about the run and the food" $ do
      rlv mustDance (Map.fromList [("walk",  Right (Just True))
                                  ,("run",   Left (Just False))
                                  ,("eat",   Left (Just False))
                                  ,("drink", Left (Just False))])
        `shouldBe`
        Node (Q Ask And (Just $ Pre "three of:"))
        [ Node (Q Ask And (Just $ Pre "both"))
          [ Node (Q View (Simply "walk") Nothing) []
          , Node (Q Ask (Simply "run")  Nothing) []
          ]
        , Node (Q Ask Or (Just $ Pre "either"))
          [ Node (Q Ask (Simply "eat") Nothing) []
          , Node (Q Ask (Simply "drink") Nothing) [] ] ]

    it "should handle a Walk=True,Eat=False by remaining curious about the run and the drink" $ do
      rlv mustDance (Map.fromList [("walk",  Right (Just True))
                                  ,("run",   Left (Just False))
                                  ,("eat",   Right (Just False))
                                  ,("drink", Left (Just False))])
        `shouldBe`
        Node (Q Ask And (Just $ Pre "three of:"))
        [ Node (Q Ask And (Just $ Pre "both"))
          [ Node (Q View (Simply "walk") Nothing) []
          , Node (Q Ask (Simply "run")  Nothing) []
          ]
        , Node (Q Ask Or (Just $ Pre "either"))
          [ Node (Q View (Simply "eat") Nothing) []
          , Node (Q Ask (Simply "drink") Nothing) [] ] ]

    it "should demand walk even when Run=True, Drink=True" $ do
      rlv mustDance (Map.fromList [("walk",  Left  $ Just True)
                                  ,("run",   Right $ Just True)
                                  ,("eat",   Left  $ Just True)
                                  ,("drink", Right $ Just True)])
        `shouldBe`
        Node (Q Ask And (Just $ Pre "three of:"))
        [ Node (Q Ask And (Just $ Pre "both"))
          [ Node (Q Ask (Simply "walk") Nothing) []
          , Node (Q View (Simply "run")  Nothing) []
          ]
        , Node (Q View Or (Just $ Pre "either"))
          [ Node (Q Hide (Simply "eat") Nothing) []
          , Node (Q View (Simply "drink") Nothing) [] ] ]

    it "should demand walk even when Run=True, Eat=True" $ do
      rlv mustDance (Map.fromList [("walk",  Left  $ Just True)
                                  ,("run",   Right $ Just True)
                                  ,("eat",   Right $ Just True)
                                  ,("drink", Left  $ Just True)])
        `shouldBe`
        Node (Q Ask And (Just $ Pre "three of:"))
        [ Node (Q Ask And (Just $ Pre "both"))
          [ Node (Q Ask (Simply "walk") Nothing) []
          , Node (Q View (Simply "run")  Nothing) []
          ]
        , Node (Q View Or (Just $ Pre "either"))
          [ Node (Q View (Simply "eat") Nothing) []
          , Node (Q Hide (Simply "drink") Nothing) [] ] ]

    it "should demand walk even when Run=True, Eat=True, Drink=True" $ do
      rlv mustDance (Map.fromList [("walk",  Left  $ Just True)
                                  ,("run",   Right $ Just True)
                                  ,("eat",   Right $ Just True)
                                  ,("drink", Right $ Just True)])
        `shouldBe`
        Node (Q Ask And (Just $ Pre "three of:"))
        [ Node (Q Ask And (Just $ Pre "both"))
          [ Node (Q Ask (Simply "walk") Nothing) []
          , Node (Q View (Simply "run")  Nothing) []
          ]
        , Node (Q View Or (Just $ Pre "either"))
          [ Node (Q View (Simply "eat") Nothing) []
          , Node (Q View (Simply "drink") Nothing) [] ] ]

    it "should show all when all true" $ do
      rlv mustDance (Map.fromList [("walk",  Right $ Just True)
                                  ,("run",   Right $ Just True)
                                  ,("eat",   Right $ Just True)
                                  ,("drink", Right $ Just True)])
        `shouldBe`
        Node (Q View And (Just $ Pre "three of:"))
        [ Node (Q View And (Just $ Pre "both"))
          [ Node (Q View (Simply "walk") Nothing) []
          , Node (Q View (Simply "run")  Nothing) []
          ]
        , Node (Q View Or (Just $ Pre "either"))
          [ Node (Q View (Simply "eat") Nothing) []
          , Node (Q View (Simply "drink") Nothing) [] ] ]



  describe "native2tree / tree2native" $ do
    it "should round-trip mustSing" $ do
      tree2native (native2tree mustSing) `shouldBe` mustSing
    it "should round-trip mustDance" $ do
      tree2native (native2tree mustDance) `shouldBe` mustDance

type SingLabel = String

mustSing :: Item SingLabel
mustSing =
  All (Pre "both")
  [ Leaf "walk"
  , Any (Pre "either")
    [ Leaf "eat"
    , Leaf "drink" ] ]

mustDance :: Item SingLabel
mustDance =
  All (Pre "three of:")
  [ All (Pre "both")
    [ Leaf "walk"
    , Leaf "run" ]
  , Any (Pre "either")
    [ Leaf "eat"
    , Leaf "drink" ] ]

