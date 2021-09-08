
import Test.Hspec
import AnyAll.Types
import AnyAll.Relevance
import qualified Data.Map.Strict as Map

type SingLabel = String

main :: IO ()
main = hspec $ do
  let rlv item optimism marking = relevant marking optimism item
  describe "with mustSing," $ do
    it "should ask for confirmation of assumptions, even if true" $ do
      rlv mustSing Ask (Map.fromList [("walk",  Left $ Just True)
                                     ,("eat",   Left $ Just True)
                                     ,("drink", Left $ Just True)])
        `shouldBe` Map.fromList [("drink",Ask),("eat",Ask),("walk",Ask)]

    it "should ask for confirmation of assumptions, even if false" $ do
      rlv mustSing Ask (Map.fromList [("walk",  Left $ Just False)
                                     ,("eat",   Left $ Just True)
                                     ,("drink", Left $ Just True)])
        `shouldBe` Map.fromList [("drink",Ask),("eat",Ask),("walk",Ask)]

    it "should ask for confirmation of assumptions, when Nothing" $ do
      rlv mustSing Ask (Map.fromList [("walk",  Left   Nothing)
                                     ,("eat",   Left $ Just True)
                                     ,("drink", Left $ Just True)])
        `shouldBe` Map.fromList [("drink",Ask),("eat",Ask),("walk",Ask)]

    it "should consider a Walk=R False to be dispositive" $ do
      flip dispositive mustSing (Map.fromList [("walk",  Right $ Just False)
                                              ,("eat",   Left $ Just True)
                                              ,("drink", Left $ Just True)])
        `shouldBe` True

    it "should consider a Walk=R True, Eat=R True to be dispositive" $ do
      flip dispositive mustSing (Map.fromList [("walk",  Right $ Just True)
                                              ,("eat",   Right $ Just True)
                                              ,("drink", Left $ Just True)])
        `shouldBe` True

    it "should short-circuit a confirmed False in an And list" $ do
      rlv mustSing Ask (Map.fromList [("walk",  Right $ Just False)
                                     ,("eat",   Left $ Just True)
                                     ,("drink", Left $ Just True)])
        `shouldBe` Map.fromList [("drink",Hide),("eat",Hide),("walk",View)]

    it "should nest given a confirmed True in an And list" $ do
      rlv mustSing Ask (Map.fromList [("walk",  Right $ Just True)
                                     ,("eat",   Left $ Just True)
                                     ,("drink", Left $ Just True)])
        `shouldBe` Map.fromList [("drink",Ask),("eat",Ask),("walk",View)]

    it "should stop given a confirmed Eat  =True in an Or list" $ do
      rlv mustSing Ask (Map.fromList [("walk",  Right $ Just True)
                                     ,("eat",   Right $ Just True)
                                     ,("drink", Left $ Just True)])
        `shouldBe` Map.fromList [("drink",Hide),("eat",View),("walk",View)]

    it "should stop given a confirmed Drink=True in an Or list" $ do
      rlv mustSing Ask (Map.fromList [("walk",  Right $ Just True)
                                     ,("eat",   Left  $ Just True)
                                     ,("drink", Right $ Just True)])
        `shouldBe` Map.fromList [("drink",View),("eat",Hide),("walk",View)]

    it "should demand walk even when Drink=True" $ do
      rlv mustSing Ask (Map.fromList [("walk",  Left  $ Just True)
                                     ,("eat",   Left  $ Just True)
                                     ,("drink", Right $ Just True)])
        `shouldBe` Map.fromList [("drink",View),("eat",Hide),("walk",Ask)]

  describe "with mustDance," $ do
    it "should ask for everything when nothing is known" $ do
      rlv mustDance Ask (Map.fromList [("walk",  Left Nothing)
                                      ,("run",   Left Nothing)
                                      ,("eat",   Left Nothing)
                                      ,("drink", Left Nothing)])
        `shouldBe` Map.fromList [("drink",Ask),("eat",Ask),("walk",Ask),("run",Ask)]

    it "should ask for everything when everything is assumed true" $ do
      rlv mustDance Ask (Map.fromList [("walk",  Left (Just True))
                                      ,("run",   Left (Just True))
                                      ,("eat",   Left (Just True))
                                      ,("drink", Left (Just True))])
        `shouldBe` Map.fromList [("drink",Ask),("eat",Ask),("walk",Ask),("run",Ask)]

    it "should ask for everything when everything is assumed false" $ do
      rlv mustDance Ask (Map.fromList [("walk",  Left (Just False))
                                      ,("run",   Left (Just False))
                                      ,("eat",   Left (Just False))
                                      ,("drink", Left (Just False))])
        `shouldBe` Map.fromList [("drink",Ask),("eat",Ask),("walk",Ask),("run",Ask)]

    it "should handle a Walk=False by stopping" $ do
      rlv mustDance Ask (Map.fromList [("walk",  Right (Just False))
                                      ,("run",   Left (Just False))
                                      ,("eat",   Left (Just False))
                                      ,("drink", Left (Just False))])
        `shouldBe` Map.fromList [("drink",Hide),("eat",Hide),("walk",View),("run",Hide)]

    it "should handle a Run=False by stopping" $ do
      rlv mustDance Ask (Map.fromList [("walk",  Left (Just False))
                                      ,("run",   Right (Just False))
                                      ,("eat",   Left (Just False))
                                      ,("drink", Left (Just False))])
        `shouldBe` Map.fromList [("drink",Hide),("eat",Hide),("walk",Hide),("run",View)]

    it "should handle a Walk=True by remaining curious" $ do
      rlv mustDance Ask (Map.fromList [("walk",  Right (Just True))
                                      ,("run",   Left (Just False))
                                      ,("eat",   Left (Just False))
                                      ,("drink", Left (Just False))])
        `shouldBe` Map.fromList [("drink",Ask),("eat",Ask),("walk",View),("run",Ask)]

    it "should handle a Walk=True,Eat=False by remaining curious" $ do
      rlv mustDance Ask (Map.fromList [("walk",  Right (Just True))
                                      ,("run",   Left (Just False))
                                      ,("eat",   Right (Just False))
                                      ,("drink", Left (Just False))])
        `shouldBe` Map.fromList [("drink",Ask),("eat",View),("walk",View),("run",Ask)]



    it "should demand walk even when Run=True, Drink=True" $ do
      rlv mustDance Ask (Map.fromList [("walk",  Left  $ Just True)
                                      ,("run",   Right $ Just True)
                                      ,("eat",   Left  $ Just True)
                                      ,("drink", Right $ Just True)])
        `shouldBe` Map.fromList [("drink",View),("eat",Hide),("walk",Ask),("run",View)]



  describe "native2tree / tree2native" $ do
    it "should round-trip mustSing" $ do
      tree2native (native2tree mustSing) `shouldBe` mustSing
    it "should round-trip mustDance" $ do
      tree2native (native2tree mustDance) `shouldBe` mustDance


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

