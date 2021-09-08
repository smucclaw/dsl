
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
    it "should demand walk even when Run=True, Drink=True" $ do
      rlv mustDance Ask (Map.fromList [("walk",  Left  $ Just True)
                                      ,("run",   Right $ Just True)
                                      ,("eat",   Left  $ Just True)
                                      ,("drink", Right $ Just True)])
        `shouldBe` Map.fromList [("drink",View),("eat",Hide),("walk",Ask),("run",View)]


mustSing :: Item SingLabel
mustSing =
  All (Pre "both")
  [ Leaf "walk"
  , Any (Pre "either")
    [ Leaf "eat"
    , Leaf "drink" ] ]

mustDance :: Item SingLabel
mustDance =
  All (Pre "all three")
  [ All (Pre "both")
    [ Leaf "walk"
    , Leaf "run" ]
  , Any (Pre "either")
    [ Leaf "eat"
    , Leaf "drink" ] ]

