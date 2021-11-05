{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import AnyAll.Types
import AnyAll.Relevance
import qualified Data.Map.Strict as Map
import Data.Tree
import Data.Maybe (fromJust)
import Data.Text.Lazy as TL

ln, lt, lf, rt, rf, rn :: Default Bool
ln = Default $ Left Nothing
lt = Default $ Left $ Just True
lf = Default $ Left $ Just False
rt = Default $ Right $ Just True
rf = Default $ Right $ Just False
rn = Default $ Right Nothing

main :: IO ()
main = hspec $ do
  let markup m = Marking (Default <$> m)
      rlv item marking = relevant Hard DPNormal (markup marking) Nothing item
  describe "with mustSing," $ do
    it "should ask for confirmation of assumptions, even if true" $ do
      rlv mustSing (Map.fromList [("walk",  Left $ Just True)
                                 ,("eat",   Left $ Just True)
                                 ,("drink", Left $ Just True)])
        `shouldBe`
        Node (Q View And ln)
        [ Node (Q Ask (Simply "walk") lt) []
        , Node (Q View Or ln)
          [ Node (Q Ask (Simply "eat") lt) []
          , Node (Q Ask (Simply "drink") lt) []
          ]
        ]

    it "should ask for confirmation of assumptions, even if false" $ do
      rlv mustSing (Map.fromList [("walk",  Left $ Just False)
                                 ,("eat",   Left $ Just True)
                                 ,("drink", Left $ Just True)])
        `shouldBe`
        Node (Q View And ln)
        [ Node (Q Ask (Simply "walk") lf) []
        , Node (Q View Or ln)
          [ Node (Q Ask (Simply "eat") lt) []
          , Node (Q Ask (Simply "drink") lt) []
          ]
        ]

    it "should ask for confirmation of assumptions, when Nothing" $ do
      rlv mustSing (Map.fromList [("walk",  Left   Nothing)
                                 ,("eat",   Left $ Just True)
                                 ,("drink", Left $ Just True)])
        `shouldBe`
        Node (Q View And ln)
        [ Node (Q Ask (Simply "walk") ln) []
        , Node (Q View Or ln)
          [ Node (Q Ask (Simply "eat") lt) []
          , Node (Q Ask (Simply "drink") lt) [] ] ]

    it "when Hard, should consider a Walk=R False to be dispositive" $ do
      flip (dispositive Hard) mustSing (markup $ Map.fromList [("walk",  Right $ Just False)
                                                              ,("eat",   Left $ Just True)
                                                              ,("drink", Left $ Just True)])
        `shouldBe` [Leaf "walk"]

    it "when Soft, should consider a Walk=L False to be dispositive" $ do
      flip (dispositive Soft) mustSing (markup $ Map.fromList [("walk",  Left $ Just False)
                                                              ,("eat",   Left $ Just True)
                                                              ,("drink", Left $ Just True)])
        `shouldBe` [Leaf "walk"]

    it "when Soft, should consider a Walk=L True, drink=L True to be dispositive" $ do
      flip (dispositive Soft) mustSing (markup $ Map.fromList [("walk",  Left $ Just True)
                                                              ,("eat",   Left $ Nothing)
                                                              ,("drink", Left $ Just True)])
        `shouldBe` [Leaf "walk", Leaf "drink"]

    it "should consider a Walk=R True, Eat=R True to be dispositive" $ do
      flip (dispositive Hard) mustSing (markup $ Map.fromList [("walk",  Right $ Just True)
                                                              ,("eat",   Right $ Just True)
                                                              ,("drink", Left $ Just True)])
        `shouldBe` [Leaf "walk", Leaf "eat"]

    it "should short-circuit a confirmed False in an And list" $ do
      rlv mustSing (Map.fromList [("walk",  Right $ Just False)
                                 ,("eat",   Left $ Just True)
                                 ,("drink", Left $ Just True)])
        `shouldBe`
        Node (Q View And lf)
        [ Node (Q View (Simply "walk") rf) []
        , Node (Q Hide Or ln)
          [ Node (Q Hide (Simply "eat") lt) []
          , Node (Q Hide (Simply "drink") lt) [] ] ]

    it "given a confirmed True in an And list, should recurse into the eat/drink limb" $ do
      rlv mustSing (Map.fromList [("walk",  Right $ Just True)
                                 ,("eat",   Left $ Just True)
                                 ,("drink", Left $ Just True)])
        `shouldBe`
        Node (Q View And ln)
        [ Node (Q View (Simply "walk") rt) []
        , Node (Q View Or ln)
          [ Node (Q Ask (Simply "eat") lt) []
          , Node (Q Ask (Simply "drink") lt) [] ] ]

    it "should stop given a confirmed Eat  =True in an Or list" $ do
      rlv mustSing (Map.fromList [("walk",  Right $ Just True)
                                 ,("eat",   Right $ Just True)
                                 ,("drink", Left $ Just True)])
        `shouldBe`
        Node (Q View And lt)
        [ Node (Q View (Simply "walk") rt) []
        , Node (Q View Or lt)
          [ Node (Q View (Simply "eat") rt) []
          , Node (Q Hide (Simply "drink") lt) [] ] ]

    it "should stop given a confirmed Drink=True in an Or list" $ do
      rlv mustSing (Map.fromList [("walk",  Right $ Just True)
                                 ,("eat",   Left  $ Just True)
                                 ,("drink", Right $ Just True)])
        `shouldBe`
        Node (Q View And lt)
        [ Node (Q View (Simply "walk") rt) []
        , Node (Q View Or lt)
          [ Node (Q Hide (Simply "eat") lt) []
          , Node (Q View (Simply "drink") rt) [] ] ]

    it "should demand walk even when Drink=True" $ do
      rlv mustSing (Map.fromList [("walk",  Left  $ Just True)
                                 ,("eat",   Left  $ Just True)
                                 ,("drink", Right $ Just True)])
        `shouldBe`
        Node (Q View And ln)
        [ Node (Q Ask (Simply "walk") lt) []
        , Node (Q View Or lt)
          [ Node (Q Hide (Simply "eat") lt) []
          , Node (Q View (Simply "drink") rt) [] ] ]

  describe "with mustDance," $ do
    it "should ask for everything when nothing is known" $ do
      rlv mustDance (Map.fromList [("walk",  Left Nothing)
                                  ,("run",   Left Nothing)
                                  ,("eat",   Left Nothing)
                                  ,("drink", Left Nothing)])
        `shouldBe`
        Node (Q View And ln)
        [ Node (Q View And ln)
          [ Node (Q Ask (Simply "walk") ln) []
          , Node (Q Ask (Simply "run")  ln) []
          ]
        , Node (Q View Or ln)
          [ Node (Q Ask (Simply "eat") ln) []
          , Node (Q Ask (Simply "drink") ln) [] ] ]

    it "should ask for everything when everything is assumed true" $ do
      rlv mustDance (Map.fromList [("walk",  Left (Just True))
                                  ,("run",   Left (Just True))
                                  ,("eat",   Left (Just True))
                                  ,("drink", Left (Just True))])
        `shouldBe`
        Node (Q View And ln)
        [ Node (Q View And ln)
          [ Node (Q Ask (Simply "walk") lt) []
          , Node (Q Ask (Simply "run")  lt) []
          ]
        , Node (Q View Or ln)
          [ Node (Q Ask (Simply "eat") lt) []
          , Node (Q Ask (Simply "drink") lt) [] ] ]

    it "should ask for everything when everything is assumed false" $ do
      rlv mustDance (Map.fromList [("walk",  Left (Just False))
                                  ,("run",   Left (Just False))
                                  ,("eat",   Left (Just False))
                                  ,("drink", Left (Just False))])
        `shouldBe`
        Node (Q View And ln)
        [ Node (Q View And ln)
          [ Node (Q Ask (Simply "walk")  lf) []
          , Node (Q Ask (Simply "run")   lf) []
          ]
        , Node (Q View Or ln)
          [ Node (Q Ask (Simply "eat")  lf) []
          , Node (Q Ask (Simply "drink") lf) [] ] ]

    it "should handle a Walk=False by stopping" $ do
      rlv mustDance (Map.fromList [("walk",  Right (Just False))
                                  ,("run",   Left (Just False))
                                  ,("eat",   Left (Just False))
                                  ,("drink", Left (Just False))])
        `shouldBe`
        Node (Q View And lf)
        [ Node (Q View And lf)
          [ Node (Q View (Simply "walk")  rf) []
          , Node (Q Hide (Simply "run")   lf) []
          ]
        , Node (Q Hide Or ln)
          [ Node (Q Hide (Simply "eat")  lf) []
          , Node (Q Hide (Simply "drink")  lf) [] ] ]

    it "should handle a Run=False by stopping (in future this will depend on displaypref" $ do
      rlv mustDance (Map.fromList [("walk",  Left (Just False))
                                  ,("run",   Right (Just False))
                                  ,("eat",   Right (Just True))
                                  ,("drink", Right (Just True))])
        `shouldBe`
        Node (Q View And lf)
        [ Node (Q View And lf)
          [ Node (Q Hide (Simply "walk") lf) []
          , Node (Q View (Simply "run")  rf) []
          ]
        , Node (Q Hide Or lt)
          [ Node (Q View (Simply "eat") rt) []
          , Node (Q View (Simply "drink") rt) [] ] ]

    it "should handle a Walk=True by remaining curious about the run and the food" $ do
      rlv mustDance (Map.fromList [("walk",  Right (Just True))
                                  ,("run",   Left (Just False))
                                  ,("eat",   Left (Just False))
                                  ,("drink", Left (Just False))])
        `shouldBe`
        Node (Q View And ln)
        [ Node (Q View And ln)
          [ Node (Q View (Simply "walk") rt) []
          , Node (Q Ask (Simply "run") lf) []
          ]
        , Node (Q View Or ln)
          [ Node (Q Ask (Simply "eat")  lf) []
          , Node (Q Ask (Simply "drink")  lf) [] ] ]

    it "should handle a Walk=True,Eat=False by remaining curious about the run and the drink" $ do
      rlv mustDance (Map.fromList [("walk",  Right (Just True))
                                  ,("run",   Left (Just False))
                                  ,("eat",   Right (Just False))
                                  ,("drink", Left (Just False))])
        `shouldBe`
        Node (Q View And  ln)
        [ Node (Q View And ln)
          [ Node (Q View (Simply "walk") rt) []
          , Node (Q Ask (Simply "run")   lf) []
          ]
        , Node (Q View Or ln)
          [ Node (Q View (Simply "eat")  rf) []
          , Node (Q Ask (Simply "drink")  lf) [] ] ]

    it "should demand walk even when Run=True, Drink=True" $ do
      rlv mustDance (Map.fromList [("walk",  Left  $ Just True)
                                  ,("run",   Right $ Just True)
                                  ,("eat",   Left  $ Just True)
                                  ,("drink", Right $ Just True)])
        `shouldBe`
        Node (Q View And ln)
        [ Node (Q View And ln)
          [ Node (Q Ask (Simply "walk") lt) []
          , Node (Q View (Simply "run")  rt) []
          ]
        , Node (Q View Or lt)
          [ Node (Q Hide (Simply "eat") lt) []
          , Node (Q View (Simply "drink") rt) [] ] ]

    it "should demand walk even when Run=True, Eat=True" $ do
      rlv mustDance (Map.fromList [("walk",  Left  $ Just True)
                                  ,("run",   Right $ Just True)
                                  ,("eat",   Right $ Just True)
                                  ,("drink", Left  $ Just True)])
        `shouldBe`
        Node (Q View And ln)
        [ Node (Q View And ln)
          [ Node (Q Ask (Simply "walk") lt) []
          , Node (Q View (Simply "run")  rt) []
          ]
        , Node (Q View Or lt)
          [ Node (Q View (Simply "eat") rt) []
          , Node (Q Hide (Simply "drink") lt) [] ] ]

    it "should demand walk even when Run=True, Eat=True, Drink=True" $ do
      rlv mustDance (Map.fromList [("walk",  Left  $ Just True)
                                  ,("run",   Right $ Just True)
                                  ,("eat",   Right $ Just True)
                                  ,("drink", Right $ Just True)])
        `shouldBe`
        Node (Q View And ln)
        [ Node (Q View And ln)
          [ Node (Q Ask (Simply "walk") lt) []
          , Node (Q View (Simply "run")  rt) []
          ]
        , Node (Q View Or lt)
          [ Node (Q View (Simply "eat") rt) []
          , Node (Q View (Simply "drink") rt) [] ] ]

    it "should show all when all true" $ do
      rlv mustDance (Map.fromList [("walk",  Right $ Just True)
                                  ,("run",   Right $ Just True)
                                  ,("eat",   Right $ Just True)
                                  ,("drink", Right $ Just True)])
        `shouldBe`
        Node (Q View And lt)
        [ Node (Q View And lt)
          [ Node (Q View (Simply "walk") rt) []
          , Node (Q View (Simply "run")  rt) []
          ]
        , Node (Q View Or lt)
          [ Node (Q View (Simply "eat") rt) []
          , Node (Q View (Simply "drink") rt) [] ] ]


  describe "native2tree / tree2native" $ do
    it "should round-trip mustSing" $ do
      tree2native (native2tree mustSing) `shouldBe` mustSing
    it "should round-trip mustDance" $ do
      tree2native (native2tree mustDance) `shouldBe` mustDance

  describe "JSON conversion" $ do
    it "should encode Default" $ do
      asJSONDefault (Default (Left (Just True))) `shouldBe` "{\"getDefault\":{\"Left\":true}}"
    it "should encode Q mustSing" $ do
      asJSON (rlv mustSing (Map.fromList [("walk",  Left  $ Just True)
                                         ,("eat",   Left  $ Just True)
                                         ,("drink", Right $ Just True)]))
      `shouldBe` "[{\"shouldView\":\"View\",\"andOr\":{\"tag\":\"And\"},\"mark\":{\"getDefault\":{\"Left\":null}}},[[{\"shouldView\":\"Ask\",\"andOr\":{\"tag\":\"Simply\",\"contents\":\"walk\"},\"mark\":{\"getDefault\":{\"Left\":true}}},[]],[{\"shouldView\":\"View\",\"andOr\":{\"tag\":\"Or\"},\"mark\":{\"getDefault\":{\"Left\":true}}},[[{\"shouldView\":\"Hide\",\"andOr\":{\"tag\":\"Simply\",\"contents\":\"eat\"},\"mark\":{\"getDefault\":{\"Left\":true}}},[]],[{\"shouldView\":\"View\",\"andOr\":{\"tag\":\"Simply\",\"contents\":\"drink\"},\"mark\":{\"getDefault\":{\"Right\":true}}},[]]]]]]"
    it "should decode Q mustSing" $ do
      fromJSON "[{\"shouldView\":\"View\",\"andOr\":{\"tag\":\"And\"},\"mark\":{\"getDefault\":{\"Left\":null}}},[[{\"shouldView\":\"Ask\",\"andOr\":{\"tag\":\"Simply\",\"contents\":\"walk\"},\"mark\":{\"getDefault\":{\"Left\":true}}},[]],[{\"shouldView\":\"View\",\"andOr\":{\"tag\":\"Or\"},\"mark\":{\"getDefault\":{\"Left\":true}}},[[{\"shouldView\":\"Hide\",\"andOr\":{\"tag\":\"Simply\",\"contents\":\"eat\"},\"mark\":{\"getDefault\":{\"Left\":true}}},[]],[{\"shouldView\":\"View\",\"andOr\":{\"tag\":\"Simply\",\"contents\":\"drink\"},\"mark\":{\"getDefault\":{\"Right\":true}}},[]]]]]]"
      `shouldBe` (Just (rlv mustSing (Map.fromList [("walk",  Left  $ Just True)
                                                   ,("eat",   Left  $ Just True)
                                                   ,("drink", Right $ Just True)])))
    it "should encode Q mustNot" $ do
      asJSON (rlv mustNot (Map.fromList [("walk",  Left  $ Just True)
                                        ,("eat",   Left  $ Just True)]))
        `shouldBe` "[{\"shouldView\":\"View\",\"andOr\":{\"tag\":\"And\"},\"mark\":{\"getDefault\":{\"Left\":null}}},[[{\"shouldView\":\"Ask\",\"andOr\":{\"tag\":\"Simply\",\"contents\":\"walk\"},\"mark\":{\"getDefault\":{\"Left\":true}}},[]],[{\"shouldView\":\"View\",\"andOr\":{\"tag\":\"Neg\"},\"mark\":{\"getDefault\":{\"Left\":null}}},[[{\"shouldView\":\"Ask\",\"andOr\":{\"tag\":\"Simply\",\"contents\":\"eat\"},\"mark\":{\"getDefault\":{\"Left\":true}}},[]]]]]]"

  -- [{"shouldView":"View"
  --  ,"andOr":{"tag":"And"}
  --  ,"mark":{"getDefault":{"Left":null}}}
  -- ,[[{"shouldView":"Ask"
  --    ,"andOr":{"tag":"Simply"
  --             ,"contents":"walk"}
  --    ,"mark":{"getDefault":{"Left":true}}}
  --   ,[]]
  --  ,[{"shouldView":"View"
  --    ,"andOr":{"tag":"Neg"}
  --    ,"mark":{"getDefault":{"Left":null}}}
  --   ,[[{"shouldView":"Ask"
  --      ,"andOr":{"tag":"Simply"
  --               ,"contents":"eat"}
  --      ,"mark":{"getDefault":{"Left":true}}}
  --     ,[]]]]]]

    it "should roundtrip Q mustNot" $ do
      let qNot = rlv mustNot (Map.fromList [("walk",  Left  $ Just True)
                                           ,("eat",   Left  $ Just True)])
      fromJust (fromJSON (asJSON qNot)) `shouldBe` qNot

type SingLabel = TL.Text

mustSing :: Item SingLabel
mustSing =
  All
  [ Leaf "walk"
  , Any
    [ Leaf "eat"
    , Leaf "drink" ] ]

mustNot :: Item SingLabel
mustNot =
  All
  [ Leaf "walk"
  , Not (Leaf "eat") ]

mustDance :: Item SingLabel
mustDance =
  All
  [ All
    [ Leaf "walk"
    , Leaf "run" ]
  , Any
    [ Leaf "eat"
    , Leaf "drink" ] ]

