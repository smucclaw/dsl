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
        Node (Q View And (Just $ Pre "both") ln)
        [ Node (Q Ask (Simply "walk") Nothing lt) []
        , Node (Q View Or (Just $ Pre "either") ln)
          [ Node (Q Ask (Simply "eat") Nothing lt) []
          , Node (Q Ask (Simply "drink") Nothing lt) []
          ]
        ]

    it "should ask for confirmation of assumptions, even if false" $ do
      rlv mustSing (Map.fromList [("walk",  Left $ Just False)
                                 ,("eat",   Left $ Just True)
                                 ,("drink", Left $ Just True)])
        `shouldBe`
        Node (Q View And (Just $ Pre "both") ln)
        [ Node (Q Ask (Simply "walk") Nothing lf) []
        , Node (Q View Or (Just $ Pre "either") ln)
          [ Node (Q Ask (Simply "eat") Nothing lt) []
          , Node (Q Ask (Simply "drink") Nothing lt) []
          ]
        ]

    it "should ask for confirmation of assumptions, when Nothing" $ do
      rlv mustSing (Map.fromList [("walk",  Left   Nothing)
                                 ,("eat",   Left $ Just True)
                                 ,("drink", Left $ Just True)])
        `shouldBe`
        Node (Q View And (Just $ Pre "both") ln)
        [ Node (Q Ask (Simply "walk") Nothing ln) []
        , Node (Q View Or (Just $ Pre "either") ln)
          [ Node (Q Ask (Simply "eat") Nothing lt) []
          , Node (Q Ask (Simply "drink") Nothing lt) [] ] ]

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
        Node (Q View And (Just $ Pre "both") lf)
        [ Node (Q View (Simply "walk") Nothing rf) []
        , Node (Q Hide Or (Just $ Pre "either") ln)
          [ Node (Q Hide (Simply "eat") Nothing lt) []
          , Node (Q Hide (Simply "drink") Nothing lt) [] ] ]

    it "given a confirmed True in an And list, should recurse into the eat/drink limb" $ do
      rlv mustSing (Map.fromList [("walk",  Right $ Just True)
                                 ,("eat",   Left $ Just True)
                                 ,("drink", Left $ Just True)])
        `shouldBe`
        Node (Q View And (Just $ Pre "both") ln)
        [ Node (Q View (Simply "walk") Nothing rt) []
        , Node (Q View Or (Just $ Pre "either") ln)
          [ Node (Q Ask (Simply "eat") Nothing lt) []
          , Node (Q Ask (Simply "drink") Nothing lt) [] ] ]

    it "should stop given a confirmed Eat  =True in an Or list" $ do
      rlv mustSing (Map.fromList [("walk",  Right $ Just True)
                                 ,("eat",   Right $ Just True)
                                 ,("drink", Left $ Just True)])
        `shouldBe`
        Node (Q View And (Just $ Pre "both") lt)
        [ Node (Q View (Simply "walk") Nothing rt) []
        , Node (Q View Or (Just $ Pre "either") lt)
          [ Node (Q View (Simply "eat") Nothing rt) []
          , Node (Q Hide (Simply "drink") Nothing lt) [] ] ]

    it "should stop given a confirmed Drink=True in an Or list" $ do
      rlv mustSing (Map.fromList [("walk",  Right $ Just True)
                                 ,("eat",   Left  $ Just True)
                                 ,("drink", Right $ Just True)])
        `shouldBe`
        Node (Q View And (Just $ Pre "both") lt)
        [ Node (Q View (Simply "walk") Nothing rt) []
        , Node (Q View Or (Just $ Pre "either") lt)
          [ Node (Q Hide (Simply "eat") Nothing lt) []
          , Node (Q View (Simply "drink") Nothing rt) [] ] ]

    it "should demand walk even when Drink=True" $ do
      rlv mustSing (Map.fromList [("walk",  Left  $ Just True)
                                 ,("eat",   Left  $ Just True)
                                 ,("drink", Right $ Just True)])
        `shouldBe`
        Node (Q View And (Just $ Pre "both") ln)
        [ Node (Q Ask (Simply "walk") Nothing lt) []
        , Node (Q View Or (Just $ Pre "either") lt)
          [ Node (Q Hide (Simply "eat") Nothing lt) []
          , Node (Q View (Simply "drink") Nothing rt) [] ] ]

  describe "with mustDance," $ do
    it "should ask for everything when nothing is known" $ do
      rlv mustDance (Map.fromList [("walk",  Left Nothing)
                                  ,("run",   Left Nothing)
                                  ,("eat",   Left Nothing)
                                  ,("drink", Left Nothing)])
        `shouldBe`
        Node (Q View And (Just $ Pre "three of:") ln)
        [ Node (Q View And (Just $ Pre "both") ln)
          [ Node (Q Ask (Simply "walk") Nothing ln) []
          , Node (Q Ask (Simply "run")  Nothing ln) []
          ]
        , Node (Q View Or (Just $ Pre "either") ln)
          [ Node (Q Ask (Simply "eat") Nothing ln) []
          , Node (Q Ask (Simply "drink") Nothing ln) [] ] ]

    it "should ask for everything when everything is assumed true" $ do
      rlv mustDance (Map.fromList [("walk",  Left (Just True))
                                  ,("run",   Left (Just True))
                                  ,("eat",   Left (Just True))
                                  ,("drink", Left (Just True))])
        `shouldBe`
        Node (Q View And (Just $ Pre "three of:") ln)
        [ Node (Q View And (Just $ Pre "both") ln)
          [ Node (Q Ask (Simply "walk") Nothing lt) []
          , Node (Q Ask (Simply "run")  Nothing lt) []
          ]
        , Node (Q View Or (Just $ Pre "either") ln)
          [ Node (Q Ask (Simply "eat") Nothing lt) []
          , Node (Q Ask (Simply "drink") Nothing lt) [] ] ]

    it "should ask for everything when everything is assumed false" $ do
      rlv mustDance (Map.fromList [("walk",  Left (Just False))
                                  ,("run",   Left (Just False))
                                  ,("eat",   Left (Just False))
                                  ,("drink", Left (Just False))])
        `shouldBe`
        Node (Q View And (Just $ Pre "three of:") ln)
        [ Node (Q View And (Just $ Pre "both") ln)
          [ Node (Q Ask (Simply "walk") Nothing lf) []
          , Node (Q Ask (Simply "run")  Nothing lf) []
          ]
        , Node (Q View Or (Just $ Pre "either") ln)
          [ Node (Q Ask (Simply "eat") Nothing lf) []
          , Node (Q Ask (Simply "drink") Nothing lf) [] ] ]

    it "should handle a Walk=False by stopping" $ do
      rlv mustDance (Map.fromList [("walk",  Right (Just False))
                                  ,("run",   Left (Just False))
                                  ,("eat",   Left (Just False))
                                  ,("drink", Left (Just False))])
        `shouldBe`
        Node (Q View And (Just $ Pre "three of:") lf)
        [ Node (Q View And (Just $ Pre "both") lf)
          [ Node (Q View (Simply "walk") Nothing rf) []
          , Node (Q Hide (Simply "run")  Nothing lf) []
          ]
        , Node (Q Hide Or (Just $ Pre "either") ln)
          [ Node (Q Hide (Simply "eat") Nothing lf) []
          , Node (Q Hide (Simply "drink") Nothing lf) [] ] ]

    it "should handle a Run=False by stopping (in future this will depend on displaypref" $ do
      rlv mustDance (Map.fromList [("walk",  Left (Just False))
                                  ,("run",   Right (Just False))
                                  ,("eat",   Right (Just True))
                                  ,("drink", Right (Just True))])
        `shouldBe`
        Node (Q View And (Just $ Pre "three of:") lf)
        [ Node (Q View And (Just $ Pre "both") lf)
          [ Node (Q Hide (Simply "walk") Nothing lf) []
          , Node (Q View (Simply "run")  Nothing rf) []
          ]
        , Node (Q Hide Or (Just $ Pre "either") lt)
          [ Node (Q View (Simply "eat") Nothing rt) []
          , Node (Q View (Simply "drink") Nothing rt) [] ] ]

    it "should handle a Walk=True by remaining curious about the run and the food" $ do
      rlv mustDance (Map.fromList [("walk",  Right (Just True))
                                  ,("run",   Left (Just False))
                                  ,("eat",   Left (Just False))
                                  ,("drink", Left (Just False))])
        `shouldBe`
        Node (Q View And (Just $ Pre "three of:") ln)
        [ Node (Q View And (Just $ Pre "both") ln)
          [ Node (Q View (Simply "walk") Nothing rt) []
          , Node (Q Ask (Simply "run")  Nothing lf) []
          ]
        , Node (Q View Or (Just $ Pre "either") ln)
          [ Node (Q Ask (Simply "eat") Nothing lf) []
          , Node (Q Ask (Simply "drink") Nothing lf) [] ] ]

    it "should handle a Walk=True,Eat=False by remaining curious about the run and the drink" $ do
      rlv mustDance (Map.fromList [("walk",  Right (Just True))
                                  ,("run",   Left (Just False))
                                  ,("eat",   Right (Just False))
                                  ,("drink", Left (Just False))])
        `shouldBe`
        Node (Q View And (Just $ Pre "three of:") ln)
        [ Node (Q View And (Just $ Pre "both") ln)
          [ Node (Q View (Simply "walk") Nothing rt) []
          , Node (Q Ask (Simply "run")  Nothing lf) []
          ]
        , Node (Q View Or (Just $ Pre "either") ln)
          [ Node (Q View (Simply "eat") Nothing rf) []
          , Node (Q Ask (Simply "drink") Nothing lf) [] ] ]

    it "should demand walk even when Run=True, Drink=True" $ do
      rlv mustDance (Map.fromList [("walk",  Left  $ Just True)
                                  ,("run",   Right $ Just True)
                                  ,("eat",   Left  $ Just True)
                                  ,("drink", Right $ Just True)])
        `shouldBe`
        Node (Q View And (Just $ Pre "three of:") ln)
        [ Node (Q View And (Just $ Pre "both") ln)
          [ Node (Q Ask (Simply "walk") Nothing lt) []
          , Node (Q View (Simply "run")  Nothing rt) []
          ]
        , Node (Q View Or (Just $ Pre "either") lt)
          [ Node (Q Hide (Simply "eat") Nothing lt) []
          , Node (Q View (Simply "drink") Nothing rt) [] ] ]

    it "should demand walk even when Run=True, Eat=True" $ do
      rlv mustDance (Map.fromList [("walk",  Left  $ Just True)
                                  ,("run",   Right $ Just True)
                                  ,("eat",   Right $ Just True)
                                  ,("drink", Left  $ Just True)])
        `shouldBe`
        Node (Q View And (Just $ Pre "three of:") ln)
        [ Node (Q View And (Just $ Pre "both") ln)
          [ Node (Q Ask (Simply "walk") Nothing lt) []
          , Node (Q View (Simply "run")  Nothing rt) []
          ]
        , Node (Q View Or (Just $ Pre "either") lt)
          [ Node (Q View (Simply "eat") Nothing rt) []
          , Node (Q Hide (Simply "drink") Nothing lt) [] ] ]

    it "should demand walk even when Run=True, Eat=True, Drink=True" $ do
      rlv mustDance (Map.fromList [("walk",  Left  $ Just True)
                                  ,("run",   Right $ Just True)
                                  ,("eat",   Right $ Just True)
                                  ,("drink", Right $ Just True)])
        `shouldBe`
        Node (Q View And (Just $ Pre "three of:") ln)
        [ Node (Q View And (Just $ Pre "both") ln)
          [ Node (Q Ask (Simply "walk") Nothing lt) []
          , Node (Q View (Simply "run")  Nothing rt) []
          ]
        , Node (Q View Or (Just $ Pre "either") lt)
          [ Node (Q View (Simply "eat") Nothing rt) []
          , Node (Q View (Simply "drink") Nothing rt) [] ] ]

    it "should show all when all true" $ do
      rlv mustDance (Map.fromList [("walk",  Right $ Just True)
                                  ,("run",   Right $ Just True)
                                  ,("eat",   Right $ Just True)
                                  ,("drink", Right $ Just True)])
        `shouldBe`
        Node (Q View And (Just $ Pre "three of:") lt)
        [ Node (Q View And (Just $ Pre "both") lt)
          [ Node (Q View (Simply "walk") Nothing rt) []
          , Node (Q View (Simply "run")  Nothing rt) []
          ]
        , Node (Q View Or (Just $ Pre "either") lt)
          [ Node (Q View (Simply "eat") Nothing rt) []
          , Node (Q View (Simply "drink") Nothing rt) [] ] ]


  describe "native2tree / tree2native" $ do
    it "should round-trip mustSing" $ do
      tree2native (native2tree mustSing) `shouldBe` mustSing
    it "should round-trip mustDance" $ do
      tree2native (native2tree mustDance) `shouldBe` mustDance

  describe "JSON conversion" $ do
    it "should encode Default left just true" $ do
      asJSONDefault (Default (Left (Just True))) `shouldBe` "{\"getDefault\":{\"Left\":true}}"
    it "should encode Default left just false" $ do
      asJSONDefault (Default (Left (Just False))) `shouldBe` "{\"getDefault\":{\"Left\":false}}"
    it "should encode Default left nothing" $ do
      asJSONDefault (Default (Left (Nothing :: Maybe Bool))) `shouldBe` "{\"getDefault\":{\"Left\":null}}"
    it "should encode Q mustSing" $ do
      asJSON (rlv mustSing (Map.fromList [("walk",  Left  $ Just True)
                                         ,("eat",   Left  $ Just True)
                                         ,("drink", Right $ Just True)]))
      `shouldBe` "[{\"shouldView\":\"View\",\"andOr\":{\"tag\":\"And\"},\"prePost\":{\"tag\":\"Pre\",\"contents\":\"both\"},\"mark\":{\"getDefault\":{\"Left\":null}}},[[{\"shouldView\":\"Ask\",\"andOr\":{\"tag\":\"Simply\",\"contents\":\"walk\"},\"prePost\":null,\"mark\":{\"getDefault\":{\"Left\":true}}},[]],[{\"shouldView\":\"View\",\"andOr\":{\"tag\":\"Or\"},\"prePost\":{\"tag\":\"Pre\",\"contents\":\"either\"},\"mark\":{\"getDefault\":{\"Left\":true}}},[[{\"shouldView\":\"Hide\",\"andOr\":{\"tag\":\"Simply\",\"contents\":\"eat\"},\"prePost\":null,\"mark\":{\"getDefault\":{\"Left\":true}}},[]],[{\"shouldView\":\"View\",\"andOr\":{\"tag\":\"Simply\",\"contents\":\"drink\"},\"prePost\":null,\"mark\":{\"getDefault\":{\"Right\":true}}},[]]]]]]"
    it "should decode Q mustSing" $ do
      fromJSON   "[{\"shouldView\":\"View\",\"andOr\":{\"tag\":\"And\"},\"prePost\":{\"tag\":\"Pre\",\"contents\":\"both\"},\"mark\":{\"getDefault\":{\"Left\":null}}},[[{\"shouldView\":\"Ask\",\"andOr\":{\"tag\":\"Simply\",\"contents\":\"walk\"},\"prePost\":null,\"mark\":{\"getDefault\":{\"Left\":true}}},[]],[{\"shouldView\":\"View\",\"andOr\":{\"tag\":\"Or\"},\"prePost\":{\"tag\":\"Pre\",\"contents\":\"either\"},\"mark\":{\"getDefault\":{\"Left\":true}}},[[{\"shouldView\":\"Hide\",\"andOr\":{\"tag\":\"Simply\",\"contents\":\"eat\"},\"prePost\":null,\"mark\":{\"getDefault\":{\"Left\":true}}},[]],[{\"shouldView\":\"View\",\"andOr\":{\"tag\":\"Simply\",\"contents\":\"drink\"},\"prePost\":null,\"mark\":{\"getDefault\":{\"Right\":true}}},[]]]]]]"
      `shouldBe` (Just (rlv mustSing (Map.fromList [("walk",  Left  $ Just True)
                                                   ,("eat",   Left  $ Just True)
                                                   ,("drink", Right $ Just True)])))
    it "should encode Q mustNot" $ do
      asJSON (rlv mustNot (Map.fromList [("walk",  Left  $ Just True)
                                        ,("eat",   Left  $ Just True)]))
        `shouldBe` "[{\"shouldView\":\"View\",\"andOr\":{\"tag\":\"And\"},\"prePost\":{\"tag\":\"Pre\",\"contents\":\"both\"},\"mark\":{\"getDefault\":{\"Left\":null}}},[[{\"shouldView\":\"Ask\",\"andOr\":{\"tag\":\"Simply\",\"contents\":\"walk\"},\"prePost\":null,\"mark\":{\"getDefault\":{\"Left\":true}}},[]],[{\"shouldView\":\"View\",\"andOr\":{\"tag\":\"Neg\"},\"prePost\":null,\"mark\":{\"getDefault\":{\"Left\":null}}},[[{\"shouldView\":\"Ask\",\"andOr\":{\"tag\":\"Simply\",\"contents\":\"eat\"},\"prePost\":null,\"mark\":{\"getDefault\":{\"Left\":true}}},[]]]]]]"

  -- [{"shouldView":"View"
  --  ,"andOr":{"tag":"And"}
  --  ,"prePost":{"tag":"Pre"
  --             ,"contents":"both"}
  --  ,"mark":{"getDefault":{"Left":null}}}
  -- ,[[{"shouldView":"Ask"
  --    ,"andOr":{"tag":"Simply"
  --             ,"contents":"walk"}
  --    ,"prePost":null
  --    ,"mark":{"getDefault":{"Left":true}}}
  --   ,[]]
  --  ,[{"shouldView":"View"
  --    ,"andOr":{"tag":"Neg"}
  --    ,"prePost":null
  --    ,"mark":{"getDefault":{"Left":null}}}
  --   ,[[{"shouldView":"Ask"
  --      ,"andOr":{"tag":"Simply"
  --               ,"contents":"eat"}
  --      ,"prePost":null
  --      ,"mark":{"getDefault":{"Left":true}}}
  --     ,[]]]]]]

    it "should roundtrip Q mustNot" $ do
      let qNot = rlv mustNot (Map.fromList [("walk",  Left  $ Just True)
                                           ,("eat",   Left  $ Just True)])
      fromJust (fromJSON (asJSON qNot)) `shouldBe` qNot

type SingLabel = TL.Text

mustSing :: Item SingLabel
mustSing =
  All (Pre "both")
  [ Leaf "walk"
  , Any (Pre "either")
    [ Leaf "eat"
    , Leaf "drink" ] ]

mustNot :: Item SingLabel
mustNot =
  All (Pre "both")
  [ Leaf "walk"
  , Not (Leaf "eat") ]

mustDance :: Item SingLabel
mustDance =
  All (Pre "three of:")
  [ All (Pre "both")
    [ Leaf "walk"
    , Leaf "run" ]
  , Any (Pre "either")
    [ Leaf "eat"
    , Leaf "drink" ] ]

