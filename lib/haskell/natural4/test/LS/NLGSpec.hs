{-# LANGUAGE OverloadedStrings #-}

module LS.NLGSpec where

import Test.Hspec
import Data.List.NonEmpty (NonEmpty(..))
import LS.NLP.NLG
import LS.NLP.NL4
import Parsing.PDPASpec (expected_pdpadbno1)
import AnyAll( BoolStruct(..), Label(..) )
import Data.Map (fromList, empty)
import LS.Types
import LS.Rule
import PGF (mkCId)

spec :: Spec
spec = do
  env <- runIO $ myNLGEnv rodentsInterp (mkCId "NL4Eng")
  describe "test rodents" $ do
    it "Should return questions about rodent damage" $ do
        let questions = linBStext env $ mkConstraintText env GqPREPOST GqCONSTR rodentsBSR
        questions `shouldBe` All Nothing [Any (Just (Pre "Is the Loss or Damage caused by")) [Leaf "rodents?",Leaf "insects?",Leaf "vermin?",Leaf "birds?"],Not (Any Nothing [All Nothing [Leaf "is Loss or Damage to contents?",Leaf "is Loss or Damage caused by birds?"],All Nothing [Leaf "is Loss or Damage ensuing loss?",Leaf "is Loss or Damage covered?",Not (Any Nothing [Leaf "does any other exclusion apply?",Any (Just (Pre "did an animal cause water to escape from")) [Leaf "a household appliance?",Leaf "a swimming pool?",Leaf "a plumbing, heating, or air conditioning system?"]])]])]

  describe "test not bird" $ do
    it "Should return questions about not bird damage" $ do
        let questions = linBStext env $ mkConstraintText env GqPREPOST GqCONSTR notRodentsBSR
        questions `shouldBe` All Nothing [Any (Just (Pre "Is the Loss or Damage caused by")) [Leaf "rodents?",Leaf "insects?",Leaf "vermin?",Leaf "birds?"],Not (Any Nothing [All Nothing [Leaf "is Loss or Damage to contents?",Leaf "isn't Loss or Damage caused by birds?"],All Nothing [Leaf "is Loss or Damage ensuing loss?",Leaf "is Loss or Damage covered?",Not (Any Nothing [Leaf "does any other exclusion apply?",Any (Just (Pre "did an animal cause water to escape from")) [Leaf "a household appliance?",Leaf "a swimming pool?",Leaf "a plumbing, heating, or air conditioning system?"]])]])]

  describe "test PDPA" $ do
    it "Should return questions about PDPA" $ do
      questions <- ruleQuestions env Nothing (head expected_pdpadbno1)
      questions `shouldBe` [Not (Leaf "is the organisation a public agency?"), Leaf "does the data breach occur on or after 1 Feb 2022?", Leaf "has the organisation become aware that a data breach may have occurred?"]

  describe "test expandRulesForNLG for rodents" $ do
    it "should not change rodentsandvermin.csv" $ do
        let rodentsExpanded = expandRulesForNLG env rodentsRules
        rodentsExpanded `shouldBe` rodentsRules

  envMustSing <- runIO $ myNLGEnv mustsing5Interp (mkCId "NL4Eng")
  describe "test expandRulesForNLG for mustsing5" $ do
    it "should change mustsing5.csv" $ do
        let mustsing5Expanded = expandRulesForNLG envMustSing mustsing5Rules
        mustsing5Expanded `shouldBe` mustsing5ExpandedGold

  envPDPA <- runIO $ myNLGEnv pdpa1withUnexpandedUponInterp (mkCId "NL4Eng")
  describe "test expandRulesForNLG for pdpa1 with added UPON expansion" $ do
    it "should change pdpa1 with added UPON expansion" $ do
        let pdpa1Expanded = expandRulesForNLG envPDPA pdpa1withUnexpandedUpon
        pdpa1Expanded `shouldBe` pdpa1withExpandedUponGold

rodentsRules = [ Hornlike
    { name = [ MTT "Loss or Damage" ]
    , super = Nothing
    , keyword = Decide
    , given = Nothing
    , giveth = Nothing
    , upon = Nothing
    , clauses =
        [ HC
            { hHead = RPBoolStructR
                [ MTT "Loss or Damage" ] RPis
                ( Not
                    ( Leaf
                        ( RPMT
                            [ MTT "Covered" ]
                        )
                    )
                )
            , hBody = Just rodentsBSR }
        ]
    , rlabel = Just
        ( "§"
        , 1
        , "Animal Damage"
        )
    , lsource = Nothing
    , srcref = Just
        ( SrcRef
            { url = "test/examples/rodentsandvermin-latest.csv"
            , short = "test/examples/rodentsandvermin-latest.csv"
            , srcrow = 2
            , srccol = 20
            , version = Nothing
            }
        )
    , defaults = []
    , symtab = []
    }
  ]

mustsing5Rules = [ Regulative
    { subj = Leaf
        (
            ( MTT "Person" :| []
            , Nothing
            ) :| []
        )
    , rkeyword = REvery
    , who = Just
        ( Leaf
            ( RPMT
                [ MTT "Qualifies" ]
            )
        )
    , cond = Nothing
    , deontic = DMust
    , action = Leaf
        (
            ( MTT "sing" :| []
            , Nothing
            ) :| []
        )
    , temporal = Nothing
    , hence = Nothing
    , lest = Nothing
    , rlabel = Nothing
    , lsource = Nothing
    , srcref = Just
        ( SrcRef
            { url = "test/MustSing5.csv"
            , short = "test/MustSing5.csv"
            , srcrow = 2
            , srccol = 1
            , version = Nothing
            }
        )
    , upon = Nothing
    , given = Nothing
    , having = Nothing
    , wwhere = []
    , defaults = []
    , symtab = []
    }
  , Hornlike
    { name =
        [ MTT "drinks" ]
    , super = Nothing
    , keyword = Means
    , given = Nothing
    , giveth = Nothing
    , upon = Nothing
    , clauses =
        [ HC
            { hHead = RPBoolStructR
                        [ MTT "drinks" ] RPis drinksBSR
            , hBody = Nothing
            }
        ]
    , rlabel = Just
        ( "§"
        , 1
        , "What does it mean to drink?"
        )
    , lsource = Nothing
    , srcref = Just
        ( SrcRef
            { url = "test/MustSing5.csv"
            , short = "test/MustSing5.csv"
            , srcrow = 3
            , srccol = 15
            , version = Nothing
            }
        )
    , defaults = []
    , symtab = []
    }
  , Hornlike
    { name =
        [ MTT "Qualifies" ]
    , super = Nothing
    , keyword = Means
    , given = Nothing
    , giveth = Nothing
    , upon = Nothing
    , clauses =
        [ HC
            { hHead = RPBoolStructR
                [ MTT "Qualifies" ] RPis qualifiesBSR
            , hBody = Nothing
            }
        ]
    , rlabel = Nothing
    , lsource = Nothing
    , srcref = Just
        ( SrcRef
            { url = "test/MustSing5.csv"
            , short = "test/MustSing5.csv"
            , srcrow = 6
            , srccol = 7
            , version = Nothing
            }
        )
    , defaults = []
    , symtab = []
    }
  ]

mustsing5ExpandedGold = [Regulative {subj = Leaf ((MTT "Person" :| [],Nothing) :| []), rkeyword = REvery, who = Just (All Nothing [Leaf (RPMT [MTT "walks"]),Any Nothing [All Nothing [Any (Just (PrePost "consumes" "beverage")) [Leaf (RPMT [MTT "an alcoholic"]),Leaf (RPMT [MTT "non-alcoholic"])],Any (Just (Pre "whether")) [Leaf (RPMT [MTT "in part"]),Leaf (RPMT [MTT "in whole"])]],Leaf (RPMT [MTT "eats"])]]), cond = Nothing, deontic = DMust, action = Leaf ((MTT "sing" :| [],Nothing) :| []), temporal = Nothing, hence = Nothing, lest = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/MustSing5.csv", short = "test/MustSing5.csv", srcrow = 2, srccol = 1, version = Nothing}), upon = Nothing, given = Nothing, having = Nothing, wwhere = [], defaults = [], symtab = []}]

pdpa1withExpandedUponGold = [ Regulative
    { subj = Leaf
        (
            ( MTT "Organisation" :| [], Nothing ) :| []
        ), rkeyword = REvery, who = Just
        ( Not
            ( Leaf ( RPMT [ MTT "is", MTT "a Public Agency" ] ) )
        ), cond = Just
        ( Any Nothing
            [ Leaf
                ( RPConstraint
                    [ MTT "the data breach occurs" ] ( RPTC TOn )
                    [ MTT "1 Feb 2022" ]
                ), Leaf
                ( RPConstraint
                    [ MTT "the data breach occurs" ] ( RPTC TAfter )
                    [ MTT "1 Feb 2022" ]
                )
            ]
        ), deontic = DMust, action = Leaf
        (
            ( MTT "assess" :|
                [ MTT "if it is a Notifiable Data Breach" ], Nothing
            ) :|
            [
                ( MTT "by" :|
                    [ MTT "performing", MTT "NDB Qualification" ], Nothing
                )
            ]
        ), temporal = Just
        ( TemporalConstraint TBefore ( Just 30 ) "days" ), hence = Just
        ( RuleAlias [ MTT "Notification" ] ), lest = Just
        ( Regulative
            { subj = Leaf
                (
                    ( MTT "the PDPC" :| [], Nothing ) :| []
                ), rkeyword = RParty, who = Nothing, cond = Nothing, deontic = DMay, action = Leaf
                (
                    ( MTT "demand" :|
                        [ MTT "an explanation for your inaction" ], Nothing
                    ) :| []
                ), temporal = Nothing, hence = Just
                ( Regulative
                    { subj = Leaf
                        (
                            ( MTT "You" :| [], Nothing ) :| []
                        ), rkeyword = RParty, who = Nothing, cond = Nothing, deontic = DMust, action = Leaf
                        (
                            ( MTT "respond" :| [], Nothing ) :|
                            [
                                ( MTT "to" :| [ MTT "the PDPC" ], Nothing ),
                                ( MTT "about" :|
                                    [ MTT "your inaction" ], Nothing
                                )
                            ]
                        ), temporal = Nothing, hence = Nothing, lest = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Just
                        ( SrcRef
                            { url = "test/Parsing/pdpa/pdpadbno-1.csv", short = "test/Parsing/pdpa/pdpadbno-1.csv", srcrow = 3, srccol = 13, version = Nothing
                            }
                        ), upon = Nothing, given = Nothing, having = Nothing, wwhere = [], defaults = [], symtab = []
                    }
                ), lest = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Just
                ( SrcRef
                    { url = "test/Parsing/pdpa/pdpadbno-1.csv", short = "test/Parsing/pdpa/pdpadbno-1.csv", srcrow = 2, srccol = 11, version = Nothing
                    }
                ), upon = Nothing, given = Nothing, having = Nothing, wwhere = [], defaults = [], symtab = []
            }
        ), rlabel = Just
        ( "§", 2, "Assess" ), lsource = Nothing, srcref = Just
        ( SrcRef
            { url = "test/Parsing/pdpa/pdpadbno-1.csv", short = "test/Parsing/pdpa/pdpadbno-1.csv", srcrow = 1, srccol = 1, version = Nothing
            }
        ), upon = Just
        (
            ( MTT "becoming aware a data breach may have occurred" :| [], Nothing
            ) :| []
        ), given = Nothing, having = Nothing, wwhere = [], defaults = [], symtab = []
    }, DefNameAlias
    { name =
        [ MTT "You" ], detail =
        [ MTT "Organisation" ], nlhint = Nothing, srcref = Just
        ( SrcRef
            { url = "test/Parsing/pdpa/pdpadbno-1.csv", short = "test/Parsing/pdpa/pdpadbno-1.csv", srcrow = 2, srccol = 3, version = Nothing
            }
        )
    }, Hornlike
    { name =
        [ MTT "becoming aware" ], super = Nothing, keyword = Means, given = Nothing, giveth = Nothing, upon = Nothing, clauses =
        [ HC
            { hHead = RPBoolStructR
                [ MTT "becoming aware" ] RPis
                ( Leaf
                    ( RPMT
                        [ MTT "becoming aware", MTT "a data breach may have occurred"
                        ]
                    )
                ), hBody = Nothing
            }
        ], rlabel = Nothing, lsource = Nothing, srcref = Just
        ( SrcRef
            { url = "test/Parsing/pdpa/pdpadbno-1.csv", short = "test/Parsing/pdpa/pdpadbno-1.csv", srcrow = 6, srccol = 7, version = Nothing
            }
        ), defaults = [], symtab = []
    }
  ]


qualifiesBSR :: BoolStructR
qualifiesBSR = All Nothing
                    [ Leaf
                        ( RPMT
                            [ MTT "walks" ]
                        )
                    , Any Nothing
                        [ Leaf
                            ( RPMT
                                [ MTT "drinks" ]
                            )
                        , Leaf
                            ( RPMT
                                [ MTT "eats" ]
                            )
                        ]
                    ]


drinksBSR :: BoolStructR
drinksBSR = All Nothing
                    [ Any
                        ( Just
                            ( PrePost "consumes" "beverage" )
                        )
                        [ Leaf
                            ( RPMT
                                [ MTT "an alcoholic" ]
                            )
                        , Leaf
                            ( RPMT
                                [ MTT "non-alcoholic" ]
                            )
                        ]
                    , Any
                        ( Just
                            ( Pre "whether" )
                        )
                        [ Leaf
                            ( RPMT
                                [ MTT "in part" ]
                            )
                        , Leaf
                            ( RPMT
                                [ MTT "in whole" ]
                            )
                        ]
                    ]

rodentsBSR :: BoolStructR
rodentsBSR = All Nothing
  [ Any
      ( Just
          ( Pre "Loss or Damage caused by" )
      )
      [ Leaf
          ( RPMT
              [ MTT "rodents" ]
          )
      , Leaf
          ( RPMT
              [ MTT "insects" ]
          )
      , Leaf
          ( RPMT
              [ MTT "vermin" ]
          )
      , Leaf
          ( RPMT
              [ MTT "birds" ]
          )
      ]
  , Not
      ( Any Nothing
          [ All Nothing
              [ Leaf
                  ( RPConstraint
                      [ MTT "Loss or Damage" ] RPis
                      [ MTT "to Contents" ]
                  )
              , Leaf
                  ( RPConstraint
                      [ MTT "Loss or Damage" ] RPis
                      [ MTT "caused by"
                      , MTT "birds"
                      ]
                  )
              ]
          , All Nothing
              [ Leaf
                  ( RPConstraint
                      [ MTT "Loss or Damage" ] RPis
                      [ MTT "ensuing loss" ]
                  )
              , Leaf
                  ( RPConstraint
                      [ MTT "Loss or Damage" ] RPis
                      [ MTT "Covered" ]
                  )
              , Not
                  ( Any Nothing
                      [ Leaf
                          ( RPMT
                              [ MTT "any other exclusion applies" ]
                          )
                      , Any
                          ( Just
                              ( Pre "an animal caused water to escape from" )
                          )
                          [ Leaf
                              ( RPMT
                                  [ MTT "a household appliance" ]
                              )
                          , Leaf
                              ( RPMT
                                  [ MTT "a swimming pool" ]
                              )
                          , Leaf
                              ( RPMT
                                  [ MTT "a plumbing, heating, or air conditioning system" ]
                              )
                          ]
                      ]
                  )
              ]
          ]
      )
  ]



notRodentsBSR :: BoolStructR
notRodentsBSR = All Nothing
  [ Any
      ( Just
          ( Pre "Loss or Damage caused by" )
      )
      [ Leaf
          ( RPMT
              [ MTT "rodents" ]
          )
      , Leaf
          ( RPMT
              [ MTT "insects" ]
          )
      , Leaf
          ( RPMT
              [ MTT "vermin" ]
          )
      , Leaf
          ( RPMT
              [ MTT "birds" ]
          )
      ]
  , Not
      ( Any Nothing
          [ All Nothing
              [ Leaf
                  ( RPConstraint
                      [ MTT "Loss or Damage" ] RPis
                      [ MTT "to Contents" ]
                  )
              , Leaf
                  ( RPBoolStructR
                      [ MTT "Loss or Damage" ] RPis
                      (Not
                        (Leaf
                          (RPMT
                            [ MTT "caused by"
                            , MTT "birds"
                            ])
                        )
                      )
                  )
                ]
          , All Nothing
              [ Leaf
                  ( RPConstraint
                      [ MTT "Loss or Damage" ] RPis
                      [ MTT "ensuing loss" ]
                  )
              , Leaf
                  ( RPConstraint
                      [ MTT "Loss or Damage" ] RPis
                      [ MTT "Covered" ]
                  )
              , Not
                  ( Any Nothing
                      [ Leaf
                          ( RPMT
                              [ MTT "any other exclusion applies" ]
                          )
                      , Any
                          ( Just
                              ( Pre "an animal caused water to escape from" )
                          )
                          [ Leaf
                              ( RPMT
                                  [ MTT "a household appliance" ]
                              )
                          , Leaf
                              ( RPMT
                                  [ MTT "a swimming pool" ]
                              )
                          , Leaf
                              ( RPMT
                                  [ MTT "a plumbing, heating, or air conditioning system" ]
                              )
                          ]
                      ]
                  )
              ]
          ]
      )
  ]

pdpa1withUnexpandedUpon = [ Regulative
    { subj = Leaf
        (
            ( MTT "Organisation" :| []
            , Nothing
            ) :| []
        )
    , rkeyword = REvery
    , who = Just
        ( Not
            ( Leaf
                ( RPMT
                    [ MTT "is"
                    , MTT "a Public Agency"
                    ]
                )
            )
        )
    , cond = Just
        ( Any Nothing
            [ Leaf
                ( RPConstraint
                    [ MTT "the data breach occurs" ] ( RPTC TOn )
                    [ MTT "1 Feb 2022" ]
                )
            , Leaf
                ( RPConstraint
                    [ MTT "the data breach occurs" ] ( RPTC TAfter )
                    [ MTT "1 Feb 2022" ]
                )
            ]
        )
    , deontic = DMust
    , action = Leaf
        (
            ( MTT "assess" :|
                [ MTT "if it is a Notifiable Data Breach" ]
            , Nothing
            ) :|
            [
                ( MTT "by" :|
                    [ MTT "performing"
                    , MTT "NDB Qualification"
                    ]
                , Nothing
                )
            ]
        )
    , temporal = Just
        ( TemporalConstraint TBefore
            ( Just 30 ) "days"
        )
    , hence = Just
        ( RuleAlias
            [ MTT "Notification" ]
        )
    , lest = Just
        ( Regulative
            { subj = Leaf
                (
                    ( MTT "the PDPC" :| []
                    , Nothing
                    ) :| []
                )
            , rkeyword = RParty
            , who = Nothing
            , cond = Nothing
            , deontic = DMay
            , action = Leaf
                (
                    ( MTT "demand" :|
                        [ MTT "an explanation for your inaction" ]
                    , Nothing
                    ) :| []
                )
            , temporal = Nothing
            , hence = Just
                ( Regulative
                    { subj = Leaf
                        (
                            ( MTT "You" :| []
                            , Nothing
                            ) :| []
                        )
                    , rkeyword = RParty
                    , who = Nothing
                    , cond = Nothing
                    , deontic = DMust
                    , action = Leaf
                        (
                            ( MTT "respond" :| []
                            , Nothing
                            ) :|
                            [
                                ( MTT "to" :|
                                    [ MTT "the PDPC" ]
                                , Nothing
                                )
                            ,
                                ( MTT "about" :|
                                    [ MTT "your inaction" ]
                                , Nothing
                                )
                            ]
                        )
                    , temporal = Nothing
                    , hence = Nothing
                    , lest = Nothing
                    , rlabel = Nothing
                    , lsource = Nothing
                    , srcref = Just
                        ( SrcRef
                            { url = "test/Parsing/pdpa/pdpadbno-1.csv"
                            , short = "test/Parsing/pdpa/pdpadbno-1.csv"
                            , srcrow = 3
                            , srccol = 13
                            , version = Nothing
                            }
                        )
                    , upon = Nothing
                    , given = Nothing
                    , having = Nothing
                    , wwhere = []
                    , defaults = []
                    , symtab = []
                    }
                )
            , lest = Nothing
            , rlabel = Nothing
            , lsource = Nothing
            , srcref = Just
                ( SrcRef
                    { url = "test/Parsing/pdpa/pdpadbno-1.csv"
                    , short = "test/Parsing/pdpa/pdpadbno-1.csv"
                    , srcrow = 2
                    , srccol = 11
                    , version = Nothing
                    }
                )
            , upon = Nothing
            , given = Nothing
            , having = Nothing
            , wwhere = []
            , defaults = []
            , symtab = []
            }
        )
    , rlabel = Just
        ( "§"
        , 2
        , "Assess"
        )
    , lsource = Nothing
    , srcref = Just
        ( SrcRef
            { url = "test/Parsing/pdpa/pdpadbno-1.csv"
            , short = "test/Parsing/pdpa/pdpadbno-1.csv"
            , srcrow = 1
            , srccol = 1
            , version = Nothing
            }
        )
    , upon = Just
        (
            ( MTT "becoming aware" :| []
            , Nothing
            ) :| []
        )
    , given = Nothing
    , having = Nothing
    , wwhere = []
    , defaults = []
    , symtab = []
    }
  , DefNameAlias
    { name =
        [ MTT "You" ]
    , detail =
        [ MTT "Organisation" ]
    , nlhint = Nothing
    , srcref = Just
        ( SrcRef
            { url = "test/Parsing/pdpa/pdpadbno-1.csv"
            , short = "test/Parsing/pdpa/pdpadbno-1.csv"
            , srcrow = 2
            , srccol = 3
            , version = Nothing
            }
        )
    }
  , Hornlike
    { name =
        [ MTT "becoming aware" ]
    , super = Nothing
    , keyword = Means
    , given = Nothing
    , giveth = Nothing
    , upon = Nothing
    , clauses =
        [ HC
            { hHead = RPBoolStructR
                [ MTT "becoming aware" ] RPis becomingAwareBSR
            , hBody = Nothing
            }
        ]
    , rlabel = Nothing
    , lsource = Nothing
    , srcref = Just
        ( SrcRef
            { url = "test/Parsing/pdpa/pdpadbno-1.csv"
            , short = "test/Parsing/pdpa/pdpadbno-1.csv"
            , srcrow = 6
            , srccol = 7
            , version = Nothing
            }
        )
    , defaults = []
    , symtab = []
    }
  ]

becomingAwareBSR :: BoolStructR
becomingAwareBSR = Leaf
                    ( RPMT
                        [ MTT "becoming aware", MTT "a data breach may have occurred" ]
                    )

pdpa1withUnexpandedUponInterp :: Interpreted
pdpa1withUnexpandedUponInterp = L4I
    { classtable = CT Data.Map.empty
    , scopetable = fromList
        [ ([ MTT "becoming aware" ]
           , fromList [
              ( [MTT "becoming aware"]
              ,
                ( (Nothing, []), [ HC { hHead = RPBoolStructR [MTT "becoming aware"] RPis
                                                    becomingAwareBSR
                                      , hBody = Nothing}
                                  ]
                )
              )       ]
            )
        ]
    , origrules = pdpa1withUnexpandedUpon
    }

mustsing5Interp :: Interpreted
mustsing5Interp = L4I
    { classtable = CT Data.Map.empty
    , scopetable = fromList
        [ ([ MTT "Qualifies" ]
           , fromList [
              ( [MTT "Qualifies"]
              ,
                ( (Nothing, []), [ HC { hHead = RPBoolStructR [MTT "Qualifies"] RPis qualifiesBSR
                                      , hBody = Nothing}
                                  ]
                )
              )       ]
            )
        ,
            (
                [ MTT "What does it mean to drink?" ]
            , fromList
                [
                    (
                        [ MTT "drinks" ]
                    ,
                        (
                            ( Nothing
                            , []
                            )
                        ,
                            [ HC
                                { hHead = RPBoolStructR
                                    [ MTT "drinks" ] RPis drinksBSR
                                , hBody = Nothing
                                }
                            ]
                        )
                    )
                ]
            )
        ] , origrules = mustsing5Rules}

rodentsInterp :: Interpreted
rodentsInterp = L4I
    { classtable = CT Data.Map.empty
    , scopetable = fromList
        [
            (
                [ MTT "Animal Damage" ]
            , fromList
                [
                    (
                        [ MTT "Loss or Damage" ]
                    ,
                        (
                            ( Nothing
                            , []
                            )
                        ,
                            [ HC
                                { hHead = RPBoolStructR
                                    [ MTT "Loss or Damage" ] RPis
                                    ( Not
                                        ( Leaf
                                            ( RPMT
                                                [ MTT "Covered" ]
                                            )
                                        )
                                    )
                                , hBody = Just rodentsBSR }
                            ]
                        )
                    )
                ]
            )
        ], origrules = rodentsRules }
