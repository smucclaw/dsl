{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LS.NLGSpec where

import AnyAll (BoolStruct (..), Label (..))
import Data.HashMap.Strict as Map (empty, fromList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.String.Interpolate (i)
import LS.NLP.NL4 (Tree (GqCONSTR, GqPREPOST))
import LS.NLP.NLG
  ( NLGEnv,
    expandRulesForNLG,
    linBStext,
    mkConstraintText,
    myNLGEnv,
    ruleQuestions,
  )
import LS.Rule
  ( Interpreted (..),
    Rule
      ( DefNameAlias,
        DefTypically,
        Hornlike,
        Regulative,
        RuleAlias,
        action,
        clauses,
        cond,
        defaults,
        deontic,
        detail,
        given,
        giveth,
        having,
        hence,
        keyword,
        lest,
        lsource,
        name,
        nlhint,
        rkeyword,
        rlabel,
        srcref,
        subj,
        super,
        symtab,
        temporal,
        upon,
        who,
        wwhere
      ),
  )
import LS.Types
  ( BoolStructR,
    ClsTab (CT),
    Deontic (DMay, DMust, DShant),
    HornClause (HC, hBody, hHead),
    MTExpr (MTI, MTT),
    MyToken (Decide, Means),
    RPRel (RPTC, RPgt, RPis),
    RegKeywords (REvery, RParty),
    RelationalPredicate (RPBoolStructR, RPConstraint, RPMT),
    SrcRef (SrcRef, short, srccol, srcrow, url, version),
    TComparison (TAfter, TBefore, TOn),
    TemporalConstraint (TemporalConstraint),
  )
import LS.XPile.Logging (fromxpLogE, xpLog)
import PGF (mkCId)
import Parsing.PDPASpec (expected_pdpadbno1)
import Test.Hspec
  ( Spec,
    describe,
    expectationFailure,
    it,
    runIO,
    shouldBe,
  )

spec :: Spec
spec = do
  let eng = mkCId "NL4Eng"
  (env, _) <- xpLog <$> runIO (myNLGEnv rodentsInterp eng)
  case env of
    Left xpLogW ->
      it [i|rodentsInterp nlgEnv is a left of: #{xpLogW}|] $ expectationFailure ""
    Right env -> do
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
          let questions = fst $ xpLog $ ruleQuestions env Nothing (head expected_pdpadbno1)
          questions `shouldBe` [Not (Leaf "is the organisation a public agency?"), Leaf "does the data breach occur on or after 1 Feb 2022?", Leaf "has the organisation become aware that a data breach may have occurred?"]

      describe "test questions from MustSing5 after rule expansion" $ do
        it "should return questions about alcoholic and non-alcoholic beverages" $ do
            let questions = fst $ xpLog $ ruleQuestions env Nothing (head mustsing5ExpandedGold)
            questions `shouldBe` [All Nothing [Leaf "does the person walk?",Any Nothing [All Nothing [Any Nothing [Leaf "does the person consume an alcoholic beverage?",Leaf "does the person consume a non-alcoholic beverage?"],Any Nothing [Leaf "does the person consume the beverage in part?",Leaf "does the person consume the beverage in whole?"]],Leaf "does the person eat?"]]]

      (envMustSing, _) <- xpLog <$> runIO (myNLGEnv mustsing5Interp eng)
      case envMustSing of
        Left xpLogW ->
          it [i|mustsing5Interp nlgEnv is a left of: #{xpLogW}|] $ expectationFailure ""
        Right envMustSing ->
          testShouldChange "mustsing5" envMustSing mustsing5Rules mustsing5ExpandedGold

      (envPDPA, _) <- xpLog <$> runIO (myNLGEnv pdpa1withUnexpandedUponInterp eng)
      case envPDPA of
        Left xpLogW ->
          it [i|pdpa1withUnexpandedUponInterp nlgEnv is a left of: #{xpLogW}|] $
            expectationFailure ""
        Right envPDPA -> do
          testShouldChange
            "pdpa1 with added UPON expansion" envPDPA
            pdpa1withUnexpandedUpon pdpa1withExpandedUponGold

          (envPDPAFull, _) <- xpLog <$> runIO (myNLGEnv pdpafullInterp eng)
          case envPDPAFull of
            Left xpLogW ->
              it [i|pdpafullInterp nlgEnv is a left of: #{xpLogW}|] $ expectationFailure ""
            Right envPDPAFull ->
              testShouldChange
                "pdpa full" envPDPAFull pdpafullRules pdpafullExpandedGold

          testNoChange "rodentsandvermin" env rodentsRules
          testNoChange "pdpadbno-1 (original)" envPDPA expected_pdpadbno1

---------------------------------------------------------------

testNoChange :: String -> NLGEnv -> [Rule] -> Spec
testNoChange description env ogrules =
  describe ("test expandRulesForNLG for " <> description) $ do
    it ("should not change " <> description) $ do
        let expanded = expandRulesForNLG env ogrules
        expanded `shouldBe` ogrules

testShouldChange :: String -> NLGEnv -> [Rule] -> [Rule] -> Spec
testShouldChange description env ogrules exprules =
  describe ("test expandRulesForNLG for " <> description) $ do
    it ("should change " <> description) $ do
        let expanded = expandRulesForNLG env ogrules
        expanded `shouldBe` exprules

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
    { classtable = CT Map.empty
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
    { classtable = CT Map.empty
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
    { classtable = CT Map.empty
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

pdpafullInterp :: Interpreted
pdpafullInterp = L4I {classtable = CT (fromList []), scopetable = fromList [([MTT "NDB Qualification"],fromList [([MTT "it is a Notifiable Data Breach"],((Nothing,[]),[HC {hHead = RPMT [MTT "it is a Notifiable Data Breach"], hBody = Just (All Nothing [Leaf (RPMT [MTT "a data breach",MTT "occurred"]),Not (Leaf (RPMT [MTT "the data breach occurred only within an organisation"])),Any Nothing [Leaf (RPMT [MTT "it results in, or is likely to result in, significant harm to an affected individual"]),Leaf (RPMT [MTT "it is, or is likely to be, of a significant scale"])]])}]))]),([MTT "Notification"],fromList [([MTT "Notification"],((Nothing,[]),[HC {hHead = RPBoolStructR [MTT "Notification"] RPis (All Nothing [Leaf (RPMT [MTT "Notify PDPC"]),Leaf (RPMT [MTT "Notify Individuals"])]), hBody = Nothing}]))]),([MTT "a data breach",MTT "occurred"],fromList [([MTT "a data breach",MTT "occurred"],((Nothing,[]),[HC {hHead = RPBoolStructR [MTT "a data breach",MTT "occurred"] RPis (Any Nothing [Any (Just (PrePost "any unauthorised" "of personal data")) [Leaf (RPMT [MTT "access"]),Leaf (RPMT [MTT "use"]),Leaf (RPMT [MTT "disclosure"]),Leaf (RPMT [MTT "copying"]),Leaf (RPMT [MTT "modification"]),Leaf (RPMT [MTT "disposal"])],Any (Just (PrePost "loss of storage medium on which personal data is stored in circumstances where the unauthorised" "of the personal data is likely to occur")) [Leaf (RPMT [MTT "access"]),Leaf (RPMT [MTT "use"]),Leaf (RPMT [MTT "disclosure"]),Leaf (RPMT [MTT "copying"]),Leaf (RPMT [MTT "modification"]),Leaf (RPMT [MTT "disposal"])]]), hBody = Nothing}]))]),([MTT "it is, or is likely to be, of a significant scale"],fromList [([MTT "it is, or is likely to be, of a significant scale"],((Nothing,[]),[HC {hHead = RPMT [MTT "it is, or is likely to be, of a significant scale"], hBody = Just (Leaf (RPConstraint [MTT "the number of affected individuals"] RPgt [MTT "the prescribed threshold of affected individuals"]))}]))]),([MTT "it results in, or is likely to result in, significant harm to an affected individual"],fromList [([MTT "it results in, or is likely to result in, significant harm to an affected individual"],((Nothing,[]),[HC {hHead = RPBoolStructR [MTT "it results in, or is likely to result in, significant harm to an affected individual"] RPis (All Nothing [Leaf (RPMT [MTT "the data breach is in relation to any prescribed personal data or class of personal data relating to the individual"]),Not (Any Nothing [Leaf (RPMT [MTT "the organisation has taken any action ",MTT "to render it unlikely that the notifiable data breach will result in significant harm to the individual"]),Leaf (RPMT [MTT "the organisation already implemented any technological measure",MTT "to render it unlikely that the notifiable data breach will result in significant harm to the individual"])])]), hBody = Nothing}]))]),([MTT "the data breach is in relation to any prescribed personal data or class of personal data relating to the individual"],fromList [([MTT "the data breach is in relation to any prescribed personal data or class of personal data relating to the individual"],((Nothing,[]),[HC {hHead = RPBoolStructR [MTT "the data breach is in relation to any prescribed personal data or class of personal data relating to the individual"] RPis (All Nothing [Any (Just (Pre "the data breach relates to the individual's")) [Leaf (RPMT [MTT "full name"]),Leaf (RPMT [MTT "alias"]),Leaf (RPMT [MTT "identification number"])],Leaf (RPMT [MTT "the data breach relates to",MTT "any of the prescribed personal data or classes of personal data relating to the individual"])]), hBody = Nothing}]))]),([MTT "the data breach relates to",MTT "any of the prescribed personal data or classes of personal data relating to the individual"],fromList [([MTT "the data breach relates to",MTT "any of the prescribed personal data or classes of personal data relating to the individual"],((Nothing,[]),[HC {hHead = RPMT [MTT "the data breach relates to",MTT "any of the prescribed personal data or classes of personal data relating to the individual"], hBody = Just (Any Nothing [Leaf (RPMT [MTI 1,MTT "The amount of any wages, salary, fee, commission, bonus, gratuity, allowance or other remuneration paid or payable to the individual by any person, whether under a contract of service or a contract for services."]),Leaf (RPMT [MTI 2,MTT "The income of the individual from the sale of any goods or property."]),Leaf (RPMT [MTI 3,MTT "The number of any credit card, charge card or debit card issued to or in the name of the individual."]),Leaf (RPMT [MTI 4,MTT "The number assigned to any account the individual has with any organisation that is a bank or finance company."]),Any (Just (Pre "5 Any information that identifies, or is likely to lead to the identification of, the individual as a child or young person who \8212")) [Leaf (RPMT [MTT "5.a",MTT "is or had been the subject of any investigation under the CYPA;"]),Leaf (RPMT [MTT "5.b",MTT "is or had been arrested, on or after 1 July 2020, for an offence committed under any written law;"]),Leaf (RPMT [MTT "5.c",MTT "is or had been taken into care or custody by the Director-General of Social Welfare, a protector, any officer generally or specially authorised in that behalf in writing by the Director-General or protector or a police officer under the CYPA;"]),Leaf (RPMT [MTT "5.d",MTT "is attending or had attended a family programme in relation to an application to be made under section 50 of the CYPA;"]),Leaf (RPMT [MTT "5.e",MTT "is or was the subject of an order made by a court under the CYPA; or"]),Leaf (RPMT [MTT "5.f",MTT "is or had been concerned in any proceedings in any court or on appeal from any court, whether the individual is the person against or in respect of whom the proceedings are taken or a witness in those proceedings."])],Any (Just (Pre "6 Any information that identifies, or is likely to lead to the identification of \8212 b")) [Leaf (RPMT [MTT "6.a",MTT "the individual who has been or is the subject of any investigation, examination, assessment or treatment under the VAA relating to whether the individual is a vulnerable adult experiencing or at risk of abuse, neglect or self-neglect;"]),Leaf (RPMT [MTT "6.b",MTT "the individual as a vulnerable adult who has been committed to a place of temporary care and protection or place of safety or to the care of a fit person under the VAA;"]),Leaf (RPMT [MTT "6.c",MTT "the individual as a vulnerable adult who is the subject of an order made by a court under the VAA;"]),Leaf (RPMT [MTT "6.d",MTT "a place of temporary care and protection or place of safety in which an individual or a vulnerable adult mentioned in sub-paragraph (a), (b) or (c) is committed, or the location of such a place of temporary care and protection or place of safety; or"]),Leaf (RPMT [MTT "6.e",MTT "a fit person under whose care an individual or a vulnerable adult mentioned in sub-paragraph (a), (b) or (c) is placed, or the location of the premises of such a fit person."])],Any (Just (Pre "7 Any private key of or relating to the individual that is used or may be used \8212")) [Leaf (RPMT [MTT "7.a",MTT "to create a secure electronic record or secure electronic signature;"]),Leaf (RPMT [MTT "7.b",MTT "to verify the integrity of a secure electronic record; or"]),Leaf (RPMT [MTT "7.c",MTT "to verify the authenticity or integrity of a secure electronic signature."])],Leaf (RPMT [MTI 8,MTT "The net worth of the individual."]),Leaf (RPMT [MTI 9,MTT "The deposit of moneys by the individual with any organisation."]),Leaf (RPMT [MTI 10,MTT "The withdrawal by the individual of moneys deposited with any organisation."]),Leaf (RPMT [MTI 11,MTT "The granting by an organisation of advances, loans and other facilities by which the individual, being a customer of the organisation, has access to funds or financial guarantees."]),Leaf (RPMT [MTI 12,MTT "The incurring by the organisation of any liabilities other than those mentioned in paragraph 11 on behalf of the individual."]),Leaf (RPMT [MTI 13,MTT "The payment of any moneys, or transfer of any property, by any person to the individual, including the amount of the moneys paid or the value of the property transferred, as the case may be."]),Leaf (RPMT [MTI 14,MTT "The creditworthiness of the individual."]),Leaf (RPMT [MTI 15,MTT "The individual\8217s investment in any capital markets products."]),Any (Just (Pre "16 The existence, and amount due or outstanding, of any debt \8212")) [Leaf (RPMT [MTT "16.a",MTT "owed by the individual to an organisation; or"]),Leaf (RPMT [MTT "16.b",MTT "owed by an organisation to the individual."])],Any (Just (Pre "17 Any of the following:")) [Leaf (RPMT [MTT "17.a",MTT "the terms and conditions of any accident and health policy or life policy (called in this item the applicable policy) of which the individual is the policy owner or under which the individual is a beneficiary;"]),Leaf (RPMT [MTT "17.b",MTT "the premium payable by the policy owner under the applicable policy;"]),Leaf (RPMT [MTT "17.c",MTT "the benefits payable to any beneficiary under the applicable policy;"]),Leaf (RPMT [MTT "17.d",MTT "any information relating to any claim on, or payment under, the applicable policy, including the condition of the health of the individual and the diagnosis, treatment, prevention or alleviation of any ailment, condition, disability, disease, disorder or injury that the individual has suffered or is suffering from;"]),Leaf (RPMT [MTT "17.e",MTT "any other information that the individual is the policy owner of, or a beneficiary under, an applicable policy."])],Any (Just (Pre "18 The assessment, diagnosis, treatment, prevention or alleviation by a health professional of any of the following affecting the individual:")) [Leaf (RPMT [MTT "18.a",MTT "any sexually-transmitted disease such as Chlamydial Genital Infection, Gonorrhoea and Syphilis;"]),Leaf (RPMT [MTT "18.b",MTT "Human Immunodeficiency Virus Infection;"]),Leaf (RPMT [MTT "mental illness;"]),Leaf (RPMT [MTT "18.c",MTT "schizophrenia or delusional disorder;"]),Leaf (RPMT [MTT "18.d",MTT "substance abuse and addiction, including drug addiction and alcoholism"])],Any (Just (Pre "19 The provision of treatment to the individual for or in respect of \8212")) [Leaf (RPMT [MTT "19.a",MTT "the donation or receipt of a human egg or human sperm; or"]),Leaf (RPMT [MTT "19.b",MTT "any contraceptive operation or procedure or abortion."])],Any (Just (Pre "20 Any of the following:")) [Leaf (RPMT [MTT "20.a",MTT "subject to section 4(4)(b) of the Act, the donation and removal of any organ from the body of the deceased individual for the purpose of its transplantation into the body of another individual;"]),Leaf (RPMT [MTT "20.b",MTT "the donation and removal of any specified organ from the individual, being a living organ donor, for the purpose of its transplantation into the body of another individual;"]),Leaf (RPMT [MTT "20.c",MTT "the transplantation of any organ mentioned in sub-paragraph (a) or (b) into the body of the individual."])],Leaf (RPMT [MTI 21,MTT "Subject to section 4(4)(b) of the Act, the suicide or attempted suicide of the individual."]),Leaf (RPMT [MTI 22,MTT "Domestic abuse, child abuse or sexual abuse involving or alleged to involve the individual."]),Any (Just (Pre "23 Any of the following:")) [Leaf (RPMT [MTT "23.a",MTT "information that the individual is or had been adopted pursuant to an adoption order made under the Adoption of Children Act (Cap. 4), or is or had been the subject of an application for an adoption order;"]),Leaf (RPMT [MTT "23.b",MTT "the identity of the natural father or mother of the individual;"]),Leaf (RPMT [MTT "23.c",MTT "the identity of the adoptive father or mother of the individual;"]),Leaf (RPMT [MTT "23.d",MTT "the identity of any applicant for an adoption order;"]),Leaf (RPMT [MTT "23.e",MTT "the identity of any person whose consent is necessary under that Act for an adoption order to be made, whether or not the court has dispensed with the consent of that person in accordance with that Act;"]),Leaf (RPMT [MTT "23.f",MTT "any other information that the individual is or had been an adopted child or relating to the adoption of the individual."])]])}]))]),([MTT "the prescribed threshold of affected individuals"],fromList [([MTT "the prescribed threshold of affected individuals"],((Nothing,[]),[HC {hHead = RPBoolStructR [MTT "the prescribed threshold of affected individuals"] RPis (Leaf (RPMT [MTI 500])), hBody = Nothing}]))])], origrules = pdpafullRules}

pdpafullRules :: [Rule]
pdpafullRules = [Regulative {subj = Leaf ((MTT "Organisation" :| [],Nothing) :| []), rkeyword = REvery, who = Just (Not (Leaf (RPMT [MTT "is",MTT "a Public Agency"]))), cond = Just (Any Nothing [Leaf (RPConstraint [MTT "the data breach occurred"] (RPTC TOn) [MTT "1 Feb 2022"]),Leaf (RPConstraint [MTT "the data breach occurred"] (RPTC TAfter) [MTT "1 Feb 2022"])]), deontic = DMust, action = Leaf ((MTT "assess" :| [MTT "if it is a Notifiable Data Breach"],Nothing) :| [(MTT "by" :| [MTT "evaluating",MTT "NDB Qualification"],Nothing)]), temporal = Just (TemporalConstraint TBefore (Just 30) "days"), hence = Just (RuleAlias [MTT "Notification"]), lest = Just (RuleAlias [MTT "PDPC query with demand"]), rlabel = Just ("\167",1,"Assessment"), lsource = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 8, srccol = 24, version = Nothing}), upon = Just ((MTT "becoming aware a data breach may have occurred" :| [],Nothing) :| []), given = Nothing, having = Nothing, wwhere = [], defaults = [], symtab = []},Regulative {subj = Leaf ((MTT "the PDPC" :| [],Nothing) :| []), rkeyword = RParty, who = Nothing, cond = Nothing, deontic = DMay, action = Leaf ((MTT "query" :| [MTT "You"],Nothing) :| [(MTT "with" :| [MTT "a demand"],Nothing),(MTT "for" :| [MTT "an explanation of your inaction"],Nothing)]), temporal = Nothing, hence = Just (RuleAlias [MTT "Respond to PDPC"]), lest = Nothing, rlabel = Just ("\167",1,"PDPC query with demand"), lsource = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 7, srccol = 37, version = Nothing}), upon = Nothing, given = Nothing, having = Nothing, wwhere = [], defaults = [], symtab = []},Regulative {subj = Leaf ((MTT "You" :| [],Nothing) :| []), rkeyword = RParty, who = Nothing, cond = Nothing, deontic = DMust, action = Leaf ((MTT "respond to" :| [MTT "the PDPC"],Nothing) :| [(MTT "with" :| [MTT "an explanation of your inaction"],Nothing)]), temporal = Nothing, hence = Nothing, lest = Nothing, rlabel = Just ("\167",1,"Respond to PDPC"), lsource = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 7, srccol = 45, version = Nothing}), upon = Nothing, given = Nothing, having = Nothing, wwhere = [], defaults = [], symtab = []},Hornlike {name = [MTT "it is a Notifiable Data Breach"], super = Nothing, keyword = Decide, given = Nothing, giveth = Nothing, upon = Nothing, clauses = [HC {hHead = RPMT [MTT "it is a Notifiable Data Breach"], hBody = Just (All Nothing [Leaf (RPMT [MTT "a data breach",MTT "occurred"]),Not (Leaf (RPMT [MTT "the data breach occurred only within an organisation"])),Any Nothing [Leaf (RPMT [MTT "it results in, or is likely to result in, significant harm to an affected individual"]),Leaf (RPMT [MTT "it is, or is likely to be, of a significant scale"])]])}], rlabel = Just ("\167",1,"NDB Qualification"), lsource = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 8, srccol = 72, version = Nothing}), defaults = [], symtab = []},Hornlike {name = [MTT "it is, or is likely to be, of a significant scale"], super = Nothing, keyword = Decide, given = Nothing, giveth = Nothing, upon = Nothing, clauses = [HC {hHead = RPMT [MTT "it is, or is likely to be, of a significant scale"], hBody = Just (Leaf (RPConstraint [MTT "the number of affected individuals"] RPgt [MTT "the prescribed threshold of affected individuals"]))}], rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 8, srccol = 123, version = Nothing}), defaults = [], symtab = []},Hornlike {name = [MTT "the prescribed threshold of affected individuals"], super = Nothing, keyword = Means, given = Nothing, giveth = Nothing, upon = Nothing, clauses = [HC {hHead = RPBoolStructR [MTT "the prescribed threshold of affected individuals"] RPis (Leaf (RPMT [MTI 500])), hBody = Nothing}], rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 17, srccol = 126, version = Nothing}), defaults = [], symtab = []},Hornlike {name = [MTT "Notification"], super = Nothing, keyword = Means, given = Nothing, giveth = Nothing, upon = Nothing, clauses = [HC {hHead = RPBoolStructR [MTT "Notification"] RPis (All Nothing [Leaf (RPMT [MTT "Notify PDPC"]),Leaf (RPMT [MTT "Notify Individuals"])]), hBody = Nothing}], rlabel = Just ("\167",1,"Notification"), lsource = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 9, srccol = 134, version = Nothing}), defaults = [], symtab = []},Regulative {subj = Leaf ((MTT "You" :| [],Nothing) :| []), rkeyword = RParty, who = Nothing, cond = Just (All Nothing [Leaf (RPMT [MTT "it is a Notifiable Data Breach"]),Not (Leaf (RPMT [MTT "you are a Public Agency"]))]), deontic = DMust, action = Leaf ((MTT "NOTIFY" :| [MTT "the PDPC"],Nothing) :| [(MTT "in" :| [MTT "the form and manner specified at www.pdpc.gov.sg"],Nothing),(MTT "with" :| [MTT "a Notification Message"],Nothing),(MTT "containing" :| [MTT "a list of individuals for whom notification waiver is sought"],Nothing),(MTT "and" :| [MTT "justification for why"],Nothing)]), temporal = Just (TemporalConstraint TBefore (Just 3) "DAYS"), hence = Just (RuleAlias [MTT "PDPC prohibit notify individuals"]), lest = Nothing, rlabel = Just ("\167",2,"Notify PDPC"), lsource = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 9, srccol = 139, version = Nothing}), upon = Nothing, given = Nothing, having = Nothing, wwhere = [], defaults = [], symtab = []},Regulative {subj = Leaf ((MTT "the PDPC" :| [],Nothing) :| []), rkeyword = RParty, who = Nothing, cond = Nothing, deontic = DMay, action = Leaf ((MTT "NOTIFY" :| [MTT "you"],Nothing) :| [(MTT "with" :| [MTT "a list of individuals to exclude from notification"],Nothing)]), temporal = Nothing, hence = Just (RuleAlias [MTT "Cannot notify individuals"]), lest = Nothing, rlabel = Just ("\167",2,"PDPC prohibit notify individuals"), lsource = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 9, srccol = 155, version = Nothing}), upon = Nothing, given = Nothing, having = Nothing, wwhere = [], defaults = [], symtab = []},Regulative {subj = Leaf ((MTT "YOU" :| [],Nothing) :| []), rkeyword = RParty, who = Nothing, cond = Nothing, deontic = DShant, action = Leaf ((MTT "NOTIFY" :| [MTT "each of the Notifiable Individuals"],Nothing) :| [(MTT "who" :| [MTT "are on the affected list"],Nothing)]), temporal = Nothing, hence = Nothing, lest = Nothing, rlabel = Just ("\167",2,"Cannot notify individuals"), lsource = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 9, srccol = 163, version = Nothing}), upon = Nothing, given = Nothing, having = Nothing, wwhere = [], defaults = [], symtab = []},Regulative {subj = Leaf ((MTT "You" :| [],Nothing) :| []), rkeyword = RParty, who = Nothing, cond = Just (All Nothing [Leaf (RPMT [MTT "it is a Notifiable Data Breach"]),Not (Leaf (RPMT [MTT "you are a Public Agency"]))]), deontic = DMust, action = Leaf ((MTT "NOTIFY" :| [MTT "each of the Notifiable Individuals"],Nothing) :| [(MTT "in" :| [MTT "any manner that is reasonable in the circumstances"],Nothing),(MTT "with" :| [MTT "a message obeying a certain format"],Nothing)]), temporal = Just (TemporalConstraint TBefore (Just 3) "days"), hence = Nothing, lest = Just (RuleAlias [MTT "Notify and explain"]), rlabel = Just ("\167",2,"Notify Individuals"), lsource = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 9, srccol = 175, version = Nothing}), upon = Nothing, given = Nothing, having = Nothing, wwhere = [], defaults = [], symtab = []},Regulative {subj = Leaf ((MTT "You" :| [],Nothing) :| []), rkeyword = RParty, who = Nothing, cond = Nothing, deontic = DMust, action = Leaf ((MTT "notify" :| [MTT "each of the Notifiable Individuals"],Nothing) :| []), temporal = Nothing, hence = Nothing, lest = Nothing, rlabel = Just ("\167",2,"Notify and explain"), lsource = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 9, srccol = 191, version = Nothing}), upon = Nothing, given = Nothing, having = Nothing, wwhere = [], defaults = [], symtab = []},Hornlike {name = [MTT "the data breach relates to",MTT "any of the prescribed personal data or classes of personal data relating to the individual"], super = Nothing, keyword = Decide, given = Nothing, giveth = Nothing, upon = Nothing, clauses = [HC {hHead = RPMT [MTT "the data breach relates to",MTT "any of the prescribed personal data or classes of personal data relating to the individual"], hBody = Just (Any Nothing [Leaf (RPMT [MTI 1,MTT "The amount of any wages, salary, fee, commission, bonus, gratuity, allowance or other remuneration paid or payable to the individual by any person, whether under a contract of service or a contract for services."]),Leaf (RPMT [MTI 2,MTT "The income of the individual from the sale of any goods or property."]),Leaf (RPMT [MTI 3,MTT "The number of any credit card, charge card or debit card issued to or in the name of the individual."]),Leaf (RPMT [MTI 4,MTT "The number assigned to any account the individual has with any organisation that is a bank or finance company."]),Any (Just (Pre "5 Any information that identifies, or is likely to lead to the identification of, the individual as a child or young person who \8212")) [Leaf (RPMT [MTT "5.a",MTT "is or had been the subject of any investigation under the CYPA;"]),Leaf (RPMT [MTT "5.b",MTT "is or had been arrested, on or after 1 July 2020, for an offence committed under any written law;"]),Leaf (RPMT [MTT "5.c",MTT "is or had been taken into care or custody by the Director-General of Social Welfare, a protector, any officer generally or specially authorised in that behalf in writing by the Director-General or protector or a police officer under the CYPA;"]),Leaf (RPMT [MTT "5.d",MTT "is attending or had attended a family programme in relation to an application to be made under section 50 of the CYPA;"]),Leaf (RPMT [MTT "5.e",MTT "is or was the subject of an order made by a court under the CYPA; or"]),Leaf (RPMT [MTT "5.f",MTT "is or had been concerned in any proceedings in any court or on appeal from any court, whether the individual is the person against or in respect of whom the proceedings are taken or a witness in those proceedings."])],Any (Just (Pre "6 Any information that identifies, or is likely to lead to the identification of \8212 b")) [Leaf (RPMT [MTT "6.a",MTT "the individual who has been or is the subject of any investigation, examination, assessment or treatment under the VAA relating to whether the individual is a vulnerable adult experiencing or at risk of abuse, neglect or self-neglect;"]),Leaf (RPMT [MTT "6.b",MTT "the individual as a vulnerable adult who has been committed to a place of temporary care and protection or place of safety or to the care of a fit person under the VAA;"]),Leaf (RPMT [MTT "6.c",MTT "the individual as a vulnerable adult who is the subject of an order made by a court under the VAA;"]),Leaf (RPMT [MTT "6.d",MTT "a place of temporary care and protection or place of safety in which an individual or a vulnerable adult mentioned in sub-paragraph (a), (b) or (c) is committed, or the location of such a place of temporary care and protection or place of safety; or"]),Leaf (RPMT [MTT "6.e",MTT "a fit person under whose care an individual or a vulnerable adult mentioned in sub-paragraph (a), (b) or (c) is placed, or the location of the premises of such a fit person."])],Any (Just (Pre "7 Any private key of or relating to the individual that is used or may be used \8212")) [Leaf (RPMT [MTT "7.a",MTT "to create a secure electronic record or secure electronic signature;"]),Leaf (RPMT [MTT "7.b",MTT "to verify the integrity of a secure electronic record; or"]),Leaf (RPMT [MTT "7.c",MTT "to verify the authenticity or integrity of a secure electronic signature."])],Leaf (RPMT [MTI 8,MTT "The net worth of the individual."]),Leaf (RPMT [MTI 9,MTT "The deposit of moneys by the individual with any organisation."]),Leaf (RPMT [MTI 10,MTT "The withdrawal by the individual of moneys deposited with any organisation."]),Leaf (RPMT [MTI 11,MTT "The granting by an organisation of advances, loans and other facilities by which the individual, being a customer of the organisation, has access to funds or financial guarantees."]),Leaf (RPMT [MTI 12,MTT "The incurring by the organisation of any liabilities other than those mentioned in paragraph 11 on behalf of the individual."]),Leaf (RPMT [MTI 13,MTT "The payment of any moneys, or transfer of any property, by any person to the individual, including the amount of the moneys paid or the value of the property transferred, as the case may be."]),Leaf (RPMT [MTI 14,MTT "The creditworthiness of the individual."]),Leaf (RPMT [MTI 15,MTT "The individual\8217s investment in any capital markets products."]),Any (Just (Pre "16 The existence, and amount due or outstanding, of any debt \8212")) [Leaf (RPMT [MTT "16.a",MTT "owed by the individual to an organisation; or"]),Leaf (RPMT [MTT "16.b",MTT "owed by an organisation to the individual."])],Any (Just (Pre "17 Any of the following:")) [Leaf (RPMT [MTT "17.a",MTT "the terms and conditions of any accident and health policy or life policy (called in this item the applicable policy) of which the individual is the policy owner or under which the individual is a beneficiary;"]),Leaf (RPMT [MTT "17.b",MTT "the premium payable by the policy owner under the applicable policy;"]),Leaf (RPMT [MTT "17.c",MTT "the benefits payable to any beneficiary under the applicable policy;"]),Leaf (RPMT [MTT "17.d",MTT "any information relating to any claim on, or payment under, the applicable policy, including the condition of the health of the individual and the diagnosis, treatment, prevention or alleviation of any ailment, condition, disability, disease, disorder or injury that the individual has suffered or is suffering from;"]),Leaf (RPMT [MTT "17.e",MTT "any other information that the individual is the policy owner of, or a beneficiary under, an applicable policy."])],Any (Just (Pre "18 The assessment, diagnosis, treatment, prevention or alleviation by a health professional of any of the following affecting the individual:")) [Leaf (RPMT [MTT "18.a",MTT "any sexually-transmitted disease such as Chlamydial Genital Infection, Gonorrhoea and Syphilis;"]),Leaf (RPMT [MTT "18.b",MTT "Human Immunodeficiency Virus Infection;"]),Leaf (RPMT [MTT "mental illness;"]),Leaf (RPMT [MTT "18.c",MTT "schizophrenia or delusional disorder;"]),Leaf (RPMT [MTT "18.d",MTT "substance abuse and addiction, including drug addiction and alcoholism"])],Any (Just (Pre "19 The provision of treatment to the individual for or in respect of \8212")) [Leaf (RPMT [MTT "19.a",MTT "the donation or receipt of a human egg or human sperm; or"]),Leaf (RPMT [MTT "19.b",MTT "any contraceptive operation or procedure or abortion."])],Any (Just (Pre "20 Any of the following:")) [Leaf (RPMT [MTT "20.a",MTT "subject to section 4(4)(b) of the Act, the donation and removal of any organ from the body of the deceased individual for the purpose of its transplantation into the body of another individual;"]),Leaf (RPMT [MTT "20.b",MTT "the donation and removal of any specified organ from the individual, being a living organ donor, for the purpose of its transplantation into the body of another individual;"]),Leaf (RPMT [MTT "20.c",MTT "the transplantation of any organ mentioned in sub-paragraph (a) or (b) into the body of the individual."])],Leaf (RPMT [MTI 21,MTT "Subject to section 4(4)(b) of the Act, the suicide or attempted suicide of the individual."]),Leaf (RPMT [MTI 22,MTT "Domestic abuse, child abuse or sexual abuse involving or alleged to involve the individual."]),Any (Just (Pre "23 Any of the following:")) [Leaf (RPMT [MTT "23.a",MTT "information that the individual is or had been adopted pursuant to an adoption order made under the Adoption of Children Act (Cap. 4), or is or had been the subject of an application for an adoption order;"]),Leaf (RPMT [MTT "23.b",MTT "the identity of the natural father or mother of the individual;"]),Leaf (RPMT [MTT "23.c",MTT "the identity of the adoptive father or mother of the individual;"]),Leaf (RPMT [MTT "23.d",MTT "the identity of any applicant for an adoption order;"]),Leaf (RPMT [MTT "23.e",MTT "the identity of any person whose consent is necessary under that Act for an adoption order to be made, whether or not the court has dispensed with the consent of that person in accordance with that Act;"]),Leaf (RPMT [MTT "23.f",MTT "any other information that the individual is or had been an adopted child or relating to the adoption of the individual."])]])}], rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 8, srccol = 233, version = Nothing}), defaults = [], symtab = []},DefNameAlias {name = [MTT "You"], detail = [MTT "Organisation"], nlhint = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 9, srccol = 26, version = Nothing})},DefTypically {name = [MTT "is",MTT "a Public Agency"], defaults = [RPConstraint [MTT "is",MTT "a Public Agency"] RPis [MTT "not"]], srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 9, srccol = 27, version = Nothing})},DefNameAlias {name = [MTT "NDB"], detail = [MTT "it is a Notifiable Data Breach"], nlhint = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 9, srccol = 74, version = Nothing})},Hornlike {name = [MTT "a data breach",MTT "occurred"], super = Nothing, keyword = Means, given = Nothing, giveth = Nothing, upon = Nothing, clauses = [HC {hHead = RPBoolStructR [MTT "a data breach",MTT "occurred"] RPis (Any Nothing [Any (Just (PrePost "any unauthorised" "of personal data")) [Leaf (RPMT [MTT "access"]),Leaf (RPMT [MTT "use"]),Leaf (RPMT [MTT "disclosure"]),Leaf (RPMT [MTT "copying"]),Leaf (RPMT [MTT "modification"]),Leaf (RPMT [MTT "disposal"])],Any (Just (PrePost "loss of storage medium on which personal data is stored in circumstances where the unauthorised" "of the personal data is likely to occur")) [Leaf (RPMT [MTT "access"]),Leaf (RPMT [MTT "use"]),Leaf (RPMT [MTT "disclosure"]),Leaf (RPMT [MTT "copying"]),Leaf (RPMT [MTT "modification"]),Leaf (RPMT [MTT "disposal"])]]), hBody = Nothing}], rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 10, srccol = 74, version = Nothing}), defaults = [], symtab = []},Hornlike {name = [MTT "the data breach is in relation to any prescribed personal data or class of personal data relating to the individual"], super = Nothing, keyword = Means, given = Nothing, giveth = Nothing, upon = Nothing, clauses = [HC {hHead = RPBoolStructR [MTT "the data breach is in relation to any prescribed personal data or class of personal data relating to the individual"] RPis (All Nothing [Any (Just (Pre "the data breach relates to the individual's")) [Leaf (RPMT [MTT "full name"]),Leaf (RPMT [MTT "alias"]),Leaf (RPMT [MTT "identification number"])],Leaf (RPMT [MTT "the data breach relates to",MTT "any of the prescribed personal data or classes of personal data relating to the individual"])]), hBody = Nothing}], rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 12, srccol = 91, version = Nothing}), defaults = [], symtab = []},Hornlike {name = [MTT "it results in, or is likely to result in, significant harm to an affected individual"], super = Nothing, keyword = Means, given = Nothing, giveth = Nothing, upon = Nothing, clauses = [HC {hHead = RPBoolStructR [MTT "it results in, or is likely to result in, significant harm to an affected individual"] RPis (All Nothing [Leaf (RPMT [MTT "the data breach is in relation to any prescribed personal data or class of personal data relating to the individual"]),Not (Any Nothing [Leaf (RPMT [MTT "the organisation has taken any action ",MTT "to render it unlikely that the notifiable data breach will result in significant harm to the individual"]),Leaf (RPMT [MTT "the organisation already implemented any technological measure",MTT "to render it unlikely that the notifiable data breach will result in significant harm to the individual"])])]), hBody = Nothing}], rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 10, srccol = 90, version = Nothing}), defaults = [], symtab = []},DefNameAlias {name = [MTT "the PDPC Exclusion List"], detail = [MTT "with",MTT "a list of individuals to exclude from notification"], nlhint = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 11, srccol = 160, version = Nothing})}]

pdpafullExpandedGold :: [Rule]
pdpafullExpandedGold = [Regulative {subj = Leaf ((MTT "Organisation" :| [],Nothing) :| []), rkeyword = REvery, who = Just (Not (Leaf (RPMT [MTT "is",MTT "a Public Agency"]))), cond = Just (Any Nothing [Leaf (RPConstraint [MTT "the data breach occurred"] (RPTC TOn) [MTT "1 Feb 2022"]),Leaf (RPConstraint [MTT "the data breach occurred"] (RPTC TAfter) [MTT "1 Feb 2022"])]), deontic = DMust, action = Leaf ((MTT "assess" :| [MTT "if it is a Notifiable Data Breach"],Nothing) :| [(MTT "by" :| [MTT "evaluating",MTT "NDB Qualification"],Nothing)]), temporal = Just (TemporalConstraint TBefore (Just 30) "days"), hence = Just (RuleAlias [MTT "Notification"]), lest = Just (RuleAlias [MTT "PDPC query with demand"]), rlabel = Just ("\167",1,"Assessment"), lsource = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 8, srccol = 24, version = Nothing}), upon = Just ((MTT "becoming aware a data breach may have occurred" :| [],Nothing) :| []), given = Nothing, having = Nothing, wwhere = [], defaults = [], symtab = []},Regulative {subj = Leaf ((MTT "the PDPC" :| [],Nothing) :| []), rkeyword = RParty, who = Nothing, cond = Nothing, deontic = DMay, action = Leaf ((MTT "query" :| [MTT "You"],Nothing) :| [(MTT "with" :| [MTT "a demand"],Nothing),(MTT "for" :| [MTT "an explanation of your inaction"],Nothing)]), temporal = Nothing, hence = Just (RuleAlias [MTT "Respond to PDPC"]), lest = Nothing, rlabel = Just ("\167",1,"PDPC query with demand"), lsource = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 7, srccol = 37, version = Nothing}), upon = Nothing, given = Nothing, having = Nothing, wwhere = [], defaults = [], symtab = []},Regulative {subj = Leaf ((MTT "You" :| [],Nothing) :| []), rkeyword = RParty, who = Nothing, cond = Nothing, deontic = DMust, action = Leaf ((MTT "respond to" :| [MTT "the PDPC"],Nothing) :| [(MTT "with" :| [MTT "an explanation of your inaction"],Nothing)]), temporal = Nothing, hence = Nothing, lest = Nothing, rlabel = Just ("\167",1,"Respond to PDPC"), lsource = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 7, srccol = 45, version = Nothing}), upon = Nothing, given = Nothing, having = Nothing, wwhere = [], defaults = [], symtab = []},Hornlike {name = [MTT "it is a Notifiable Data Breach"], super = Nothing, keyword = Decide, given = Nothing, giveth = Nothing, upon = Nothing, clauses = [HC {hHead = RPMT [MTT "it is a Notifiable Data Breach"], hBody = Just (All Nothing [Any Nothing [Any (Just (PrePost "any unauthorised" "of personal data")) [Leaf (RPMT [MTT "access"]),Leaf (RPMT [MTT "use"]),Leaf (RPMT [MTT "disclosure"]),Leaf (RPMT [MTT "copying"]),Leaf (RPMT [MTT "modification"]),Leaf (RPMT [MTT "disposal"])],Any (Just (PrePost "loss of storage medium on which personal data is stored in circumstances where the unauthorised" "of the personal data is likely to occur")) [Leaf (RPMT [MTT "access"]),Leaf (RPMT [MTT "use"]),Leaf (RPMT [MTT "disclosure"]),Leaf (RPMT [MTT "copying"]),Leaf (RPMT [MTT "modification"]),Leaf (RPMT [MTT "disposal"])]],Not (Leaf (RPMT [MTT "the data breach occurred only within an organisation"])),Any Nothing [All Nothing [All Nothing [Any (Just (Pre "the data breach relates to the individual's")) [Leaf (RPMT [MTT "full name"]),Leaf (RPMT [MTT "alias"]),Leaf (RPMT [MTT "identification number"])],Any Nothing [Leaf (RPMT [MTI 1,MTT "The amount of any wages, salary, fee, commission, bonus, gratuity, allowance or other remuneration paid or payable to the individual by any person, whether under a contract of service or a contract for services."]),Leaf (RPMT [MTI 2,MTT "The income of the individual from the sale of any goods or property."]),Leaf (RPMT [MTI 3,MTT "The number of any credit card, charge card or debit card issued to or in the name of the individual."]),Leaf (RPMT [MTI 4,MTT "The number assigned to any account the individual has with any organisation that is a bank or finance company."]),Any (Just (Pre "5 Any information that identifies, or is likely to lead to the identification of, the individual as a child or young person who \8212")) [Leaf (RPMT [MTT "5.a",MTT "is or had been the subject of any investigation under the CYPA;"]),Leaf (RPMT [MTT "5.b",MTT "is or had been arrested, on or after 1 July 2020, for an offence committed under any written law;"]),Leaf (RPMT [MTT "5.c",MTT "is or had been taken into care or custody by the Director-General of Social Welfare, a protector, any officer generally or specially authorised in that behalf in writing by the Director-General or protector or a police officer under the CYPA;"]),Leaf (RPMT [MTT "5.d",MTT "is attending or had attended a family programme in relation to an application to be made under section 50 of the CYPA;"]),Leaf (RPMT [MTT "5.e",MTT "is or was the subject of an order made by a court under the CYPA; or"]),Leaf (RPMT [MTT "5.f",MTT "is or had been concerned in any proceedings in any court or on appeal from any court, whether the individual is the person against or in respect of whom the proceedings are taken or a witness in those proceedings."])],Any (Just (Pre "6 Any information that identifies, or is likely to lead to the identification of \8212 b")) [Leaf (RPMT [MTT "6.a",MTT "the individual who has been or is the subject of any investigation, examination, assessment or treatment under the VAA relating to whether the individual is a vulnerable adult experiencing or at risk of abuse, neglect or self-neglect;"]),Leaf (RPMT [MTT "6.b",MTT "the individual as a vulnerable adult who has been committed to a place of temporary care and protection or place of safety or to the care of a fit person under the VAA;"]),Leaf (RPMT [MTT "6.c",MTT "the individual as a vulnerable adult who is the subject of an order made by a court under the VAA;"]),Leaf (RPMT [MTT "6.d",MTT "a place of temporary care and protection or place of safety in which an individual or a vulnerable adult mentioned in sub-paragraph (a), (b) or (c) is committed, or the location of such a place of temporary care and protection or place of safety; or"]),Leaf (RPMT [MTT "6.e",MTT "a fit person under whose care an individual or a vulnerable adult mentioned in sub-paragraph (a), (b) or (c) is placed, or the location of the premises of such a fit person."])],Any (Just (Pre "7 Any private key of or relating to the individual that is used or may be used \8212")) [Leaf (RPMT [MTT "7.a",MTT "to create a secure electronic record or secure electronic signature;"]),Leaf (RPMT [MTT "7.b",MTT "to verify the integrity of a secure electronic record; or"]),Leaf (RPMT [MTT "7.c",MTT "to verify the authenticity or integrity of a secure electronic signature."])],Leaf (RPMT [MTI 8,MTT "The net worth of the individual."]),Leaf (RPMT [MTI 9,MTT "The deposit of moneys by the individual with any organisation."]),Leaf (RPMT [MTI 10,MTT "The withdrawal by the individual of moneys deposited with any organisation."]),Leaf (RPMT [MTI 11,MTT "The granting by an organisation of advances, loans and other facilities by which the individual, being a customer of the organisation, has access to funds or financial guarantees."]),Leaf (RPMT [MTI 12,MTT "The incurring by the organisation of any liabilities other than those mentioned in paragraph 11 on behalf of the individual."]),Leaf (RPMT [MTI 13,MTT "The payment of any moneys, or transfer of any property, by any person to the individual, including the amount of the moneys paid or the value of the property transferred, as the case may be."]),Leaf (RPMT [MTI 14,MTT "The creditworthiness of the individual."]),Leaf (RPMT [MTI 15,MTT "The individual\8217s investment in any capital markets products."]),Any (Just (Pre "16 The existence, and amount due or outstanding, of any debt \8212")) [Leaf (RPMT [MTT "16.a",MTT "owed by the individual to an organisation; or"]),Leaf (RPMT [MTT "16.b",MTT "owed by an organisation to the individual."])],Any (Just (Pre "17 Any of the following:")) [Leaf (RPMT [MTT "17.a",MTT "the terms and conditions of any accident and health policy or life policy (called in this item the applicable policy) of which the individual is the policy owner or under which the individual is a beneficiary;"]),Leaf (RPMT [MTT "17.b",MTT "the premium payable by the policy owner under the applicable policy;"]),Leaf (RPMT [MTT "17.c",MTT "the benefits payable to any beneficiary under the applicable policy;"]),Leaf (RPMT [MTT "17.d",MTT "any information relating to any claim on, or payment under, the applicable policy, including the condition of the health of the individual and the diagnosis, treatment, prevention or alleviation of any ailment, condition, disability, disease, disorder or injury that the individual has suffered or is suffering from;"]),Leaf (RPMT [MTT "17.e",MTT "any other information that the individual is the policy owner of, or a beneficiary under, an applicable policy."])],Any (Just (Pre "18 The assessment, diagnosis, treatment, prevention or alleviation by a health professional of any of the following affecting the individual:")) [Leaf (RPMT [MTT "18.a",MTT "any sexually-transmitted disease such as Chlamydial Genital Infection, Gonorrhoea and Syphilis;"]),Leaf (RPMT [MTT "18.b",MTT "Human Immunodeficiency Virus Infection;"]),Leaf (RPMT [MTT "mental illness;"]),Leaf (RPMT [MTT "18.c",MTT "schizophrenia or delusional disorder;"]),Leaf (RPMT [MTT "18.d",MTT "substance abuse and addiction, including drug addiction and alcoholism"])],Any (Just (Pre "19 The provision of treatment to the individual for or in respect of \8212")) [Leaf (RPMT [MTT "19.a",MTT "the donation or receipt of a human egg or human sperm; or"]),Leaf (RPMT [MTT "19.b",MTT "any contraceptive operation or procedure or abortion."])],Any (Just (Pre "20 Any of the following:")) [Leaf (RPMT [MTT "20.a",MTT "subject to section 4(4)(b) of the Act, the donation and removal of any organ from the body of the deceased individual for the purpose of its transplantation into the body of another individual;"]),Leaf (RPMT [MTT "20.b",MTT "the donation and removal of any specified organ from the individual, being a living organ donor, for the purpose of its transplantation into the body of another individual;"]),Leaf (RPMT [MTT "20.c",MTT "the transplantation of any organ mentioned in sub-paragraph (a) or (b) into the body of the individual."])],Leaf (RPMT [MTI 21,MTT "Subject to section 4(4)(b) of the Act, the suicide or attempted suicide of the individual."]),Leaf (RPMT [MTI 22,MTT "Domestic abuse, child abuse or sexual abuse involving or alleged to involve the individual."]),Any (Just (Pre "23 Any of the following:")) [Leaf (RPMT [MTT "23.a",MTT "information that the individual is or had been adopted pursuant to an adoption order made under the Adoption of Children Act (Cap. 4), or is or had been the subject of an application for an adoption order;"]),Leaf (RPMT [MTT "23.b",MTT "the identity of the natural father or mother of the individual;"]),Leaf (RPMT [MTT "23.c",MTT "the identity of the adoptive father or mother of the individual;"]),Leaf (RPMT [MTT "23.d",MTT "the identity of any applicant for an adoption order;"]),Leaf (RPMT [MTT "23.e",MTT "the identity of any person whose consent is necessary under that Act for an adoption order to be made, whether or not the court has dispensed with the consent of that person in accordance with that Act;"]),Leaf (RPMT [MTT "23.f",MTT "any other information that the individual is or had been an adopted child or relating to the adoption of the individual."])]]],Not (Any Nothing [Leaf (RPMT [MTT "the organisation has taken any action ",MTT "to render it unlikely that the notifiable data breach will result in significant harm to the individual"]),Leaf (RPMT [MTT "the organisation already implemented any technological measure",MTT "to render it unlikely that the notifiable data breach will result in significant harm to the individual"])])],Leaf (RPConstraint [MTT "the number of affected individuals"] RPgt [MTT "the prescribed threshold of affected individuals"])]])}], rlabel = Just ("\167",1,"NDB Qualification"), lsource = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 8, srccol = 72, version = Nothing}), defaults = [], symtab = []},Hornlike {name = [MTT "the prescribed threshold of affected individuals"], super = Nothing, keyword = Means, given = Nothing, giveth = Nothing, upon = Nothing, clauses = [HC {hHead = RPBoolStructR [MTT "the prescribed threshold of affected individuals"] RPis (Leaf (RPMT [MTI 500])), hBody = Nothing}], rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 17, srccol = 126, version = Nothing}), defaults = [], symtab = []},Hornlike {name = [MTT "Notification"], super = Nothing, keyword = Means, given = Nothing, giveth = Nothing, upon = Nothing, clauses = [HC {hHead = RPBoolStructR [MTT "Notification"] RPis (All Nothing [Leaf (RPMT [MTT "Notify PDPC"]),Leaf (RPMT [MTT "Notify Individuals"])]), hBody = Nothing}], rlabel = Just ("\167",1,"Notification"), lsource = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 9, srccol = 134, version = Nothing}), defaults = [], symtab = []},Regulative {subj = Leaf ((MTT "You" :| [],Nothing) :| []), rkeyword = RParty, who = Nothing, cond = Just (All Nothing [All Nothing [Any Nothing [Any (Just (PrePost "any unauthorised" "of personal data")) [Leaf (RPMT [MTT "access"]),Leaf (RPMT [MTT "use"]),Leaf (RPMT [MTT "disclosure"]),Leaf (RPMT [MTT "copying"]),Leaf (RPMT [MTT "modification"]),Leaf (RPMT [MTT "disposal"])],Any (Just (PrePost "loss of storage medium on which personal data is stored in circumstances where the unauthorised" "of the personal data is likely to occur")) [Leaf (RPMT [MTT "access"]),Leaf (RPMT [MTT "use"]),Leaf (RPMT [MTT "disclosure"]),Leaf (RPMT [MTT "copying"]),Leaf (RPMT [MTT "modification"]),Leaf (RPMT [MTT "disposal"])]],Not (Leaf (RPMT [MTT "the data breach occurred only within an organisation"])),Any Nothing [All Nothing [All Nothing [Any (Just (Pre "the data breach relates to the individual's")) [Leaf (RPMT [MTT "full name"]),Leaf (RPMT [MTT "alias"]),Leaf (RPMT [MTT "identification number"])],Any Nothing [Leaf (RPMT [MTI 1,MTT "The amount of any wages, salary, fee, commission, bonus, gratuity, allowance or other remuneration paid or payable to the individual by any person, whether under a contract of service or a contract for services."]),Leaf (RPMT [MTI 2,MTT "The income of the individual from the sale of any goods or property."]),Leaf (RPMT [MTI 3,MTT "The number of any credit card, charge card or debit card issued to or in the name of the individual."]),Leaf (RPMT [MTI 4,MTT "The number assigned to any account the individual has with any organisation that is a bank or finance company."]),Any (Just (Pre "5 Any information that identifies, or is likely to lead to the identification of, the individual as a child or young person who \8212")) [Leaf (RPMT [MTT "5.a",MTT "is or had been the subject of any investigation under the CYPA;"]),Leaf (RPMT [MTT "5.b",MTT "is or had been arrested, on or after 1 July 2020, for an offence committed under any written law;"]),Leaf (RPMT [MTT "5.c",MTT "is or had been taken into care or custody by the Director-General of Social Welfare, a protector, any officer generally or specially authorised in that behalf in writing by the Director-General or protector or a police officer under the CYPA;"]),Leaf (RPMT [MTT "5.d",MTT "is attending or had attended a family programme in relation to an application to be made under section 50 of the CYPA;"]),Leaf (RPMT [MTT "5.e",MTT "is or was the subject of an order made by a court under the CYPA; or"]),Leaf (RPMT [MTT "5.f",MTT "is or had been concerned in any proceedings in any court or on appeal from any court, whether the individual is the person against or in respect of whom the proceedings are taken or a witness in those proceedings."])],Any (Just (Pre "6 Any information that identifies, or is likely to lead to the identification of \8212 b")) [Leaf (RPMT [MTT "6.a",MTT "the individual who has been or is the subject of any investigation, examination, assessment or treatment under the VAA relating to whether the individual is a vulnerable adult experiencing or at risk of abuse, neglect or self-neglect;"]),Leaf (RPMT [MTT "6.b",MTT "the individual as a vulnerable adult who has been committed to a place of temporary care and protection or place of safety or to the care of a fit person under the VAA;"]),Leaf (RPMT [MTT "6.c",MTT "the individual as a vulnerable adult who is the subject of an order made by a court under the VAA;"]),Leaf (RPMT [MTT "6.d",MTT "a place of temporary care and protection or place of safety in which an individual or a vulnerable adult mentioned in sub-paragraph (a), (b) or (c) is committed, or the location of such a place of temporary care and protection or place of safety; or"]),Leaf (RPMT [MTT "6.e",MTT "a fit person under whose care an individual or a vulnerable adult mentioned in sub-paragraph (a), (b) or (c) is placed, or the location of the premises of such a fit person."])],Any (Just (Pre "7 Any private key of or relating to the individual that is used or may be used \8212")) [Leaf (RPMT [MTT "7.a",MTT "to create a secure electronic record or secure electronic signature;"]),Leaf (RPMT [MTT "7.b",MTT "to verify the integrity of a secure electronic record; or"]),Leaf (RPMT [MTT "7.c",MTT "to verify the authenticity or integrity of a secure electronic signature."])],Leaf (RPMT [MTI 8,MTT "The net worth of the individual."]),Leaf (RPMT [MTI 9,MTT "The deposit of moneys by the individual with any organisation."]),Leaf (RPMT [MTI 10,MTT "The withdrawal by the individual of moneys deposited with any organisation."]),Leaf (RPMT [MTI 11,MTT "The granting by an organisation of advances, loans and other facilities by which the individual, being a customer of the organisation, has access to funds or financial guarantees."]),Leaf (RPMT [MTI 12,MTT "The incurring by the organisation of any liabilities other than those mentioned in paragraph 11 on behalf of the individual."]),Leaf (RPMT [MTI 13,MTT "The payment of any moneys, or transfer of any property, by any person to the individual, including the amount of the moneys paid or the value of the property transferred, as the case may be."]),Leaf (RPMT [MTI 14,MTT "The creditworthiness of the individual."]),Leaf (RPMT [MTI 15,MTT "The individual\8217s investment in any capital markets products."]),Any (Just (Pre "16 The existence, and amount due or outstanding, of any debt \8212")) [Leaf (RPMT [MTT "16.a",MTT "owed by the individual to an organisation; or"]),Leaf (RPMT [MTT "16.b",MTT "owed by an organisation to the individual."])],Any (Just (Pre "17 Any of the following:")) [Leaf (RPMT [MTT "17.a",MTT "the terms and conditions of any accident and health policy or life policy (called in this item the applicable policy) of which the individual is the policy owner or under which the individual is a beneficiary;"]),Leaf (RPMT [MTT "17.b",MTT "the premium payable by the policy owner under the applicable policy;"]),Leaf (RPMT [MTT "17.c",MTT "the benefits payable to any beneficiary under the applicable policy;"]),Leaf (RPMT [MTT "17.d",MTT "any information relating to any claim on, or payment under, the applicable policy, including the condition of the health of the individual and the diagnosis, treatment, prevention or alleviation of any ailment, condition, disability, disease, disorder or injury that the individual has suffered or is suffering from;"]),Leaf (RPMT [MTT "17.e",MTT "any other information that the individual is the policy owner of, or a beneficiary under, an applicable policy."])],Any (Just (Pre "18 The assessment, diagnosis, treatment, prevention or alleviation by a health professional of any of the following affecting the individual:")) [Leaf (RPMT [MTT "18.a",MTT "any sexually-transmitted disease such as Chlamydial Genital Infection, Gonorrhoea and Syphilis;"]),Leaf (RPMT [MTT "18.b",MTT "Human Immunodeficiency Virus Infection;"]),Leaf (RPMT [MTT "mental illness;"]),Leaf (RPMT [MTT "18.c",MTT "schizophrenia or delusional disorder;"]),Leaf (RPMT [MTT "18.d",MTT "substance abuse and addiction, including drug addiction and alcoholism"])],Any (Just (Pre "19 The provision of treatment to the individual for or in respect of \8212")) [Leaf (RPMT [MTT "19.a",MTT "the donation or receipt of a human egg or human sperm; or"]),Leaf (RPMT [MTT "19.b",MTT "any contraceptive operation or procedure or abortion."])],Any (Just (Pre "20 Any of the following:")) [Leaf (RPMT [MTT "20.a",MTT "subject to section 4(4)(b) of the Act, the donation and removal of any organ from the body of the deceased individual for the purpose of its transplantation into the body of another individual;"]),Leaf (RPMT [MTT "20.b",MTT "the donation and removal of any specified organ from the individual, being a living organ donor, for the purpose of its transplantation into the body of another individual;"]),Leaf (RPMT [MTT "20.c",MTT "the transplantation of any organ mentioned in sub-paragraph (a) or (b) into the body of the individual."])],Leaf (RPMT [MTI 21,MTT "Subject to section 4(4)(b) of the Act, the suicide or attempted suicide of the individual."]),Leaf (RPMT [MTI 22,MTT "Domestic abuse, child abuse or sexual abuse involving or alleged to involve the individual."]),Any (Just (Pre "23 Any of the following:")) [Leaf (RPMT [MTT "23.a",MTT "information that the individual is or had been adopted pursuant to an adoption order made under the Adoption of Children Act (Cap. 4), or is or had been the subject of an application for an adoption order;"]),Leaf (RPMT [MTT "23.b",MTT "the identity of the natural father or mother of the individual;"]),Leaf (RPMT [MTT "23.c",MTT "the identity of the adoptive father or mother of the individual;"]),Leaf (RPMT [MTT "23.d",MTT "the identity of any applicant for an adoption order;"]),Leaf (RPMT [MTT "23.e",MTT "the identity of any person whose consent is necessary under that Act for an adoption order to be made, whether or not the court has dispensed with the consent of that person in accordance with that Act;"]),Leaf (RPMT [MTT "23.f",MTT "any other information that the individual is or had been an adopted child or relating to the adoption of the individual."])]]],All Nothing [Not (Leaf (RPMT [MTT "the organisation has taken any action ",MTT "to render it unlikely that the notifiable data breach will result in significant harm to the individual"])),Not (Leaf (RPMT [MTT "the organisation already implemented any technological measure",MTT "to render it unlikely that the notifiable data breach will result in significant harm to the individual"]))]],Leaf (RPConstraint [MTT "the number of affected individuals"] RPgt [MTT "the prescribed threshold of affected individuals"])]],Not (Leaf (RPMT [MTT "you are a Public Agency"]))]), deontic = DMust, action = Leaf ((MTT "NOTIFY" :| [MTT "the PDPC"],Nothing) :| [(MTT "in" :| [MTT "the form and manner specified at www.pdpc.gov.sg"],Nothing),(MTT "with" :| [MTT "a Notification Message"],Nothing),(MTT "containing" :| [MTT "a list of individuals for whom notification waiver is sought"],Nothing),(MTT "and" :| [MTT "justification for why"],Nothing)]), temporal = Just (TemporalConstraint TBefore (Just 3) "DAYS"), hence = Just (RuleAlias [MTT "PDPC prohibit notify individuals"]), lest = Nothing, rlabel = Just ("\167",2,"Notify PDPC"), lsource = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 9, srccol = 139, version = Nothing}), upon = Nothing, given = Nothing, having = Nothing, wwhere = [], defaults = [], symtab = []},Regulative {subj = Leaf ((MTT "the PDPC" :| [],Nothing) :| []), rkeyword = RParty, who = Nothing, cond = Nothing, deontic = DMay, action = Leaf ((MTT "NOTIFY" :| [MTT "you"],Nothing) :| [(MTT "with" :| [MTT "a list of individuals to exclude from notification"],Nothing)]), temporal = Nothing, hence = Just (RuleAlias [MTT "Cannot notify individuals"]), lest = Nothing, rlabel = Just ("\167",2,"PDPC prohibit notify individuals"), lsource = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 9, srccol = 155, version = Nothing}), upon = Nothing, given = Nothing, having = Nothing, wwhere = [], defaults = [], symtab = []},Regulative {subj = Leaf ((MTT "YOU" :| [],Nothing) :| []), rkeyword = RParty, who = Nothing, cond = Nothing, deontic = DShant, action = Leaf ((MTT "NOTIFY" :| [MTT "each of the Notifiable Individuals"],Nothing) :| [(MTT "who" :| [MTT "are on the affected list"],Nothing)]), temporal = Nothing, hence = Nothing, lest = Nothing, rlabel = Just ("\167",2,"Cannot notify individuals"), lsource = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 9, srccol = 163, version = Nothing}), upon = Nothing, given = Nothing, having = Nothing, wwhere = [], defaults = [], symtab = []},Regulative {subj = Leaf ((MTT "You" :| [],Nothing) :| []), rkeyword = RParty, who = Nothing, cond = Just (All Nothing [All Nothing [Any Nothing [Any (Just (PrePost "any unauthorised" "of personal data")) [Leaf (RPMT [MTT "access"]),Leaf (RPMT [MTT "use"]),Leaf (RPMT [MTT "disclosure"]),Leaf (RPMT [MTT "copying"]),Leaf (RPMT [MTT "modification"]),Leaf (RPMT [MTT "disposal"])],Any (Just (PrePost "loss of storage medium on which personal data is stored in circumstances where the unauthorised" "of the personal data is likely to occur")) [Leaf (RPMT [MTT "access"]),Leaf (RPMT [MTT "use"]),Leaf (RPMT [MTT "disclosure"]),Leaf (RPMT [MTT "copying"]),Leaf (RPMT [MTT "modification"]),Leaf (RPMT [MTT "disposal"])]],Not (Leaf (RPMT [MTT "the data breach occurred only within an organisation"])),Any Nothing [All Nothing [All Nothing [Any (Just (Pre "the data breach relates to the individual's")) [Leaf (RPMT [MTT "full name"]),Leaf (RPMT [MTT "alias"]),Leaf (RPMT [MTT "identification number"])],Any Nothing [Leaf (RPMT [MTI 1,MTT "The amount of any wages, salary, fee, commission, bonus, gratuity, allowance or other remuneration paid or payable to the individual by any person, whether under a contract of service or a contract for services."]),Leaf (RPMT [MTI 2,MTT "The income of the individual from the sale of any goods or property."]),Leaf (RPMT [MTI 3,MTT "The number of any credit card, charge card or debit card issued to or in the name of the individual."]),Leaf (RPMT [MTI 4,MTT "The number assigned to any account the individual has with any organisation that is a bank or finance company."]),Any (Just (Pre "5 Any information that identifies, or is likely to lead to the identification of, the individual as a child or young person who \8212")) [Leaf (RPMT [MTT "5.a",MTT "is or had been the subject of any investigation under the CYPA;"]),Leaf (RPMT [MTT "5.b",MTT "is or had been arrested, on or after 1 July 2020, for an offence committed under any written law;"]),Leaf (RPMT [MTT "5.c",MTT "is or had been taken into care or custody by the Director-General of Social Welfare, a protector, any officer generally or specially authorised in that behalf in writing by the Director-General or protector or a police officer under the CYPA;"]),Leaf (RPMT [MTT "5.d",MTT "is attending or had attended a family programme in relation to an application to be made under section 50 of the CYPA;"]),Leaf (RPMT [MTT "5.e",MTT "is or was the subject of an order made by a court under the CYPA; or"]),Leaf (RPMT [MTT "5.f",MTT "is or had been concerned in any proceedings in any court or on appeal from any court, whether the individual is the person against or in respect of whom the proceedings are taken or a witness in those proceedings."])],Any (Just (Pre "6 Any information that identifies, or is likely to lead to the identification of \8212 b")) [Leaf (RPMT [MTT "6.a",MTT "the individual who has been or is the subject of any investigation, examination, assessment or treatment under the VAA relating to whether the individual is a vulnerable adult experiencing or at risk of abuse, neglect or self-neglect;"]),Leaf (RPMT [MTT "6.b",MTT "the individual as a vulnerable adult who has been committed to a place of temporary care and protection or place of safety or to the care of a fit person under the VAA;"]),Leaf (RPMT [MTT "6.c",MTT "the individual as a vulnerable adult who is the subject of an order made by a court under the VAA;"]),Leaf (RPMT [MTT "6.d",MTT "a place of temporary care and protection or place of safety in which an individual or a vulnerable adult mentioned in sub-paragraph (a), (b) or (c) is committed, or the location of such a place of temporary care and protection or place of safety; or"]),Leaf (RPMT [MTT "6.e",MTT "a fit person under whose care an individual or a vulnerable adult mentioned in sub-paragraph (a), (b) or (c) is placed, or the location of the premises of such a fit person."])],Any (Just (Pre "7 Any private key of or relating to the individual that is used or may be used \8212")) [Leaf (RPMT [MTT "7.a",MTT "to create a secure electronic record or secure electronic signature;"]),Leaf (RPMT [MTT "7.b",MTT "to verify the integrity of a secure electronic record; or"]),Leaf (RPMT [MTT "7.c",MTT "to verify the authenticity or integrity of a secure electronic signature."])],Leaf (RPMT [MTI 8,MTT "The net worth of the individual."]),Leaf (RPMT [MTI 9,MTT "The deposit of moneys by the individual with any organisation."]),Leaf (RPMT [MTI 10,MTT "The withdrawal by the individual of moneys deposited with any organisation."]),Leaf (RPMT [MTI 11,MTT "The granting by an organisation of advances, loans and other facilities by which the individual, being a customer of the organisation, has access to funds or financial guarantees."]),Leaf (RPMT [MTI 12,MTT "The incurring by the organisation of any liabilities other than those mentioned in paragraph 11 on behalf of the individual."]),Leaf (RPMT [MTI 13,MTT "The payment of any moneys, or transfer of any property, by any person to the individual, including the amount of the moneys paid or the value of the property transferred, as the case may be."]),Leaf (RPMT [MTI 14,MTT "The creditworthiness of the individual."]),Leaf (RPMT [MTI 15,MTT "The individual\8217s investment in any capital markets products."]),Any (Just (Pre "16 The existence, and amount due or outstanding, of any debt \8212")) [Leaf (RPMT [MTT "16.a",MTT "owed by the individual to an organisation; or"]),Leaf (RPMT [MTT "16.b",MTT "owed by an organisation to the individual."])],Any (Just (Pre "17 Any of the following:")) [Leaf (RPMT [MTT "17.a",MTT "the terms and conditions of any accident and health policy or life policy (called in this item the applicable policy) of which the individual is the policy owner or under which the individual is a beneficiary;"]),Leaf (RPMT [MTT "17.b",MTT "the premium payable by the policy owner under the applicable policy;"]),Leaf (RPMT [MTT "17.c",MTT "the benefits payable to any beneficiary under the applicable policy;"]),Leaf (RPMT [MTT "17.d",MTT "any information relating to any claim on, or payment under, the applicable policy, including the condition of the health of the individual and the diagnosis, treatment, prevention or alleviation of any ailment, condition, disability, disease, disorder or injury that the individual has suffered or is suffering from;"]),Leaf (RPMT [MTT "17.e",MTT "any other information that the individual is the policy owner of, or a beneficiary under, an applicable policy."])],Any (Just (Pre "18 The assessment, diagnosis, treatment, prevention or alleviation by a health professional of any of the following affecting the individual:")) [Leaf (RPMT [MTT "18.a",MTT "any sexually-transmitted disease such as Chlamydial Genital Infection, Gonorrhoea and Syphilis;"]),Leaf (RPMT [MTT "18.b",MTT "Human Immunodeficiency Virus Infection;"]),Leaf (RPMT [MTT "mental illness;"]),Leaf (RPMT [MTT "18.c",MTT "schizophrenia or delusional disorder;"]),Leaf (RPMT [MTT "18.d",MTT "substance abuse and addiction, including drug addiction and alcoholism"])],Any (Just (Pre "19 The provision of treatment to the individual for or in respect of \8212")) [Leaf (RPMT [MTT "19.a",MTT "the donation or receipt of a human egg or human sperm; or"]),Leaf (RPMT [MTT "19.b",MTT "any contraceptive operation or procedure or abortion."])],Any (Just (Pre "20 Any of the following:")) [Leaf (RPMT [MTT "20.a",MTT "subject to section 4(4)(b) of the Act, the donation and removal of any organ from the body of the deceased individual for the purpose of its transplantation into the body of another individual;"]),Leaf (RPMT [MTT "20.b",MTT "the donation and removal of any specified organ from the individual, being a living organ donor, for the purpose of its transplantation into the body of another individual;"]),Leaf (RPMT [MTT "20.c",MTT "the transplantation of any organ mentioned in sub-paragraph (a) or (b) into the body of the individual."])],Leaf (RPMT [MTI 21,MTT "Subject to section 4(4)(b) of the Act, the suicide or attempted suicide of the individual."]),Leaf (RPMT [MTI 22,MTT "Domestic abuse, child abuse or sexual abuse involving or alleged to involve the individual."]),Any (Just (Pre "23 Any of the following:")) [Leaf (RPMT [MTT "23.a",MTT "information that the individual is or had been adopted pursuant to an adoption order made under the Adoption of Children Act (Cap. 4), or is or had been the subject of an application for an adoption order;"]),Leaf (RPMT [MTT "23.b",MTT "the identity of the natural father or mother of the individual;"]),Leaf (RPMT [MTT "23.c",MTT "the identity of the adoptive father or mother of the individual;"]),Leaf (RPMT [MTT "23.d",MTT "the identity of any applicant for an adoption order;"]),Leaf (RPMT [MTT "23.e",MTT "the identity of any person whose consent is necessary under that Act for an adoption order to be made, whether or not the court has dispensed with the consent of that person in accordance with that Act;"]),Leaf (RPMT [MTT "23.f",MTT "any other information that the individual is or had been an adopted child or relating to the adoption of the individual."])]]],All Nothing [Not (Leaf (RPMT [MTT "the organisation has taken any action ",MTT "to render it unlikely that the notifiable data breach will result in significant harm to the individual"])),Not (Leaf (RPMT [MTT "the organisation already implemented any technological measure",MTT "to render it unlikely that the notifiable data breach will result in significant harm to the individual"]))]],Leaf (RPConstraint [MTT "the number of affected individuals"] RPgt [MTT "the prescribed threshold of affected individuals"])]],Not (Leaf (RPMT [MTT "you are a Public Agency"]))]), deontic = DMust, action = Leaf ((MTT "NOTIFY" :| [MTT "each of the Notifiable Individuals"],Nothing) :| [(MTT "in" :| [MTT "any manner that is reasonable in the circumstances"],Nothing),(MTT "with" :| [MTT "a message obeying a certain format"],Nothing)]), temporal = Just (TemporalConstraint TBefore (Just 3) "days"), hence = Nothing, lest = Just (RuleAlias [MTT "Notify and explain"]), rlabel = Just ("\167",2,"Notify Individuals"), lsource = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 9, srccol = 175, version = Nothing}), upon = Nothing, given = Nothing, having = Nothing, wwhere = [], defaults = [], symtab = []},Regulative {subj = Leaf ((MTT "You" :| [],Nothing) :| []), rkeyword = RParty, who = Nothing, cond = Nothing, deontic = DMust, action = Leaf ((MTT "notify" :| [MTT "each of the Notifiable Individuals"],Nothing) :| []), temporal = Nothing, hence = Nothing, lest = Nothing, rlabel = Just ("\167",2,"Notify and explain"), lsource = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 9, srccol = 191, version = Nothing}), upon = Nothing, given = Nothing, having = Nothing, wwhere = [], defaults = [], symtab = []},DefNameAlias {name = [MTT "You"], detail = [MTT "Organisation"], nlhint = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 9, srccol = 26, version = Nothing})},DefTypically {name = [MTT "is",MTT "a Public Agency"], defaults = [RPConstraint [MTT "is",MTT "a Public Agency"] RPis [MTT "not"]], srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 9, srccol = 27, version = Nothing})},DefNameAlias {name = [MTT "NDB"], detail = [MTT "it is a Notifiable Data Breach"], nlhint = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 9, srccol = 74, version = Nothing})},DefNameAlias {name = [MTT "the PDPC Exclusion List"], detail = [MTT "with",MTT "a list of individuals to exclude from notification"], nlhint = Nothing, srcref = Just (SrcRef {url = "test/pdpa-full.csv", short = "test/pdpa-full.csv", srcrow = 11, srccol = 160, version = Nothing})}]