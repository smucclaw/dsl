{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
module Parsing.PDPASpec where

import Text.Megaparsec
import LS.Lib
import AnyAll hiding (asJSON)
import LS.BasicTypes
import LS.Types
import LS.Rule
import Test.Hspec
import qualified Data.ByteString.Lazy as BS
import Data.List.NonEmpty (NonEmpty ((:|)))
import Test.Hspec.Megaparsec (shouldParse)


filetest :: (HasCallStack, ShowErrorComponent e, Show b, Eq b) => String -> String -> (String -> MyStream -> Either (ParseErrorBundle MyStream e) b) -> b -> SpecWith ()
filetest testfile desc parseFunc expected =
  it (testfile ++ ": " ++ desc ) $ do
  testcsv <- BS.readFile ("test/Parsing/pdpa/" <> testfile <> ".csv")
  parseFunc testfile `traverse` exampleStreams testcsv
    `shouldParse` [ expected ]

spec :: Spec
spec = do
    let runConfig = defaultRC { sourceURL = "test/Spec" }
        runConfigDebug = runConfig { debug = True }
    let  combine (a,b) = a ++ b
    let _parseWith1 f x y s = f <$> runMyParser combine runConfigDebug x y s
    let  parseR       x y s = runMyParser combine runConfig x y s
    let _parseR1      x y s = runMyParser combine runConfigDebug x y s
    let  parseOther   x y s = runMyParser id      runConfig x y s
    let _parseOther1  x y s = runMyParser id      runConfigDebug x y s

    describe "PDPA" $ do
      filetest "pdpadbno-1"   "must assess" (parseR pToplevel) expected_pdpadbno1
      filetest "pdpadbno-1-b" "must assess" (parseR pToplevel) expected_pdpadbno1

      filetest "pdpadbno-2" "data intermediaries"
        (parseR pToplevel)
        [ defaultReg
            { subj = mkLeaf ((MTT "Data Intermediary" :| [], Nothing) :| []),
              rkeyword = REvery,
              who = Just (mkLeaf (mkRpmt [ "is not",  "processing personal data on behalf of and for the purposes of a public agency"])),
              cond = Just (mkLeaf (mkRpmt ["the data breach occurs on or after the date of commencement of PDP(A)A 2020 \167\&13"])),
              deontic = DMust,
              action = mkLeaf ((MTT <$> "NOTIFY" :| ["the Organisation"], Nothing) :| [(MTT <$> "for which" :| ["you act as a Data Intermediary"], Nothing)]),
              temporal = Just (TemporalConstraint TVague (Just 0) "without undue delay"),
              rlabel = Just ("\167", 2, "Data Intermediary non PA"),
              upon = Just ((MTT <$> "becoming aware a data breach involving a client Organisation may have occurred" :| [], Nothing) :| [])
            },
          DefNameAlias {
          name = [MTT "You"], detail = [MTT "Data Intermediary"], nlhint = Nothing, srcref = mkTestSrcRef 2 3}
        ]

      filetest "pdpadbno-3" "data intermediaries"
        (parseR pToplevel)
        [ defaultReg
            { subj = mkLeaf ((MTT <$> "Data Intermediary" :| [], Nothing) :| []),
              rkeyword = REvery,
              who = Just (mkLeaf (mkRpmt ["processes personal data on behalf of and for the purposes of a public agency"])),
              deontic = DMust,
              action = mkLeaf ((MTT <$> "NOTIFY" :| ["the Public Agency"], Nothing) :| [(MTT <$> "for which" :| ["you act as a Data Intermediary"], Nothing)]),
              temporal = Just (TemporalConstraint TVague (Just 0) "without undue delay"),
              rlabel = Just ("\167", 2, "Data Intermediary for PA"),
              upon = Just ((MTT <$> "becoming aware a data breach involving a client public agency may have occurred" :| [], Nothing) :| [])
            },
          DefNameAlias
            { name = [MTT "You"],
              detail = [MTT "Data Intermediary"],
              nlhint = Nothing,
              srcref = mkTestSrcRef 2 3
            }
        ]

      filetest "pdpadbno-5" "notification to PDPC"
        (parseR pToplevel)
        [ defaultReg
            { subj = mkLeaf ((MTT "You" :| [], Nothing) :| []),
              rkeyword = RParty,
              cond = Just (All Nothing [mkLeaf (mkRpmt [ "it is",  "an NDB"]), Not (mkLeaf (mkRpmt ["you are a Public Agency"]))]),
              deontic = DMust,
              action = mkLeaf ((MTT <$> "NOTIFY" :| ["the PDPC"], Nothing) :| [ (MTT <$> "in" :| ["the form and manner specified at www.pdpc.gov.sg"], Nothing),
                                                                                (MTT <$> "with" :| ["a Notification Message"], Nothing),
                                                                                (MTT <$> "and" :| ["a list of individuals for whom notification waiver is sought"], Nothing)]),
              temporal = Just (TemporalConstraint TBefore (Just 3) "days"),
              hence =
                Just
                  ( defaultReg
                      { subj = mkLeaf ((MTT <$> "the PDPC" :| [], Nothing) :| []),
                        rkeyword = RParty,
                        deontic = DMay,
                        action = mkLeaf ((MTT <$> "NOTIFY" :| ["you"], Nothing) :| [(MTT <$> "with" :| ["a list of individuals to exclude from notification"], Nothing)]),
                        srcref = Nothing
                      }
                  ),
              rlabel = Just ("\167", 2, "Notify PDPC")
            },
          DefNameAlias
            { name = [MTT "the PDPC Exclusion List"],
              detail = MTT <$> ["with", "a list of individuals to exclude from notification"],
              nlhint = Nothing,
              srcref = mkTestSrcRef 2 1
            }
        ]

      filetest "pdpadbno-6" "exemption: unlikely"
        (parseR pToplevel)
        [ defaultHorn
          { name = MTT <$> 
            [ "it is"
            , "unlikely that the notifiable data breach will result in significant harm to the affected individual"
            ]
          , keyword = Decide
          , given = Just
            (
              ( MTT "an individual" :| []
              , Nothing
              ) :|
              [
                ( MTT <$> "who" :| [ "is affected by an NDB" ]
                , Nothing
                )
              ]
            )
          , upon = Nothing
          , clauses =
              [ HC
                { hHead = mkRpmt
                          [ "it is"
                          , "unlikely that the notifiable data breach will result in significant harm to the affected individual"
                          ]
                , hBody = Just
                          ( Any Nothing
                            [ Leaf
                              ( mkRpmt
                                [ "the organisation has taken any action"
                                , "to"
                                , "render it unlikely that the notifiable data breach will result in significant harm to the individual"
                                ]
                              )
                            , Leaf
                              ( mkRpmt
                                [ "the organisation already implemented any technological measure"
                                , "to"
                                , "render it unlikely that the notifiable data breach will result in significant harm to the individual"
                                ]
                              )
                            ]
                          )
                }
              ]
          , rlabel =Just ("\167", 2, "Unlikely")
          , lsource = Nothing
          , srcref = dummyRef
          }
        , DefNameAlias
          { name = [ MTT "Unlikely" ]
          , detail = MTT <$> 
            [ "it is"
            , "unlikely that the notifiable data breach will result in significant harm to the affected individual"
            ]
          , nlhint = Nothing
          , srcref = mkTestSrcRef 2 5
          }
        ]

      filetest "pdpadbno-7" "notification to users"
        (parseR pToplevel)
        [ defaultReg
            { subj = mkLeaf ((MTT <$> "You" :| [], Nothing) :| []),
              rkeyword = RParty,
              cond =
                Just
                  ( All
                      Nothing
                      [ mkLeaf (mkRpmt ["it is", "an NDB"]),
                        Not (mkLeaf (mkRpmt ["you are a Public Agency"]))
                      ]
                  ),
              deontic = DMust,
              action = mkLeaf ((MTT <$> "NOTIFY" :| ["each of the Notifiable Individuals"], Nothing) :| [(MTT <$> "in" :| ["any manner that is reasonable in the circumstances"], Nothing), (MTT <$> "with" :| ["a message obeying a certain format"], Nothing)]),
              temporal = Just (TemporalConstraint TBefore (Just 3) "days"),
              rlabel = Just ("\167", 2, "Notify Individuals"),
              srcref = mkTestSrcRef 1 1,
              wwhere =
                [ defaultHorn
                    { name = [MTT "the Notifiable Individuals"],
                      keyword = Means,
                      clauses = [HC {hHead = mkRpmt ["the Notifiable Individuals"], hBody = Nothing}],
                      srcref = mkTestSrcRef 2 9
                    }
                ]
            },
          defaultHorn
            { name = [MTT "the Notifiable Individuals"],
              keyword = Means,
              clauses =
                [ HC
                    { hHead =
                        RPBoolStructR
                          [MTT "the Notifiable Individuals"]
                          RPis
                          ( All
                              Nothing
                              [ mkRpmtLeaf ["the set of individuals affected by the NDB"],
                                Not (mkRpmtLeaf ["the individuals who are deemed", "Unlikely"]),
                                Not (mkRpmtLeaf ["the individuals on", "the PDPC Exclusion List"]),
                                Not (mkRpmtLeaf ["the individuals on", "the LEA Exclusion List"])
                              ]
                          ),
                      hBody = Nothing
                    }
                ],
              srcref = mkTestSrcRef 2 9
            }
        ]

expected_pdpadbno1 :: [Rule]
expected_pdpadbno1 =
            [ defaultReg
              { subj = Leaf
                (
                  ( pure (MTT "Organisation")
                  , Nothing
                  ) :| []
                )
              , rkeyword = REvery
              , who = Just (Not (Leaf (RPMT [MTT "is", MTT "a Public Agency"])))
              , cond = Just (
                  Any Nothing 
                  [ Leaf (RPConstraint [MTT "the data breach occurs"] (RPTC TOn) [MTT "1 Feb 2022"])
                  , Leaf (RPConstraint [MTT "the data breach occurs"] (RPTC TAfter) [MTT "1 Feb 2022"])])
              , deontic = DMust
              , action = Leaf
                (
                  ( MTT "assess" :| [ MTT "if it is a Notifiable Data Breach" ]
                  , Nothing
                  ) :|
                  [
                    ( MTT <$> "by" :|
                      [ "performing"
                      , "NDB Qualification"
                      ]
                    , Nothing
                    )
                  ]
                )
              , temporal = Just ( TemporalConstraint TBefore (Just 30) "days" )
              , hence = Just ( RuleAlias [MTT "Notification"] )
              , lest = Just
                ( defaultReg
                    { subj = Leaf
                        (
                            ( MTT "the PDPC" :| []
                            , Nothing
                            ) :| []
                        )
                    , rkeyword = RParty
                    , deontic = DMay
                    , action = Leaf
                        (
                            ( MTT <$> "demand" :| [ "an explanation for your inaction" ]
                            , Nothing
                            ) :| []
                        )
                    , temporal = Nothing
                    , srcref = Nothing
                    , hence = Just
                        ( defaultReg
                            { subj = Leaf
                                (
                                    ( MTT "You" :| []
                                    , Nothing
                                    ) :| []
                                )
                            , rkeyword = RParty
                            , deontic = DMust
                            , srcref = Nothing
                            , action = Leaf
                                (
                                    ( MTT "respond" :| []
                                    , Nothing
                                    ) :|
                                    [
                                        ( MTT <$> "to" :| [ "the PDPC" ]
                                        , Nothing
                                        )
                                    ,
                                        ( MTT <$> "about" :| [ "your inaction" ]
                                        , Nothing
                                        )
                                    ]
                                )
                            }
                        )
                    }
                )
            , upon = Just
                (
                    ( MTT "becoming aware a data breach may have occurred" :| []
                    , Nothing
                    ) :| []
                )
            , rlabel = Just ("\167",2,"Assess")
            }
            , DefNameAlias
            { name = [ MTT "You" ]
            , detail = [ MTT "Organisation" ]
            , nlhint = Nothing
            , srcref = mkTestSrcRef 2 3
            }
            ]
