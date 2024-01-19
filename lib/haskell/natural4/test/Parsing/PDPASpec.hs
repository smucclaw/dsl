{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Parsing.PDPASpec (spec, expected_pdpadbno1) where

import Text.Megaparsec
import LS.Lib
import AnyAll hiding (asJSON)
import LS.BasicTypes
import LS.Types
import LS.Rule
import Test.Hspec
import Data.ByteString.Lazy qualified as BS
import Data.List.NonEmpty (NonEmpty ((:|)), fromList)
import Test.Hspec.Megaparsec (shouldParse)
import Data.Text qualified as T
import System.FilePath ((</>), (-<.>))

filetest :: (HasCallStack, ShowErrorComponent e, Show b, Eq b) => String -> String -> (String -> MyStream -> Either (ParseErrorBundle MyStream e) b) -> b -> SpecWith ()
filetest testfile desc parseFunc expected =
  it (testfile <> ": " <> desc ) do
  testcsv <- BS.readFile $ "test" </> "Parsing" </> "pdpa" </> testfile -<.> "csv"
  parseFunc testfile `traverse` exampleStreams testcsv
    `shouldParse` [ expected ]

mkMTExprMulti :: [T.Text] -> TypedMulti
mkMTExprMulti xs = (MTT <$> fromList xs, Nothing)

mkParamText :: [[T.Text]] -> ParamText
mkParamText = fromList . fmap mkMTExprMulti

spec :: Spec
spec = do
    let runConfig = defaultRC { sourceURL = T.pack $ "test" </> "Spec" }
        runConfigDebug = runConfig { debug = True }
    let  combine (a,b) = a ++ b
    let _parseWith1 f x y s = f <$> runMyParser combine runConfigDebug x y s
    let  parseR       x y s = runMyParser combine runConfig x y s
    let _parseR1      x y s = runMyParser combine runConfigDebug x y s
    let  parseOther   x y s = runMyParser id      runConfig x y s
    let _parseOther1  x y s = runMyParser id      runConfigDebug x y s

    describe "PDPA" do
      filetest "pdpadbno-1"   "must assess" (parseR pToplevel) expected_pdpadbno1

      filetest "pdpadbno-2" "data intermediaries"
        (parseR pToplevel)
        [ defaultReg
            { subj = mkLeaf $ mkParamText [["Data Intermediary"]],
              rkeyword = REvery,
              who = Just (mkRpmtLeaf [ "is not",  "processing personal data on behalf of and for the purposes of a public agency"]),
              cond = Just (mkRpmtLeaf ["the data breach occurs on or after the date of commencement of PDP(A)A 2020 \167\&13"]),
              deontic = DMust,
              action = mkLeaf $ mkParamText [["NOTIFY", "the Organisation"], ["for which", "you act as a Data Intermediary"]],
              temporal = Just (TemporalConstraint TVague (Just 0) "without undue delay"),
              rlabel = Just ("\167", 2, "Data Intermediary non PA"),
              upon = Just $ mkParamText [["becoming aware a data breach involving a client Organisation may have occurred"]]
            },
          DefNameAlias {
          name = [MTT "You"], detail = [MTT "Data Intermediary"], nlhint = Nothing, srcref = mkTestSrcRef 2 3}
        ]

      filetest "pdpadbno-3" "data intermediaries"
        (parseR pToplevel)
        [ defaultReg
            { subj = mkLeaf $ mkParamText [["Data Intermediary"]],
              rkeyword = REvery,
              who = Just (mkRpmtLeaf ["processes personal data on behalf of and for the purposes of a public agency"]),
              deontic = DMust,
              action = mkLeaf $ mkParamText [["NOTIFY", "the Public Agency"], ["for which", "you act as a Data Intermediary"]],
              temporal = Just (TemporalConstraint TVague (Just 0) "without undue delay"),
              rlabel = Just ("\167", 2, "Data Intermediary for PA"),
              upon = Just $ mkParamText [["becoming aware a data breach involving a client public agency may have occurred"]]
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
            { subj = mkLeaf $ mkParamText [["You"]],
              rkeyword = RParty,
              cond = Just (All Nothing [mkRpmtLeaf [ "it is",  "an NDB"], Not (mkRpmtLeaf ["you are a Public Agency"])]),
              deontic = DMust,
              action = mkLeaf $ mkParamText
                    [ ["NOTIFY", "the PDPC"],
                      ["in", "the form and manner specified at www.pdpc.gov.sg"],
                      ["with", "a Notification Message"],
                      ["and", "a list of individuals for whom notification waiver is sought"]
                    ],
              temporal = Just (TemporalConstraint TBefore (Just 3) "days"),
              hence =
                Just
                  ( defaultReg
                      { subj = mkLeaf $ mkParamText [["the PDPC"]],
                        rkeyword = RParty,
                        deontic = DMay,
                        action = mkLeaf $ mkParamText [["NOTIFY", "you"], ["with", "a list of individuals to exclude from notification"]],
                        srcref = mkTestSrcRef 4 11
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
          , given = Just $ mkParamText [["an individual"] ,["who",  "is affected by an NDB" ]]
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
            { subj = mkLeaf $ mkParamText [["You"]],
              rkeyword = RParty,
              cond =
                Just
                  ( All
                      Nothing
                      [ mkRpmtLeaf ["it is", "an NDB"],
                        Not (mkRpmtLeaf ["you are a Public Agency"])
                      ]
                  ),
              deontic = DMust,
              action = mkLeaf $ mkParamText [ ["NOTIFY", "each of the Notifiable Individuals"],
                                              ["in", "any manner that is reasonable in the circumstances"],
                                              ["with", "a message obeying a certain format"]],
              temporal = Just (TemporalConstraint TBefore (Just 3) "days"),
              rlabel = Just ("\167", 2, "Notify Individuals"),
              srcref = mkTestSrcRef 1 1,
              wwhere =
                [ defaultHorn
                    { name = [MTT "the Notifiable Individuals"],
                      keyword = Means,
                      clauses = [HC {hHead = mkRpmt ["the Notifiable Individuals"], hBody = Nothing}],
                      srcref = mkTestSrcRef 1 1
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
              { subj = Leaf $ mkParamText [["Organisation"]]
              , rkeyword = REvery
              , who = Just (Not (Leaf (RPMT [MTT "is", MTT "a Public Agency"])))
              , cond = Just (
                  Any Nothing
                  [ Leaf (RPConstraint [MTT "the data breach occurs"] (RPTC TOn) [MTT "1 Feb 2022"])
                  , Leaf (RPConstraint [MTT "the data breach occurs"] (RPTC TAfter) [MTT "1 Feb 2022"])])
              , deontic = DMust
              , action = Leaf $ mkParamText [["assess", "if it is a Notifiable Data Breach" ], ["by", "performing", "NDB Qualification"]]
              , temporal = Just ( TemporalConstraint TBefore (Just 30) "days" )
              , hence = Just ( RuleAlias [MTT "Notification"] )
              , lest = Just
                ( srctest 2 11 $ defaultReg
                    { subj = Leaf
                        (
                            ( MTT "the PDPC" :| []
                            , Nothing
                            ) :| []
                        )
                    , rkeyword = RParty
                    , deontic = DMay
                    , action = Leaf $ mkParamText [["demand", "an explanation for your inaction" ]]
                    , temporal = Nothing
                    , srcref = Nothing
                    , hence = Just
                        ( srctest 3 13 $ defaultReg
                            { subj = Leaf $ mkParamText [["You"]]
                            , rkeyword = RParty
                            , deontic = DMust
                            , action = Leaf $ mkParamText
                                                [ ["respond"],
                                                  ["to", "the PDPC"],
                                                  ["about", "your inaction"]
                                                ]
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
