{-# LANGUAGE OverloadedStrings #-}
module Parsing.MegaparsingSpec where

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
  testcsv <- BS.readFile ("test/Parsing/megaparsing/" <> testfile <> ".csv")
  parseFunc testfile `traverse` exampleStreams testcsv
    `shouldParse` [ expected ]

parserTests :: Spec
parserTests  = do
    let runConfig = defaultRC { sourceURL = "test/Spec" }
        runConfigDebug = runConfig { debug = True }
    let  combine (a,b) = a ++ b
    let _parseWith1 f x y s = f <$> runMyParser combine runConfigDebug x y s
    let  parseR       x y s = runMyParser combine runConfig x y s
    let _parseR1      x y s = runMyParser combine runConfigDebug x y s
    let _parseOther1  x y s = runMyParser id      runConfigDebug x y s

    describe "megaparsing" $ do

      it "should parse an unconditional" $ do
        parseR pRules "" (exampleStream ",EVERY,person,,\n,MUST,,,\n,->,sing,,\n")
          `shouldParse` [ srcrow2 $ defaultReg { subj = mkLeafPT "person"
                                               , deontic = DMust
                                               } ]

      it "should parse a rule label" $ do
        parseR pRules "" (exampleStream ",\xc2\xa7,Hello\n")
          `shouldParse` [srcrow2 $ RuleGroup {rlabel = Just ("\167",1,"Hello"), srcref = srcref defaultReg}]

      it "should parse a rule label followed by something" $ do
        parseR pRules "" (exampleStream "\xc2\xa7,Hello\n,something\nMEANS,something\n")
          `shouldParse`
          [ Hornlike
              { name = [MTT "something"],
                super = Nothing,
                keyword = Means,
                given = Nothing,
                upon = Nothing,
                clauses =
                  [ HC
                      { hHead = RPBoolStructR [MTT "something"] RPis (mkLeafR "something"),
                        hBody = Nothing
                      }
                  ],
                rlabel = Just ("\167", 1, "Hello"),
                lsource = Nothing,
                srcref = mkTestSrcRef 1 1,
                defaults = [],
                symtab = []
              }
          ]
      it "should parse a single OtherVal" $ do
        parseR pRules "" (exampleStream ",,,,\n,EVERY,person,,\n,WHO,walks,,\n,MUST,,,\n,->,sing,,\n")
          `shouldParse` [ srccol1 . srcrow2 $ defaultReg { who = Just (mkLeafR "walks") } ]

      it "should parse the null temporal EVENTUALLY" $ do
        parseR pRules "" (exampleStream ",,,,\n,EVERY,person,,\n,WHO,walks,,\n,MUST,EVENTUALLY,,\n,->,sing,,\n")
          `shouldParse` [ srccol1 . srcrow2 $ defaultReg { who = Just (mkLeafR "walks") } ]

      it "should parse dummySing" $ do
        parseR pRules "" (exampleStream ",,,,\n,EVERY,person,,\n,WHO,walks,// comment,continued comment should be ignored\n,AND,runs,,\n,AND,eats,,\n,OR,drinks,,\n,MUST,,,\n,->,sing,,\n")
          `shouldParse` [ srccol1 <$> srcrow2 $ defaultReg {
                            who = Just (All Nothing
                                         [ mkLeafR "walks"
                                         , mkLeafR "runs"
                                         , Any Nothing
                                           [ mkLeafR "eats"
                                           , mkLeafR "drinks"
                                           ]
                                         ])
                            } ]

      let imbibeRule = [ defaultReg {
                           who = Just (Any Nothing
                                       [ mkLeafR "walks"
                                       , mkLeafR "runs"
                                       , mkLeafR "eats"
                                       , All Nothing
                                         [ mkLeafR "drinks"
                                         , mkLeafR "swallows" ]
                                       ])
                           } ]

      it "should parse indentedDummySing" $ do
        parseR pRules "" (exampleStream ",,,,\n,EVERY,person,,\n,WHO,walks,// comment,continued comment should be ignored\n,OR,runs,,\n,OR,eats,,\n,OR,,drinks,\n,,AND,swallows,\n,MUST,,,\n,->,sing,,\n")
          `shouldParse` (srccol1 <$> srcrow2 <$> imbibeRule)

      filetest "indented-1" "parse indented-1.csv (inline boolean expression)"
        (parseR pRules) (srcrow2 <$> imbibeRule)

      filetest "indented-1-checkboxes" "should parse indented-1-checkboxes.csv (with checkboxes)"
        (parseR pRules) (srcrow2 <$> imbibeRule)

      let degustates = defaultHorn
            { name = [MTT "degustates"]
            , keyword = Means
            , given = Nothing
            , upon = Nothing
            , clauses = [ HC
                          { hHead = RPBoolStructR [MTT "degustates"]
                                    RPis (Any Nothing [mkLeafR "eats"
                                                      ,mkLeafR "drinks"])
                          , hBody = Nothing } ]
            }

      filetest "simple-constitutive-1" "should parse a simple constitutive rule"
        (parseR pRules) [srcrow2 degustates]

      filetest "simple-constitutive-1-checkboxes" "should parse a simple constitutive rule with checkboxes"
        (parseR pRules) [degustates { srcref = mkTestSrcRef 4 1 }]

      -- inline constitutive rules are temporarily disabled; we need to think about how to intermingle a "sameline" parser with a multiline object.
      -- we also need to think about getting the sameline parser to not consume all the godeepers at once, because an inline constitutive rule actually starts with a godeeper.

      filetest "indented-2" "inline constitutive rule"
        (parseR pRules) 
        [ Regulative
            { subj = mkLeaf ((MTT "person" :| [], Nothing) :| []),
              rkeyword = REvery,
              who =
                Just
                  ( All
                      Nothing
                      [ mkLeafR "walks",
                        mkLeafR "degustates"
                      ]
                  ),
              cond = Nothing,
              deontic = DMust,
              action = mkLeaf ((MTT "sing" :| [], Nothing) :| []),
              temporal = Nothing,
              hence = Nothing,
              lest = Nothing,
              rlabel = Nothing,
              lsource = Nothing,
              srcref = mkTestSrcRef 1 1,
              upon = Nothing,
              given = Nothing,
              having = Nothing,
              wwhere = [],
              defaults = [],
              symtab = []
            },
          Hornlike
            { name = [MTT "degustates"],
              super = Nothing,
              keyword = Means,
              given = Nothing,
              upon = Nothing,
              clauses =
                [ HC
                    { hHead =
                        RPBoolStructR
                          [MTT "degustates"]
                          RPis
                          ( Any
                              Nothing
                              [ mkLeafR "eats",
                                mkLeafR "imbibes"
                              ]
                          ),
                      hBody = Nothing
                    }
                ],
              rlabel = Nothing,
              lsource = Nothing,
              srcref = mkTestSrcRef 2 3,
              defaults = [],
              symtab = []
            }
        ]

      filetest "indented-3" "defined names in natural positions"
        (parseR pRules)
        [ Regulative
            { subj = mkLeaf ((MTT "person" :| [], Nothing) :| []),
              rkeyword = REvery,
              who =
                Just
                  ( All
                      Nothing
                      [ mkLeafR "walks",
                        mkLeafR "degustates"
                      ]
                  ),
              cond = Nothing,
              deontic = DMust,
              action = mkLeaf ((MTT "sing" :| [], Nothing) :| []),
              temporal = Nothing,
              hence = Nothing,
              lest = Nothing,
              rlabel = Nothing,
              lsource = Nothing,
              srcref = mkTestSrcRef 1 1,
              upon = Nothing,
              given = Nothing,
              having = Nothing,
              wwhere = [],
              defaults = [],
              symtab = []
            },
          Hornlike
            { name = [MTT "imbibes"],
              super = Nothing,
              keyword = Means,
              given = Nothing,
              upon = Nothing,
              clauses =
                [ HC
                    { hHead =
                        RPBoolStructR
                          [MTT "imbibes"]
                          RPis
                          ( All
                              Nothing
                              [ mkLeafR "drinks",
                                Any
                                  Nothing
                                  [ mkLeafR "swallows",
                                    mkLeafR "spits"
                                  ]
                              ]
                          ),
                      hBody = Nothing
                    }
                ],
              rlabel = Nothing,
              lsource = Nothing,
              srcref = mkTestSrcRef 3 5,
              defaults = [],
              symtab = []
            },
          Hornlike
            { name = [MTT "degustates"],
              super = Nothing,
              keyword = Means,
              given = Nothing,
              upon = Nothing,
              clauses =
                [ HC
                    { hHead =
                        RPBoolStructR
                          [MTT "degustates"]
                          RPis
                          ( Any
                              Nothing
                              [ mkLeafR "eats",
                                mkLeafR "imbibes"
                              ]
                          ),
                      hBody = Nothing
                    }
                ],
              rlabel = Nothing,
              lsource = Nothing,
              srcref = mkTestSrcRef 2 3,
              defaults = [],
              symtab = []
            }
        ]
        
      let mustsing1 = [ defaultReg {
                          rlabel = Just ("\167",1,"Matt Wadd's Rule")
                          , subj = Leaf
                            (
                              ( MTT "Person" :| []
                              , Nothing
                              ) :| []
                            )
                          , rkeyword = REvery
                          , who = Just
                            ( All Nothing
                              [ Leaf
                                ( mkRpmt ["walks"] )
                              , Any Nothing
                                [ Leaf
                                  ( mkRpmt ["eats"])
                                , Leaf
                                  ( mkRpmt ["drinks"] )
                                ]
                              ]
                            )
                          }
                      ]

      filetest "mustsing-1" "mustsing-1: should handle the most basic form of Matt Wadd's rule"
        (parseR pRules) mustsing1

      let if_king_wishes = [ defaultReg
                          { who = Just $ All Nothing
                                  [ mkLeafR "walks"
                                  , mkLeafR "eats"
                                  ]
                          , cond = Just $ mkLeafR "the King wishes"
                          }
                        ]

      let king_pays_singer = defaultReg
                          { subj = mkLeafPT "King"
                          , rkeyword = RParty
                          , deontic = DMay
                          , action = mkLeafPT "pay"
                          , temporal = Just (TemporalConstraint TAfter (Just 20) "min")
                          }


      let king_pays_singer_eventually =
            king_pays_singer { temporal = Nothing }

      let singer_must_pay = defaultReg
                              { rkeyword = RParty
                              , subj = mkLeafPT "Singer"
                              , action = mkLeafPT "pay"
                              , temporal = Just (TemporalConstraint TBefore (Just 1) "supper")
                              }


      let singer_chain = [ defaultReg
                         { subj = mkLeafPT "person"
                         , who = Just $ All Nothing
                                 [ mkLeafR "walks"
                                 , mkLeafR "eats"
                                 ]
                         , cond = Just $ mkLeafR "the King wishes"
                         , hence = Just king_pays_singer
                         , lest  = Just singer_must_pay
                         , srcref = Nothing
                         } ]

      filetest "if-king-wishes-1" "should parse kingly permutations 1"
        (parseR pRules) if_king_wishes

      filetest "if-king-wishes-2" "should parse kingly permutations 2"
        (parseR pRules) if_king_wishes

      filetest "if-king-wishes-3" "should parse kingly permutations 3"
        (parseR pRules) if_king_wishes

      filetest "chained-regulatives-part1" "should parse chained-regulatives part 1"
        (parseR pRules) [king_pays_singer]

      filetest "chained-regulatives-part2" "should parse chained-regulatives part 2"
        (parseR pRules) [singer_must_pay]

      filetest "chained-regulatives" "should parse chained-regulatives.csv"
        (parseR pRules) (srcrow1' <$> srcrow_ <$> singer_chain)

      filetest "chained-regulatives-part1-alternative-1" "should parse alternative deadline/action arrangement 1"
        (parseR pRules) [king_pays_singer]

      filetest "chained-regulatives-part1-alternative-2" "should parse alternative deadline/action arrangement 2"
        (parseR pRules) [king_pays_singer]

      filetest "chained-regulatives-part1-alternative-3" "should parse alternative deadline/action arrangement 3"
        (parseR pRules) [king_pays_singer]

      filetest "chained-regulatives-part1-alternative-4" "should parse alternative arrangement 4, no deadline at all"
        (parseR pRules) [king_pays_singer_eventually]

      let if_king_wishes_singer = if_king_wishes ++
            [ DefNameAlias [MTT "singer"] [MTT "person"] Nothing
              (mkTestSrcRef 2 2) ]

      let if_king_wishes_singer_nextline = if_king_wishes ++
            [ DefNameAlias [MTT "singer"] [MTT "person"] Nothing
              (mkTestSrcRef 2 3) ]

      filetest "nl-aliases" "should parse natural language aliases (\"NL Aliases\") aka inline defined names"
        (parseR pRules) if_king_wishes_singer

      filetest "nl-aliases-2" "should parse natural language aliases (\"NL Aliases\") on the next line"
        (parseR pRules) if_king_wishes_singer_nextline

      let singer_must_pay_params =
            singer_must_pay { action = mkLeaf ((MTT "pay" :| []                 , Nothing)
                                             :| [(MTT "to"     :| [MTT "the King"], Nothing)
                                                ,(MTT "amount" :| [MTT "$20"]     , Nothing)]) }

      filetest "action-params-singer" "should parse action params"
        (parseR pRules) [singer_must_pay_params]
