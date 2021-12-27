{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Test.Hspec
-- import Test.Hspec.Megaparsec hiding (shouldParse)
import Text.Megaparsec
import qualified Data.Text.Lazy as Text
import LS.Lib
import LS.Parser
import AnyAll hiding (asJSON)
import LS.Types
import LS.Error
import qualified Data.ByteString.Lazy as BS
import Data.List.NonEmpty (NonEmpty ((:|)), fromList)
import Options.Generic (getRecordPure, unwrapRecord)
import Debug.Trace (traceShowM)

-- | Create an expectation by saying what the result should be.
--
-- > parse letterChar "" "x" `shouldParse` 'x'
shouldParse ::
  ( HasCallStack,
    ShowErrorComponent e,
    Show a,
    Eq a
  ) =>
  -- | Result of parsing as returned by function like 'parse'
  Either (ParseErrorBundle MyStream e) a ->
  -- | Desired result
  a ->
  Expectation
r `shouldParse` v = case r of
  Left e ->
    expectationFailure $
      "expected: " ++ show v
        ++ "\nbut parsing failed with error:\n"
        ++ errorBundlePrettyCustom e
  Right x -> x `shouldBe` v

defaultReg, defaultCon, defaultHorn :: Rule
defaultReg = Regulative
  { subj = mkLeaf "person"
  , keyword = Every
  , who = Nothing
  , cond = Nothing
  , deontic = DMust
  , action = mkLeaf "sing"
  , temporal = Nothing
  , hence = Nothing
  , lest = Nothing
  , rlabel = Nothing
  , lsource = Nothing
  , srcref = Nothing
  , upon = Nothing
  , given = Nothing
  , having = Nothing
  }

defaultCon = Constitutive
  { name = []
  , keyword = Means
  , letbind = mkLeafR "Undefined"
  , cond = Nothing
  , rlabel = Nothing
  , lsource = Nothing
  , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing})
  , given = Nothing
  }

defaultHorn = Hornlike
  { name = []
  , keyword = Means
  , given = Nothing
  , upon  = Nothing
  , clauses = []
  , rlabel = Nothing
  , lsource = Nothing
  , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing})
  }


main :: IO ()
main = do
  cmdlineOpts <- unwrapRecord $ (maybe (error "failed to parse empty args") id $ getRecordPure  [])
  runConfig_ <- getConfig $ cmdlineOpts
  let runConfig = runConfig_ { sourceURL = "test/Spec" }
      runConfigDebug = runConfig { debug = True }
  let combine (a,b) = a ++ b
  let parseR = runMyParser combine runConfig
  let parseR1 x y s = runMyParser combine runConfigDebug x y s <* traceShowM (tokenVal <$> unMyStream s)
  let parseOther  = runMyParser id runConfig
  let parseOther1 = runMyParser id runConfigDebug

  hspec $ do
    describe "Nothing Test" $ do
      it "should be nothing" $ do
        (Nothing :: Maybe ()) `shouldBe` (Nothing :: Maybe ())

    describe "megaparsing" $ do


      it "should parse an unconditional" $ do
        parseR pRules "" (exampleStream ",EVERY,person,,\n,MUST,,,\n,->,sing,,\n")
          `shouldParse` [ defaultReg { subj = mkLeaf "person"
                                     , deontic = DMust
                                     } ]

      it "should parse a rule label" $ do
        parseR pRules "" (exampleStream ",\xc2\xa7,Hello\n")
          `shouldParse` [RuleGroup {rlabel = Just ("\167",1,"Hello")}]

      it "should parse a single OtherVal" $ do
        parseR pRules "" (exampleStream ",,,,\n,EVERY,person,,\n,WHO,walks,,\n,MUST,,,\n,->,sing,,\n")
          `shouldParse` [ defaultReg { who = Just (mkLeafR "walks") } ]

      it "should parse the null temporal EVENTUALLY" $ do
        parseR pRules "" (exampleStream ",,,,\n,EVERY,person,,\n,WHO,walks,,\n,MUST,EVENTUALLY,,\n,->,sing,,\n")
          `shouldParse` [ defaultReg { who = Just (mkLeafR "walks") } ]

      it "should parse dummySing" $ do
        parseR pRules "" (exampleStream ",,,,\n,EVERY,person,,\n,WHO,walks,// comment,continued comment should be ignored\n,AND,runs,,\n,AND,eats,,\n,OR,drinks,,\n,MUST,,,\n,->,sing,,\n")
          `shouldParse` [ defaultReg {
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
          `shouldParse` imbibeRule

      it "should parse indented-1.csv (inline boolean expression)" $ do
        mycsv <- BS.readFile "test/indented-1.csv"
        parseR pRules "" (exampleStream mycsv) `shouldParse` imbibeRule


      it "should parse indented-1-checkboxes.csv (with checkboxes)" $ do
        mycsv <- BS.readFile "test/indented-1-checkboxes.csv"
        parseR pRules "" (exampleStream mycsv) `shouldParse` imbibeRule

      let degustates = defaultCon
                       { name = ["degustates"]
                       , letbind = Any Nothing [ mkLeafR "eats", mkLeafR "drinks" ]
                       , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing})
                       }

      it "should parse a simple constitutive rule" $ do
        mycsv <- BS.readFile "test/simple-constitutive-1.csv"
        parseR1 pRules "" (exampleStream mycsv) `shouldParse` [degustates]
{-
      it "should parse a simple constitutive rule with checkboxes" $ do
        mycsv <- BS.readFile "test/simple-constitutive-1-checkboxes.csv"
        parseR1 pRules "" (exampleStream mycsv) `shouldParse` [degustates { srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 3, srccol = 2, version = Nothing}) }]

      let imbibeRule2 = [ defaultReg
                          { who = Just $ All Nothing
                                  [ mkLeafR "walks"
                                  , mkLeafR "degustates"
                                  ]
                          , srcref = Nothing
                          }
                        , defaultCon
                          { name = ["degustates"]
                          , letbind = Any Nothing [ mkLeafR "eats", mkLeafR "imbibes" ]
                          , cond = Nothing
                          , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 3, srccol = 3, version = Nothing})
                          }
                        ]

      let imbibeRule3 = imbibeRule2 ++ [
            defaultCon
              { name = ["imbibes"]
              , letbind = All Nothing
                          [ mkLeafR "drinks"
                          , Any Nothing [ mkLeafR "swallows"
                                        , mkLeafR "spits" ]
                          ]
              , cond = Nothing
              , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 4, srccol = 5, version = Nothing})
              } ]
      
      it "should parse indented-2.csv (inline constitutive rule)" $ do
        mycsv <- BS.readFile "test/indented-2.csv"
        parseR pRules "" (exampleStream mycsv) `shouldParse` imbibeRule2

      it "should parse indented-3.csv (defined names in natural positions)" $ do
        mycsv <- BS.readFile "test/indented-3.csv"
        parseR pRules "" (exampleStream mycsv) `shouldParse` imbibeRule3

      let mustsing1 = [ defaultReg {
                          rlabel = Just ("\167",1,"Matt Wadd's Rule")
                          , subj = Leaf
                            (
                              ( "Person" :| []
                              , Nothing
                              ) :| []
                            )
                          , keyword = Every
                          , who = Just
                            ( All Nothing
                              [ Leaf
                                ( RPParamText
                                  (
                                    ( "walks" :| []
                                    , Nothing
                                    ) :| []
                                  )
                                )
                              , Any Nothing
                                [ Leaf
                                  ( RPParamText
                                    (
                                      ( "eats" :| []
                                      , Nothing
                                      ) :| []
                                    )
                                  )
                                , Leaf
                                  ( RPParamText
                                    (
                                      ( "drinks" :| []
                                      , Nothing
                                      ) :| []
                                    )
                                  )
                                ]
                              ]
                            )
                          }
                      ]
      
      it "mustsing-1: should handle the most basic form of Matt Wadd's rule" $ do
        mycsv <- BS.readFile "test/mustsing-1.csv"
        parseR pRules "" (exampleStream mycsv) `shouldParse` mustsing1
        

      let if_king_wishes = [ defaultReg
                          { who = Just $ All Nothing
                                  [ mkLeafR "walks"
                                  , mkLeafR "eats"
                                  ]
                          , cond = Just $ mkLeafR "the King wishes"
                          }
                        ]

      let king_pays_singer = defaultReg
                          { subj = mkLeaf "King"
                          , keyword = Party
                          , deontic = DMay
                          , action = mkLeaf "pay"
                          , temporal = Just (TemporalConstraint TAfter 20 "min")
                          }
                        

      let king_pays_singer_eventually =
            king_pays_singer { temporal = Nothing }

      let singer_must_pay = defaultReg
                              { keyword = Party
                              , subj = mkLeaf "Singer"
                              , action = mkLeaf "pay"
                              , temporal = Just (TemporalConstraint TBefore 1 "supper")
                              }
                        

      let singer_chain = [ defaultReg
                         { subj = mkLeaf "person"
                         , who = Just $ All Nothing
                                 [ mkLeafR "walks"
                                 , mkLeafR "eats"
                                 ]
                         , cond = Just $ mkLeafR "the King wishes"
                         , hence = Just king_pays_singer
                         , lest  = Just singer_must_pay
                         } ]

      it "should parse kingly permutations 1" $ do
        mycsv <- BS.readFile "test/if-king-wishes-1.csv"
        parseR pRules "" (exampleStream mycsv) `shouldParse` if_king_wishes

      it "should parse kingly permutations 2" $ do
        mycsv <- BS.readFile "test/if-king-wishes-2.csv"
        parseR pRules "" (exampleStream mycsv) `shouldParse` if_king_wishes

      it "should parse kingly permutations 3" $ do
        mycsv <- BS.readFile "test/if-king-wishes-3.csv"
        parseR pRules "" (exampleStream mycsv) `shouldParse` if_king_wishes

      it "should parse chained-regulatives part 1" $ do
        mycsv <- BS.readFile "test/chained-regulatives-part1.csv"
        parseR pRules "" (exampleStream mycsv) `shouldParse` [king_pays_singer]

      it "should parse chained-regulatives part 2" $ do
        mycsv <- BS.readFile "test/chained-regulatives-part2.csv"
        parseR pRules "" (exampleStream mycsv) `shouldParse` [singer_must_pay]

      it "should parse chained-regulatives.csv" $ do
        mycsv <- BS.readFile "test/chained-regulatives.csv"
        parseR pRules "" (exampleStream mycsv) `shouldParse` singer_chain

      it "should parse alternative deadline/action arrangement 1" $ do
        mycsv <- BS.readFile "test/chained-regulatives-part1-alternative-1.csv"
        parseR pRules "" (exampleStream mycsv) `shouldParse` [king_pays_singer]

      it "should parse alternative deadline/action arrangement 2" $ do
        mycsv <- BS.readFile "test/chained-regulatives-part1-alternative-2.csv"
        parseR pRules "" (exampleStream mycsv) `shouldParse` [king_pays_singer]

      it "should parse alternative deadline/action arrangement 3" $ do
        mycsv <- BS.readFile "test/chained-regulatives-part1-alternative-3.csv"
        parseR pRules "" (exampleStream mycsv) `shouldParse` [king_pays_singer]

      it "should parse alternative arrangement 4, no deadline at all" $ do
        mycsv <- BS.readFile "test/chained-regulatives-part1-alternative-4.csv"
        parseR pRules "" (exampleStream mycsv) `shouldParse` [king_pays_singer_eventually]

      let if_king_wishes_singer = if_king_wishes ++
            [ DefNameAlias ["singer"] ["person"] Nothing
              (Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 3, srccol = 1, version = Nothing})) ]

      let if_king_wishes_singer_nextline = if_king_wishes ++
            [ DefNameAlias ["singer"] ["person"] Nothing
              (Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 2, version = Nothing})) ]

      let if_king_wishes_singer_2 = if_king_wishes ++
            [ DefNameAlias ["singer"] ["person"] Nothing
              (Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 3, srccol = 5, version = Nothing})) ]

      it "should parse natural language aliases (\"NL Aliases\") aka inline defined names" $ do
        mycsv <- BS.readFile "test/nl-aliases.csv"
        parseR pRules "" (exampleStream mycsv) `shouldParse` if_king_wishes_singer

      it "should parse natural language aliases (\"NL Aliases\") on the next line" $ do
        mycsv <- BS.readFile "test/nl-aliases-2.csv"
        parseR pRules "" (exampleStream mycsv) `shouldParse` if_king_wishes_singer_nextline

      let singer_must_pay_params =
            singer_must_pay { action = Leaf (("pay" :| []                 , Nothing)
                                             :| [("to"     :| ["the King"], Nothing)
                                                ,("amount" :| ["$20"]     , Nothing)]) }

      it "should parse action params" $ do
        mycsv <- BS.readFile "test/action-params-singer.csv"
        parseR pRules "" (exampleStream mycsv) `shouldParse` [singer_must_pay_params]

      it "should parse despite interrupting newlines" $ do
        mycsv <- BS.readFile "test/blank-lines.csv"
        parseR pRules "" (exampleStream mycsv) `shouldParse` if_king_wishes_singer_2
      -- XXX: this is awful and needs to be fixed.  wtf, head.tail?

    describe "megaparsing MEANS" $ do

      let bobUncle1 = defaultHorn
            { name = ["Bob's your uncle"]
            , keyword = Means
            , clauses =
              [ HC2 { hHead = RPParamText (("Bob's your uncle" :| [],Nothing) :| [])
                    , hBody = Just $ Not ( Any Nothing [mkLeafR "Bob is estranged"
                                                       ,mkLeafR "Bob is dead"])}]
            , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 1, version = Nothing}) }

          bobUncle2 = bobUncle1
            { clauses = 
              [ HC2 { hHead = RPParamText (("Bob's your uncle" :| [],Nothing) :| [])
                    , hBody = Just $ Any Nothing [Not $ mkLeafR "Bob is estranged"
                                                 ,      mkLeafR "Bob is dead" ] } ] }
      
      it "should start a bool struct with an indented NOT" $ do
        let testfile = "test/bob-head-1.csv"
        testcsv <- BS.readFile testfile
        parseR pRules testfile (exampleStream testcsv)
          `shouldParse` [bobUncle1]

      it "should handle less indentation" $ do
        let testfile = "test/bob-head-2.csv"
        testcsv <- BS.readFile testfile
        parseR pRules testfile (exampleStream testcsv)
          `shouldParse` [bobUncle1]

      it "should handle outdentation" $ do
        let testfile = "test/bob-head-3.csv"
        testcsv <- BS.readFile testfile
        parseR pRules testfile (exampleStream testcsv)
          `shouldParse` [bobUncle2]

    describe "megaparsing UNLESS semantics" $ do

      let dayOfSilence = [ defaultReg { cond = Just ( Not ( mkLeafR "day of silence" ) ) } ] 

      let observanceMandatory = [ defaultReg { cond = Just
                                               ( Not
                                                 ( All Nothing
                                                   [ mkLeafR "day of silence"
                                                   , mkLeafR "observance is mandatory"
                                                   ]
                                                 )
                                               ) } ]

      let dayOfSong = [ defaultReg { cond = Just ( All Nothing [ Not ( mkLeafR "day of silence" )
                                                               , mkLeafR "day of song" ] ) } ]

      let silenceKing = [ defaultReg { cond = Just ( All Nothing [ mkLeafR "the king wishes"
                                                                 , Not ( mkLeafR "day of silence" )
                                                                 ] ) } ]
            
      it "should read EVERY MUST UNLESS" $ do
        let testfile = "test/unless-regulative-1.csv"
        testcsv <- BS.readFile testfile
        parseR pRules testfile (exampleStream testcsv)
          `shouldParse` dayOfSilence
                      
      it "should read EVERY MUST UNLESS IF" $ do
        let testfile = "test/unless-regulative-2.csv"
        testcsv <- BS.readFile testfile
        parseR pRules testfile (exampleStream testcsv)
          `shouldParse` silenceKing
                      
      it "should read EVERY MUST IF UNLESS" $ do
        let testfile = "test/unless-regulative-3.csv"
        testcsv <- BS.readFile testfile
        parseR pRules testfile (exampleStream testcsv)
          `shouldParse` silenceKing
                      
      it "should read EVERY UNLESS MUST IF" $ do
        let testfile = "test/unless-regulative-4.csv"
        testcsv <- BS.readFile testfile
        parseR pRules testfile (exampleStream testcsv)
          `shouldParse` silenceKing
                      
      it "should read EVERY IF MUST UNLESS" $ do
        let testfile = "test/unless-regulative-5.csv"
        testcsv <- BS.readFile testfile
        parseR pRules testfile (exampleStream testcsv)
          `shouldParse` silenceKing
                      
      let silenceMourning = [
            defaultReg { cond = Just ( All Nothing [
                                         mkLeafR "the king wishes"
                                         , Not
                                           ( Any Nothing
                                             [ mkLeafR "day of silence"
                                             , mkLeafR "day of mourning"
                                             ]
                                           )
                                         ] ) } ]

      it "should read EVERY MUST IF UNLESS OR" $ do
        let testfile = "test/unless-regulative-6.csv"
        testcsv <- BS.readFile testfile
        parseR pRules testfile (exampleStream testcsv)
          `shouldParse` silenceMourning

      let mourningForbids = [
            defaultReg { cond = Just ( All Nothing [
                                         mkLeafR "the king wishes"
                                         , Not
                                           ( All Nothing
                                             [ mkLeafR "day of mourning"
                                             , mkLeafR "mourning forbids singing"
                                             ]
                                           ) ] ) } ]
                                         
      it "should read EVERY MUST IF UNLESS AND" $ do
        let testfile = "test/unless-regulative-7.csv"
        testcsv <- BS.readFile testfile
        parseR pRules testfile (exampleStream testcsv)
          `shouldParse` mourningForbids
                      
      it "should read IF NOT when joined" $ do
        let testfile = "test/ifnot-1-joined.csv"
        testcsv <- BS.readFile testfile
        parseR pRules testfile (exampleStream testcsv)
          `shouldParse` dayOfSilence
                      
      it "should read IF-NOT when separate" $ do
        let testfile = "test/ifnot-2-separate.csv"
        testcsv <- BS.readFile testfile
        parseR pRules testfile (exampleStream testcsv)
          `shouldParse` dayOfSilence

      it "should handle NOT ... AND indented" $ do
        let testfile = "test/ifnot-4-indentation-explicit.csv"
        testcsv <- BS.readFile testfile
        parseR pRules testfile (exampleStream testcsv)
          `shouldParse` observanceMandatory
                      
      it "should handle NOT AND indented the other way" $ do
        let testfile = "test/ifnot-5-indentation-explicit.csv"
        testcsv <- BS.readFile testfile
        parseR pRules testfile (exampleStream testcsv)
          `shouldParse` dayOfSong
                      
      it "should work for constitutive rules" $ do
        let testfile = "test/bob-tail-1.csv"
        testcsv <- BS.readFile testfile
        parseR pRules testfile (exampleStream testcsv)
          `shouldParse` [ defaultHorn
                          { name = ["Bob's your uncle"]
                          , keyword = Means
                          , clauses = [
                              HC2 { hHead = RPParamText (("Bob's your uncle" :| [],Nothing) :| [])
                                  , hBody = Just (All Nothing [Any Nothing [Leaf (RPParamText (("Bob is your mother's brother" :| [],Nothing) :| []))
                                                                           ,Leaf (RPParamText (("Bob is your father's brother" :| [],Nothing) :| []))]
                                                              ,Not (Leaf (RPParamText (("Bob is estranged" :| [],Nothing) :| [])))])}]
                          , rlabel = Nothing
                          , lsource = Nothing
                          , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 1, version = Nothing}
                                          )
                          }
                        ]

      it "should handle pilcrows" $ do
        let testfile = "test/pilcrows-1.csv"
        testcsv <- BS.readFile testfile
        parseR pRules testfile `traverse` (exampleStreams testcsv)
          `shouldParse` [ dayOfSilence 
                        , dayOfSong
                        ]
        -- forM_ (exampleStreams testcsv) $ \stream ->
        --   parseR pRules testfile stream
        --     `shouldParse` [ defaultCon 
        --                   ]

  -- upgrade single OR group to bypass the top level AND group

  -- defNameAlias should absorb the WHO limb

    describe "megaparsing scenarios" $ do
      it "should handle labeled given/expect" $ do
        let testfile = "test/scenario-1.csv"
        testcsv <- BS.readFile testfile
        parseR pRules testfile `traverse` (exampleStreams testcsv)
          `shouldParse`
          [ [ Scenario
            { scgiven =
                [ RPConstraint [ "amount saved" ] RPis [ "22000" ]
                , RPConstraint
                  [ "earnings"
                  , "amount"
                  ] RPis [ "25000" ]
                , RPConstraint
                  [ "earnings"
                  , "steadiness"
                  ] RPis [ "steady" ]
                ]
            , expect =
              [ HC
                { relPred = RPConstraint [ "investment" ] RPis [ "savings" ]
                , relWhen = Just
                            ( HBRP
                              ( Leaf
                                ( RPConstraint [ "dependents" ] RPis [ "5" ] )
                              )
                            )
                }
              , HC
                { relPred = RPConstraint [ "investment" ] RPis [ "combination" ]
                , relWhen = Just
                            ( HBRP
                              ( Leaf
                                ( RPConstraint [ "dependents" ] RPis [ "3" ] )
                              )
                            )
                }
              , HC
                { relPred = RPConstraint [ "investment" ] RPis [ "stocks" ]
                , relWhen = Just
                            ( HBRP
                              ( Leaf
                                ( RPConstraint [ "dependents" ] RPis [ "0" ] )
                              )
                            )
                }
              ]
            , rlabel = Just
                       ( "§"
                       , 1
                       , "Scenario 1"
                       )
            , lsource = Nothing
            , srcref = Just
                       ( SrcRef
                         { url = "test/Spec"
                         , short = "test/Spec"
                         , srcrow = 1
                         , srccol = 2
                         , version = Nothing
                         }
                       )
            }
          ] ]
          
    -- describe "megaparsing DECIDE layouts" $ do
    --   it "should handle multiline" $ do
    --     let testfile = "test/financialadvisor-decide-1.csv"
    --     testcsv <- BS.readFile testfile
    --     parseR pRules testfile `traverse` (exampleStreams testcsv)
    --       `shouldParse`
    --       [ [ Scenario

    describe "revised parser" $ do
      let simpleHorn = [ Hornlike
              { name = ["X"]
              , keyword = Decide
              , given = Nothing
              , upon = Nothing
              , rlabel = Nothing
              , lsource = Nothing
              , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing})
              , clauses =
                [ HC2
                  { hHead = RPConstraint ["X"] RPis ["Y"]
                  , hBody = Just $ All Nothing [ Leaf (RPConstraint ["Z"] RPis ["Q"])
                                               , Leaf (RPConstraint ["P"] RPgt ["NP"]) ]
                  } ]
              }
            ]
      it "should parse horn clauses 1" $ do
        let testfile = "test/horn-1.csv"
        testcsv <- BS.readFile testfile
        parseR pToplevel testfile `traverse` (exampleStreams testcsv)
          `shouldParse` [ simpleHorn ]
              
      it "should parse horn clauses 2" $ do
        let testfile = "test/horn-2.csv"
        testcsv <- BS.readFile testfile
        parseR pToplevel testfile `traverse` (exampleStreams testcsv)
          `shouldParse` [ simpleHorn ]
             
      it "should parse horn clauses 3" $ do
        let testfile = "test/horn-3.csv"
        testcsv <- BS.readFile testfile
        parseR pToplevel testfile `traverse` (exampleStreams testcsv)
          `shouldParse` [ simpleHorn ]
              
-}

    describe "our new parser" $ do
      let myand = LS.Types.And
          myor  = LS.Types.Or

      it "should inject Deeper tokens to match indentation" $ do
        let testfile = "test/indent-2-a.csv"
        testcsv <- BS.readFile testfile
        let mystreams = exampleStreams testcsv
        fmap tokenVal . unMyStream <$> mystreams `shouldBe` [
          [GoDeeper,Other "a"
          ,UnDeeper,myand,GoDeeper,Other "b"
          ,UnDeeper,myor,GoDeeper,Other "c"
          ,UnDeeper,myor,GoDeeper,MPNot,GoDeeper,Other "d"
          ,UnDeeper,UnDeeper]]

      let abcd = (MyAll [MyLeaf (text2pt "a")
                        ,MyAny[MyLeaf (text2pt "b")
                              ,MyLeaf (text2pt "c")
                              ,MyNot (MyLeaf (text2pt "d"))]],[])
          
      it "should handle indent-2-a" $ do
        let testfile = "test/indent-2-a.csv"
        testcsv <- BS.readFile testfile
        parseOther exprP testfile `traverse` exampleStreams testcsv
          `shouldParse` [abcd]
        
      it "should handle indent-2-b" $ do
        let testfile = "test/indent-2-b.csv"
        testcsv <- BS.readFile testfile
        parseOther exprP testfile `traverse` exampleStreams testcsv
          `shouldParse` [abcd]
      let ablcd = (MyAny [MyLeaf (text2pt "top1")
                        , MyLeaf (text2pt "top2")
                        , MyLabel "this is a label" $ MyAny [ MyLeaf (text2pt "mid3")
                                                            , MyLeaf (text2pt "mid4") ]
                        ],[])
      it "should handle indent-2-c which has a label" $ do
        let testfile = "test/indent-2-c.csv"
        testcsv <- BS.readFile testfile
        parseOther exprP testfile `traverse` exampleStreams testcsv
          `shouldParse` [ablcd]

      it "should handle indent-2-d which goes out, in, out" $ do
        let testfile = "test/indent-2-d.csv"
        testcsv <- BS.readFile testfile
        parseOther exprP testfile `traverse` exampleStreams testcsv
          `shouldParse` [
          (MyAny [MyLeaf (text2pt "term1")
                 , MyAll [ MyLeaf (text2pt "term2")
                         , MyLeaf (text2pt "term3")
                         ]
                 , MyLeaf (text2pt "term4")
                 , MyLeaf (text2pt "term5")
                 ],[]) ]
        
    describe "parser elements and fragments ... should parse" $ do
      let actionFragment1 :: BoolStructP
          actionFragment1 = Leaf (text2pt "win")

      it "(action-1) a one-word BoolStructP" $ do
        let testfile = "test/action-1.csv"
        testcsv <- BS.readFile testfile
        parseOther pDoAction testfile `traverse` exampleStreams testcsv
          `shouldParse` [(actionFragment1,[])]

      it "(action-2) a two-word BoolStructP" $ do
        let testfile = "test/action-2.csv"
        testcsv <- BS.readFile testfile
        parseOther pDoAction testfile `traverse` exampleStreams testcsv
          `shouldParse` [(Leaf $ ("win" :| ["gloriously"]
                                 , Nothing):|[]
                         ,[])]
      
{-
    describe "WHO / WHICH / WHOSE parsing of BoolStructR" $ do

      let whoStructR_1 = defaultReg
                         { who = Just ( Leaf ( RPParamText ( ( "eats" :| [] , Nothing ) :| [] ) ) ) }

          whoStructR_2 = defaultReg
                         { who = Just ( Leaf ( RPParamText ( ( "eats" :| ["rudely"] , Nothing ) :| [] ) ) ) }
          
          whoStructR_3 = defaultReg
                         { who = Just ( Leaf ( RPParamText ( ( "eats" :| ["without", "manners"] , Nothing ) :| [] ) ) ) }
          
          whoStructR_4 = defaultReg
                         { who = Just ( Leaf ( RPParamText ( ( "eats" :| ["without", "manners"] , Nothing )
                                                             :|          [("sans" :| ["decorum"], Nothing)]) )) }
          
          mkWhoStruct x xs = defaultReg
                         { who = Just ( Leaf ( RPParamText ( (,Nothing) <$> fromList x :| [fromList xs] ))) }

          whoStructR_5 = defaultReg
                         { who = Just ( Leaf ( RPConstraint ["eyes"] RPis ["eyes"] )) }
          
      it "(who-1) should handle a simple RPParamText" $ do
        let testfile = "test/who-1.csv"
        testcsv <- BS.readFile testfile
        parseR pToplevel testfile `traverse` exampleStreams testcsv
          `shouldParse` [ [ whoStructR_1 ] ]
          
      it "(who-2) should handle a simple RPParamText" $ do
        let testfile = "test/who-2.csv"
        testcsv <- BS.readFile testfile
        parseR pToplevel testfile `traverse` exampleStreams testcsv
          `shouldParse` [ [ whoStructR_2 ] ]
          
      it "(who-3) should handle a simple RPParamText" $ do
        let testfile = "test/who-3.csv"
        testcsv <- BS.readFile testfile
        parseR pToplevel testfile `traverse` exampleStreams testcsv
          `shouldParse` [ [ whoStructR_3 ] ]
          
      it "(who-4-a) should handle a multiline RPParamText without indentation" $ do
        let testfile = "test/who-4-a.csv"
        testcsv <- BS.readFile testfile
        parseR pToplevel testfile `traverse` exampleStreams testcsv
          `shouldParse` [ [ mkWhoStruct (Text.words "eats without manners") (Text.words "sans decorum") ] ]
          
      it "(who-4-b) flat style, variant" $ do
        let testfile = "test/who-4-b.csv"
        testcsv <- BS.readFile testfile
        parseR pToplevel testfile `traverse` exampleStreams testcsv
          `shouldParse` [ [ whoStructR_4 ] ]
          
      it "(who-5) should be a constraint" $ do
        let testfile = "test/who-5.csv"
        testcsv <- BS.readFile testfile
        parseR pToplevel testfile `traverse` exampleStreams testcsv
          `shouldParse` [ [ whoStructR_5 ] ]
-}
      
