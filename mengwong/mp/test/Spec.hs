{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
-- import Test.Hspec.Megaparsec hiding (shouldParse)
import Text.Megaparsec
import LS.Lib
import AnyAll hiding (asJSON)
import LS.Types
import LS.Error
import qualified Data.ByteString.Lazy as BS
import Data.List.NonEmpty (NonEmpty ((:|)))
import Options.Generic (getRecordPure, unwrapRecord)

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

defaultReg, defaultCon :: Rule
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
  , upon = []
  , given = Nothing
  , having = Nothing
  , orig = []
  }

defaultCon = Constitutive
  { name = ""
  , keyword = Means
  , letbind = Leaf $ text2pt "Undefined"
  , cond = Nothing
  , rlabel = Nothing
  , lsource = Nothing
  , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing})
  , given = Nothing
  , orig = []
  }


main :: IO ()
main = do
  cmdlineOpts <- unwrapRecord $ (maybe (error "failed to parse empty args") id $ getRecordPure  [])
  runConfig_ <- getConfig $ cmdlineOpts
  let runConfig = runConfig_ { sourceURL = "test/Spec" }
  let combine (a,b) = a ++ b
  let parseR = runMyParser combine runConfig

  hspec $ do
    describe "Nothing Test" $ do
      it "should be nothing" $ do
        (Nothing :: Maybe ()) `shouldBe` (Nothing :: Maybe ())
    describe "megaparsing" $ do

      it "should parse an unconditional" $ do
        parseR pRules "" (exampleStream ",,,,\n,EVERY,person,,\n,MUST,,,\n,->,sing,,\n")
          `shouldParse` [ defaultReg { subj = mkLeaf "person"
                                     , deontic = DMust
                                     } ]

      it "should parse a single OtherVal" $ do
        parseR pRules "" (exampleStream ",,,,\n,EVERY,person,,\n,WHO,walks,,\n,MUST,,,\n,->,sing,,\n")
          `shouldParse` [ defaultReg { who = Just (mkLeaf "walks") } ]

      it "should parse the null temporal EVENTUALLY" $ do
        parseR pRules "" (exampleStream ",,,,\n,EVERY,person,,\n,WHO,walks,,\n,MUST,EVENTUALLY,,\n,->,sing,,\n")
          `shouldParse` [ defaultReg { who = Just (mkLeaf "walks") } ]

      it "should parse dummySing" $ do
        parseR pRules "" (exampleStream ",,,,\n,EVERY,person,,\n,WHO,walks,// comment,continued comment should be ignored\n,AND,runs,,\n,AND,eats,,\n,OR,drinks,,\n,MUST,,,\n,->,sing,,\n")
          `shouldParse` [ defaultReg {
                            who = Just (All allof
                                         [ mkLeaf "walks"
                                         , mkLeaf "runs"
                                         , Any anyof
                                           [ mkLeaf "eats"
                                           , mkLeaf "drinks"
                                           ]
                                         ])
                            } ]

      let imbibeRule = [ defaultReg {
                           who = Just (Any anyof
                                       [ mkLeaf "walks"
                                       , mkLeaf "runs"
                                       , mkLeaf "eats"
                                       , All allof
                                         [ mkLeaf "drinks"
                                         , mkLeaf "swallows" ]
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
                       { name = "degustates"
                       , letbind = Any anyof [ mkLeaf "eats", mkLeaf "drinks" ]
                       , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing})
                       }

      it "should parse a simple constitutive rule" $ do
        mycsv <- BS.readFile "test/simple-constitutive-1.csv"
        parseR pRules "" (exampleStream mycsv) `shouldParse` [degustates]

      it "should parse a simple constitutive rule with checkboxes" $ do
        mycsv <- BS.readFile "test/simple-constitutive-1-checkboxes.csv"
        parseR pRules "" (exampleStream mycsv) `shouldParse` [degustates { srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 3, srccol = 2, version = Nothing}) }]

      let imbibeRule2 = [ defaultReg
                          { who = Just $ All allof
                                  [ mkLeaf "walks"
                                  , mkLeaf "degustates"
                                  ]
                          , srcref = Nothing
                          }
                        , defaultCon
                          { name = "degustates"
                          , letbind = Any anyof [ mkLeaf "eats", mkLeaf "imbibes" ]
                          , cond = Nothing
                          , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 3, srccol = 3, version = Nothing})
                          }
                        ]

      let imbibeRule3 = imbibeRule2 ++ [
            defaultCon
              { name = "imbibes"
              , letbind = All allof
                          [ mkLeaf "drinks"
                          , Any anyof [ mkLeaf "swallows"
                                , mkLeaf "spits" ]
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

      let if_king_wishes = [ defaultReg
                          { who = Just $ All allof
                                  [ mkLeaf "walks"
                                  , mkLeaf "eats"
                                  ]
                          , cond = Just $ mkLeaf "the King wishes"
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
                         , who = Just $ All allof
                                 [ mkLeaf "walks"
                                 , mkLeaf "eats"
                                 ]
                         , cond = Just $ mkLeaf "the King wishes"
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
            [ DefNameAlias ("singer") (mkLeaf "person") Nothing
              (Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 3, srccol = 1, version = Nothing})) ]

      let if_king_wishes_singer_2 = if_king_wishes ++
            [ DefNameAlias ("singer") (mkLeaf "person") Nothing
              (Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 3, srccol = 5, version = Nothing})) ]

      it "should parse natural language aliases (\"NL Aliases\") aka inline defined names" $ do
        mycsv <- BS.readFile "test/nl-aliases.csv"
        parseR pRules "" (exampleStream mycsv) `shouldParse` if_king_wishes_singer

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

      let bobUncle = defaultCon { name = "Bob's your uncle"
                                , letbind = Not
                                            ( Any anyof
                                              [ mkLeaf "Bob is estranged"
                                              , mkLeaf "Bob is dead"
                                              ]
                                            )
                                }
      
      it "should start a bool struct" $ do
        let testfile = "test/bob-head-1.csv"
        testcsv <- BS.readFile testfile
        parseR pRules testfile (exampleStream testcsv)
          `shouldParse` [bobUncle]

    describe "megaparsing UNLESS semantics" $ do

      let dayOfSilence = [ defaultReg { cond = Just ( Not ( mkLeaf "day of silence" ) ) } ] 

      let observanceMandatory = [ defaultReg { cond = Just
                                               ( Not
                                                 ( All allof
                                                   [ mkLeaf "day of silence"
                                                   , mkLeaf "observance is mandatory"
                                                   ]
                                                 )
                                               ) } ]

      let dayOfSong = [ defaultReg { cond = Just ( All allof [ Not ( mkLeaf "day of silence" )
                                                               , mkLeaf "day of song" ] ) } ]

      let silenceKing = [ defaultReg { cond = Just ( All allof [ mkLeaf "the king wishes"
                                                                 , Not ( mkLeaf "day of silence" )
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
            defaultReg { cond = Just ( All allof [
                                         mkLeaf "the king wishes"
                                         , Not
                                           ( Any anyof
                                             [ mkLeaf "day of silence"
                                             , mkLeaf "day of mourning"
                                             ]
                                           )
                                         ] ) } ]

      it "should read EVERY MUST IF UNLESS OR" $ do
        let testfile = "test/unless-regulative-6.csv"
        testcsv <- BS.readFile testfile
        parseR pRules testfile (exampleStream testcsv)
          `shouldParse` silenceMourning

      let mourningForbids = [
            defaultReg { cond = Just ( All allof [
                                         mkLeaf "the king wishes"
                                         , Not
                                           ( All allof
                                             [ mkLeaf "day of mourning"
                                             , mkLeaf "mourning forbids singing"
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
          `shouldParse` [ defaultCon 
                          { name = "Bob's your uncle"
                          , letbind = Any anyof
                                      [ mkLeaf "Bob is your mother's brother"
                                      , mkLeaf "Bob is your father's brother"
                                      ]
                          , cond = Just $ Not
                                ( mkLeaf "Bob is estranged" )
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
          
    describe "megaparsing DECIDE layouts" $ do
      it "should handle multiline" $ do
        let testfile = "test/financialadvisor-decide-1.csv"
        testcsv <- BS.readFile testfile
        parseR pRules testfile `traverse` (exampleStreams testcsv)
          `shouldParse`
          [ [ Scenario
