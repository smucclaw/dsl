{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

-- import Test.Hspec.Megaparsec hiding (shouldParse)
import Text.Megaparsec
import LS.Lib
import LS.Parser
import LS.RelationalPredicates
import LS.ParamText
import AnyAll hiding (asJSON)
import LS.Types
import LS.Error

import Test.Hspec
import qualified Data.ByteString.Lazy as BS
import Data.List.NonEmpty (NonEmpty ((:|)), fromList)
import Debug.Trace (traceShowM)
import qualified Data.Text.Lazy as Text
import System.Environment (lookupEnv)
import Data.Maybe (isJust)

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
  mpd <- lookupEnv "MP_DEBUG"
  let runConfig_ = RC
        { debug = isJust mpd
        , callDepth = 0
        , parseCallStack = []
        , sourceURL = "STDIN"
        , asJSON = False
        , toNLG = False
        , toBabyL4 = False
        , toProlog = False
        , toUppaal = False
        , saveAKA = False
        , wantNotRules = False
        }
  let runConfig = runConfig_ { sourceURL = "test/Spec" }
      runConfigDebug = runConfig { debug = True }
  let combine (a,b) = a ++ b
  let parseR = runMyParser combine runConfig
  let parseR1 x y s = runMyParser combine runConfigDebug x y s <* traceShowM (tokenVal <$> unMyStream s)
  let parseOther  = runMyParser id runConfig
  let parseOther1 x y s = runMyParser id runConfigDebug x y s <* traceShowM (tokenVal <$> unMyStream s)

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

      filetest "indented-1" "parse indented-1.csv (inline boolean expression)" 
        (parseR pRules) imbibeRule


      filetest "indented-1-checkboxes" "should parse indented-1-checkboxes.csv (with checkboxes)" 
        (parseR pRules) imbibeRule

      let degustates = defaultHorn { name = ["degustates"]
                                   , keyword = Means
                                   , given = Nothing
                                   , upon = Nothing
                                   , clauses = [ HC2 { hHead = RPParamText (("degustates" :| [],Nothing) :| [])
                                                     , hBody = Just (Any Nothing [Leaf (RPParamText (("eats"   :| [],Nothing) :| []))
                                                                                 ,Leaf (RPParamText (("drinks" :| [],Nothing) :| []))
                                                                                 ])}]
                                   , srcref = Just (SrcRef { url = "test/Spec"
                                                           , short = "test/Spec"
                                                           , srcrow = 2
                                                           , srccol = 1
                                                           , version = Nothing }) }
      
      filetest "simple-constitutive-1" "should parse a simple constitutive rule" 
        (parseR pRules) [degustates]

      filetest "simple-constitutive-1-checkboxes" "should parse a simple constitutive rule with checkboxes" 
        (parseR pRules) [degustates { srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 5, srccol = 2, version = Nothing}) }]

      let imbibeRule2 srcrow srccol = [
            defaultReg
              { who = Just $ All Nothing
                      [ mkLeafR "walks"
                      , mkLeafR "degustates"
                      ]
              , srcref = Nothing
              }
            , defaultHorn { name = ["degustates"]
                          , keyword = Means
                          , clauses = [HC2 { hHead = RPParamText (("degustates" :| [],Nothing) :| [])
                                           , hBody = Just (Any Nothing [Leaf (RPParamText (("eats" :| [],Nothing) :| []))
                                                                       ,Leaf (RPParamText (("imbibes" :| [],Nothing) :| []))])}]
                          , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec"
                                                  , srcrow = srcrow, srccol = srccol
                                                  , version = Nothing})
                          }
            ]


      let imbibeRule3 sr2 sc2 = imbibeRule2 sr2 sc2 ++ [
            defaultHorn { name = ["imbibes"]
                        , keyword = Means
                        , given = Nothing
                        , upon = Nothing
                        , clauses = [HC2 { hHead = RPParamText (("imbibes" :| [],Nothing) :| [])
                                         , hBody = Just (All Nothing [Leaf (RPParamText (("drinks" :| [],Nothing) :| []))
                                                                     ,Any Nothing [Leaf (RPParamText (("swallows" :| [],Nothing) :| []))
                                                                                  ,Leaf (RPParamText (("spits" :| [],Nothing) :| []))]])}]
                        , rlabel = Nothing
                        , lsource = Nothing
                        , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 4, srccol = 5, version = Nothing})}
            ]              
      
      filetest "indented-2" "should parse indented-2.csv (inline constitutive rule)" 
        (parseR pRules) $ imbibeRule2 4 3

      filetest "indented-3" "should parse indented-3.csv (defined names in natural positions)" 
        (parseR pRules) $ imbibeRule3 3 3

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
        (parseR pRules) singer_chain

      filetest "chained-regulatives-part1-alternative-1" "should parse alternative deadline/action arrangement 1" 
        (parseR pRules) [king_pays_singer]

      filetest "chained-regulatives-part1-alternative-2" "should parse alternative deadline/action arrangement 2" 
        (parseR pRules) [king_pays_singer]

      filetest "chained-regulatives-part1-alternative-3" "should parse alternative deadline/action arrangement 3" 
        (parseR pRules) [king_pays_singer]

      filetest "chained-regulatives-part1-alternative-4" "should parse alternative arrangement 4, no deadline at all" 
        (parseR pRules) [king_pays_singer_eventually]

      let if_king_wishes_singer = if_king_wishes ++
            [ DefNameAlias ["singer"] ["person"] Nothing
              (Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 3, srccol = 1, version = Nothing})) ]

      let if_king_wishes_singer_nextline = if_king_wishes ++
            [ DefNameAlias ["singer"] ["person"] Nothing
              (Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 2, version = Nothing})) ]

      let if_king_wishes_singer_2 = if_king_wishes ++
            [ DefNameAlias ["singer"] ["person"] Nothing
              (Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 3, srccol = 5, version = Nothing})) ]

      filetest "nl-aliases" "should parse natural language aliases (\"NL Aliases\") aka inline defined names" 
        (parseR pRules) if_king_wishes_singer

      filetest "nl-aliases-2" "should parse natural language aliases (\"NL Aliases\") on the next line" 
        (parseR pRules) if_king_wishes_singer_nextline

      let singer_must_pay_params =
            singer_must_pay { action = Leaf (("pay" :| []                 , Nothing)
                                             :| [("to"     :| ["the King"], Nothing)
                                                ,("amount" :| ["$20"]     , Nothing)]) }

      filetest "action-params-singer" "should parse action params" 
        (parseR pRules) [singer_must_pay_params]

      filetest "blank-lines" "should parse despite interrupting newlines" 
        (parseR pRules) if_king_wishes_singer_2

    describe "megaparsing MEANS" $ do

      let bobUncle1 = defaultHorn
            { name = ["Bob's your uncle"]
            , keyword = Means
            , clauses =
              [ HC2 { hHead = RPParamText (("Bob's your uncle" :| [],Nothing) :| [])
                    , hBody = Just $ Not ( Any Nothing [mkLeafR "Bob is estranged"
                                                       ,mkLeafR "Bob is dead"])}]
            , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 1, version = Nothing}) }

      filetest "bob-head-1" "bob-head-1: less indented NOT"
        (parseR pRules) []

      filetest "bob-head-1-b" "more indented NOT"
        (parseR pRules) [bobUncle1]

{--

-- this is commented out because it leads to an infinite loop.
-- What we need to do: instead of whatever it's doing now,
-- have it use the new Parser.hs code to wrap whatever inner RelationalPredicate parser combinator we're trying to find.

      it "should handle less indentation" $ do
        let testfile = "test/bob-head-2.csv"
        testcsv <- BS.readFile testfile
        parseR pRules testfile (exampleStream testcsv)
          `shouldParse` [bobUncle1]

      let bobUncle2 = bobUncle1
            { clauses = 
              [ HC2 { hHead = RPParamText (("Bob's your uncle" :| [],Nothing) :| [])
                    , hBody = Just $ Any Nothing [Not $ mkLeafR "Bob is estranged"
                                                 ,      mkLeafR "Bob is dead" ] } ] }
      
      filetest "bob-head-3" "should handle outdentation"
        (parseR pRules) [bobUncle2]

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
            
      filetest "unless-regulative-1" "read EVERY MUST UNLESS" 
        (parseR pRules) dayOfSilence
                      
      filetest "unless-regulative-2" "read EVERY MUST UNLESS IF" 
        (parseR pRules) silenceKing
                      
      filetest "unless-regulative-3" "read EVERY MUST IF UNLESS" 
        (parseR pRules) silenceKing
                      
      filetest "unless-regulative-4" "read EVERY UNLESS MUST IF" 
        (parseR pRules) silenceKing
                      
      filetest "unless-regulative-5" "read EVERY IF MUST UNLESS" 
        (parseR pRules) silenceKing
                      
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

      filetest "unless-regulative-6" "should read EVERY MUST IF UNLESS OR" 
        (parseR pRules) silenceMourning

      let mourningForbids = [
            defaultReg { cond = Just ( All Nothing [
                                         mkLeafR "the king wishes"
                                         , Not
                                           ( All Nothing
                                             [ mkLeafR "day of mourning"
                                             , mkLeafR "mourning forbids singing"
                                             ]
                                           ) ] ) } ]
                                         
      filetest "unless-regulative-7" "should read EVERY MUST IF UNLESS AND" 
        (parseR pRules) mourningForbids
                      
      filetest "ifnot-1-joined" "should read IF NOT when joined" 
        (parseR pRules) dayOfSilence
                      
      filetest "ifnot-2-separate" "should read IF-NOT when separate" 
        (parseR pRules) dayOfSilence

      filetest "ifnot-4-indentation-explicit" "should handle NOT ... AND indented" 
        (parseR pRules) observanceMandatory
                      
      filetest "ifnot-5-indentation-explicit" "should handle NOT AND indented the other way" 
        (parseR pRules) dayOfSong
                      
      filetest "bob-tail-1" "should work for constitutive rules" 
        (parseR pRules) [ defaultHorn
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

      filetest "pilcrows-1" "should handle pilcrows" 
        (parseR pRules) [ dayOfSilence 
                        , dayOfSong
                        ]
        -- forM_ (exampleStreams testcsv) $ \stream ->
        --   parseR pRules testfile stream
        --     `shouldParse` [ defaultCon 
        --                   ]

  -- upgrade single OR group to bypass the top level AND group

  -- defNameAlias should absorb the WHO limb

    describe "megaparsing scenarios" $ do
      filetest "scenario-1" "should handle labeled given/expect" 
        (parseR pRules)
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
-}

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

      filetest "horn-0-1" "should parse a fragment"
        (parseOther pRelPred) ( RPConstraint ["X"] RPis ["Y"], [] )

      filetest "horn-1" "should parse horn clause on a single line"
        (parseR1 pToplevel) simpleHorn
              
      filetest "horn-2" "should parse horn clauses 2"
        (parseR pToplevel) simpleHorn 
             
      filetest "horn-3" "should parse horn clauses 3"
        (parseR pToplevel) simpleHorn 


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
          
      filetest "indent-2-a" "should handle indent-2-a"
        (parseOther exprP) abcd
        
      filetest "indent-2-b" "should handle indent-2-b"
        (parseOther exprP) abcd 

      let ablcd = (MyAny [MyLeaf (text2pt "top1")
                        , MyLeaf (text2pt "top2")
                        , MyLabel "this is a label" $ MyAny [ MyLeaf (text2pt "mid3")
                                                            , MyLeaf (text2pt "mid4") ]
                        ],[])

      filetest "indent-2-c" "should handle indent-2-c which has a label"
        (parseOther exprP) ablcd 

      filetest "indent-2-d" "should handle indent-2-d which goes out, in, out"
        (parseOther exprP)
          (MyAny [MyLeaf (text2pt "term1")
                 , MyAll [ MyLeaf (text2pt "term2")
                         , MyLeaf (text2pt "term3")
                         ]
                 , MyLeaf (text2pt "term4")
                 , MyLeaf (text2pt "term5")
                 ],[])

    describe "parser elements and fragments ... should parse" $ do
      let ptFragment1 :: ParamText
          ptFragment1 = ("one word" :| []  , Nothing) :| []
          ptFragment2 = ("one word" :| [], Just (SimpleType TOne "String")) :| []
          ptFragment3  = ("two" :| ["words"], Nothing) :| []
          ptFragment3b = ("two" :| ["words"], Just (SimpleType TOne "String")) :| []


      filetest "paramtext-1" "paramtext-1 a single-token untyped ParamText"
        (parseOther pParamText) (ptFragment1,[])
        
      filetest "paramtext-2" "should paramtext-2 a single-token ParamText typed with IS | A"
        (parseOther pParamText) (ptFragment2,[])
        
      filetest "paramtext-2-a" "should paramtext-2-a a single-token ParamText typed with IS A"
        (parseOther pParamText) (ptFragment2,[])
        
      filetest "paramtext-2-b" "should paramtext-2-b a single-token ParamText typed with ::"
        (parseOther pParamText) (ptFragment2,[])
        
      filetest "paramtext-3" "should paramtext-3 a multi-token ParamText, untyped"
        (parseOther pParamText) (ptFragment3,[])
        
      filetest "paramtext-3-b" "should paramtext-3-b a multi-token ParamText, typed String"
        (parseOther pParamText) (ptFragment3b,[])


      let actionFragment1 :: BoolStructP
          actionFragment1 = Leaf (text2pt "win")

      filetest "action-1" "should a one-word BoolStructP"
        (parseOther pDoAction)(actionFragment1,[])

      filetest "action-2" "should a two-word BoolStructP"
        (parseOther pDoAction) (Leaf $ ("win" :| ["gloriously"]
                                                 , Nothing):|[]
                               ,[])
      
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
          
      filetest "who-1" "should handle a simple RPParamText"
        (parseR pToplevel) [ whoStructR_1 ] 
          
      filetest "who-2" "should handle a simple RPParamText"
        (parseR pToplevel) [ whoStructR_2 ] 
          
      filetest "who-3" "should handle a simple RPParamText"
        (parseR pToplevel) [ whoStructR_3 ] 

{- infinite loop here
      filetest "who-4-a" "should handle a multiline RPParamText without indentation"
        (parseR pToplevel) [ mkWhoStruct (Text.words "eats without manners") (Text.words "sans decorum") ] 
          
      filetest "who-4-b" "should flat style, variant"
        (parseR pToplevel) [ whoStructR_4 ] 
          
      filetest "who-5" "should be a constraint"
        (parseR pToplevel) [ whoStructR_5 ] 
-}

filetest testfile desc parseFunc expected =
  it ("(" ++ testfile ++ ") " ++ desc) $ do
  testcsv <- BS.readFile ("test/" <> testfile <> ".csv")
  parseFunc testfile `traverse` exampleStreams testcsv
    `shouldParse` [ expected ]
  
