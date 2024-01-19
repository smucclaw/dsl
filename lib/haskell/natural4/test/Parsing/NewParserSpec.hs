{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Parsing.NewParserSpec (spec) where

import Text.Megaparsec
import LS.Lib
import LS.Parser
import LS.RelationalPredicates
import LS.Tokens
import AnyAll hiding (asJSON)
import LS.BasicTypes
import LS.Types
import LS.Rule
import Test.Hspec
import Data.ByteString.Lazy qualified as BS
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified as T
import Test.Hspec.Megaparsec (shouldParse)
import System.FilePath ((</>), (-<.>))

filetest :: (HasCallStack, ShowErrorComponent e, Show b, Eq b) => String -> String -> (String -> MyStream -> Either (ParseErrorBundle MyStream e) b) -> b -> SpecWith ()
filetest testfile desc parseFunc expected =
  it (testfile <> ": " <> desc ) do
  testcsv <- BS.readFile $ "test" </> "Parsing" </> "newparser" </> testfile -<.> "csv"
  parseFunc testfile `traverse` exampleStreams testcsv
    `shouldParse` [ expected ]

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

    describe "revised parser" do

      let simpleHorn = [ defaultHorn
              { name = [MTT "X"]
              , keyword = Decide
              , srcref = mkTestSrcRef 1 1
              , clauses =
                [ HC
                  { hHead = RPConstraint [MTT "X"] RPis [MTT "Y"]
                  , hBody = Just $ All Nothing [ mkLeaf (RPConstraint [MTT "Z"] RPis [MTT "Q"])
                                               , mkLeaf (RPConstraint [MTT "P"] RPgt [MTT "NP"]) ]
                  } ]
              }
            ]

      let simpleHorn02 = [ defaultHorn
              { name = [MTT "X"]
              , keyword = Decide
              , srcref = mkTestSrcRef 1 1
              , clauses =
                [ HC
                  { hHead = RPConstraint [MTT "X"] RPis [MTT "Y"]
                  , hBody = Nothing
                  } ]
              }
            ]

      filetest "horn-0-1" "should parse X IS Y"
        (parseOther pRelPred) ( RPConstraint [MTT "X"] RPis [MTT "Y"], [] )

      filetest "horn-0-1" "should parse X IS Y using the other parser"
        (parseOther pBSR) ( mkLeaf (RPConstraint [MTT "X"] RPis [MTT "Y"] ), [] )

      filetest "horn-0-3" "should parse X1 X2 IS Y"
        (parseOther pBSR) ( mkLeaf (RPConstraint [MTT "X1", MTT "X2"] RPis [MTT "Y"] ), [] )

      filetest "horn-0-2" "should parse DECIDE X IS Y" (parseR pToplevel) simpleHorn02

      filetest "horn-2" "should parse horn clauses 2"
        (parseR pToplevel) simpleHorn

    describe "our new parser" do
      let myand = LS.Types.And
          myor  = LS.Types.Or

      it "should inject Deeper tokens to match indentation" do
        let testfile = "test" </> "Parsing" </> "newparser" </> "indent-2-a" -<.> "csv"
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

      filetest "indent-2-b" "should handle indent-2-b"
        (parseOther exprP) abcd

      let ablcd = (MyAny [MyLeaf (text2pt "top1")
                        , MyLeaf (text2pt "top2")
                        , MyLabel [MTT "this is a label"] Nothing $ MyAny [ MyLeaf (text2pt "mid3")
                                                                          , MyLeaf (text2pt "mid4") ]
                        ],[])

      filetest "indent-2-c-3" "label left"  (parseOther exprP) ablcd

      filetest "indent-2-d" "should handle indent-2-d which goes out, in, out"
        (parseOther exprP)
          (MyAny [MyLeaf (text2pt "term1")
                 , MyAll [ MyLeaf (text2pt "term2")
                         , MyLeaf (text2pt "term3")
                         ]
                 , MyLeaf (text2pt "term4")
                 , MyLeaf (text2pt "term5")
                 ],[])

    describe "parser elements and fragments ... should parse" do
      let ptFragment1 :: ParamText
          ptFragment1 = (MTT <$> "one word" :| []  , Nothing) :| []
          ptFragment2 = (MTT <$> "one word" :| [], Just (SimpleType TOne "String")) :| []
          ptFragment3  = (MTT <$> "two" :| ["words"], Nothing) :| []
          ptFragment3b = (MTT <$> "two" :| ["words"], Just (SimpleType TOne "String")) :| []
          ptFragment4a = ptFragment3b <> ((MTT <$> "next" :| ["line"], Nothing) :| [])


      filetest "paramtext-1" "paramtext-1 a single-token untyped ParamText"
        (parseOther pParamText) (ptFragment1,[])

      filetest "paramtext-2" "a single-token ParamText typed with IS | A"
        (parseOther pParamText) (ptFragment2,[])

      filetest "paramtext-2-a" "a single-token ParamText typed with IS A"
        (parseOther pParamText) (ptFragment2,[])

      filetest "paramtext-2-b" "a single-token ParamText typed with ::"
        (parseOther pParamText) (ptFragment2,[])

      filetest "paramtext-3" "a multi-token ParamText, untyped"
        (parseOther pParamText) (ptFragment3,[])

      filetest "paramtext-3-b" "a multi-token ParamText, typed String"
        (parseOther pParamText) (ptFragment3b,[])

      filetest "paramtext-4-a" "a multi-line ParamText, typed String"
        (parseOther pParamText) (ptFragment4a,[DefNameAlias {name = [MTT "TwoWords"], detail = [MTT "two", MTT "words"], nlhint = Nothing, srcref = mkTestSrcRef 2 2}])


      let actionFragment1 :: BoolStructP
          actionFragment1 = mkLeaf (text2pt "win")

      filetest "action-1" "should a one-word BoolStructP"
        (parseOther pDoAction)(actionFragment1,[])

      filetest "action-2" "should a two-word BoolStructP"
        (parseOther pDoAction) (mkLeaf $ (MTT <$> "win" :| ["gloriously"]
                                         , Nothing):|[]
                               ,[])

    describe "WHO / WHICH / WHOSE parsing of BoolStructR" do

      let whoStructR_1 = defaultReg { who = Just ( mkRpmtLeaf ["eats"] ) }
          whoStructR_2 = defaultReg { who = Just ( mkRpmtLeaf ["eats", "rudely"] ) }
          whoStructR_3 = defaultReg { who = Just ( mkRpmtLeaf ["eats", "without", "manners"] ) }

      filetest "who-1" "should handle a simple RPMT"
        (parseR pToplevel) [ whoStructR_1 ]

      filetest "who-2" "should handle a simple RPMT"
        (parseR pToplevel) [ whoStructR_2 ]

      filetest "who-3" "should handle a simple RPMT"
        (parseR pToplevel) [ whoStructR_3 ]

      it "sameline fourIs float" do
        parseOther _fourIs "" (exampleStream "A,IS,IS,IS\n")
          `shouldParse` ((A_An,Is,Is,Is), [])

      it "sameline threeIs float" do
        parseOther _threeIs "" (exampleStream "IS,IS,IS,IS\n")
          `shouldParse` ((Is,(Is,Is),Is), [])

    describe "MISC" do
      let unauthorisedExpected = [
            defaultHorn { name = [MTT "a Data Breach"],
              clauses =
                [ HC
                    { hHead = RPBoolStructR [MTT "a Data Breach"] RPis (mkRpmtLeaf ["a Notifiable Data Breach"]),
                      hBody = Just (mkLeaf (RPMT [MTT "a data breach", MTT "occurred"]))
                    }
                ],
              rlabel = Just ("\167", 1, "NDB Qualification"),
              srcref = mkTestSrcRef 1 1
            },
            DefNameAlias
            { name = [MTT "NDB"],
              detail = [MTT "a Notifiable Data Breach"],
              nlhint = Nothing,
              srcref = mkTestSrcRef 2 4
            },
            defaultHorn
            { name = [MTT "a data breach", MTT "occurred"],
              clauses =
                [ HC
                    { hHead =
                        RPBoolStructR
                          [MTT "a data breach", MTT "occurred"]
                          RPis
                          ( Any
                              (Just (PrePost "any unauthorised" "of personal data"))
                              [ mkRpmtLeaf ["access"],
                                mkRpmtLeaf ["use"],
                                mkRpmtLeaf ["disclosure"],
                                mkRpmtLeaf ["copying"],
                                mkRpmtLeaf ["modification"],
                                mkRpmtLeaf ["disposal"]
                              ]
                          ),
                      hBody = Nothing
                    }
                ],
              srcref = mkTestSrcRef 3 4
            }
            ]
      filetest "unauthorised" "should parse correctly"
        (parseR pToplevel) unauthorisedExpected
