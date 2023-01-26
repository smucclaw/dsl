{-# LANGUAGE OverloadedStrings #-}
module Parsing.NewParserSpec where

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
import qualified Data.ByteString.Lazy as BS
import Data.List.NonEmpty (NonEmpty ((:|)))
import Test.Hspec.Megaparsec (shouldParse)

filetest :: (HasCallStack, ShowErrorComponent e, Show b, Eq b) => String -> String -> (String -> MyStream -> Either (ParseErrorBundle MyStream e) b) -> b -> SpecWith ()
filetest testfile desc parseFunc expected =
  it (testfile ++ ": " ++ desc ) $ do
  testcsv <- BS.readFile ("test/Parsing/newparser/" <> testfile <> ".csv")
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
    let  parseOther   x y s = runMyParser id      runConfig x y s
    let _parseOther1  x y s = runMyParser id      runConfigDebug x y s

    describe "revised parser" $ do

      let simpleHorn = [ defaultHorn
              { name = ["X"]
              , keyword = Decide
              , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing})
              , clauses =
                [ HC
                  { hHead = RPConstraint ["X"] RPis ["Y"]
                  , hBody = Just $ All Nothing [ mkLeaf (RPConstraint ["Z"] RPis ["Q"])
                                               , mkLeaf (RPConstraint ["P"] RPgt ["NP"]) ]
                  } ]
              }
            ]

      let simpleHorn02 = [ defaultHorn
              { name = ["X"]
              , keyword = Decide
              , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing})
              , clauses =
                [ HC
                  { hHead = RPConstraint ["X"] RPis ["Y"]
                  , hBody = Nothing
                  } ]
              }
            ]

      filetest "horn-0-1" "should parse X IS Y"
        (parseOther pRelPred) ( RPConstraint ["X"] RPis ["Y"], [] )

      filetest "horn-0-1" "should parse X IS Y using the other parser"
        (parseOther pBSR) ( mkLeaf (RPConstraint ["X"] RPis ["Y"] ), [] )

      filetest "horn-0-3" "should parse X1 X2 IS Y"
        (parseOther pBSR) ( mkLeaf (RPConstraint ["X1", "X2"] RPis ["Y"] ), [] )

      filetest "horn-0-2" "should parse DECIDE X IS Y" (parseR pToplevel) simpleHorn02

      filetest "horn-2" "should parse horn clauses 2"
        (parseR pToplevel) simpleHorn

    describe "our new parser" $ do
      let myand = LS.Types.And
          myor  = LS.Types.Or

      it "should inject Deeper tokens to match indentation" $ do
        let testfile = "test/Parsing/newparser/indent-2-a.csv"
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
                        , MyLabel ["this is a label"] Nothing $ MyAny [ MyLeaf (text2pt "mid3")
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

    describe "parser elements and fragments ... should parse" $ do
      let ptFragment1 :: ParamText
          ptFragment1 = ("one word" :| []  , Nothing) :| []
          ptFragment2 = ("one word" :| [], Just (SimpleType TOne "String")) :| []
          ptFragment3  = ("two" :| ["words"], Nothing) :| []
          ptFragment3b = ("two" :| ["words"], Just (SimpleType TOne "String")) :| []
          ptFragment4a = ptFragment3b <> (("next" :| ["line"], Nothing) :| [])


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
        (parseOther pParamText) (ptFragment4a,[DefNameAlias {name = ["TwoWords"], detail = ["two","words"], nlhint = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 2, version = Nothing})}])


      let actionFragment1 :: BoolStructP
          actionFragment1 = mkLeaf (text2pt "win")

      filetest "action-1" "should a one-word BoolStructP"
        (parseOther pDoAction)(actionFragment1,[])

      filetest "action-2" "should a two-word BoolStructP"
        (parseOther pDoAction) (mkLeaf $ ("win" :| ["gloriously"]
                                                 , Nothing):|[]
                               ,[])

    describe "WHO / WHICH / WHOSE parsing of BoolStructR" $ do

      let whoStructR_1 = defaultReg { who = Just ( mkLeaf ( RPMT ["eats"] ) ) }
          whoStructR_2 = defaultReg { who = Just ( mkLeaf ( RPMT ["eats", "rudely"] ) ) }
          whoStructR_3 = defaultReg { who = Just ( mkLeaf ( RPMT ["eats", "without", "manners"] ) ) }

      filetest "who-1" "should handle a simple RPMT"
        (parseR pToplevel) [ whoStructR_1 ]

      filetest "who-2" "should handle a simple RPMT"
        (parseR pToplevel) [ whoStructR_2 ]

      filetest "who-3" "should handle a simple RPMT"
        (parseR pToplevel) [ whoStructR_3 ]

      it "sameline fourIs float" $ do
        parseOther _fourIs "" (exampleStream "A,IS,IS,IS\n")
          `shouldParse` ((A_An,Is,Is,Is), [])

      it "sameline threeIs float" $ do
        parseOther _threeIs "" (exampleStream "IS,IS,IS,IS\n")
          `shouldParse` ((Is,(Is,Is),Is), [])

    describe "MISC" $ do
      let unauthorisedExpected = [Hornlike {name = ["a Data Breach"], super = Nothing, keyword = Means, given = Nothing, upon = Nothing, clauses = [HC {hHead = RPBoolStructR ["a Data Breach"] RPis (mkLeaf (RPMT ["a Notifiable Data Breach"])), hBody = Just (mkLeaf (RPMT ["a data breach","occurred"]))}], rlabel = Just ("\167",1,"NDB Qualification"), lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing}), defaults = [], symtab = []},DefNameAlias {name = ["NDB"], detail = ["a Notifiable Data Breach"], nlhint = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 4, version = Nothing})},Hornlike {name = ["a data breach","occurred"], super = Nothing, keyword = Means, given = Nothing, upon = Nothing, clauses = [HC {hHead = RPBoolStructR ["a data breach","occurred"] RPis (Any (Just (PrePost "any unauthorised" "of personal data")) [mkLeaf (RPMT ["access"]),mkLeaf (RPMT ["use"]),mkLeaf (RPMT ["disclosure"]),mkLeaf (RPMT ["copying"]),mkLeaf (RPMT ["modification"]),mkLeaf (RPMT ["disposal"])]), hBody = Nothing}], rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 3, srccol = 4, version = Nothing}), defaults = [], symtab = []}]
      filetest "unauthorised" "should parse correctly"
        (parseR pToplevel) unauthorisedExpected

srcrow_, srcrow1', srcrow1, srcrow2, srccol1, srccol2 :: Rule -> Rule
srcrow', srccol' :: Int -> Rule -> Rule
srcrow_   w = w { srcref = Nothing, hence = srcrow_ <$> (hence w), lest = srcrow_ <$> (lest w) }
srcrow1'  w = w { srcref = (\x -> x  { srcrow = 1 }) <$> srcref defaultReg }
srcrow1     = srcrow' 1
srcrow2     = srcrow' 2
srcrow' n w = w { srcref = (\x -> x  { srcrow = n }) <$> srcref w }
srccol1     = srccol' 1
srccol2     = srccol' 2
srccol' n w = w { srcref = (\x -> x  { srccol = n }) <$> srcref w }
