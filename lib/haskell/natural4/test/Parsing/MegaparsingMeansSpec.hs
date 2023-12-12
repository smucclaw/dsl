{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Parsing.MegaparsingMeansSpec where

-- import qualified Test.Hspec.Megaparsec as THM
import Text.Megaparsec
import LS.Lib
import AnyAll hiding (asJSON)
import LS.BasicTypes
import LS.Types
import LS.Rule
import Test.Hspec
import qualified Data.ByteString.Lazy as BS
import Test.Hspec.Megaparsec (shouldParse)

filetest :: (HasCallStack, ShowErrorComponent e, Show b, Eq b) => String -> String -> (String -> MyStream -> Either (ParseErrorBundle MyStream e) b) -> b -> SpecWith ()
filetest testfile desc parseFunc expected =
  it (testfile ++ ": " ++ desc ) do
  testcsv <- BS.readFile ("test/Parsing/megaparsing-means/" <> testfile <> ".csv")
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
    let _parseOther1  x y s = runMyParser id      runConfigDebug x y s

    describe "megaparsing MEANS" do

      let bobUncle1 = defaultHorn
            { name = [MTT "Bob's your uncle"]
            , keyword = Means
            , clauses =
              [HC { hHead = RPBoolStructR [MTT "Bob's your uncle"] RPis (Not (Any Nothing [mkRpmtLeaf ["Bob is estranged"]
                                                                                          ,mkRpmtLeaf ["Bob is dead"]]))
                   , hBody = Nothing}]
            , srcref = mkTestSrcRef 1 1 }

      filetest "bob-head-1" "less indented NOT" (parseR pRules) [srcrow2 bobUncle1]

      filetest "bob-head-1-b" "more indented NOT"
        (parseR pRules) [srcrow2 bobUncle1]

      let bobUncle2 =
            bobUncle1
              { clauses =
                  [ HC
                      { hHead =
                          RPBoolStructR
                            [MTT "Bob's your uncle"]
                            RPis
                            ( Any
                                Nothing
                                [ Not (mkRpmtLeaf ["Bob is estranged"]),
                                  mkRpmtLeaf ["Bob is dead"]
                                ]
                            ),
                        hBody = Nothing
                      }
                  ]
              }

      filetest "bob-head-2" "handle less indentation"
          (parseR pRules) [srcrow2 bobUncle2]

      filetest "bob-head-3" "should handle outdentation"
        (parseR pRules) [srcrow2 bobUncle2]

      filetest "bob-tail-1" "should work for constitutive rules"
        (parseR pRules) [ srcrow2 defaultHorn
                          { name = [MTT "Bob's your uncle"]
                          , keyword = Means
                          , clauses =  [ HC
                                         { hHead = RPBoolStructR [MTT "Bob's your uncle" ] RPis
                                           ( All Nothing
                                             [ Any Nothing
                                               [ Leaf ( mkRpmt ["Bob is your mother's brother"] )
                                               , Leaf ( mkRpmt ["Bob is your father's brother"] )
                                               ]
                                             , Not
                                               ( Leaf ( mkRpmt ["Bob is just a family friend"] )
                                               )
                                             ]
                                           )
                                         , hBody = Nothing
                                         }
                                       ] } ]
