{-# LANGUAGE OverloadedStrings #-}
module MegaparsingMeansSpec where

-- import qualified Test.Hspec.Megaparsec as THM
import Text.Megaparsec
import LS.Lib
import AnyAll hiding (asJSON)
import LS.BasicTypes
import LS.Types
import Test.Hspec
import qualified Data.ByteString.Lazy as BS
import Test.Hspec.Megaparsec (shouldParse)

filetest :: (HasCallStack, ShowErrorComponent e, Show b, Eq b) => String -> String -> (String -> MyStream -> Either (ParseErrorBundle MyStream e) b) -> b -> SpecWith ()
filetest testfile desc parseFunc expected =
  it (testfile ++ ": " ++ desc ) $ do
  testcsv <- BS.readFile ("test/" <> testfile <> ".csv")
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

    describe "megaparsing MEANS" $ do

      let bobUncle1 = defaultHorn
            { name = ["Bob's your uncle"]
            , keyword = Means
            , clauses =
              [HC { hHead = RPBoolStructR ["Bob's your uncle"] RPis (Not (Any Nothing [mkLeaf (RPMT ["Bob is estranged"])
                                                                                       ,mkLeaf (RPMT ["Bob is dead"])]))
                   , hBody = Nothing}]
            , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing}) }

      filetest "bob-head-1" "less indented NOT" (parseR pRules) [srcrow2 bobUncle1]

      filetest "bob-head-1-b" "more indented NOT"
        (parseR pRules) [srcrow2 bobUncle1]

      let bobUncle2 = bobUncle1
            { clauses =
              [HC { hHead = RPBoolStructR ["Bob's your uncle"] RPis (Any Nothing [Not (mkLeaf (RPMT ["Bob is estranged"]))
                                                                                  ,mkLeaf (RPMT ["Bob is dead"])])
                   , hBody = Nothing } ] }

      filetest "bob-head-2" "handle less indentation"
          (parseR pRules) [srcrow2 bobUncle2]

      filetest "bob-head-3" "should handle outdentation"
        (parseR pRules) [srcrow2 bobUncle2]

      filetest "bob-tail-1" "should work for constitutive rules"
        (parseR pRules) [ srcrow2 defaultHorn
                          { name = ["Bob's your uncle"]
                          , keyword = Means
                          , clauses =  [ HC
                                         { hHead = RPBoolStructR [ "Bob's your uncle" ] RPis
                                           ( All Nothing
                                             [ Any Nothing
                                               [ Leaf
                                                 ( RPMT [ "Bob is your mother's brother" ] )
                                               , Leaf
                                                 ( RPMT [ "Bob is your father's brother" ] )
                                               ]
                                             , Not
                                               ( Leaf
                                                 ( RPMT [ "Bob is just a family friend" ] )
                                               )
                                             ]
                                           )
                                         , hBody = Nothing
                                         }
                                       ] } ]

-- bits of infrastructure
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
