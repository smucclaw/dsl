{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Parsing.MegaparsingUnlessSpec (spec) where

import Text.Megaparsec
import LS.Lib
import AnyAll hiding (asJSON)
import LS.BasicTypes
import LS.Types
import LS.Rule
import Test.Hspec
import Data.ByteString.Lazy qualified as BS
import Data.Text qualified as T
import Test.Hspec.Megaparsec (shouldParse)
import System.FilePath ((</>), (-<.>))

filetest :: (HasCallStack, ShowErrorComponent e, Show b, Eq b) => String -> String -> (String -> MyStream -> Either (ParseErrorBundle MyStream e) b) -> b -> SpecWith ()
filetest testfile desc parseFunc expected =
  it (testfile <> ": " <> desc ) do
  testcsv <- BS.readFile ("test" </> "Parsing" </> "megaparsing-unless" </> testfile -<.> "csv")
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
    let _parseOther1  x y s = runMyParser id      runConfigDebug x y s

    describe "megaparsing UNLESS semantics" do

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
                                                               , mkLeafR "day of song" ] )
                                   , srcref = (\x -> x  { srcrow = 1 }) <$> (srcref defaultReg) } ]

      let wishSilence = [ mkLeafR "the king wishes"
                        , Not ( mkLeafR "day of silence" )
                        ]

      let silenceKing =
            [ defaultReg { cond = Just ( All Nothing wishSilence ) } ]

      let _silenceKingReversed =
            [ defaultReg { cond = Just ( All Nothing (reverse wishSilence) ) } ]

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

      it "pilcrows-1" do
        testcsv <- BS.readFile $ "test" </> "Parsing" </> "megaparsing-unless" </> "pilcrows-1" -<.> "csv"
        parseR pRules "pilcrows-1" `traverse` exampleStreams testcsv
          `shouldParse` [ dayOfSilence, srcrow2 <$> dayOfSong ]
