{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
-- import Test.Hspec.Megaparsec hiding (shouldParse)
import Text.Megaparsec
import Lib
import AnyAll hiding (asJSON)
import Types
import Error
import qualified Data.ByteString.Lazy as BS
import Control.Monad.Reader (ReaderT(runReaderT))
import Control.Monad.Writer (WriterT(runWriterT))



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
  { every = "person"
  , who = Nothing
  , cond = Nothing
  , deontic = DMust
  , action = ("sing",[])
  , temporal = Nothing
  , hence = Nothing
  , lest = Nothing
  , rlabel = Nothing
  , lsource = Nothing
  , srcref = Nothing
  }

defaultCon = Constitutive
  { term = ""
  , cond = Nothing
  , rlabel = Nothing
  , lsource = Nothing
  , srcref = Nothing
  }

main :: IO ()
main = do
  runConfig_ <- getConfig
  let runConfig = runConfig_ { sourceURL = "test/Spec" }
  let combine (a,b) = a ++ b
  let parseR p = parse (fmap combine $ runReaderT (runWriterT p) runConfig)

  hspec $ do
    describe "Nothing Test" $ do
      it "should be nothing" $ do
        (Nothing :: Maybe ()) `shouldBe` (Nothing :: Maybe ())
    describe "megaparsing" $ do

      it "should parse an unconditional" $ do
        parseR (pRule <* eof) "" (exampleStream ",,,,\n,EVERY,person,,\n,MUST,,,\n,->,sing,,\n")
          `shouldParse` [ defaultReg { every = "person"
                                     , deontic = DMust
                                     , action = ("sing",[])
                                     } ]

      it "should parse a single OtherVal" $ do
        parseR (pRule <* eof) "" (exampleStream ",,,,\n,EVERY,person,,\n,WHO,walks,,\n,MUST,,,\n,->,sing,,\n")
          `shouldParse` [ defaultReg { who = Just (Leaf "walks") } ]

      it "should parse the null temporal EVENTUALLY" $ do
        parseR (pRule <* eof) "" (exampleStream ",,,,\n,EVERY,person,,\n,WHO,walks,,\n,MUST,EVENTUALLY,,\n,->,sing,,\n")
          `shouldParse` [ defaultReg { who = Just (Leaf "walks") } ]

      it "should parse dummySing" $ do
        parseR (pRule <* eof) "" (exampleStream ",,,,\n,EVERY,person,,\n,WHO,walks,// comment,continued comment should be ignored\n,AND,runs,,\n,AND,eats,,\n,OR,drinks,,\n,MUST,,,\n,->,sing,,\n")
          `shouldParse` [ defaultReg {
                            who = Just (All
                                         ( Pre "Who" )
                                         [ Leaf "walks"
                                         , Leaf "runs"
                                         , Any
                                           ( Pre "any of:" )
                                           [ Leaf "eats"
                                           , Leaf "drinks"
                                           ]
                                         ])
                            } ]

      let imbibeRule = [ defaultReg {
                           who = Just (Any
                                       ( Pre "Who" )
                                       [ Leaf "walks"
                                       , Leaf "runs"
                                       , Leaf "eats"
                                       , All ( Pre "all of:" )
                                         [ Leaf "drinks"
                                         , Leaf "swallows" ]
                                       ])
                           } ]

      it "should parse indentedDummySing" $ do
        parseR (pRule <* eof) "" (exampleStream ",,,,\n,EVERY,person,,\n,WHO,walks,// comment,continued comment should be ignored\n,OR,runs,,\n,OR,eats,,\n,OR,,drinks,\n,,AND,swallows,\n,MUST,,,\n,->,sing,,\n")
          `shouldParse` imbibeRule

      it "should parse indented-1.csv (inline boolean expression)" $ do
        mycsv <- BS.readFile "test/indented-1.csv"
        parseR (pRule <* eof) "" (exampleStream mycsv) `shouldParse` imbibeRule


      it "should parse indented-1-checkboxes.csv (with checkboxes)" $ do
        mycsv <- BS.readFile "test/indented-1-checkboxes.csv"
        parseR (pRule <* eof) "" (exampleStream mycsv) `shouldParse` imbibeRule

      let degustates = defaultCon
                       { term = "degustates"
                       , cond = Just $ Any ( Pre "any of:" ) [ Leaf "eats", Leaf "drinks" ]
                       }

      it "should parse a simple constitutive rule" $ do
        mycsv <- BS.readFile "test/simple-constitutive-1.csv"
        parseR (pRule <* eof) "" (exampleStream mycsv) `shouldParse` [degustates]

      it "should parse a simple constitutive rule with checkboxes" $ do
        mycsv <- BS.readFile "test/simple-constitutive-1-checkboxes.csv"
        parseR (pRule <* eof) "" (exampleStream mycsv) `shouldParse` [degustates]

      let imbibeRule2 = [ defaultReg
                          { who = Just $ All
                                  ( Pre "Who" )
                                  [ Leaf "walks"
                                  , Leaf "degustates"
                                  ]
                          , action = ("sing", [])
                          }
                        , defaultCon
                          { term = "degustates"
                          , cond = Just $ Any ( Pre "any of:" ) [ Leaf "eats", Leaf "imbibes" ]
                          }
                        ]

      let imbibeRule3 = imbibeRule2 ++ [
            defaultCon
              { term = "imbibes"
              , cond = Just $ All ( Pre "all of:" )
                [ Leaf "drinks"
                , Any ( Pre "any of:") [ Leaf "swallows"
                                       , Leaf "spits" ]
                ]
              } ]
      
      it "should parse indented-2.csv (inline constitutive rule)" $ do
        mycsv <- BS.readFile "test/indented-2.csv"
        parseR (pRule <* eof) "" (exampleStream mycsv) `shouldParse` imbibeRule2

      it "should parse indented-3.csv (defined terms in natural positions)" $ do
        mycsv <- BS.readFile "test/indented-3.csv"
        parseR (pRule <* eof) "" (exampleStream mycsv) `shouldParse` imbibeRule3

      let if_king_wishes = [ defaultReg
                          { who = Just $ All
                                  ( Pre "Who" )
                                  [ Leaf "walks"
                                  , Leaf "eats"
                                  ]
                          , cond = Just $ Leaf "the King wishes"
                          }
                        ]

      let king_pays_singer = [ defaultReg
                          { every = "King"
                          , deontic = DMay
                          , action = ("pay", [])
                          , temporal = Just (TAfter "20min")
                          }
                        ]

      let king_pays_singer_eventually = do
            r <- king_pays_singer
            return $ r { temporal = Nothing }

      let singer_must_pay = [ defaultReg
                              { every = "Singer"
                              , action = ("pay", [])
                              , temporal = Just (TBefore "supper")
                              }
                        ]

      let singer_chain = [ defaultReg
                         { every = "person"
                         , who = Just $ All
                                 ( Pre "Who" )
                                 [ Leaf "walks"
                                 , Leaf "eats"
                                 ]
                         , cond = Just $ Leaf "the King wishes"
                         , hence = Just king_pays_singer
                         , lest  = Just singer_must_pay
                         } ]

      it "should parse kingly permutations 1" $ do
        mycsv <- BS.readFile "test/if-king-wishes-1.csv"
        parseR (pRule <* eof) "" (exampleStream mycsv) `shouldParse` if_king_wishes

      it "should parse kingly permutations 2" $ do
        mycsv <- BS.readFile "test/if-king-wishes-2.csv"
        parseR (pRule <* eof) "" (exampleStream mycsv) `shouldParse` if_king_wishes

      it "should parse kingly permutations 3" $ do
        mycsv <- BS.readFile "test/if-king-wishes-3.csv"
        parseR (pRule <* eof) "" (exampleStream mycsv) `shouldParse` if_king_wishes

      it "should parse chained-regulatives part 1" $ do
        mycsv <- BS.readFile "test/chained-regulatives-part1.csv"
        parseR (pRule <* eof) "" (exampleStream mycsv) `shouldParse` king_pays_singer

      it "should parse chained-regulatives part 2" $ do
        mycsv <- BS.readFile "test/chained-regulatives-part2.csv"
        parseR (pRule <* eof) "" (exampleStream mycsv) `shouldParse` singer_must_pay

      it "should parse chained-regulatives.csv" $ do
        mycsv <- BS.readFile "test/chained-regulatives.csv"
        parseR (pRule <* eof) "" (exampleStream mycsv) `shouldParse` singer_chain

      it "should parse alternative deadline/action arrangement 1" $ do
        mycsv <- BS.readFile "test/chained-regulatives-part1-alternative-1.csv"
        parseR (pRule <* eof) "" (exampleStream mycsv) `shouldParse` king_pays_singer

      it "should parse alternative deadline/action arrangement 2" $ do
        mycsv <- BS.readFile "test/chained-regulatives-part1-alternative-2.csv"
        parseR (pRule <* eof) "" (exampleStream mycsv) `shouldParse` king_pays_singer

      it "should parse alternative deadline/action arrangement 3" $ do
        mycsv <- BS.readFile "test/chained-regulatives-part1-alternative-3.csv"
        parseR (pRule <* eof) "" (exampleStream mycsv) `shouldParse` king_pays_singer

      it "should parse alternative arrangement 4, no deadline at all" $ do
        mycsv <- BS.readFile "test/chained-regulatives-part1-alternative-4.csv"
        parseR (pRule <* eof) "" (exampleStream mycsv) `shouldParse` king_pays_singer_eventually

      let if_king_wishes_singer = if_king_wishes ++
            [ DefTermAlias "(\"singer\")" "person" Nothing
              (Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing})) ]

      let if_king_wishes_singer_2 = if_king_wishes ++
            [ DefTermAlias "(\"singer\")" "person" Nothing
              (Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 2, version = Nothing})) ]

      it "should parse natural language aliases (\"NL Aliases\") aka inline defined terms" $ do
        mycsv <- BS.readFile "test/nl-aliases.csv"
        parseR (pRule <* eof) "" (exampleStream mycsv) `shouldParse` if_king_wishes_singer

      let singer_must_pay_params = do
            smp <- singer_must_pay
            return $ smp { action = ("pay", [("to",["the King"])
                                            ,("amount",["$20"])]) }

      it "should parse action params" $ do
        mycsv <- BS.readFile "test/action-params-singer.csv"
        parseR (pRule <* eof) "" (exampleStream mycsv) `shouldParse` singer_must_pay_params

      it "should parse despite interrupting newlines" $ do
        mycsv <- BS.readFile "test/blank-lines.csv"
        parseR (pRule <* eof) "" (head . tail $ exampleStreams mycsv) `shouldParse` if_king_wishes_singer_2
      -- XXX: this is awful and needs to be fixed.  wtf, head.tail?

    describe "megaparsing MEANS" $ do

      let bobUncle = defaultCon { term = "Bob's your uncle"
                                 , cond = Just
                                   ( Not
                                     ( Any
                                       ( Pre "any of:" )
                                       [ Leaf "Bob is estranged"
                                       , Leaf "Bob is dead"
                                       ]
                                     )
                                   )
                                 }
      
      it "should start a bool struct" $ do
        let testfile = "test/bob-head-1.csv"
        testcsv <- BS.readFile testfile
        parseR (pRule <* eof) testfile (exampleStream testcsv)
          `shouldParse` [bobUncle]

    describe "megaparsing UNLESS semantics" $ do

      let dayOfSilence = [ defaultReg { cond = Just ( Not ( Leaf "day of silence" ) ) } ] 

      let observanceMandatory = [ defaultReg { cond = Just
                                               ( Not
                                                 ( All
                                                   ( Pre "all of:" ) -- why not "both"? ¯\_(ツ)_/¯
                                                   [ Leaf "day of silence"
                                                   , Leaf "observance is mandatory"
                                                   ]
                                                 )
                                               ) } ]

      let dayOfSong = [ defaultReg { cond = Just ( All ( Pre "all of:" ) [ Not ( Leaf "day of silence" )
                                                                         , Leaf "day of song" ] ) } ]
          
      let silenceKing = [ defaultReg { cond = Just ( All ( Pre "both" ) [
                                                         Leaf "the king wishes"
                                                         , Not ( Leaf "day of silence" )
                                                         ] ) } ]
            
      it "should read EVERY MUST UNLESS" $ do
        let testfile = "test/unless-regulative-1.csv"
        testcsv <- BS.readFile testfile
        parseR (pRule <* eof) testfile (exampleStream testcsv)
          `shouldParse` dayOfSilence
                      
      it "should read EVERY MUST UNLESS IF" $ do
        let testfile = "test/unless-regulative-2.csv"
        testcsv <- BS.readFile testfile
        parseR (pRule <* eof) testfile (exampleStream testcsv)
          `shouldParse` silenceKing
                      
      it "should read EVERY MUST IF UNLESS" $ do
        let testfile = "test/unless-regulative-3.csv"
        testcsv <- BS.readFile testfile
        parseR (pRule <* eof) testfile (exampleStream testcsv)
          `shouldParse` silenceKing
                      
      it "should read EVERY UNLESS MUST IF" $ do
        let testfile = "test/unless-regulative-4.csv"
        testcsv <- BS.readFile testfile
        parseR (pRule <* eof) testfile (exampleStream testcsv)
          `shouldParse` silenceKing
                      
      it "should read EVERY IF MUST UNLESS" $ do
        let testfile = "test/unless-regulative-5.csv"
        testcsv <- BS.readFile testfile
        parseR (pRule <* eof) testfile (exampleStream testcsv)
          `shouldParse` silenceKing
                      
      let silenceMourning = [
            defaultReg { cond = Just ( All ( Pre "both" ) [
                                         Leaf "the king wishes"
                                         , Not
                                           ( Any
                                             ( Pre "any of:" )
                                             [ Leaf "day of silence"
                                             , Leaf "day of mourning"
                                             ]
                                           )
                                         ] ) } ]

      it "should read EVERY MUST IF UNLESS OR" $ do
        let testfile = "test/unless-regulative-6.csv"
        testcsv <- BS.readFile testfile
        parseR (pRule <* eof) testfile (exampleStream testcsv)
          `shouldParse` silenceMourning

      let mourningForbids = [
            defaultReg { cond = Just ( All ( Pre "both" ) [
                                         Leaf "the king wishes"
                                         , Not
                                           ( All
                                             ( Pre "all of:" )
                                             [ Leaf "day of mourning"
                                             , Leaf "mourning forbids singing"
                                             ]
                                           ) ] ) } ]
                                         
      it "should read EVERY MUST IF UNLESS AND" $ do
        let testfile = "test/unless-regulative-7.csv"
        testcsv <- BS.readFile testfile
        parseR (pRule <* eof) testfile (exampleStream testcsv)
          `shouldParse` mourningForbids
                      
      it "should read IF NOT when joined" $ do
        let testfile = "test/ifnot-1-joined.csv"
        testcsv <- BS.readFile testfile
        parseR (pRule <* eof) testfile (exampleStream testcsv)
          `shouldParse` dayOfSilence
                      
      it "should read IF-NOT when separate" $ do
        let testfile = "test/ifnot-2-separate.csv"
        testcsv <- BS.readFile testfile
        parseR (pRule <* eof) testfile (exampleStream testcsv)
          `shouldParse` dayOfSilence

      it "should handle NOT ... AND indented" $ do
        let testfile = "test/ifnot-4-indentation-explicit.csv"
        testcsv <- BS.readFile testfile
        parseR (pRule <* eof) testfile (exampleStream testcsv)
          `shouldParse` observanceMandatory
                      
      it "should handle NOT AND indented the other way" $ do
        let testfile = "test/ifnot-5-indentation-explicit.csv"
        testcsv <- BS.readFile testfile
        parseR (pRule <* eof) testfile (exampleStream testcsv)
          `shouldParse` dayOfSong
                      
      it "should work for constitutive rules" $ do
        let testfile = "test/bob-tail-1.csv"
        testcsv <- BS.readFile testfile
        parseR (pRule <* eof) testfile (exampleStream testcsv)
          `shouldParse` [ Constitutive
                          { term = "Bob's your uncle"
                          , cond = Just
                            ( All
                              ( Pre "both" )
                              [ Any
                                ( Pre "any of:" )
                                [ Leaf "Bob is your mother's brother"
                                , Leaf "Bob is your father's brother"
                                ]
                              , Not
                                ( Leaf "Bob is estranged" )
                              ]
                            )
                          , rlabel = Nothing
                          , lsource = Nothing
                          , srcref = Nothing
                          }
                        ]

  -- upgrade single OR group to bypass the top level AND group

  -- defTermAlias should absorb the WHO limb
