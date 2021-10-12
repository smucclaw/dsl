{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Lib
import AnyAll
import Types
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = hspec $ do
  describe "Nothing Test" $ do
    it "should be nothing" $ do
      (Nothing :: Maybe ()) `shouldBe` (Nothing :: Maybe ())
  describe "megaparsing" $ do

    it "should parse an unconditional" $ do
      parse (pRule <* eof) "" (exampleStream ",,,,\n,EVERY,person,,\n,MUST,,,\n,->,sing,,\n")
        `shouldParse`
        [ Regulative {
            every = "person"
            , who = Any ( Pre "Always" ) [ ]
            , deontic = DMust
            , action = "sing"
            , temporal = Nothing
            } ]
  
    it "should parse a single OtherVal" $ do
      parse (pRule <* eof) "" (exampleStream ",,,,\n,EVERY,person,,\n,WHO,walks,,\n,MUST,,,\n,->,sing,,\n")
        `shouldParse`
        [ Regulative {
            every = "person"
            , who = Leaf "walks"
            , deontic = DMust
            , action = "sing"
            , temporal = Nothing
            } ]
  
    it "should parse dummySing" $ do
      parse (pRule <* eof) "" (exampleStream ",,,,\n,EVERY,person,,\n,WHO,walks,// comment,continued comment should be ignored\n,AND,runs,,\n,AND,eats,,\n,OR,drinks,,\n,MUST,,,\n,->,sing,,\n")
        `shouldParse`
        [ Regulative {
            every = "person"
            , who = All
              ( Pre "Who" )
              [ Leaf "walks"
              , Leaf "runs"
              , Any
                ( Pre "any of:" )
                [ Leaf "eats"
                , Leaf "drinks"
                ]
              ]
            , deontic = DMust
            , action = "sing"
            , temporal = Nothing
            } ]

    let imbibeRule = [ Regulative {
                         every = "person"
                         , who = Any
                           ( Pre "Who" )
                           [ Leaf "walks"
                           , Leaf "runs"
                           , Leaf "eats"
                           , All ( Pre "all of:" )
                             [ Leaf "drinks"
                             , Leaf "swallows" ]
                           ]
                         , deontic = DMust
                         , action = "sing"
                         , temporal = Nothing
                         } ]
    
    it "should parse indentedDummySing" $ do
      parse (pRule <* eof) "" (exampleStream ",,,,\n,EVERY,person,,\n,WHO,walks,// comment,continued comment should be ignored\n,OR,runs,,\n,OR,eats,,\n,OR,,drinks,\n,,AND,swallows,\n,MUST,,,\n,->,sing,,\n")
        `shouldParse` imbibeRule
  
    it "should parse indented-1.csv (inline boolean expression)" $ do
      mycsv <- BS.readFile "test/indented-1.csv"
      parse (pRule <* eof) "" (exampleStream mycsv) `shouldParse` imbibeRule

 
    it "should parse indented-1-checkboxes.csv (with checkboxes)" $ do
      mycsv <- BS.readFile "test/indented-1-checkboxes.csv"
      parse (pRule <* eof) "" (exampleStream mycsv) `shouldParse` imbibeRule

    let degustates = Constitutive
                     { term = "degustates"
                     , cond = Any ( Pre "any of:" ) [ Leaf "eats", Leaf "drinks" ]
                     }
  
    it "should parse a simple constitutive rule" $ do
      mycsv <- BS.readFile "test/simple-constitutive-1.csv"
      parse (pRule <* eof) "" (exampleStream mycsv) `shouldParse` [degustates]
  
    it "should parse a simple constitutive rule with checkboxes" $ do
      mycsv <- BS.readFile "test/simple-constitutive-1-checkboxes.csv"
      parse (pRule <* eof) "" (exampleStream mycsv) `shouldParse` [degustates]
  
    let imbibeRule2 = [ Regulative
                        { every = "person"
                        , who = All
                                ( Pre "Who" )
                                [ Leaf "walks"
                                , Leaf "degustates"
                                ]
                        , deontic = DMust
                        , action = "sing"
                        , temporal = Nothing
                        }
                      , Constitutive
                        { term = "degustates"
                        , cond = Any ( Pre "any of:" ) [ Leaf "eats", Leaf "drinks" ]
                        }
                      ]

    it "should parse indented-2.csv (inline constitutive rule)" $ do
      mycsv <- BS.readFile "test/indented-2.csv"
      parse (pRule <* eof) "" (exampleStream mycsv) `shouldParse` imbibeRule2
  
    it "should render a box" $ do
      asBoxes <$> asCSV indentedDummySing
        `shouldBe` Right "abc\n"

-- upgrade single OR group to bypass the top level AND group

