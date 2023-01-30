{-# LANGUAGE OverloadedStrings #-}
module Parsing.PDPASpec where

import Text.Megaparsec
import LS.Lib
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
  testcsv <- BS.readFile ("test/Parsing/pdpa/" <> testfile <> ".csv")
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

    describe "PDPA" $ do

      let expected_pdpadbno1 =
            [ defaultReg
              { subj = Leaf
                (
                  ( "Organisation" :| []
                  , Nothing
                  ) :| []
                )
              , rkeyword = REvery
              , who = Just
                ( Leaf
                  ( RPMT
                    [ "is"
                    , "not"
                    , "a Public Agency"
                    ]
                  )
                )
              , cond = Just
                ( Leaf
                  ( RPMT [ "the data breach occurs on or after the date of commencement of PDP(A)A 2020 §13" ] )
                )
              , deontic = DMust
              , action = Leaf
                (
                  ( "assess" :| [ "if it is a Notifiable Data Breach" ]
                  , Nothing
                  ) :|
                  [
                    ( "by" :|
                      [ "performing"
                      , "NDB Qualification"
                      ]
                    , Nothing
                    )
                  ]
                )
              , temporal = Just ( TemporalConstraint TBefore (Just 30) "days" )
              , hence = Just ( RuleAlias ["Notification"] )
              , lest = Just
                ( defaultReg
                    { subj = Leaf
                        (
                            ( "the PDPC" :| []
                            , Nothing
                            ) :| []
                        )
                    , rkeyword = RParty
                    , deontic = DMay
                    , action = Leaf
                        (
                            ( "demand" :| [ "an explanation for your inaction" ]
                            , Nothing
                            ) :| []
                        )
                    , temporal = Nothing
                    , srcref = Nothing
                    , hence = Just
                        ( defaultReg
                            { subj = Leaf
                                (
                                    ( "You" :| []
                                    , Nothing
                                    ) :| []
                                )
                            , rkeyword = RParty
                            , deontic = DMust
                            , srcref = Nothing
                            , action = Leaf
                                (
                                    ( "respond" :| []
                                    , Nothing
                                    ) :|
                                    [
                                        ( "to" :| [ "the PDPC" ]
                                        , Nothing
                                        )
                                    ,
                                        ( "about" :| [ "your inaction" ]
                                        , Nothing
                                        )
                                    ]
                                )
                            }
                        )
                    }
                )
            , upon = Just
                (
                    ( "becoming aware a data breach may have occurred" :| []
                    , Nothing
                    ) :| []
                )
            , rlabel = Just ("\167",2,"Assess")
            }
            , DefNameAlias
            { name = [ "You" ]
            , detail = [ "Organisation" ]
            , nlhint = Nothing
            , srcref = Just
                ( SrcRef
                    { url = "test/Spec"
                    , short = "test/Spec"
                    , srcrow = 2
                    , srccol = 3
                    , version = Nothing
                    }
                )
            }
            ]

      filetest "pdpadbno-1"   "must assess" (parseR pToplevel) expected_pdpadbno1
      filetest "pdpadbno-1-b" "must assess" (parseR pToplevel) expected_pdpadbno1

      filetest "pdpadbno-2" "data intermediaries"
        (parseR pToplevel) [Regulative {subj = mkLeaf (("Data Intermediary" :| [],Nothing) :| []), rkeyword = REvery, who = Just (mkLeaf (RPMT ["is not","processing personal data on behalf of and for the purposes of a public agency"])), cond = Just (mkLeaf (RPMT ["the data breach occurs on or after the date of commencement of PDP(A)A 2020 \167\&13"])), deontic = DMust, action = mkLeaf (("NOTIFY" :| ["the Organisation"],Nothing) :| [("for which" :| ["you act as a Data Intermediary"],Nothing)]), temporal = Just (TemporalConstraint TVague (Just 0) "without undue delay"), hence = Nothing, lest = Nothing, rlabel = Just ("\167",2,"Data Intermediary non PA"), lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing}), upon = Just (("becoming aware a data breach involving a client Organisation may have occurred" :| [],Nothing) :| []), given = Nothing, having = Nothing, wwhere = [], defaults = [], symtab = []},DefNameAlias {name = ["You"], detail = ["Data Intermediary"], nlhint = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 3, version = Nothing})}]

      filetest "pdpadbno-3" "data intermediaries"
        (parseR pToplevel) [Regulative {subj = mkLeaf (("Data Intermediary" :| [],Nothing) :| []), rkeyword = REvery, who = Just (mkLeaf (RPMT ["processes personal data on behalf of and for the purposes of a public agency"])), cond = Nothing, deontic = DMust, action = mkLeaf (("NOTIFY" :| ["the Public Agency"],Nothing) :| [("for which" :| ["you act as a Data Intermediary"],Nothing)]), temporal = Just (TemporalConstraint TVague (Just 0) "without undue delay"), hence = Nothing, lest = Nothing, rlabel = Just ("\167",2,"Data Intermediary for PA"), lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing}), upon = Just (("becoming aware a data breach involving a client public agency may have occurred" :| [],Nothing) :| []), given = Nothing, having = Nothing, wwhere = [], defaults = [], symtab = []},DefNameAlias {name = ["You"], detail = ["Data Intermediary"], nlhint = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 3, version = Nothing})}]

      filetest "pdpadbno-5" "notification to PDPC"
        (parseR pToplevel) [Regulative {subj = mkLeaf (("You" :| [],Nothing) :| []), rkeyword = RParty, who = Nothing, cond = Just (All Nothing [mkLeaf (RPMT ["it is","an NDB"]),Not (mkLeaf (RPMT ["you are a Public Agency"]))]), deontic = DMust, action = mkLeaf (("NOTIFY" :| ["the PDPC"],Nothing) :| [("in" :| ["the form and manner specified at www.pdpc.gov.sg"],Nothing),("with" :| ["a Notification Message"],Nothing),("and" :| ["a list of individuals for whom notification waiver is sought"],Nothing)]), temporal = Just (TemporalConstraint TBefore (Just 3) "days"), hence = Just (Regulative {subj = mkLeaf (("the PDPC" :| [],Nothing) :| []), rkeyword = RParty, who = Nothing, cond = Nothing, deontic = DMay, action = mkLeaf (("NOTIFY" :| ["you"],Nothing) :| [("with" :| ["a list of individuals to exclude from notification"],Nothing)]), temporal = Nothing, hence = Nothing, lest = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, upon = Nothing, given = Nothing, having = Nothing, wwhere = [], defaults = [], symtab = []}), lest = Nothing, rlabel = Just ("\167",2,"Notify PDPC"), lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing}), upon = Nothing, given = Nothing, having = Nothing, wwhere = [], defaults = [], symtab = []},DefNameAlias {name = ["the PDPC Exclusion List"], detail = ["with","a list of individuals to exclude from notification"], nlhint = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 1, version = Nothing})}]

      filetest "pdpadbno-6" "exemption: unlikely"
        (parseR pToplevel)
        [ defaultHorn
          { name =
            [ "it is"
            , "unlikely that the notifiable data breach will result in significant harm to the affected individual"
            ]
          , keyword = Decide
          , given = Just
            (
              ( "an individual" :| []
              , Nothing
              ) :|
              [
                ( "who" :| [ "is affected by an NDB" ]
                , Nothing
                )
              ]
            )
          , upon = Nothing
          , clauses =
              [ HC
                { hHead = RPMT
                          [ "it is"
                          , "unlikely that the notifiable data breach will result in significant harm to the affected individual"
                          ]
                , hBody = Just
                          ( Any Nothing
                            [ Leaf
                              ( RPMT
                                [ "the organisation has taken any action"
                                , "to"
                                , "render it unlikely that the notifiable data breach will result in significant harm to the individual"
                                ]
                              )
                            , Leaf
                              ( RPMT
                                [ "the organisation already implemented any technological measure"
                                , "to"
                                , "render it unlikely that the notifiable data breach will result in significant harm to the individual"
                                ]
                              )
                            ]
                          )
                }
              ]
          , rlabel =Just ("\167", 2, "Unlikely")
          , lsource = Nothing
          , srcref = Just
            ( SrcRef
              { url = "test/Spec"
              , short = "test/Spec"
              , srcrow = 1
              , srccol = 1
              , version = Nothing
              }
            )
          }
        , DefNameAlias
          { name = [ "Unlikely" ]
          , detail =
            [ "it is"
            , "unlikely that the notifiable data breach will result in significant harm to the affected individual"
            ]
          , nlhint = Nothing
          , srcref = Just
            ( SrcRef
              { url = "test/Spec"
              , short = "test/Spec"
              , srcrow = 2
              , srccol = 5
              , version = Nothing
              }
            )
          }
        ]

      filetest "pdpadbno-7" "notification to users"
        (parseR pToplevel) [Regulative {subj = mkLeaf (("You" :| [],Nothing) :| []), rkeyword = RParty, who = Nothing, cond = Just (All Nothing [mkLeaf (RPMT ["it is","an NDB"]),Not (mkLeaf (RPMT ["you are a Public Agency"]))]), deontic = DMust, action = mkLeaf (("NOTIFY" :| ["each of the Notifiable Individuals"],Nothing) :| [("in" :| ["any manner that is reasonable in the circumstances"],Nothing),("with" :| ["a message obeying a certain format"],Nothing)]), temporal = Just (TemporalConstraint TBefore (Just 3) "days"), hence = Nothing, lest = Nothing, rlabel = Just ("\167",2,"Notify Individuals"), lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing}), upon = Nothing, given = Nothing, having = Nothing, wwhere = [Hornlike {name = ["the Notifiable Individuals"], super = Nothing, keyword = Means, given = Nothing, upon = Nothing, clauses = [HC {hHead = RPMT ["the Notifiable Individuals"], hBody = Nothing}], rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 9, version = Nothing}), defaults = [], symtab = []}], defaults = [], symtab = []},Hornlike {name = ["the Notifiable Individuals"], super = Nothing, keyword = Means, given = Nothing, upon = Nothing, clauses = [HC {hHead = RPBoolStructR ["the Notifiable Individuals"] RPis (All Nothing [mkLeaf (RPMT ["the set of individuals affected by the NDB"]),Not (mkLeaf (RPMT ["the individuals who are deemed","Unlikely"])),Not (mkLeaf (RPMT ["the individuals on","the PDPC Exclusion List"])),Not (mkLeaf (RPMT ["the individuals on","the LEA Exclusion List"]))]), hBody = Nothing}], rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 9, version = Nothing}), defaults = [], symtab = []}]
