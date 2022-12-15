{-# LANGUAGE OverloadedStrings #-}
module ParserSpec where

-- import qualified Test.Hspec.Megaparsec as THM
import Text.Megaparsec
import LS.Lib
import LS.Parser
import LS.Interpreter
import LS.RelationalPredicates
import LS.Tokens
import AnyAll hiding (asJSON)
import LS.BasicTypes
import LS.Types
import LS.Error
import TestNLG
import Test.QuickCheck
import LS.NLP.WordNet

-- import LS.XPile.SVG
import LS.XPile.VueJSON
import LS.XPile.CoreL4
import Test.Hspec
import qualified Data.ByteString.Lazy as BS
import Data.List.NonEmpty (NonEmpty ((:|)))
import Debug.Trace (traceM)
import System.Environment (lookupEnv)
import Data.Maybe (isJust)
import Control.Monad (when, guard)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Test.QuickCheck.Arbitrary.Generic
import LS.NLP.NLG (NLGEnv, myNLGEnv)
import Control.Concurrent.Async (async, wait)
import Test.Hspec.Megaparsec (shouldParse)

-- if you just want to run a test in the repl, this might be enough:
-- λ: runMyParser id defaultRC ((,) <$> pOtherVal <*> (pToken GoDeeper *> pOtherVal <* pToken UnDeeper <* Text.Megaparsec.eof)) "" (exampleStream "foo,bar")
-- Right (("foo","bar"),[])
--
-- λ: runMyParser id defaultRC ((,,,) $>| pOtherVal |>| pOtherVal |>| pOtherVal |>< pOtherVal) "" (exampleStream "foo,foo,foo,bar")
-- Right (("foo","foo","foo","bar"),[])

-- | Create an expectation by saying what the result should be.
--
-- > parse letterChar "" "x" `shouldParse` 'x'

defaultScenario :: Rule
defaultScenario = Scenario
  { scgiven = []
  , expect = []
  , rlabel = Nothing
  , lsource = Nothing
  , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing})
  , defaults = []
  , symtab = []
  }

scenario1 :: Rule
scenario1 = Scenario
    { scgiven =
        [ RPMT
            [ "the data breach is in relation to any prescribed personal data or class of personal data relating to the individual"
            ],
          RPMT ["loss of storage medium on which personal data is stored in circumstances where the unauthorised", "disposal", "of the personal data is likely to occur"],
          RPMT ["the data breach occurred only within an organisation"]
        ],
      expect = [ExpRP (RPMT ["IT IS", "not", "A Notifiable Data Breach"])],
      rlabel = Just ("SCENARIO", 1, "Misplaced storage drive"),
      lsource = Nothing,
      srcref = Nothing,
      defaults = [],
      symtab = []
    }
scenario2a :: Rule
scenario2a = Scenario
    { scgiven =
        [ RPConstraint ["Organisation's name"] RPis ["ABC"],
          RPMT ["the data breach is in relation to any prescribed personal data or class of personal data relating to the individual"],
          RPMT ["unauthorised", "disclosure", "of personal data", "occurred"],
          RPMT ["not", "the data breach occurred only within an organisation"],
          RPMT ["the data breach relates to", "the individual's", "full name"],
          RPMT ["the data breach relates to", "The number of any credit card, charge card or debit card issued to or in the name of the individual."]
        ],
      expect =
        [ ExpRP (RPMT ["IT IS", "A Notifiable Data Breach"]),
          ExpRP (RPMT ["Organisation", "must", "notify PDPC"]),
          ExpRP (RPMT ["Organisation", "must", "notify Affected Individuals"])
        ],
      rlabel = Just ("SCENARIO", 1, "Data breach involving multiple organisations for ABC"),
      lsource = Nothing,
      srcref = Nothing,
      defaults = [],
      symtab = []
    }

scenario2b :: Rule
scenario2b = Scenario
    { scgiven =
        [ RPParamText (("Organisation's name" :| [], Just (InlineEnum TOne (("DEF" :| ["GHI"], Nothing) :| []))) :| []),
          RPMT ["the data breach is in relation to any prescribed personal data or class of personal data relating to the individual"],
          RPMT ["unauthorised", "disclosure", "of personal data", "occurred"],
          RPMT ["not", "the data breach occurred only within an organisation"],
          RPMT ["the data breach relates to", "the individual's", "full name"],
          RPMT ["the data breach relates to", "The number of any credit card, charge card or debit card issued to or in the name of the individual."],
          RPMT ["PDPC instructs you not to notify them"]
        ],
      expect =
        [ ExpRP (RPMT ["IT IS", "A Notifiable Data Breach"]),
          ExpRP (RPMT ["Organisation", "must", "notify PDPC"]),
          ExpRP (RPMT ["not", "Organisation", "must", "notify Affected Individuals"])
        ],
      rlabel = Just ("SCENARIO", 1, "Data breach involving multiple organisations for DEF and GHI"),
      lsource = Nothing,
      srcref = Nothing,
      defaults = [],
      symtab = []
    }

scenario3 :: Rule
scenario3 = Scenario
    { scgiven =
        [ RPConstraint ["the prescribed number of affected individuals"] RPis ["50"],
          RPMT ["unauthorised", "access", "of personal data", "occurred"],
          RPMT ["the data breach relates to", "the individual's", "identification number"],
          RPMT ["the data breach relates to", "The assessment, diagnosis, treatment, prevention or alleviation by a health professional of any of the following affecting the individual:", "any sexually-transmitted disease such as Chlamydial Genital Infection, Gonorrhoea and Syphilis;"]
        ],
      expect =
        [ ExpRP (RPMT ["IT IS", "A Notifiable Data Breach"]),
          ExpRP (RPMT ["Organisation", "must", "notify PDPC"]),
          ExpRP (RPMT ["Organisation", "must", "notify Affected Individuals"])
        ],
      rlabel = Just ("SCENARIO", 1, "Unauthorised access of patients\8217 medical records"),
      lsource = Nothing,
      srcref = Nothing,
      defaults = [],
      symtab = []
    }

scenario4 :: Rule
scenario4 = Scenario
    { scgiven =
        [ RPConstraint ["the prescribed number of affected individuals"] RPis ["1000"],
          RPMT ["unauthorised", "access", "of personal data", "occurred"],
          RPMT ["the data breach relates to", "the individual's", "full name"],
          RPMT ["the data breach relates to", "the individual's", "identification number", "any sexually-transmitted disease such as Chlamydial Genital Infection, Gonorrhoea and Syphilis;"],
          RPMT ["the data breach relates to", "The number of any credit card, charge card or debit card issued to or in the name of the individual."]
        ],
      expect =
        [ ExpRP (RPMT ["IT IS", "A Notifiable Data Breach"]),
          ExpRP (RPMT ["Organisation", "must", "notify PDPC"]),
          ExpRP (RPMT ["Organisation", "must", "notify Affected Individuals"])
        ],
      rlabel = Just ("SCENARIO", 1, "Theft of portable storage drive containing hotel guests\8217 details"),
      lsource = Nothing,
      srcref = Nothing,
      defaults = [],
      symtab = []
    }

filetest,filetest2 :: (HasCallStack, ShowErrorComponent e, Show b, Eq b) => String -> String -> (String -> MyStream -> Either (ParseErrorBundle MyStream e) b) -> b -> SpecWith ()
filetest testfile desc parseFunc expected =
  it (testfile ++ ": " ++ desc ) $ do
  testcsv <- BS.readFile ("test/" <> testfile <> ".csv")
  parseFunc testfile `traverse` exampleStreams testcsv
    `shouldParse` [ expected ]

xfiletest :: (HasCallStack, ShowErrorComponent e, Show b, Eq b) => String -> String -> (String -> MyStream -> Either (ParseErrorBundle MyStream e) b) -> b -> SpecWith ()
xfiletest testfile _desc parseFunc expected =
  xit (testfile {- ++ ": " ++ desc -}) $ do
  testcsv <- BS.readFile ("test/" <> testfile <> ".csv")
  parseFunc testfile `traverse` exampleStreams testcsv
    `shouldParse` [ expected ]

texttest :: (HasCallStack, ShowErrorComponent e, Show b, Eq b) => T.Text -> String -> (String -> MyStream -> Either (ParseErrorBundle MyStream e) b) -> b -> SpecWith ()
texttest testText desc parseFunc expected =
  it desc $ do
  let testcsv = TLE.encodeUtf8 (TL.fromStrict testText)
  parseFunc (show testText) `traverse` exampleStreams testcsv
    `shouldParse` [ expected ]

xtexttest :: (HasCallStack, ShowErrorComponent e, Show b, Eq b) => T.Text -> String -> (String -> MyStream -> Either (ParseErrorBundle MyStream e) b) -> b -> SpecWith ()
xtexttest testText desc parseFunc expected =
  xit desc $ do
    let testcsv = TLE.encodeUtf8 (TL.fromStrict testText)
    parseFunc (show testText) `traverse` exampleStreams testcsv
      `shouldParse` [ expected ]

filetest2 testfile _desc parseFunc _expected =
  it (testfile {- ++ ": " ++ desc -}) $ do
  testcsv <- BS.readFile ("test/" <> testfile <> ".csv")
  let _parsed = parseFunc testfile `traverse` exampleStreams testcsv
  return ()


preprocess :: String -> String
preprocess = filter (not . (`elem` ['!', '.']))

prop_gerundcheck :: T.Text -> Bool
prop_gerundcheck string = let str = T.unpack string in
  gfmkGerund str == mkGerund str

instance Arbitrary T.Text where
  arbitrary = T.pack <$> listOf (elements ['a' .. 'z'])
  shrink = map T.pack . shrink . T.unpack

instance Arbitrary MyToken where
  arbitrary = genericArbitrary
  shrink = genericShrink

notOther :: MyToken -> Bool
notOther (Other _) = False
notOther (RuleMarker _ _) = False
notOther _ = True

prop_rendertoken :: MyToken -> Property
prop_rendertoken mytok =
  mytok `notElem` [Distinct, Checkbox, As, EOL, GoDeeper, UnDeeper, Empty, SOF, EOF, TypeSeparator, Other "", RuleMarker 0 ""] && notOther mytok ==>
  toToken (T.pack $ renderToken mytok) === [mytok]

spec :: Spec
spec = do
  mpd <- runIO $ lookupEnv "MP_DEBUG"
  mpn <- runIO $ lookupEnv "MP_NLG"
  let runConfig_ = defaultRC {
      debug = isJust mpd,
      runNLGtests = isJust mpn || False
    }

  asyncNlgEnv <- runIO $ async $ putStrLn "Loading env" >> myNLGEnv <* putStrLn "Loaded env"
  let nlgEnv = unsafePerformIO $ wait asyncNlgEnv

  do
    describe "mkGerund" $ do
      it "behaves like gfmkGerund" $ do
        property prop_gerundcheck
    describe "renderToken" $ do
      it "is the inverse of toToken" $ do
        property prop_rendertoken
    describe "Nothing Test" $ do
      it "should be nothing" $ do
        (Nothing :: Maybe ()) `shouldBe` (Nothing :: Maybe ())
    describe "Parser tests" $ parserTests nlgEnv runConfig_
    if runNLGtests runConfig_
      then describe "NLG tests" $ nlgTests nlgEnv
      else describe "skipping NLG tests" $ do it "to enable, run with MP_NLG=True or edit Spec.hs's runNLGtests config" $ do True

parserTests :: NLGEnv -> RunConfig -> Spec
parserTests nlgEnv runConfig_ = do
    let runConfig = runConfig_ { sourceURL = "test/Spec" }
        runConfigDebug = runConfig { debug = True }
    let  combine (a,b) = a ++ b
    let  dumpStream s = traceM "* Tokens" >> traceM (pRenderStream s)
    let  parseWith  f x y s = when (debug runConfig_) (dumpStream s) >> f <$> runMyParser combine runConfig x y s
    let _parseWith1 f x y s =                          dumpStream s  >> f <$> runMyParser combine runConfigDebug x y s
    let  parseR       x y s = when (debug runConfig_) (dumpStream s) >> runMyParser combine runConfig x y s
    let _parseR1      x y s =                          dumpStream s  >> runMyParser combine runConfigDebug x y s
    let  parseOther   x y s = when (debug runConfig_) (dumpStream s) >> runMyParser id      runConfig x y s
    let _parseOther1  x y s =                          dumpStream s  >> runMyParser id      runConfigDebug x y s

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

      filetest "primitive-pNumber" "primitive number"
        (parseOther pNumber) (42, [])

      filetest "primitive-number-string-multi" "primitive number"
        (parseOther ( (,) <$> pNumber <*> pOtherVal )) ( (42,"boo"), [])


{- verbose version for reference
      filetest "primitive-number-string-single" "primitive number"
        (parseOther ( (,,,)
                      <$> pNumber
                      <*> pToken GoDeeper
                      <*> pOtherVal
                      <*> pToken UnDeeper  ))
        ( (42,GoDeeper,"boo", UnDeeper ), [])
-}

      filetest "primitive-number-string-single" "primitive number"
        (parseOther ( (,)
                      <$> pNumber
                      <*> someIndentation pOtherVal
                      ))
        ( (42,"boo"), [])

      filetest "primitive-number-string-single-indented-1" "primitive number boo indented"
        (parseOther ( (,)
                      <$> pNumber
                      <*> someIndentation pOtherVal
                      ))
        ( (42,"boo"), [])

      filetest "primitive-number-string-single-indented-1" "primitive number boo indented"
        (parseOther ( (,)
                      $>| pNumber
                      |>< pOtherVal
                    ))
        ( (42,"boo"), [])

      filetest "primitive-number-string-single-indented-2" "primitive number boo indented"
        (parseOther ( (,)
                      $*| ($>>) pNumber
                      |>< pOtherVal
                    ))
        ( (42,"boo"), [])

      filetest "primitive-number-string-single-indented-3" "primitive number boo indented"
        (parseOther ( (,)
                      $*| ($>>) pNumber
                      |>< pOtherVal
                    ))
        ( (42,"boo"), [])

      filetest "primitive-pOtherVal" "primitive number"
        (parseOther pOtherVal) ("this is a string", [])

      filetest "primitive-pOtherVal-indented" "primitive number"
        (parseOther ( id
                      $*| ($>>) pOtherVal
                      |<$ undeepers
                    ))
        ( "this is a string", [])

      filetest "primitive-pOtherVal-indented" "primitive number"
        (parseOther ( id
                      >>| pOtherVal -- consumes GoDeepers, then returns a plain parser upgraded to fancy
                      |<$ undeepers
                    ))
        ( "this is a string", [])

      filetest "primitive-pOtherVal-indented" "primitive number"
        (parseOther ( (>><) pOtherVal )) -- consumes GoDeepers, then runs the plain parser, and runs undeepers
        ( "this is a string", [])

      filetest "compound-pairOfNumbers-1" "primitive number"
        (parseOther ( (,)
                      <$> pNumber
                      <*> someIndentation pNumber
                      ))
        ( (42, 43), [])

      filetest "compound-pairOfNumbers-2" "compound-pair"
        (parseOther ( (,)
                      >>| pNumber
                      |>< pNumber
                    ))
        ( (42,43), [])

      let slPairNum = (,)
                      >>| pNumber
                      |>| pNumber

      filetest "compound-pairOfNumbers-3" "compound-pair"
        (parseOther ( slPairNum |<$ undeepers ))
        ( (42,43), [])

      filetest "compound-pairOfNumbers-4" "compound-pair"
        (parseOther ( (,)
                      $*| slPairNum
                      |>< pOtherVal
                    ))
        ( (  (42,43)
          , "my string"), [])

      filetest "inline-1-a" "line crossing"
        (parseOther ( (,,)
                      >*| slMultiTerm
                      |<| pToken Means
                      |>| pBSR
                      |<$ undeepers
                    ))
        ( ( ["Food"]
          , Means
          , Any (Just $ Pre "yummy nightshades") [ mkLeaf (RPMT ["potato"])
                                                 , mkLeaf (RPMT ["tomato"])]
          ), []
        )

      it "SLParser combinators 1 /+=" $ do
        parseOther ((,,)
                     $*| (someLiftSL (pToken (Other "foo")))
                     |>| pToken (Other "bar")
                     |>< pToken (Other "qux")) ""
         (exampleStream "foo,foo,foo,bar,qux")
          `shouldParse` (([Other "foo",Other "foo",Other "foo"],Other "bar",Other "qux"),[])

      it "SLParser combinators 2 /+=" $ do
        parseOther ((,,)
                     $*| pToken (Other "foo") /+= (liftSL $ pToken (Other "bar"))
                     |>| pToken (Other "bar")
                     |>< pToken (Other "qux")) ""
         (exampleStream "foo,foo,foo,bar,qux")
          `shouldParse` ((([Other "foo",Other "foo",Other "foo"],Other "bar"),Other "bar",Other "qux"),[])

      it "SLParser combinators 3 /+=" $ do
        parseOther ((,,)
                     $*| (pOtherVal) /+= ((,) >>| (pToken (Other "bar")) |>| pOtherVal)
                     |>| pToken (Other "bar")
                     |>< pToken (Other "qux")) ""
         (exampleStream "foo,foo,foo,bar,qux")
          `shouldParse` (((["foo","foo","foo"],(Other "bar", "qux")),Other "bar",Other "qux"),[])

      it "SLParser combinators 4 /+=" $ do
        parseOther ((,,)
                     $*| (pOtherVal) /+= ((,) >>| pOtherVal |>| pOtherVal)
                     |>| pToken (Other "bar")
                     |>< pToken (Other "qux")) ""
         (exampleStream "foo,foo,foo,bar,qux")
          `shouldParse` (((["foo","foo","foo"],("bar", "qux")),Other "bar",Other "qux"),[])

-- [TODO] disturbingly, this fails if we use the "standard" version from Parser.
      let aNLK :: Int -> SLParser ([T.Text],MyToken)
          aNLK maxDepth = mkSL $ do
            (toreturn, n) <- runSL aboveNextLineKeyword2
            debugPrint $ "got back toreturn=" ++ show toreturn ++ " with n=" ++ show n ++ "; maxDepth=" ++ show maxDepth ++ "; guard is n < maxDepth = " ++ show (n < maxDepth)
            guard (n < maxDepth)
            return (toreturn, n)

      it "SLParser combinators 5 aboveNextLineKeyword2" $ do
        parseOther ((,,)
                    $>| pOtherVal
                    |*| aboveNextLineKeyword2
                    |>< pOtherVal
                   ) ""
         (exampleStream "foo,foo,foo,\n,OR,bar")
          `shouldParse` (("foo"                          -- pOtherVal
                         ,(["foo","foo"],LS.Types.Or)    -- aboveNextLineKeyword
                         ,"bar")                         -- pOtherVal
                        ,[])

      it "SLParser combinators 6 greedy star" $ do
        parseOther ((,,,)
                     $*| (debugName "first outer pOtherVal" pOtherVal /*= aNLK 1)
                     |>| debugName "looking for foo3"    pOtherVal
                     |<| debugName "looking for the Or" (pToken LS.Types.Or)
                     |>< debugName "looking for the Bar" pOtherVal
                   ) ""
         (exampleStream "foo1,foo2,foo3,\n,OR,bar")
          `shouldParse` ( ( ( ["foo1","foo2"], ( ["foo3"],LS.Types.Or ) )
                          , "foo3"
                          , LS.Types.Or
                          , "bar" )
                        ,[])

      it "SLParser combinators 7 nongreedy star" $ do
        parseOther ((,,,)
                     $*| (debugName "first outer pOtherVal" pOtherVal /*?= aNLK 1)
                     |>| debugName "looking for foo3"    pOtherVal
                     |<| debugName "looking for the Or" (pToken LS.Types.Or)
                     |>< debugName "looking for the Bar" pOtherVal
                   ) ""
         (exampleStream "foo1,foo2,foo3,\n,OR,bar")
          `shouldParse` ( ( ( ["foo1","foo2"], ( ["foo3"],LS.Types.Or ) )
                          , "foo3"
                          , LS.Types.Or
                          , "bar" )
                        ,[])

      it "SLParser combinators 8 greedy plus" $ do
        parseOther ((,,,)
                     $*| (debugName "first outer pOtherVal" pOtherVal /+= aNLK 1)
                     |>| debugName "looking for foo3"    pOtherVal
                     |<| debugName "looking for the Or" (pToken LS.Types.Or)
                     |>< debugName "looking for the Bar" pOtherVal
                   ) ""
         (exampleStream "foo1,foo2,foo3,\n,OR,bar")
          `shouldParse` ( ( ( ["foo1","foo2"], ( ["foo3"],LS.Types.Or ) )
                          , "foo3"
                          , LS.Types.Or
                          , "bar" )
                        ,[])

      it "SLParser combinators 9 nongreedy plus" $ do
        parseOther ((,,,)
                     $*| (debugName "first outer pOtherVal" pOtherVal /+?= aNLK 1)
                     |>| debugName "looking for foo3"    pOtherVal
                     |<| debugName "looking for the Or" (pToken LS.Types.Or)
                     |>< debugName "looking for the Bar" pOtherVal
                   ) ""
         (exampleStream "foo1,foo2,foo3,\n,OR,bar")
          `shouldParse` ( ( ( ["foo1","foo2"], ( ["foo3"],LS.Types.Or ) )
                          , "foo3"
                          , LS.Types.Or
                          , "bar" )
                        ,[])

      it "SLParser combinators 10 maxdepth 0" $ do
        parseOther ((,,,)
                     $*| (debugName "first outer pOtherVal" pOtherVal /*?= aNLK 1)
                     |>| debugName "looking for foo3"    pOtherVal
                     |<| debugName "looking for the Or" (pToken LS.Types.Or)
                     |>< debugName "looking for the Bar" pOtherVal
                   ) ""
         (exampleStream "foo1,foo2,,,foo3,\n,,,OR,bar")
          `shouldParse` ( ( ( ["foo1","foo2"], ( ["foo3"],LS.Types.Or ) )
                          , "foo3"
                          , LS.Types.Or
                          , "bar" )
                        ,[])

      let inline_1 = ( ( ["Bad"] , Means , inline_pp ), [] )
          inline_2 = ( ( ["Bad"] , Means , inline_p  ), [] )
          inline_3 = ( ( ["Bad"] , Means , inline_   ), [] )
          inline_r = ((["multiwonk"],Means,Any Nothing [mkLeaf (RPMT ["poopoo"]),Any (Just (Pre "the")) [mkLeaf (RPMT ["honk"]),mkLeaf (RPMT ["ponk"])]]),[])
          inline_4 = ( ( ["a data breach occurred"] , Means , inline_4xs), [] )
          inline_pp = Any (Just $ PrePost "any unauthorised" "of personal data" ) inline_xs
          inline_p  = Any (Just $ Pre     "any unauthorised"                    ) inline_xs
          inline_   = Any Nothing                                                 inline_xs
          inline_xs = mkLeaf . RPMT . pure <$> T.words "access use disclosure copying modification disposal"
          inline4_pp= Any (Just $ PrePost
                           "loss of storage medium on which personal data is stored in circumstances where the unauthorised"
                           "of the personal data is likely to occur") inline_xs
          inline_4xs= Any Nothing [inline_pp, inline4_pp]

          pInline1 = parseOther $ do
            (,,)
              >*| debugName "subject slMultiTerm" slMultiTerm  -- "Bad"
              |<| pToken Means
              |-| debugName "made it to pBSR" pBSR
              |<$ undeepers

      filetest "inline-1-c" "line crossing" pInline1 inline_1
      filetest "inline-1-d" "line crossing" pInline1 inline_1
      filetest "inline-1-e" "line crossing" pInline1 inline_1
      filetest "inline-1-f" "line crossing" pInline1 inline_1
      filetest "inline-1-g" "line crossing" pInline1 inline_1
      filetest "inline-1-h" "line crossing" pInline1 inline_1
      filetest "inline-1-i" "line crossing" pInline1 inline_1
      filetest "inline-1-j" "line crossing" pInline1 inline_1
      filetest "inline-1-k" "line crossing" pInline1 inline_1
      filetest "inline-1-l" "line crossing" pInline1 (
        const [DefTypically { name = ["disposal"]
                            , defaults = [RPConstraint ["disposal"] RPis ["True"]]
                            , srcref = Just (SrcRef { url = "test/Spec", short = "test/Spec"
                                                    , srcrow = 3, srccol = 8, version = Nothing})}]
          <$> inline_1)
      filetest "inline-1-m" "line crossing" pInline1 inline_1
      filetest "inline-1-n" "line crossing" pInline1 inline_2
      filetest "inline-1-o" "line crossing" pInline1 inline_3
      filetest "inline-1-p" "line crossing" pInline1 ((["wonk"],Means,Any (Just (Pre "a")) [mkLeaf (RPMT ["honk"]),mkLeaf (RPMT ["ponk"])]),[])
      filetest "inline-1-q" "line crossing" pInline1 ((["poowonk"],Means,Any Nothing [mkLeaf (RPMT ["poopoo"]),mkLeaf (RPMT ["just a","honk"])]),[])
      filetest "inline-1-r" "line crossing" pInline1 inline_r
      filetest "inline-1-s" "line crossing" pInline1 inline_4

      filetest "multiterm-with-blanks-1" "p, no blanks"              (parseOther pMultiTerm) (["foo","bar","baz"],[])
      filetest "multiterm-with-blanks-2" "p, with blanks"            (parseOther pMultiTerm) (["foo","bar","baz"],[])

      filetest "multiterm-with-blanks-1" "sl, no blanks"             (parseOther (slMultiTerm |<$ undeepers)) (["foo","bar","baz"],[])
      filetest "multiterm-with-blanks-2" "sl, with blanks"           (parseOther (slMultiTerm |<$ undeepers)) (["foo","bar","baz"],[])

      filetest "boolstructp-1" "basic boolstruct of text"
        (parseOther pBoolStruct )
        ( All Nothing [Any Nothing [mkLeaf "thing1"
                                   ,mkLeaf "thing2"]
                      ,mkLeaf "thing3"], [] )


      filetest "boolstructp-2" "basic boolstruct of text"
        (parseOther pBoolStruct )
        ( Any Nothing [mkLeaf "thing1"
                      ,All Nothing [mkLeaf "thing2"
                                   ,mkLeaf "thing3"]], [] )

      filetest "boolstructp-2" "basic boolstruct of paramtext"
        (parseOther pBoolStructPT )
        ( Any Nothing [mkLeaf (("thing1" :| [],Nothing) :| [])
                      ,All Nothing [mkLeaf (("thing2" :| [],Nothing) :| [])
                                   ,mkLeaf (("thing3" :| [],Nothing) :| [])]]  , [] )


      filetest "boolstructp-3" "boolstruct including typically"
        (parseR pRules )
        [ defaultReg
          { subj = mkLeaf (("person" :| [],Nothing) :| [])
          , rkeyword = REvery
          , who = Just (Any Nothing [mkLeaf (RPMT ["is","immortal"])
                                    ,mkLeaf (RPMT ["has","health insurance"])])
          , deontic = DMay
          , action = mkLeaf (("sharpen knives" :| [],Nothing) :| [])
          , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing})
          }
        , DefTypically
          { name = ["is","immortal"]
          , defaults = [RPConstraint ["is","immortal"] RPis ["false"]]
          , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 3, srccol = 3, version = Nothing})}
        ]

      when (runNLGtests runConfig_) $ do
        let grNormal = groundrules runConfig_
            grExtend = groundrules runConfig_ { extendedGrounds = True }
            asCList  = unsafePerformIO .
                         checklist nlgEnv runConfig_ { extendedGrounds = True }

        filetest "boolstructp-3" "groundrules, non-extended"
          (parseWith grNormal pRules) [["person","has","health insurance"]]

        filetest "boolstructp-3" "groundrules, extended"
          (parseWith grExtend pRules) [ ["person","is","immortal"]
                                      , ["person","has","health insurance"]]

        filetest "boolstructp-3" "as checklist, extended"
          (parseWith asCList pRules) [ ["Does the person have health insurance?"]
                                     , ["Is the person immortal?"]]

      filetest "prulelabel-1" "standalone rule label"
        (parseOther pRuleLabel )
        ( ( "§", 1, "My First Rule" )
        , []
        )

      filetest "prulelabel-2" "rule label with just a simple token after"
        (parseOther (pRuleLabel <* pToken Given ))
        ( ( "§", 1, "My First Rule" )
        , []
        )

      filetest "scenario-units-1" "unit test 1 for scenarios"
        (parseOther pScenarioRule )
        ( scenario1
        , []
        )

      texttest "EXPECT,IT IS,A Notifiable Data Breach," "unit test 1 for scenarios"
        (parseOther pExpect )
        ( ExpRP (RPMT ["IT IS","A Notifiable Data Breach"])
        , []
        )

      xtexttest "EXPECT,NOT,IT IS,A Notifiable Data Breach," "unit test EXPECT ... NOT"
        (parseOther pExpect)
        ( ExpRP (RPBoolStructR
                 [] RPis -- [TODO] this sucks, refactor it away
                 (Not (mkLeaf (RPMT ["IT IS","A Notifiable Data Breach"]))))
        , []
        )

      xtexttest "GIVEN,not,IT IS,A Notifiable Data Breach," "unit test GIVEN ... NOT"
        (parseOther pGivens )
        (  [RPBoolStructR
                 [] RPis -- [TODO] this sucks, refactor it away
                 (Not (mkLeaf (RPMT ["IT IS","A Notifiable Data Breach"])))]
        , []
        )

      filetest "scenario-units-2-a" "unit test 2a for scenarios"
        (parseOther pScenarioRule )
        ( scenario2a
        , []
        )

  -- [TODO] we need a better notion of how to handle a nested regulative rule under a scenario EXPECT
      xtexttest "EXPECT,,Organisation,,MUST,,notify PDPC,,,," "unit test EXPECT ... MUST"
        (parseOther pExpect )
        ( ExpRP (RPMT ["Organisation","MUST","notify PDPC"])
        , []
        )

      filetest "scenario-units-2-b" "unit test 2b for scenarios"
        (parseOther pScenarioRule )
        ( scenario2b
        , []
        )

      filetest "scenario-units-3" "unit test 3 for scenarios"
        (parseOther pScenarioRule )
        ( scenario3
        , []
        )

      filetest "scenario-units-4" "unit test 4 for scenarios"
        (parseOther pScenarioRule )
        ( scenario4
        , []
        )

    -- [TODO] it'd be nice to get this working as a filetest rather than the manual way
    describe "transpiler to CoreL4" $ do
      xit "should output a class declaration for seca.csv" $ do
        let testfile = "seca"
        testcsv <- BS.readFile ("test/" <> testfile <> ".csv")
        let rules  = parseR pRules "" `traverse` (exampleStreams testcsv)
        (fmap sfl4ToCorel4 <$> rules) `shouldParse` ["\n#\n# outputted directly from XPile/CoreL4.hs\n#\n\n\n\n-- [SecA_RecoverPassengersVehicleAuthorizedOp]\ndecl s: Situation\n\n--facts\n\nfact <SecA_RecoverPassengersVehicleAuthorizedOp> fromList [([\"s\"],((Just (SimpleType TOne \"Situation\"),[]),[]))]\n\n\n# directToCore\n\n\nrule <SecA_RecoverPassengersVehicleAuthorizedOp>\nfor s: Situation\nif (secA_Applicability && currentSit_s && s == missingKeys)\nthen coverProvided s recoverPassengersVehicleAuthorizedOp SecA_RecoverPassengersVehicleAuthorizedOp\n\n\n"]

      filetest "class-1" "type definitions"
        (parseR pRules)
        [TypeDecl {name = ["Class1"], super = Just (SimpleType TOne "Object"), has = [TypeDecl {name = ["id"], super = Just (SimpleType TOne "Integer"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []}], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing}), defaults = [], symtab = []},TypeDecl {name = ["Class2"], super = Just (SimpleType TOne "Class1"), has = [TypeDecl {name = ["firstname"], super = Just (SimpleType TOne "String"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},TypeDecl {name = ["lastname"], super = Just (SimpleType TOne "String"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},TypeDecl {name = ["office address"], super = Nothing, has = [TypeDecl {name = ["line1"], super = Just (SimpleType TOne "String"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},TypeDecl {name = ["line2"], super = Just (SimpleType TOne "String"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []}], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},TypeDecl {name = ["bar address"], super = Just (SimpleType TOne "address"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},TypeDecl {name = ["work address"], super = Just (SimpleType TOne "address"), has = [TypeDecl {name = ["floor"], super = Just (SimpleType TOne "String"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},TypeDecl {name = ["company name"], super = Just (SimpleType TOne "String"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []}], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []}], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 4, version = Nothing}), defaults = [], symtab = []},TypeDecl {name = ["address"], super = Nothing, has = [TypeDecl {name = ["line1"], super = Just (SimpleType TOne "String"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},TypeDecl {name = ["line2"], super = Just (SimpleType TOne "String"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []}], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 15, version = Nothing}), defaults = [], symtab = []}]

      filetest "class-1" "parent-class identification"
        (parseOther (do
                        rules <- some pTypeDeclaration
                        let classH = classHierarchy rules
                            parent = clsParent classH "Class2"
                        return $ parent))
        (Just "Class1",[])

      filetest "class-1" "attribute enumeration"
        (parseOther (do
                        rules <- some pTypeDeclaration
                        let classH = classHierarchy rules
                            tA = getCTkeys <$> thisAttributes classH "Class2"
                        return $ tA))
        (Just ["bar address","firstname","lastname","office address","work address"], [])

      filetest "class-1" "extended attribute enumeration"
        (parseOther (do
                        rules <- some pTypeDeclaration
                        let classH = classHierarchy rules
                            eA = getCTkeys <$> extendedAttributes classH "Class2"
                        return $ eA))
        (Just ["bar address","firstname","id","lastname","office address","work address"], [])

      filetest "class-fa-1" "financial advisor data modelling"
        (parseR pToplevel) [
          TypeDecl { name = ["FinancialStatus"], super = Just (InlineEnum TOne (("adequate" :| ["inadequate"],Nothing) :| [])), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 1, version = Nothing}), defaults = [], symtab = []},
          TypeDecl {name = ["EarningsStatus"], super = Just (InlineEnum TOne (("steady" :| ["unsteady"],Nothing) :| [])), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 2, version = Nothing}), defaults = [], symtab = []},
          TypeDecl {name = ["InvestmentStrategy"], super = Just (InlineEnum TOne (("savings" :| ["stocks","combination"],Nothing) :| [])), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 3, version = Nothing}), defaults = [], symtab = []},
          TypeDecl {name = ["Person"], super = Nothing, has = [
            TypeDecl {name = ["dependents"], super = Just (SimpleType TOne "Number"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},
            TypeDecl {name = ["amountSaved"], super = Just (SimpleType TOne "Number"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},
            TypeDecl {name = ["earnings"], super = Just (SimpleType TOne "Number"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},
            TypeDecl {name = ["steadiness"], super = Just (SimpleType TOne "EarningsStatus"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},
            TypeDecl {name = ["income"], super = Just (SimpleType TOne "FinancialStatus"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},
            TypeDecl {name = ["savingsAccount"], super = Just (SimpleType TOne "FinancialStatus"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},
            TypeDecl {name = ["isDead"], super = Just (SimpleType TOne "Boolean"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},
            TypeDecl {name = ["spendthrift"], super = Just (SimpleType TOne "Boolean"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []},
            TypeDecl {name = ["investment"], super = Just (SimpleType TOne "InvestmentStrategy"), has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []}],
            enums = Nothing, given = Nothing, upon = Nothing, rlabel = Just ("\167",2,"person type"), lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 5, version = Nothing}), defaults = [], symtab = []}
          ]

      filetest "class-fa-2" "financial advisor decision modelling"
        (parseR pToplevel)
        [ Hornlike {
          name = ["p", "investment"],
          super = Nothing,
          keyword = Decide,
          given = Just (("p" :| [], Just (SimpleType TOne "Person")) :| []),
          upon = Nothing,
          clauses =
            [ HC {hHead = RPConstraint ["p", "investment"] RPis ["savings"], hBody = Just (mkLeaf (RPConstraint ["p", "savingsAccount"] RPis ["inadequate"]))},
              HC {hHead = RPConstraint ["p", "investment"] RPis ["stocks"], hBody = Just (All Nothing [mkLeaf (RPConstraint ["p", "savingsAccount"] RPis ["adequate"]), mkLeaf (RPConstraint ["p", "income"] RPis ["adequate"])])},
              HC {hHead = RPConstraint ["p", "investment"] RPis ["combination"], hBody = Just (mkLeaf (RPMT ["OTHERWISE"]))},
              HC {hHead = RPConstraint ["p", "minSavings"] RPis ["p's dependents", "*", "5000"], hBody = Nothing},
              HC {hHead = RPConstraint ["p", "savingsAccount"] RPis ["adequate"], hBody = Just (mkLeaf (RPConstraint ["p", "amountSaved"] RPgt ["p", "minSavings"]))},
              HC {hHead = RPConstraint ["p", "savingsAccount"] RPis ["inadequate"], hBody = Just (mkLeaf (RPMT ["OTHERWISE"]))},
              HC {hHead = RPConstraint ["p", "minIncome"] RPis ["15000 + 4000 * p's dependents"], hBody = Nothing},
              HC {hHead = RPConstraint ["p", "income"] RPis ["adequate"], hBody = Just (All Nothing [mkLeaf (RPConstraint ["p", "earnings"] RPgt ["p", "minIncome"]), mkLeaf (RPConstraint ["p", "steadiness"] RPis ["steady"])])},
              HC {hHead = RPConstraint ["p", "blah"] RPis ["42"], hBody = Nothing}
            ],
          rlabel = Nothing,
          lsource = Nothing,
          srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing}),
          defaults = [],
          symtab = []
        }
        ]


-- sl style
    describe "sameOrNextLine" $ do
      let potatoParser = parseOther (sameOrNextLine
                                      (flip const $>| (pToken Declare) |*| (someSL (liftSL pOtherVal)))
                                      (flip const $>| (pToken Has    ) |*| (someSL (liftSL pOtherVal))))
          potatoExpect = ( ( [ "Potato" ]
                           , [ "genus", "species" ] ), [] )

      filetest "sameornext-1-same"  "a b on same line"  potatoParser potatoExpect
      filetest "sameornext-2-next"  "a b on next line"  potatoParser potatoExpect
      filetest "sameornext-3-dnl"   "a b on next left"  potatoParser potatoExpect
      filetest "sameornext-4-right" "a b on next right" potatoParser potatoExpect

    describe "nestedHorn" $ do
      filetest "declare-nestedhorn-1" "nestedHorn inside a HAS"
        (parseR pToplevel) [TypeDecl {name = ["Potato"], super = Nothing, has = [TypeDecl {name = ["genus","species"], super = Nothing, has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []}], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing}), defaults = [], symtab = []},Hornlike {name = ["genus","species"], super = Nothing, keyword = Means, given = Nothing, upon = Nothing, clauses = [HC {hHead = RPBoolStructR ["genus","species"] RPis (mkLeaf (RPMT ["some Linnaen thing"])), hBody = Nothing}], rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 3, srccol = 2, version = Nothing}), defaults = [], symtab = []}]

    describe "ampersand" $ do
      let nextLineP = myindented $ sameOrNextLine (someLiftSL pOtherVal) (someLiftSL pOtherVal)
      xfiletest "ampersand-1" "should fail to parse" -- [TODO] how do we run a shouldFailOn? we are expecting this to fail.
        (parseOther nextLineP) ((["Potato"],["genus", "species"]), [])
      filetest "ampersand-2" "this bed is just right"
        (parseOther nextLineP) ((["Potato"],["genus", "species"]), [])
      filetest "ampersand-3" "to the right shouldbe OK"
        (parseOther nextLineP) ((["Potato"],["genus", "species"]), [])
      xfiletest "ampersand-4" "to the right with extra should leave uncaptured uncaptured"
        (parseOther nextLineP) ((["Potato"],["genus", "species"]), [])
      filetest "ampersand-4" "to the right with extra"
        (parseOther nextLineP) ((["Potato", "uncaptured"],["genus", "species"]), [])

    describe "variable substitution and rule expansion" $ do
      let parseSM s m = do
            rs <- parseR pToplevel s m
            return $ getAndOrTree (l4interpret defaultInterpreterOptions rs) 1 (head rs)
          ab1b2 = Just
            ( Any Nothing
              [ mkLeaf "a"
              , All Nothing
                [ mkLeaf "b1"
                , mkLeaf "b2"
                ]
              ]
            )

      filetest "varsub-1-headhead" "should expand hornlike" parseSM ab1b2
      filetest "varsub-2-headbody" "should expand hornlike" parseSM ab1b2
      filetest "varsub-3-bodybody" "should expand hornlike" parseSM ab1b2
      filetest "varsub-4-bodyhead" "should expand hornlike" parseSM ab1b2

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
