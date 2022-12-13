{-# LANGUAGE OverloadedStrings #-}
module MegaparsingSpec where

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
-- import LS.XPile.Typescript

import Test.Hspec
import qualified Data.ByteString.Lazy as BS
import Data.List.NonEmpty (NonEmpty ((:|)))
import Debug.Trace (traceM)
import System.Environment (lookupEnv)
import Data.Maybe (isJust)
-- import qualified Data.Map as Map
import Control.Monad (when, guard)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Test.QuickCheck.Arbitrary.Generic
import LS.NLP.NLG (NLGEnv, myNLGEnv)
import Control.Concurrent.Async (async, wait)
-- import qualified Data.Text.Encoding as TE
-- import LS.BasicTypes (MyToken)

-- if you just want to run a test in the repl, this might be enough:
-- λ: runMyParser id defaultRC ((,) <$> pOtherVal <*> (pToken GoDeeper *> pOtherVal <* pToken UnDeeper <* Text.Megaparsec.eof)) "" (exampleStream "foo,bar")
-- Right (("foo","bar"),[])
--
-- λ: runMyParser id defaultRC ((,,,) $>| pOtherVal |>| pOtherVal |>| pOtherVal |>< pOtherVal) "" (exampleStream "foo,foo,foo,bar")
-- Right (("foo","foo","foo","bar"),[])

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

spec :: Spec
spec = do
  mpd <- runIO $ lookupEnv "MP_DEBUG"
  mpn <- runIO $ lookupEnv "MP_NLG"
  let runConfig_ = RC
        { debug = isJust mpd
        , callDepth = 0
        , oldDepth = 0
        , parseCallStack = []
        , sourceURL = "STDIN"
        , asJSON = False
        , toNLG = False
        , toBabyL4 = False
        , toProlog = False
        , toUppaal = False
        , saveAKA = False
        , wantNotRules = False
        , toGrounds = False
        , toVue = False
        , toTS = False
        , extendedGrounds = False
        , toChecklist = False
        , printstream = False
        , runNLGtests = isJust mpn || False
        }
  -- verboseCheck prop_gerundcheck
  -- quickCheck prop_rendertoken
  -- nlgEnv <- putStrLn "Loading env" >> myNLGEnv <* putStrLn "Loaded env"
  asyncNlgEnv <- runIO $ async $ putStrLn "Loading env" >> myNLGEnv <* putStrLn "Loaded env"
  let nlgEnv = unsafePerformIO $ wait asyncNlgEnv

  runIO $ hspec $ do
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

    describe "megaparsing" $ do

      it "should parse an unconditional" $ do
        parseR pRules "" (exampleStream ",EVERY,person,,\n,MUST,,,\n,->,sing,,\n")
          `shouldParse` [ srcrow2 $ defaultReg { subj = mkLeafPT "person"
                                               , deontic = DMust
                                               } ]

      it "should parse a rule label" $ do
        parseR pRules "" (exampleStream ",\xc2\xa7,Hello\n")
          `shouldParse` [srcrow2 $ RuleGroup {rlabel = Just ("\167",1,"Hello"), srcref = srcref defaultReg}]

      it "should parse a rule label followed by something" $ do
        parseR pRules "" (exampleStream "\xc2\xa7,Hello\n,something\nMEANS,something\n")
          `shouldParse` [Hornlike {name = ["something"], super = Nothing,  keyword = Means, given = Nothing, upon = Nothing, clauses = [HC {hHead = RPBoolStructR ["something"] RPis (mkLeaf (RPMT ["something"])), hBody = Nothing}], rlabel = Just ("\167",1,"Hello"), lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing}), defaults = [], symtab = []}]

      it "should parse a single OtherVal" $ do
        parseR pRules "" (exampleStream ",,,,\n,EVERY,person,,\n,WHO,walks,,\n,MUST,,,\n,->,sing,,\n")
          `shouldParse` [ srccol1 . srcrow2 $ defaultReg { who = Just (mkLeafR "walks") } ]

      it "should parse the null temporal EVENTUALLY" $ do
        parseR pRules "" (exampleStream ",,,,\n,EVERY,person,,\n,WHO,walks,,\n,MUST,EVENTUALLY,,\n,->,sing,,\n")
          `shouldParse` [ srccol1 . srcrow2 $ defaultReg { who = Just (mkLeafR "walks") } ]

      it "should parse dummySing" $ do
        parseR pRules "" (exampleStream ",,,,\n,EVERY,person,,\n,WHO,walks,// comment,continued comment should be ignored\n,AND,runs,,\n,AND,eats,,\n,OR,drinks,,\n,MUST,,,\n,->,sing,,\n")
          `shouldParse` [ srccol1 <$> srcrow2 $ defaultReg {
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
          `shouldParse` (srccol1 <$> srcrow2 <$> imbibeRule)

      filetest "indented-1" "parse indented-1.csv (inline boolean expression)"
        (parseR pRules) (srcrow2 <$> imbibeRule)

      filetest "indented-1-checkboxes" "should parse indented-1-checkboxes.csv (with checkboxes)"
        (parseR pRules) (srcrow2 <$> imbibeRule)

      let degustates = defaultHorn
            { name = ["degustates"]
            , keyword = Means
            , given = Nothing
            , upon = Nothing
            , clauses = [ HC
                          { hHead = RPBoolStructR ["degustates"]
                                    RPis (Any Nothing [mkLeaf (RPMT ["eats"])
                                                      ,mkLeaf (RPMT ["drinks"])])
                          , hBody = Nothing } ]
            }

      filetest "simple-constitutive-1" "should parse a simple constitutive rule"
        (parseR pRules) [srcrow2 degustates]

      filetest "simple-constitutive-1-checkboxes" "should parse a simple constitutive rule with checkboxes"
        (parseR pRules) [degustates { srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 4, srccol = 1, version = Nothing}) }]

      -- inline constitutive rules are temporarily disabled; we need to think about how to intermingle a "sameline" parser with a multiline object.
      -- we also need to think about getting the sameline parser to not consume all the godeepers at once, because an inline constitutive rule actually starts with a godeeper.

      filetest "indented-2" "inline constitutive rule" (parseR pRules) [Regulative {subj = mkLeaf (("person" :| [],Nothing) :| []), rkeyword = REvery, who = Just (All Nothing [mkLeaf (RPMT ["walks"]),mkLeaf (RPMT ["degustates"])]), cond = Nothing, deontic = DMust, action = mkLeaf (("sing" :| [],Nothing) :| []), temporal = Nothing, hence = Nothing, lest = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing}), upon = Nothing, given = Nothing, having = Nothing, wwhere = [], defaults = [], symtab = []},Hornlike {name = ["degustates"], super = Nothing, keyword = Means, given = Nothing, upon = Nothing, clauses = [HC {hHead = RPBoolStructR ["degustates"] RPis (Any Nothing [mkLeaf (RPMT ["eats"]),mkLeaf (RPMT ["imbibes"])]), hBody = Nothing}], rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 3, version = Nothing}), defaults = [], symtab = []}]

      filetest "indented-3" "defined names in natural positions" (parseR pRules) [Regulative {subj = mkLeaf (("person" :| [],Nothing) :| []), rkeyword = REvery, who = Just (All Nothing [mkLeaf (RPMT ["walks"]),mkLeaf (RPMT ["degustates"])]), cond = Nothing, deontic = DMust, action = mkLeaf (("sing" :| [],Nothing) :| []), temporal = Nothing, hence = Nothing, lest = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing}), upon = Nothing, given = Nothing, having = Nothing, wwhere = [], defaults = [], symtab = []},Hornlike {name = ["imbibes"], super = Nothing, keyword = Means, given = Nothing, upon = Nothing, clauses = [HC {hHead = RPBoolStructR ["imbibes"] RPis (All Nothing [mkLeaf (RPMT ["drinks"]),Any Nothing [mkLeaf (RPMT ["swallows"]),mkLeaf (RPMT ["spits"])]]), hBody = Nothing}], rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 3, srccol = 5, version = Nothing}), defaults = [], symtab = []},Hornlike {name = ["degustates"], super = Nothing, keyword = Means, given = Nothing, upon = Nothing, clauses = [HC {hHead = RPBoolStructR ["degustates"] RPis (Any Nothing [mkLeaf (RPMT ["eats"]),mkLeaf (RPMT ["imbibes"])]), hBody = Nothing}], rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 3, version = Nothing}), defaults = [], symtab = []}]

      let mustsing1 = [ defaultReg {
                          rlabel = Just ("\167",1,"Matt Wadd's Rule")
                          , subj = Leaf
                            (
                              ( "Person" :| []
                              , Nothing
                              ) :| []
                            )
                          , rkeyword = REvery
                          , who = Just
                            ( All Nothing
                              [ Leaf
                                ( RPMT ["walks"] )
                              , Any Nothing
                                [ Leaf
                                  ( RPMT ["eats"])
                                , Leaf
                                  ( RPMT ["drinks"] )
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
                          { subj = mkLeafPT "King"
                          , rkeyword = RParty
                          , deontic = DMay
                          , action = mkLeafPT "pay"
                          , temporal = Just (TemporalConstraint TAfter (Just 20) "min")
                          }


      let king_pays_singer_eventually =
            king_pays_singer { temporal = Nothing }

      let singer_must_pay = defaultReg
                              { rkeyword = RParty
                              , subj = mkLeafPT "Singer"
                              , action = mkLeafPT "pay"
                              , temporal = Just (TemporalConstraint TBefore (Just 1) "supper")
                              }


      let singer_chain = [ defaultReg
                         { subj = mkLeafPT "person"
                         , who = Just $ All Nothing
                                 [ mkLeafR "walks"
                                 , mkLeafR "eats"
                                 ]
                         , cond = Just $ mkLeafR "the King wishes"
                         , hence = Just king_pays_singer
                         , lest  = Just singer_must_pay
                         , srcref = Nothing
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
        (parseR pRules) (srcrow1' <$> srcrow_ <$> singer_chain)

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
              (Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 2, version = Nothing})) ]

      let if_king_wishes_singer_nextline = if_king_wishes ++
            [ DefNameAlias ["singer"] ["person"] Nothing
              (Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 3, version = Nothing})) ]

      -- let if_king_wishes_singer_2 = if_king_wishes ++
      --       [ DefNameAlias ["singer"] ["person"] Nothing
      --         (Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 3, srccol = 5, version = Nothing})) ]

      filetest "nl-aliases" "should parse natural language aliases (\"NL Aliases\") aka inline defined names"
        (parseR pRules) if_king_wishes_singer

      filetest "nl-aliases-2" "should parse natural language aliases (\"NL Aliases\") on the next line"
        (parseR pRules) if_king_wishes_singer_nextline

      let singer_must_pay_params =
            singer_must_pay { action = mkLeaf (("pay" :| []                 , Nothing)
                                             :| [("to"     :| ["the King"], Nothing)
                                                ,("amount" :| ["$20"]     , Nothing)]) }

      filetest "action-params-singer" "should parse action params"
        (parseR pRules) [singer_must_pay_params]
