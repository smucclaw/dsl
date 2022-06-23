{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

-- import Test.Hspec.Megaparsec hiding (shouldParse)
import Text.Megaparsec
import LS.Lib
import LS.Parser
import LS.RelationalPredicates
import LS.ParamText
import LS.Tokens
import AnyAll hiding (asJSON)
import LS.BasicTypes
import LS.Types
import LS.Error
import TestNLG
import Test.QuickCheck
import LS.NLP.WordNet

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
import Test.QuickCheck.Arbitrary.Generic
import LS.NLP.NLG (NLGEnv, myNLGEnv)
import Control.Concurrent.Async (async, wait)
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

defaultReg, defaultCon, defaultHorn, defaultScenario :: Rule
defaultReg = Regulative
  { subj = mkLeaf "person"
  , rkeyword = REvery
  , who = Nothing
  , cond = Nothing
  , deontic = DMust
  , action = mkLeaf "sing"
  , temporal = Nothing
  , hence = Nothing
  , lest = Nothing
  , rlabel = Nothing
  , lsource = Nothing
  , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing})
  , upon = Nothing
  , given = Nothing
  , having = Nothing
  , wwhere = []
  , defaults = []
  , symtab   = []
  }

defaultCon = Constitutive
  { name = []
  , keyword = Means
  , letbind = mkLeafR "Undefined"
  , cond = Nothing
  , rlabel = Nothing
  , lsource = Nothing
  , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing})
  , given = Nothing
  , defaults = []
  , symtab   = []
  }

defaultHorn = Hornlike
  { name = []
  , keyword = Means
  , given = Nothing
  , upon  = Nothing
  , clauses = []
  , rlabel = Nothing
  , lsource = Nothing
  , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing})
  , defaults = []
  , symtab   = []
  }

defaultScenario = Scenario
  { scgiven = []
  , expect = []
  , rlabel = Nothing
  , lsource = Nothing
  , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing})
  , defaults = []
  , symtab = []
  }

filetest :: (HasCallStack, ShowErrorComponent e, Show b, Eq b) => String -> String -> (String -> MyStream -> Either (ParseErrorBundle MyStream e) b) -> b -> SpecWith ()
filetest testfile desc parseFunc expected =
  it (testfile {- ++ ": " ++ desc -}) $ do
  testcsv <- BS.readFile ("test/" <> testfile <> ".csv")
  parseFunc testfile `traverse` exampleStreams testcsv
    `shouldParse` [ expected ]

xfiletest :: (HasCallStack, ShowErrorComponent e, Show b, Eq b) => String -> String -> (String -> MyStream -> Either (ParseErrorBundle MyStream e) b) -> b -> SpecWith ()
xfiletest testfile desc parseFunc expected =
  xit (testfile {- ++ ": " ++ desc -}) $ do
  testcsv <- BS.readFile ("test/" <> testfile <> ".csv")
  parseFunc testfile `traverse` exampleStreams testcsv
    `shouldParse` [ expected ]    

filetest2 testfile desc parseFunc expected =
  it (testfile {- ++ ": " ++ desc -}) $ do
  testcsv <- BS.readFile ("test/" <> testfile <> ".csv")
  let parsed = parseFunc testfile `traverse` exampleStreams testcsv
  return ()


preprocess :: String -> String
preprocess text = filter (not . (`elem` ['!', '.'])) text

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
prop_rendertoken token =
  token `notElem` [Distinct, Checkbox, As, EOL, GoDeeper, UnDeeper, Empty, SOF, EOF, TypeSeparator, Other "", RuleMarker 0 ""] && notOther token ==>
  toToken (T.pack $ renderToken token) === [token]






main :: IO ()
main = do
  mpd <- lookupEnv "MP_DEBUG"
  mpn <- lookupEnv "MP_NLG"
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
        , extendedGrounds = False
        , toChecklist = False
        , printstream = False
        , runNLGtests = isJust mpn || False
        }
  -- verboseCheck prop_gerundcheck
  -- quickCheck prop_rendertoken
  -- nlgEnv <- putStrLn "Loading env" >> myNLGEnv <* putStrLn "Loaded env"
  asyncNlgEnv <- async $ putStrLn "Loading env" >> myNLGEnv <* putStrLn "Loaded env"
  let nlgEnv = unsafePerformIO $ wait asyncNlgEnv
  hspec $ do
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
    let combine (a,b) = a ++ b
    let dumpStream s = traceM "* Tokens" >> traceM (pRenderStream s)
    let parseWith  f x y s = when (debug runConfig_) (dumpStream s) >> f <$> runMyParser combine runConfig x y s
    let parseWith1 f x y s =                          dumpStream s  >> f <$> runMyParser combine runConfigDebug x y s
    let parseR       x y s = when (debug runConfig_) (dumpStream s) >> runMyParser combine runConfig x y s
    let parseR1      x y s =                          dumpStream s  >> runMyParser combine runConfigDebug x y s
    let parseOther   x y s = when (debug runConfig_) (dumpStream s) >> runMyParser id      runConfig x y s
    let parseOther1  x y s =                          dumpStream s  >> runMyParser id      runConfigDebug x y s
    describe "megaparsing" $ do


      it "should parse an unconditional" $ do
        parseR pRules "" (exampleStream ",EVERY,person,,\n,MUST,,,\n,->,sing,,\n")
          `shouldParse` [ srcrow2 $ defaultReg { subj = mkLeaf "person"
                                               , deontic = DMust
                                               } ]

      it "should parse a rule label" $ do
        parseR pRules "" (exampleStream ",\xc2\xa7,Hello\n")
          `shouldParse` [srcrow2 $ RuleGroup {rlabel = Just ("\167",1,"Hello"), srcref = srcref defaultReg}]

      it "should parse a rule label followed by something" $ do
        parseR pRules "" (exampleStream "\xc2\xa7,Hello\n,something\nMEANS,something\n")
          `shouldParse` [Hornlike {name = ["something"], keyword = Means, given = Nothing, upon = Nothing, clauses = [HC2 {hHead = RPBoolStructR ["something"] RPis (Leaf (RPMT ["something"])), hBody = Nothing}], rlabel = Just ("\167",1,"Hello"), lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing}), defaults = [], symtab = []}]

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
            , clauses = [ HC2
                          { hHead = RPBoolStructR ["degustates"]
                                    RPis (Any Nothing [Leaf (RPMT ["eats"])
                                                      ,Leaf (RPMT ["drinks"])])
                          , hBody = Nothing } ]
            }

      filetest "simple-constitutive-1" "should parse a simple constitutive rule"
        (parseR pRules) [srcrow2 degustates]

      filetest "simple-constitutive-1-checkboxes" "should parse a simple constitutive rule with checkboxes"
        (parseR pRules) [degustates { srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 1, version = Nothing}) }]

      let imbibeRule2 srcrow srccol = [
            defaultReg
              { who = Just $ All Nothing
                      [ mkLeafR "walks"
                      , mkLeafR "degustates"
                      ]
              }
            , defaultHorn { name = ["degustates"]
                          , keyword = Means
                          , clauses = [HC2 { hHead = RPMT ["degustates"]
                                           , hBody = Just (Any Nothing [Leaf (RPMT ["eats"])
                                                                       ,Leaf (RPMT ["imbibes"])])}]
                          , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec"
                                                  , srcrow = srcrow, srccol = srccol
                                                  , version = Nothing})
                          }
            ]


      -- inline constitutive rules are temporarily disabled; we need to think about how to intermingle a "sameline" parser with a multiline object.
      -- we also need to think about getting the sameline parser to not consume all the godeepers at once, because an inline constitutive rule actually starts with a godeeper.

      filetest "indented-2" "inline constitutive rule" (parseR pRules) $ [Regulative {subj = Leaf (("person" :| [],Nothing) :| []), rkeyword = REvery, who = Just (All Nothing [Leaf (RPMT ["walks"]),Leaf (RPMT ["degustates"])]), cond = Nothing, deontic = DMust, action = Leaf (("sing" :| [],Nothing) :| []), temporal = Nothing, hence = Nothing, lest = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing}), upon = Nothing, given = Nothing, having = Nothing, wwhere = [], defaults = [], symtab = []},Hornlike {name = ["degustates"], keyword = Means, given = Nothing, upon = Nothing, clauses = [HC2 {hHead = RPMT ["degustates"], hBody = Just (Any Nothing [Leaf (RPMT ["eats"]),Leaf (RPMT ["imbibes"])])}], rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 3, version = Nothing}), defaults = [], symtab = []}]

      filetest "indented-3" "defined names in natural positions" (parseR pRules) $ [Regulative {subj = Leaf (("person" :| [],Nothing) :| []), rkeyword = REvery, who = Just (All Nothing [Leaf (RPMT ["walks"]),Leaf (RPMT ["degustates"])]), cond = Nothing, deontic = DMust, action = Leaf (("sing" :| [],Nothing) :| []), temporal = Nothing, hence = Nothing, lest = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing}), upon = Nothing, given = Nothing, having = Nothing, wwhere = [], defaults = [], symtab = []},Hornlike {name = ["imbibes"], keyword = Means, given = Nothing, upon = Nothing, clauses = [HC2 {hHead = RPMT ["imbibes"], hBody = Just (All Nothing [Leaf (RPMT ["drinks"]),Any Nothing [Leaf (RPMT ["swallows"]),Leaf (RPMT ["spits"])]])}], rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 3, srccol = 5, version = Nothing}), defaults = [], symtab = []},Hornlike {name = ["degustates"], keyword = Means, given = Nothing, upon = Nothing, clauses = [HC2 {hHead = RPMT ["degustates"], hBody = Just (Any Nothing [Leaf (RPMT ["eats"]),Leaf (RPMT ["imbibes"])])}], rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 3, version = Nothing}), defaults = [], symtab = []}]

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
                          { subj = mkLeaf "King"
                          , rkeyword = RParty
                          , deontic = DMay
                          , action = mkLeaf "pay"
                          , temporal = Just (TemporalConstraint TAfter (Just 20) "min")
                          }


      let king_pays_singer_eventually =
            king_pays_singer { temporal = Nothing }

      let singer_must_pay = defaultReg
                              { rkeyword = RParty
                              , subj = mkLeaf "Singer"
                              , action = mkLeaf "pay"
                              , temporal = Just (TemporalConstraint TBefore (Just 1) "supper")
                              }


      let singer_chain = [ defaultReg
                         { subj = mkLeaf "person"
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

      let if_king_wishes_singer_2 = if_king_wishes ++
            [ DefNameAlias ["singer"] ["person"] Nothing
              (Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 3, srccol = 5, version = Nothing})) ]

      filetest "nl-aliases" "should parse natural language aliases (\"NL Aliases\") aka inline defined names"
        (parseR pRules) if_king_wishes_singer

      filetest "nl-aliases-2" "should parse natural language aliases (\"NL Aliases\") on the next line"
        (parseR pRules) if_king_wishes_singer_nextline

      let singer_must_pay_params =
            singer_must_pay { action = Leaf (("pay" :| []                 , Nothing)
                                             :| [("to"     :| ["the King"], Nothing)
                                                ,("amount" :| ["$20"]     , Nothing)]) }

      filetest "action-params-singer" "should parse action params"
        (parseR pRules) [singer_must_pay_params]

--      filetest "blank-lines" "should parse despite interrupting newlines"
--        (parseR pRules) if_king_wishes_singer_2

    describe "megaparsing MEANS" $ do

      let bobUncle1 = defaultHorn
            { name = ["Bob's your uncle"]
            , keyword = Means
            , clauses =
              [HC2 { hHead = RPBoolStructR ["Bob's your uncle"] RPis (Not (Any Nothing [Leaf (RPMT ["Bob is estranged"])
                                                                                       ,Leaf (RPMT ["Bob is dead"])]))
                   , hBody = Nothing}]
            , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing}) }

      filetest "bob-head-1" "less indented NOT" (parseR pRules) [srcrow2 bobUncle1]

      filetest "bob-head-1-b" "more indented NOT"
        (parseR pRules) [srcrow2 bobUncle1]

      let bobUncle2 = bobUncle1
            { clauses =
              [HC2 { hHead = RPBoolStructR ["Bob's your uncle"] RPis (Any Nothing [Not (Leaf (RPMT ["Bob is estranged"]))
                                                                                  ,Leaf (RPMT ["Bob is dead"])])
                   , hBody = Nothing } ] }

      filetest "bob-head-2" "handle less indentation"
          (parseR pRules) [srcrow2 bobUncle2]

      filetest "bob-head-3" "should handle outdentation"
        (parseR pRules) [srcrow2 bobUncle2]

      filetest "bob-tail-1" "should work for constitutive rules"
        (parseR pRules) [ defaultHorn
                          { name = ["Bob's your uncle"]
                          , keyword = Means
                          , clauses = [
                              HC2 { hHead = RPBoolStructR ["Bob's your uncle"] RPis (Any Nothing [Leaf (RPMT ["Bob is your mother's brother"])
                                                                                                 ,Leaf (RPMT ["Bob is your father's brother"])])
                                  , hBody = Just (Not (Leaf (RPMT ["Bob is estranged"])))
                                  } ]
                          , rlabel = Nothing
                          , lsource = Nothing
                          , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 1, version = Nothing}
                                          )
                          }
                        ]

    describe "megaparsing UNLESS semantics" $ do

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

      let silenceKing = [ defaultReg { cond = Just ( All Nothing [ mkLeafR "the king wishes"
                                                                 , Not ( mkLeafR "day of silence" )
                                                                 ] ) } ]

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

      it "pilcrows-1" $ do
        testcsv <- BS.readFile ("test/" <> "pilcrows-1" <> ".csv")
        parseR pRules "pilcrows-1" `traverse` exampleStreams testcsv
          `shouldParse` [ dayOfSilence, srcrow2 <$> dayOfSong ]

        -- forM_ (exampleStreams testcsv) $ \stream ->
        --   parseR pRules testfile stream
        --     `shouldParse` [ defaultCon
        --                   ]

  -- upgrade single OR group to bypass the top level AND group

  -- defNameAlias should absorb the WHO limb

    -- describe "megaparsing scenarios" $ do
    --   filetest "scenario-1" "should handle labeled given/expect"
    --     (parseR pRules)
    --       [ Scenario
    --         { scgiven =
    --             [ RPConstraint [ "amount saved" ] RPis [ "22000" ]
    --             , RPConstraint
    --               [ "earnings"
    --               , "amount"
    --               ] RPis [ "25000" ]
    --             , RPConstraint
    --               [ "earnings"
    --               , "steadiness"
    --               ] RPis [ "steady" ]
    --             ]
    --         , expect =
    --           [ HC2
    --             { hHead = RPConstraint [ "investment" ] RPis [ "savings" ]
    --             , hBody = Just
    --                           ( Leaf
    --                             ( RPConstraint [ "dependents" ] RPis [ "5" ] )
    --                           )
    --             }
    --           , HC2
    --             { hHead = RPConstraint [ "investment" ] RPis [ "combination" ]
    --             , hBody = Just
    --                           ( Leaf
    --                             ( RPConstraint [ "dependents" ] RPis [ "3" ] )
    --                           )
    --             }
    --           , HC2
    --             { hHead = RPConstraint [ "investment" ] RPis [ "stocks" ]
    --             , hBody = Just
    --                           ( Leaf
    --                             ( RPConstraint [ "dependents" ] RPis [ "0" ] )
    --                         )
    --             }
    --           ]
    --         , rlabel = Just
    --                    ( "§"
    --                    , 1
    --                    , "Scenario 1"
    --                    )
    --         , lsource = Nothing
    --         , srcref = srcref defaultReg
    --         , defaults = mempty, symtab = mempty
    --         }
    --       ]

    -- describe "megaparsing DECIDE layouts" $ do
    --   it "should handle multiline" $ do
    --     let testfile = "test/financialadvisor-decide-1.csv"
    --     testcsv <- BS.readFile testfile
    --     parseR pRules testfile `traverse` (exampleStreams testcsv)
    --       `shouldParse`
    --       [ [ Scenario

    describe "revised parser" $ do

      let simpleHorn = [ defaultHorn
              { name = ["X"]
              , keyword = Decide
              , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing})
              , clauses =
                [ HC2
                  { hHead = RPConstraint ["X"] RPis ["Y"]
                  , hBody = Just $ All Nothing [ Leaf (RPConstraint ["Z"] RPis ["Q"])
                                               , Leaf (RPConstraint ["P"] RPgt ["NP"]) ]
                  } ]
              }
            ]
      let simpleHorn10 = [ defaultHorn
              { name = ["X"]
              , keyword = Decide
              , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing})
              , clauses =
                [ HC2
                  { hHead = RPConstraint ["X"] RPis ["Y"]
                  , hBody = Just $ Leaf (RPConstraint ["Z"] RPis ["Q"])
                  } ]
              }
            ]
      let simpleHorn02 = [ defaultHorn
              { name = ["X"]
              , keyword = Decide
              , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing})
              , clauses =
                [ HC2
                  { hHead = RPConstraint ["X"] RPis ["Y"]
                  , hBody = Nothing
                  } ]
              }
            ]

      filetest "horn-0-1" "should parse X IS Y"
        (parseOther pRelPred) ( RPConstraint ["X"] RPis ["Y"], [] )

      filetest "horn-0-1" "should parse X IS Y using the other parser"
        (parseOther pBSR) ( Leaf (RPConstraint ["X"] RPis ["Y"] ), [] )

      filetest "horn-0-3" "should parse X1 X2 IS Y"
        (parseOther pBSR) ( Leaf (RPConstraint ["X1", "X2"] RPis ["Y"] ), [] )

      filetest "horn-0-2" "should parse DECIDE X IS Y" (parseR pToplevel) simpleHorn02

      -- filetest "horn-1" "should parse horn clause on a single line" (parseR pToplevel) simpleHorn10

      filetest "horn-2" "should parse horn clauses 2"
        (parseR pToplevel) simpleHorn

      -- syntax unsupported at this time; we need continuation passing style
      -- filetest "horn-3" "should parse horn clauses 3" (parseR pToplevel) simpleHorn

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

      -- of the three layouts below, only 2-c-3 works.
--      filetest "indent-2-c" "label samecol" (parseOther exprP) ablcd
--      filetest "indent-2-c-2" "label right" (parseOther exprP) ablcd
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
          actionFragment1 = Leaf (text2pt "win")

      filetest "action-1" "should a one-word BoolStructP"
        (parseOther pDoAction)(actionFragment1,[])

      filetest "action-2" "should a two-word BoolStructP"
        (parseOther pDoAction) (Leaf $ ("win" :| ["gloriously"]
                                                 , Nothing):|[]
                               ,[])

    describe "WHO / WHICH / WHOSE parsing of BoolStructR" $ do

      let whoStructR_1 = defaultReg
                         { who = Just ( Leaf ( RPMT ["eats"] ) ) }

          whoStructR_2 = defaultReg
                         { who = Just ( Leaf ( RPMT ["eats", "rudely"] ) ) }

          whoStructR_3 = defaultReg
                         { who = Just ( Leaf ( RPMT ["eats", "without", "manners"] ) ) }

          whoStructR_4 = defaultReg
                         { who = Just ( Leaf ( RPMT ["eats", "sans", "decorum"] )) }

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
      let unauthorisedExpected = [Hornlike {name = ["a Data Breach"], keyword = Means, given = Nothing, upon = Nothing, clauses = [HC2 {hHead = RPBoolStructR ["a Data Breach"] RPis (Leaf (RPMT ["a Notifiable Data Breach"])), hBody = Just (Leaf (RPMT ["a data breach","occurred"]))}], rlabel = Just ("\167",1,"NDB Qualification"), lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing}), defaults = [], symtab = []},DefNameAlias {name = ["NDB"], detail = ["a Notifiable Data Breach"], nlhint = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 4, version = Nothing})},Hornlike {name = ["a data breach","occurred"], keyword = Means, given = Nothing, upon = Nothing, clauses = [HC2 {hHead = RPMT ["a data breach","occurred"], hBody = Just (Any (Just (PrePost "any unauthorised" "of personal data")) [Leaf (RPMT ["access"]),Leaf (RPMT ["use"]),Leaf (RPMT ["disclosure"]),Leaf (RPMT ["copying"]),Leaf (RPMT ["modification"]),Leaf (RPMT ["disposal"])])}], rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 3, srccol = 4, version = Nothing}), defaults = [], symtab = []}]
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
        (parseR pToplevel) [defaultReg {subj = Leaf (("Data Intermediary" :| [],Nothing) :| []), rkeyword = REvery, who = Just (Leaf (RPMT ["is not","processing personal data on behalf of and for the purposes of a public agency"])), cond = Just (Leaf (RPMT ["the data breach occurs on or after the date of commencement of PDP(A)A 2020 \167\&13"])), deontic = DMust, action = Leaf (("NOTIFY" :| ["the Organisation"],Nothing) :| [("for which" :| ["you act as a Data Intermediary"],Nothing)]), temporal = Just (TemporalConstraint TVague (Just 0) "without undue delay"), hence = Nothing, lest = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing}), upon = Just (("becoming aware a data breach involving a client Organisation may have occurred" :| [],Nothing) :| []), given = Nothing, having = Nothing, wwhere = []},DefNameAlias {name = ["You"], detail = ["Data Intermediary"], nlhint = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 2, version = Nothing})}]

      filetest "pdpadbno-3" "data intermediaries"
        (parseR pToplevel) [defaultReg {subj = Leaf (("Data Intermediary" :| [],Nothing) :| []), rkeyword = REvery, who = Just (Leaf (RPMT ["processes personal data on behalf of and for the purposes of a public agency"])), cond = Nothing, deontic = DMust, action = Leaf (("NOTIFY" :| ["the Public Agency"],Nothing) :| [("for which" :| ["you act as a Data Intermediary"],Nothing)]), temporal = Just (TemporalConstraint TVague (Just 0) "without undue delay"), hence = Nothing, lest = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing}), upon = Just (("becoming aware a data breach involving a client public agency may have occurred" :| [],Nothing) :| []), given = Nothing, having = Nothing, wwhere = []},DefNameAlias {name = ["You"], detail = ["Data Intermediary"], nlhint = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 2, version = Nothing})}]

--      filetest "pdpadbno-4" "ndb qualification" (parseR pToplevel) []

      filetest "pdpadbno-5" "notification to PDPC"
        (parseR pToplevel) [defaultReg {subj = Leaf (("You" :| [],Nothing) :| []), rkeyword = RParty, who = Nothing, cond = Just (All Nothing [Leaf (RPMT ["it is","an NDB"]),Not (Leaf (RPMT ["you are a Public Agency"]))]), deontic = DMust, action = Leaf (("NOTIFY" :| ["the PDPC"],Nothing) :| [("in" :| ["the form and manner specified at www.pdpc.gov.sg"],Nothing),("with" :| ["a Notification Message"],Nothing),("and" :| ["a list of individuals for whom notification waiver is sought"],Nothing)]), temporal = Just (TemporalConstraint TBefore (Just 3) "days"), hence = Just (defaultReg {subj = Leaf (("the PDPC" :| [],Nothing) :| []), rkeyword = RParty, who = Nothing, cond = Nothing, deontic = DMay, action = Leaf (("NOTIFY" :| ["you"],Nothing) :| [("with" :| ["a list of individuals to exclude from notification"],Nothing)]), temporal = Nothing, hence = Nothing, lest = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, upon = Nothing, given = Nothing, having = Nothing, wwhere = []}), lest = Nothing, rlabel = Just ("\167",2,"Notify PDPC"), lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing}), upon = Nothing, given = Nothing, having = Nothing, wwhere = []},DefNameAlias {name = ["the PDPC Exclusion List"], detail = ["with","a list of individuals to exclude from notification"], nlhint = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 1, version = Nothing})}]

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
              [ HC2
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
        (parseR pToplevel) [Regulative {subj = Leaf (("You" :| [],Nothing) :| []), rkeyword = RParty, who = Nothing, cond = Just (All Nothing [Leaf (RPMT ["it is","an NDB"]),Not (Leaf (RPMT ["you are a Public Agency"]))]), deontic = DMust, action = Leaf (("NOTIFY" :| ["each of the Notifiable Individuals"],Nothing) :| [("in" :| ["any manner that is reasonable in the circumstances"],Nothing),("with" :| ["a message obeying a certain format"],Nothing)]), temporal = Just (TemporalConstraint TBefore (Just 3) "days"), hence = Nothing, lest = Nothing, rlabel = Just ("\167",2,"Notify Individuals"), lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing}), upon = Nothing, given = Nothing, having = Nothing, wwhere = [Hornlike {name = ["the Notifiable Individuals"], keyword = Means, given = Nothing, upon = Nothing, clauses = [HC2 {hHead = RPMT ["the Notifiable Individuals"], hBody = Nothing}], rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 9, version = Nothing}), defaults = [], symtab = []}], defaults = [], symtab = []},Hornlike {name = ["the Notifiable Individuals"], keyword = Means, given = Nothing, upon = Nothing, clauses = [HC2 {hHead = RPMT ["the Notifiable Individuals"], hBody = Just (All Nothing [Leaf (RPMT ["the set of individuals affected by the NDB"]),Not (Leaf (RPMT ["the individuals who are deemed","Unlikely"])),Not (Leaf (RPMT ["the individuals on","the PDPC Exclusion List"])),Not (Leaf (RPMT ["the individuals on","the LEA Exclusion List"]))])}], rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 2, srccol = 9, version = Nothing}), defaults = [], symtab = []}]

{- primitives -}
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
          , Any (Just $ Pre "yummy nightshades") [ Leaf (RPMT ["potato"])
                                                 , Leaf (RPMT ["tomato"])]
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

      let aboveNextLineKeyword :: SLParser ([T.Text],MyToken)
          aboveNextLineKeyword = debugName "aboveNextLineKeyword" $ do
            (_,x,y) <- (,,)
                           $*| return ((),0)
                           ->| 1
                           |*| slMultiTerm
                           |<| choice (pToken <$> [ LS.Types.Or, LS.Types.And, LS.Types.Unless ])
            return (x,y)

          aNLK :: Int -> SLParser ([T.Text],MyToken)
          aNLK maxDepth = mkSL $ do
            (toreturn, n) <- runSL aboveNextLineKeyword
            debugPrint $ "got back toreturn=" ++ show toreturn ++ " with n=" ++ show n ++ "; maxDepth=" ++ show maxDepth ++ "; guard is n < maxDepth = " ++ show (n < maxDepth)
            guard (n < maxDepth)
            return (toreturn, n)

      -- aboveNextLineKeyword has returned ((["foo1","foo2","foo3"],Or),1)
      -- aboveNextLineKeyword has returned ((["foo2","foo3"],       Or),0)
      -- aboveNextLineKeyword has returned ((["foo3"],              Or),-1) -- to get this, maxDepth=0

      it "SLParser combinators 5 aboveNextLineKeyword" $ do
        parseOther ((,,)
                    $>| pOtherVal
                    |*| aboveNextLineKeyword
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

      -- this should serve for matching a pBSR
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

-- this test will fail; we can try uncommenting the `term p/c` stanza within Parser.hs/term but that will break action parameters.
      -- filetest "inline-1-a2" "line crossing"
      --   (parseOther ( (,,)
      --                 >*| slMultiTerm
      --                 |<| pToken Means
      --                 |>| pBSR
      --                 |<$ undeepers
      --               ))
      --   ( ( ["Food"]
      --     , Means
      --     , Any (Just $ Pre "yummy nightshades with spices") [ Leaf (RPMT ["potato","with","salt"])
      --                                                        , Leaf (RPMT ["tomato","with","pepper"])]
      --     ), []
      --   )

      let inline_1 = ( ( ["Bad"] , Means , inline_pp ), [] )
          inline_2 = ( ( ["Bad"] , Means , inline_p  ), [] )
          inline_3 = ( ( ["Bad"] , Means , inline_   ), [] )
          inline_r = ((["multiwonk"],Means,Any Nothing [Leaf (RPMT ["poopoo"]),Any (Just (Pre "the")) [Leaf (RPMT ["honk"]),Leaf (RPMT ["ponk"])]]),[])
          inline_4 = ( ( ["a data breach occurred"] , Means , inline_4xs), [] )
          inline_pp = Any (Just $ PrePost "any unauthorised" "of personal data" ) inline_xs
          inline_p  = Any (Just $ Pre     "any unauthorised"                    ) inline_xs
          inline_   = Any Nothing                                                 inline_xs
          inline_xs = Leaf . RPMT . pure <$> T.words "access use disclosure copying modification disposal"
          inline4_pp= Any (Just $ PrePost
                           "loss of storage medium on which personal data is stored in circumstances where the unauthorised"
                           "of the personal data is likely to occur") inline_xs
          inline_4xs= Any Nothing [inline_pp, inline4_pp]

          pInline1 = parseOther $ do
            let getLHS ((x,_),z) = (x,z)
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
      filetest "inline-1-p" "line crossing" pInline1 ((["wonk"],Means,Any (Just (Pre "a")) [Leaf (RPMT ["honk"]),Leaf (RPMT ["ponk"])]),[])
      filetest "inline-1-q" "line crossing" pInline1 ((["poowonk"],Means,Any Nothing [Leaf (RPMT ["poopoo"]),Leaf (RPMT ["just a","honk"])]),[])
      filetest "inline-1-r" "line crossing" pInline1 inline_r
      filetest "inline-1-s" "line crossing" pInline1 inline_4

      filetest "multiterm-with-blanks-1" "p, no blanks"              (parseOther pMultiTerm) (["foo","bar","baz"],[])
      filetest "multiterm-with-blanks-2" "p, with blanks"            (parseOther pMultiTerm) (["foo","bar","baz"],[])

-- expected to fail
--      filetest "multiterm-with-blanks-3" "p, with blank, next line"  (parseOther pMultiTerm) (["foo","bar","baz"],[])

      filetest "multiterm-with-blanks-1" "sl, no blanks"             (parseOther (slMultiTerm |<$ undeepers)) (["foo","bar","baz"],[])
      filetest "multiterm-with-blanks-2" "sl, with blanks"           (parseOther (slMultiTerm |<$ undeepers)) (["foo","bar","baz"],[])

-- expected to fail
--      filetest "multiterm-with-blanks-3" "sl, with blank, next line" (parseOther (slMultiTerm |<$ undeepers)) (["foo","bar","baz"],[])


-- [ Hornlike
--     { name = [ "Bad" ]
--     , keyword = Means
--     , given = Nothing
--     , upon = Nothing
--     , clauses =
--         [ HC2
--             { hHead = RPBoolStructR [ "Bad" ] RPis
--                 ( Any
--                     ( Just
--                         ( PrePost "any unauthorised" "of personal data" )
--                     )
--                     [ Leaf
--                         ( RPMT [ "access" ] )
--                     , Leaf
--                         ( RPMT [ "use" ] )
--                     , Leaf
--                         ( RPMT [ "disclosure" ] )
--                     , Leaf
--                         ( RPMT [ "copying" ] )
--                     , Leaf
--                         ( RPMT [ "modification" ] )
--                     , Leaf
--                         ( RPMT [ "disposal" ] )
--                     ]
--                 )
--             , hBody = Nothing
--             }
--         ]
--     , rlabel = Nothing
--     , lsource = Nothing
--     , srcref = Just
--         ( SrcRef
--             { url = "test/inline-1-e.csv"
--             , short = "test/inline-1-e.csv"
--             , srcrow = 2
--             , srccol = 1
--             , version = Nothing
--             }
--         )
--     , defaults = []
--     , symtab = []
--     }
-- ]



{-
    describe "Prolog" $ do

      it "pdpadbno1" $ do
        testcsv <- BS.readFile ("test/" <> "pdpadbno-1" <> ".csv")
        let dbno1 = parseR pRules "pdbadbno-1" `traverse` exampleStreams testcsv
        (show . fmap sfl4ToProlog <$> dbno1) `shouldParse` "potato"

      it "degustates" $ do
        testcsv <- BS.readFile ("test/" <> "simple-constitutive-1" <> ".csv")
        let rules = parseR pRules "simple-constitutive-1" `traverse` exampleStreams testcsv
        (show . fmap sfl4ToProlog <$> rules) `shouldParse` "potato"
-}


{- tests for the Petri net backend -}

--    describe "Petri" $ do

      -- can we output a simple petri net
      -- it "petri-1" $ do
      --   testcsv <- BS.readFile ("test/" <> "pdpadbno-1" <> ".csv")
      --   rawRules <- fromRight $ parseR pRules "pdbadbno-1" `traverse` exampleStreams testcsv
      --   let rules = insrules rawRules startGraph
      --       asPetri = renderDot $ unqtDot $ graphToDot (petriParams rules) rules
      --   asPetri `shouldBe` (myReadFile "... expected.dot")

      -- rule expansion directly, where a Rule1 says "GOTO Rule2"; can we connect up Rule2 correctly

      -- rule expansion via a single hornlike, where a Rule1 says "GOTO Rule2"; can we connect up Rule2 correctly

      -- rule expansion via two layers of hornlike, where a Rule1 says "GOTO Rule2"; can we connect up Rule2 correctly

      -- "AND" split/join should do the right thing

      -- "OR" split/join should do the right thing

      -- graph transformation eliminates if (a) { ... if (a) ... }
      -- -- we see this in the rule for Notify Individuals

      filetest "boolstructp-1" "basic boolstruct of text"
        (parseOther pBoolStruct )
        ( All Nothing [Any Nothing [Leaf "thing1"
                                   ,Leaf "thing2"]
                      ,Leaf "thing3"], [] )


      filetest "boolstructp-2" "basic boolstruct of text"
        (parseOther pBoolStruct )
        ( Any Nothing [Leaf "thing1"
                      ,All Nothing [Leaf "thing2"
                                   ,Leaf "thing3"]], [] )

      filetest "boolstructp-2" "basic boolstruct of paramtext"
        (parseOther pBoolStructPT )
        ( Any Nothing [Leaf (("thing1" :| [],Nothing) :| [])
                      ,All Nothing [Leaf (("thing2" :| [],Nothing) :| [])
                                   ,Leaf (("thing3" :| [],Nothing) :| [])]]  , [] )


      filetest "boolstructp-3" "boolstruct including typically"
        (parseR pRules )
        [ defaultReg
          { subj = Leaf (("person" :| [],Nothing) :| [])
          , rkeyword = REvery
          , who = Just (Any Nothing [Leaf (RPMT ["is","immortal"])
                                    ,Leaf (RPMT ["has","health insurance"])])
          , deontic = DMay
          , action = Leaf (("sharpen knives" :| [],Nothing) :| [])
          , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing})
          }
        , DefTypically
          { name = ["is","immortal"]
          , defaults = [RPConstraint ["is","immortal"] RPis ["false"]]
          , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 3, srccol = 3, version = Nothing})}
        ]

      when (runNLGtests runConfig_) $ do
        -- let's see if the groundrules function outputs the right things
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
        -- TODO: check why nlgQuestion reverses order, revert this once that is fixed



-- let's parse scenario rules!

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

      xfiletest "scenario-units-1" "unit test 1 for scenarios"
        (parseOther pScenarioRule )
        ( defaultScenario
        , []
        )

    -- [TODO] it'd be nice to get this working as a filetest rather than the manual way
    describe "transpiler to CoreL4" $ do
      it "should output a class declaration for seca.csv" $ do
        let testfile = "seca"
        testcsv <- BS.readFile ("test/" <> testfile <> ".csv")
        let rules  = parseR pRules "" `traverse` (exampleStreams testcsv)
        (fmap sfl4ToCorel4 <$> rules) `shouldParse` [ "#\n# outputted via CoreL4.Program types\n#\n\n\nclass Situation\nrule <SecA_RecoverPassengersVehicleAuthorizedOp>\n\n#\n# outputted directly from XPile/CoreL4.hs\n#\n\nclass Situation\n\ndecl sit: Situation\n\nrule <SecA_RecoverPassengersVehicleAuthorizedOp>\nfor sit: Situation\nif (section_A applies && currentSit sit && sit == missingKeys)\nthen coverProvided Situation recoverPassengersVehicleAuthorizedOp SecA_RecoverPassengersVehicleAuthorizedOp\n\n\n" ]

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

