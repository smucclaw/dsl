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

    describe "Parsing boolstruct" $ do
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
