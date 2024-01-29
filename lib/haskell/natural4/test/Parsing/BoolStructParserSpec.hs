{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Parsing.BoolStructParserSpec (spec) where

import AnyAll (BoolStruct (All, Any, Not), mkLeaf)
import Data.ByteString.Lazy qualified as BS
import Data.List.NonEmpty (NonEmpty ((:|)))
import LS.BasicTypes (MyStream, MyToken (Given))
import LS.Interpreter (getAndOrTree, l4interpret)
import LS.Lib
  ( exampleStreams,
    pExpect,
    pGivens,
    pRules,
    pScenarioRule,
    pToplevel,
  )
import LS.Parser (pBoolStruct)
import LS.RelationalPredicates (pBoolStructPT)
import LS.Rule
  ( Expect (ExpRP),
    Rule
      ( Scenario,
        clauses,
        defaults,
        expect,
        has,
        lsource,
        name,
        rlabel,
        scgiven,
        srcref,
        symtab
      ),
    defaultHorn,
    defaultTypeDecl,
    mkTestSrcRef,
    runMyParser,
  )
import LS.Tokens (pRuleLabel, pToken)
import LS.Types
  ( HornClause (HC, hBody, hHead),
    MTExpr (MTI, MTT),
    ParamType (TOne),
    RPRel (RPis),
    RelationalPredicate
      ( RPBoolStructR,
        RPConstraint,
        RPMT,
        RPParamText
      ),
    RunConfig (debug, extendedGrounds, sourceURL),
    TypeSig (InlineEnum),
    defaultInterpreterOptions,
    defaultRC,
    mkRpmt,
    mkRpmtLeaf,
  )
import LS.XPile.VueJSON (groundrules)
import Test.Hspec
  ( HasCallStack,
    Spec,
    SpecWith,
    describe,
    it,
    xit,
  )
import Text.Megaparsec (ParseErrorBundle, ShowErrorComponent)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import LS.NLP.NLG (NLGEnv)
import Test.Hspec.Megaparsec (shouldParse)
import System.FilePath ((</>), (-<.>))

scenario1 :: Rule
scenario1 = Scenario
    { scgiven =
        [ mkRpmt ["the data breach is in relation to any prescribed personal data or class of personal data relating to the individual"],
          mkRpmt [ "loss of storage medium on which personal data is stored in circumstances where the unauthorised",
                    "disposal",
                    "of the personal data is likely to occur"],
          mkRpmt ["the data breach occurred only within an organisation"]
        ],
      expect = [ExpRP (mkRpmt ["IT IS", "not", "A Notifiable Data Breach"])],
      rlabel = Just ("SCENARIO", 1, "Misplaced storage drive"),
      lsource = Nothing,
      srcref = Nothing,
      defaults = [],
      symtab = []
    }

scenario2a :: Rule
scenario2a = Scenario
    { scgiven =
        [ RPConstraint [MTT "Organisation's name"] RPis [MTT "ABC"],
          mkRpmt ["the data breach is in relation to any prescribed personal data or class of personal data relating to the individual"],
          mkRpmt ["unauthorised", "disclosure", "of personal data", "occurred"],
          mkRpmt ["not", "the data breach occurred only within an organisation"],
          mkRpmt ["the data breach relates to", "the individual's", "full name"],
          mkRpmt ["the data breach relates to", "The number of any credit card, charge card or debit card issued to or in the name of the individual."]
        ],
      expect =
        [ ExpRP (mkRpmt ["IT IS", "A Notifiable Data Breach"]),
          ExpRP (mkRpmt ["Organisation", "must", "notify PDPC"]),
          ExpRP (mkRpmt ["Organisation", "must", "notify Affected Individuals"])
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
        [ RPParamText ((MTT "Organisation's name" :| [], Just (InlineEnum TOne ((MTT <$> "DEF" :| ["GHI"], Nothing) :| []))) :| []),
          mkRpmt ["the data breach is in relation to any prescribed personal data or class of personal data relating to the individual"],
          mkRpmt ["unauthorised", "disclosure", "of personal data", "occurred"],
          mkRpmt ["not", "the data breach occurred only within an organisation"],
          mkRpmt ["the data breach relates to", "the individual's", "full name"],
          mkRpmt ["the data breach relates to", "The number of any credit card, charge card or debit card issued to or in the name of the individual."],
          mkRpmt ["PDPC instructs you not to notify them"]
        ],
      expect =
        [ ExpRP (mkRpmt ["IT IS", "A Notifiable Data Breach"]),
          ExpRP (mkRpmt ["Organisation", "must", "notify PDPC"]),
          ExpRP (mkRpmt ["not", "Organisation", "must", "notify Affected Individuals"])
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
        [ RPConstraint [MTT "the prescribed number of affected individuals"] RPis [MTI 50],
          mkRpmt ["unauthorised", "access", "of personal data", "occurred"],
          mkRpmt ["the data breach relates to", "the individual's", "identification number"],
          mkRpmt [ "the data breach relates to",
                    "The assessment, diagnosis, treatment, prevention or alleviation by a health professional of any of the following affecting the individual:",
                    "any sexually-transmitted disease such as Chlamydial Genital Infection, Gonorrhoea and Syphilis;"]],
      expect =
        [ ExpRP (mkRpmt ["IT IS", "A Notifiable Data Breach"]),
          ExpRP (mkRpmt ["Organisation", "must", "notify PDPC"]),
          ExpRP (mkRpmt ["Organisation", "must", "notify Affected Individuals"])
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
        [ RPConstraint [MTT "the prescribed number of affected individuals"] RPis [MTI 1000],
          mkRpmt ["unauthorised", "access", "of personal data", "occurred"],
          mkRpmt ["the data breach relates to", "the individual's", "full name"],
          mkRpmt [ "the data breach relates to",
                    "the individual's",
                    "identification number",
                    "any sexually-transmitted disease such as Chlamydial Genital Infection, Gonorrhoea and Syphilis;"],
          mkRpmt ["the data breach relates to", "The number of any credit card, charge card or debit card issued to or in the name of the individual."]
        ],
      expect =
        [ ExpRP (mkRpmt ["IT IS", "A Notifiable Data Breach"]),
          ExpRP (mkRpmt ["Organisation", "must", "notify PDPC"]),
          ExpRP (mkRpmt ["Organisation", "must", "notify Affected Individuals"])
        ],
      rlabel = Just ("SCENARIO", 1, "Theft of portable storage drive containing hotel guests\8217 details"),
      lsource = Nothing,
      srcref = Nothing,
      defaults = [],
      symtab = []
    }

filetest :: (HasCallStack, ShowErrorComponent e, Show b, Eq b) => String -> String -> (String -> MyStream -> Either (ParseErrorBundle MyStream e) b) -> b -> SpecWith ()
filetest testfile desc parseFunc expected =
  it (testfile <> ": " <> desc ) do
  testcsv <- BS.readFile $ "test" </> "Parsing" </> "boolstruct" </> testfile -<.> "csv"
  parseFunc testfile `traverse` exampleStreams testcsv
    `shouldParse` [ expected ]

pullIO :: Either (ParseErrorBundle MyStream e) [IO b] -> IO (Either (ParseErrorBundle MyStream e) [b])
pullIO = traverse sequenceA

filetestIO :: (HasCallStack, ShowErrorComponent e, Show b, Eq b) => String -> String -> (String -> MyStream -> Either (ParseErrorBundle MyStream e) (IO b)) -> b -> SpecWith ()
filetestIO testfile desc parseFunc expected =
  it (testfile <> ": " <> desc ) do
  testcsv <- BS.readFile $ "test" </> "Parsing" </> "boolstruct" </> testfile -<.> "csv"
  parseResult <- pullIO $ parseFunc testfile `traverse` exampleStreams testcsv
  parseResult `shouldParse` [ expected ]

texttest :: (HasCallStack, ShowErrorComponent e, Show b, Eq b) => T.Text -> String -> (String -> MyStream -> Either (ParseErrorBundle MyStream e) b) -> b -> SpecWith ()
texttest testText desc parseFunc expected =
  it desc do
  let testcsv = TLE.encodeUtf8 (TL.fromStrict testText)
  parseFunc (show testText) `traverse` exampleStreams testcsv
    `shouldParse` [ expected ]

xtexttest :: (HasCallStack, ShowErrorComponent e, Show b, Eq b) => T.Text -> String -> (String -> MyStream -> Either (ParseErrorBundle MyStream e) b) -> b -> SpecWith ()
xtexttest testText desc parseFunc expected =
  xit desc do
    let testcsv = TLE.encodeUtf8 (TL.fromStrict testText)
    parseFunc (show testText) `traverse` exampleStreams testcsv
      `shouldParse` [ expected ]

spec :: Spec
spec = do
    let runConfig = defaultRC { sourceURL = T.pack $ "test" </> "Spec" }
        runConfigDebug = runConfig { debug = True }
    let  combine = uncurry (<>)
    let _parseWith1 f x y s = f <$> runMyParser combine runConfigDebug x y s

    let  parseWith  f x y s = f <$> runMyParser combine runConfig x y s
    let  parseR       x y s = runMyParser combine runConfig x y s
    let _parseR1      x y s = runMyParser combine runConfigDebug x y s
    let  parseOther   x y s = runMyParser id      runConfig x y s
    let _parseOther1  x y s = runMyParser id      runConfigDebug x y s

    describe "Parsing boolstruct" do
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
        ( Any Nothing [mkLeaf ((MTT "thing1" :| [],Nothing) :| [])
                      ,All Nothing [mkLeaf ((MTT "thing2" :| [],Nothing) :| [])
                                   ,mkLeaf ((MTT "thing3" :| [],Nothing) :| [])]]  , [] )


      -- filetest "boolstructp-3" "boolstruct including typically"
      --   (parseR pRules )
      --   [ defaultReg
      --     { subj = mkLeaf (("person" :| [],Nothing) :| [])
      --     , rkeyword = REvery
      --     , who = Just (Any Nothing [mkLeaf (RPMT ["is","immortal"])
      --                               ,mkLeaf (RPMT ["has","health insurance"])])
      --     , deontic = DMay
      --     , action = mkLeaf (("sharpen knives" :| [],Nothing) :| [])
      --     , srcref = mkTestSrcRef 1 1
      --     }
      --   , DefTypically
      --     { name = ["is","immortal"]
      --     , defaults = [RPConstraint ["is","immortal"] RPis ["false"]]
      --     , srcref = mkTestSrcRef 3 3}
      --   ]

      filetest "boolstructp-3" "groundrules, non-extended"
        (parseWith (groundrules runConfig) pRules) [MTT <$> ["person","has","health insurance"]]

      filetest "boolstructp-3" "groundrules, extended"
        (parseWith (groundrules runConfig { extendedGrounds = True }) pRules) [ MTT <$> ["person","is","immortal"]
                                    , MTT <$> ["person","has","health insurance"]]

      -- filetestIO "boolstructp-3" "as checklist, extended"
      --   (parseWith asCList pRules) [ ["Does the person have health insurance?"]
      --                               , ["Is the person immortal?"]]

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
        ( ExpRP (mkRpmt ["IT IS","A Notifiable Data Breach"])
        , []
        )

      xtexttest "EXPECT,NOT,IT IS,A Notifiable Data Breach," "unit test EXPECT ... NOT"
        (parseOther pExpect)
        ( ExpRP (RPBoolStructR
                 [] RPis
                 (Not (mkRpmtLeaf ["IT IS","A Notifiable Data Breach"])))
        , []
        )

      xtexttest "GIVEN,not,IT IS,A Notifiable Data Breach," "unit test GIVEN ... NOT"
        (parseOther pGivens )
        (  [RPBoolStructR
                 [] RPis
                 (Not (mkRpmtLeaf ["IT IS","A Notifiable Data Breach"]))]
        , []
        )

      filetest "scenario-units-2-a" "unit test 2a for scenarios"
        (parseOther pScenarioRule )
        ( scenario2a
        , []
        )

      xtexttest "EXPECT,,Organisation,,MUST,,notify PDPC,,,," "unit test EXPECT ... MUST"
        (parseOther pExpect )
        ( ExpRP (mkRpmt ["Organisation","MUST","notify PDPC"])
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

    describe "nestedHorn" do
      filetest "declare-nestedhorn-1" "nestedHorn inside a HAS"
        (parseR pToplevel)
          [ defaultTypeDecl
              { name = [MTT "Potato"],
                has = [defaultTypeDecl {name = MTT <$> ["genus", "species"]}],
                srcref = mkTestSrcRef 1 1
              },
            defaultHorn
              { name = MTT <$> ["genus", "species"],
                clauses = [HC {hHead = RPBoolStructR [MTT "genus", MTT "species"] RPis (mkLeaf (RPMT [MTT "some Linnaen thing"])), hBody = Nothing}],
                srcref = mkTestSrcRef 3 2
              }
          ]


    describe "variable substitution and rule expansion" do
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
