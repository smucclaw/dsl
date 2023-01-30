{-# LANGUAGE OverloadedStrings #-}
module Parsing.BoolStructParserSpec where

import Text.Megaparsec
import LS.Lib
import LS.Parser
import LS.Interpreter
import LS.RelationalPredicates
import LS.Tokens
import AnyAll hiding (asJSON)
import LS.BasicTypes
import LS.Types
import LS.Rule
import LS.XPile.VueJSON
import Test.Hspec
import qualified Data.ByteString.Lazy as BS
import Data.List.NonEmpty (NonEmpty ((:|)))
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import LS.NLP.NLG (NLGEnv)
import Test.Hspec.Megaparsec (shouldParse)


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

filetest :: (HasCallStack, ShowErrorComponent e, Show b, Eq b) => String -> String -> (String -> MyStream -> Either (ParseErrorBundle MyStream e) b) -> b -> SpecWith ()
filetest testfile desc parseFunc expected =
  it (testfile ++ ": " ++ desc ) $ do
  testcsv <- BS.readFile ("test/Parsing/boolstruct/" <> testfile <> ".csv")
  parseFunc testfile `traverse` exampleStreams testcsv
    `shouldParse` [ expected ]

pullIO :: Either (ParseErrorBundle MyStream e) [IO b] -> IO (Either (ParseErrorBundle MyStream e) [b])
pullIO = mapM sequence

filetestIO :: (HasCallStack, ShowErrorComponent e, Show b, Eq b) => String -> String -> (String -> MyStream -> Either (ParseErrorBundle MyStream e) (IO b)) -> b -> SpecWith ()
filetestIO testfile desc parseFunc expected =
  it (testfile ++ ": " ++ desc ) $ do
  testcsv <- BS.readFile ("test/Parsing/boolstruct/" <> testfile <> ".csv")
  parseResult <- pullIO $ parseFunc testfile `traverse` exampleStreams testcsv
  parseResult `shouldParse` [ expected ]

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

parserTests :: NLGEnv -> Spec
parserTests nlgEnv = do
    let runConfig = defaultRC { sourceURL = "test/Spec" }
        runConfigDebug = runConfig { debug = True }
    let  combine (a,b) = a ++ b
    let _parseWith1 f x y s = f <$> runMyParser combine runConfigDebug x y s

    let  parseWith  f x y s = f <$> runMyParser combine runConfig x y s
    let  parseR       x y s = runMyParser combine runConfig x y s
    let _parseR1      x y s = runMyParser combine runConfigDebug x y s
    let  parseOther   x y s = runMyParser id      runConfig x y s
    let _parseOther1  x y s = runMyParser id      runConfigDebug x y s

        asCList = checklist nlgEnv (runConfig { extendedGrounds = True })

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


      -- filetest "boolstructp-3" "boolstruct including typically"
      --   (parseR pRules )
      --   [ defaultReg
      --     { subj = mkLeaf (("person" :| [],Nothing) :| [])
      --     , rkeyword = REvery
      --     , who = Just (Any Nothing [mkLeaf (RPMT ["is","immortal"])
      --                               ,mkLeaf (RPMT ["has","health insurance"])])
      --     , deontic = DMay
      --     , action = mkLeaf (("sharpen knives" :| [],Nothing) :| [])
      --     , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing})
      --     }
      --   , DefTypically
      --     { name = ["is","immortal"]
      --     , defaults = [RPConstraint ["is","immortal"] RPis ["false"]]
      --     , srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 3, srccol = 3, version = Nothing})}
      --   ]

      filetest "boolstructp-3" "groundrules, non-extended"
        (parseWith (groundrules runConfig) pRules) [["person","has","health insurance"]]

      filetest "boolstructp-3" "groundrules, extended"
        (parseWith (groundrules runConfig { extendedGrounds = True }) pRules) [ ["person","is","immortal"]
                                    , ["person","has","health insurance"]]

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
        ( ExpRP (RPMT ["IT IS","A Notifiable Data Breach"])
        , []
        )

      xtexttest "EXPECT,NOT,IT IS,A Notifiable Data Breach," "unit test EXPECT ... NOT"
        (parseOther pExpect)
        ( ExpRP (RPBoolStructR
                 [] RPis
                 (Not (mkLeaf (RPMT ["IT IS","A Notifiable Data Breach"]))))
        , []
        )

      xtexttest "GIVEN,not,IT IS,A Notifiable Data Breach," "unit test GIVEN ... NOT"
        (parseOther pGivens )
        (  [RPBoolStructR
                 [] RPis
                 (Not (mkLeaf (RPMT ["IT IS","A Notifiable Data Breach"])))]
        , []
        )

      filetest "scenario-units-2-a" "unit test 2a for scenarios"
        (parseOther pScenarioRule )
        ( scenario2a
        , []
        )

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

    describe "nestedHorn" $ do
      filetest "declare-nestedhorn-1" "nestedHorn inside a HAS"
        (parseR pToplevel) [TypeDecl {name = ["Potato"], super = Nothing, has = [TypeDecl {name = ["genus","species"], super = Nothing, has = [], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Nothing, defaults = [], symtab = []}], enums = Nothing, given = Nothing, upon = Nothing, rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 1, srccol = 1, version = Nothing}), defaults = [], symtab = []},Hornlike {name = ["genus","species"], super = Nothing, keyword = Means, given = Nothing, upon = Nothing, clauses = [HC {hHead = RPBoolStructR ["genus","species"] RPis (mkLeaf (RPMT ["some Linnaen thing"])), hBody = Nothing}], rlabel = Nothing, lsource = Nothing, srcref = Just (SrcRef {url = "test/Spec", short = "test/Spec", srcrow = 3, srccol = 2, version = Nothing}), defaults = [], symtab = []}]



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
