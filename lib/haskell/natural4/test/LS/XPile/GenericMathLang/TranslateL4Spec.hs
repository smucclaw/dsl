{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LS.XPile.GenericMathLang.TranslateL4Spec (spec) where

import AnyAll (BoolStruct (..), Label (..))
import AnyAll qualified as AA
import Control.Arrow ((>>>))
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Writer.Lazy (runWriter)
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty (NonEmpty (..), fromList, nonEmpty, toList)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import System.FilePath
import Text.Pretty.Simple qualified as Pretty
import Explainable
import Explainable.MathLang -- hiding ((|>))
import LS.Interpreter (l4interpret)
import LS.Rule (Interpreted (..), Rule (..), defaultL4I, defaultReg, defaultHorn, defaultTypeDecl)
import LS.Types
import LS.XPile.IntroReader (defaultReaderEnv)
import LS.XPile.MathLang.GenericMathLang.GenericMathLangAST
import LS.XPile.MathLang.GenericMathLang.GenericMathLangAST qualified as GML
import LS.XPile.MathLang.GenericMathLang.TranslateL4
import LS.XPile.MathLang.MathLang qualified as ML
import LS.XPile.MathLang.GenericMathLang.ToGenericMathLang (toMathLangGen, expandHornlikes, getHornlikes)
import Test.Hspec (Spec, describe, it, shouldBe, xit)
import Test.Hspec.Golden
import TextuaL4.Transform    ( transRule )
import TextuaL4.ParTextuaL   ( pRule, myLexer )
import Text.RawString.QQ (r)
import Prelude hiding (exp, seq)
import Data.String.Interpolate (i)


goldenGeneric :: Show a => String -> a -> Golden TL.Text
goldenGeneric name output_ = Golden
  { output = Pretty.pShowNoColor output_
  , encodePretty = TL.unpack
  , writeToFile = TL.writeFile
  , readFromFile = TL.readFile
  , goldenFile =  testPath <.> "expected"
  , actualFile = Just (testPath <.> "actual")
  , failFirstTime = False
  }
  where
    testPath = "test" </> "testdata" </> "golden" </> "TranslateL4Spec" </> name

spec :: Spec
spec = do

  describe "rule 2 into SimpleHL" do
    it "should become a SimpleHL" do
      let toTest = runToLC $ simplifyL4Hlike rule2givens
      case toTest of
        Left err -> err `shouldBe` MiscError "" ""
        Right res -> shClauses res `shouldBe` rule2givens_shl_gold

    -- Postpone
    -- testBaseExpify "rule 2 (basic, simple arithmetic) into BaseExp"
    --                "should become something that makes sense"
    --                 [rule2nogivens]
    --                 [EEmpty]

    -- testBaseExpify "rule 2 with givens into BaseExp"
    --                 "should become (eventually) something where the givens are... ??? at least not SetVar"
    --                 [rule2givens]
    --                 [rule2givens_gold]

    -- testBaseExpify "rule 3 into BaseExp"
    --                 "should become something with records?"
    --                 [rule3predicate]
    --                 [rule3predicate_gold]

  testBaseExpify "arithRule2"
                 "arithmetics testcase 2"
                 "should parse inside a cell"
                 [arithRule2]


  testBaseExpify "arithRule3"
                 "arithmetics testcase 3"
                 "should parse inside a cell"
                 [arithRule3]


  testBaseExpify "arithRule4"
                 "arithmetics testcase 4"
                 "mengs complex case"
                 [arithRule4]


  testBaseExpify "stringsAndBackticks"
                 "strings and backticks"
                 "should handle strings and backtics correctly"
                 [stringsAndBackticks]

  describe "toMathLang" do
    let l4i = defaultL4I {origrules = [arithRule4]}
        res@(exprs,st) = ML.toMathLang l4i

    it "should turn Rules straight to MathLang (via GenericMathLang)" $
      goldenGeneric "mathLang4" res

    it "should evaluate taxesPayable correctly when info is given" do
      let st' = st {
              symtabF = symtabF st <> [
                ("annualIncome", Val (Just "annualIncome") 10000),
                ("netWorth", Val (Just "netWorth") 0)
                ]
            , symtabP = [
                ("vivacity", PredVal (Just "vivacity") True),
                ("phaseOfMoon.waxing", PredVal (Just "phaseOfMoon.waxing") False),
                ("phaseOfMoon.gibbous", PredVal (Just "phaseOfMoon.gibbous") True),
                ("phaseOfMoon.full", PredVal (Just "phaseOfMoon.full") False) ]
            }
      case exprs of
        [expr] -> do
          (taxes, _xp, _st, _strs) <- xplainE emptyE st' $ eval expr
          taxes `shouldBe` 50.0
        _ -> mempty


  describe "evalSimple" do
    it "should evaluate 2+2" do
      let l4i = defaultL4I {origrules = [arithRule1]}
      case ML.toMathLang l4i of
        ([],_) -> mempty
        (expr:_,st) -> do
          (e, _xp, _st, _strs) <- xplainE emptyE st $ eval expr
          e `shouldBe` 4.0

  describe "arithRule2 with initialized values" do
    it "should evaluate 10-5-3-1 == 1" do
      let l4i = defaultL4I {origrules = [arithRule2withInitializedValues]}
      case ML.toMathLang l4i of
        ([], _) -> mempty
        (expr:_, st) -> do
          (res, _xp, _st, _strs) <- xplainE emptyE st $ eval expr
          res `shouldBe` 1.0

  describe "toMathLang for arithRule3" do
    let l4i_ar3 = defaultL4I {origrules = [arithRule3]}
        res_ar3@(exprs,state) = ML.toMathLang l4i_ar3
    it "should have all 4 variables in symtab" $ goldenGeneric "arithRule3_symtab" do
      symtabF state

  describe "test user-defined function" do
    it "should be a binary function in userFuns" $ goldenGeneric "testLambdaGML" do
      runToLC $ userFuns <$> l4ToLCProgram testFunApp

  testBaseExpify "testFunApp" "test function application" "should be recognised as a function" testFunApp
  testBaseExpify "nestedGenitivesInFunApp" "test function application" "fun app and nested genitives" nestedGenitivesInFunApp

  testBaseExpify "testCurrency" "parsing currencies" "should be parsed into various currencies" testCurrency

  describe "genitive->record field" do
    let gml = runToLC $ baseExpifyMTEs nestedGenitives
    case gml of
      Left err -> do
        it "unsuccesful, abort" do
          err `shouldBe` MiscError "foo" "bar"
      Right res -> do
        it "should turn xs y into a record x.y" $ goldenGeneric "nestedGenitivesGML" res
        it "should turn into correct MathLang" do
          let exp = noExtraMetadata res
              res' = ML.runToMathLang Map.empty $ ML.gml2ml exp
          case res' of
            Right ml -> ml `shouldBe` MathVar "ind.friend.age"
            Left err -> err `shouldBe` "something went wrong :("

  describe "simple fun app" do
    let l4i = defaultL4I {origrules = simpleFunApp}
        res = ML.toMathLang l4i
    it "should replace vars in function that uses its arguments once" $ goldenGeneric "simpleFunAppMathLang" res
    it "should evaluate simple fun app" do
      case res of
        ([],_) -> mempty
        (expr:_,st) -> do
          let st' = st { symtabF = symtabF st <>
                          [ ("firstArg", Val Nothing 1.0)
                          , ("secondArg", Val Nothing 0.6) ]}
          (e, _xp, _st, _strs) <- xplainE emptyE st' $ eval expr
          e `shouldBe` 0.4

  describe "repeated arguments + fun app" do
    it "should replace vars in function that uses its arguments 2x each" $ goldenGeneric "complexFunAppMathLang" do
      let l4i = defaultL4I {origrules = complexFunApp}
      ML.toMathLang l4i

  describe "nested function calls, e.g. `f x y = g x y where g x y = x + y`" do
    it "in GenMathLang, should parse two user functions" $ goldenGeneric "nestedFunAppGML" do
      runToLC $ userFuns <$> l4ToLCProgram funCallsAnotherFun
    it "in MathLang, should evaluate x + y but keep track of the call of g in state" $ goldenGeneric "nestedFunAppMathLang" do
      let l4i = defaultL4I {origrules = funCallsAnotherFun}
      ML.toMathLang l4i

  describe "nested genitives + fun app" do
    let l4i = defaultL4I {origrules = nestedGenitivesInFunApp}
    it "should turn nested genitives into records" $
      goldenGeneric "nestedGenitivesInFunApp_MathLang" $ ML.toMathLang l4i

  testLCProgram globalVars
                "pausGlobalVars"
                "extract GIVENs from PAU rules"
                "should include lists and basic types"
                paus

  testLCProgram globalVars
                "arithRule4_globalVars"
                "extract GIVENs from taxesPayable rules"
                "should include enums and basic types"
                [arithRule4]

  testBaseExpify "listsum" "list types" "list has correct type in GML" listsum

  describe "list types" do
    let l4i = defaultL4I {origrules = listsum}
        res = ML.toMathLang l4i
    it "can sum a list with a single number" $ goldenGeneric "listSumMathLang" res
    it "can evaluate summing a list with a single number" do
      case res of
        ([], _) -> mempty
        (expr:_, st) -> do
          (res, _xp, _st, _strs) <- xplainE emptyE st $ eval expr

          res `shouldBe` 16.0

  describe "testPau" do
    let l4i = defaultL4I {origrules = paus}
        res = ML.toMathLang l4i
    it "actual insurance policy" $ goldenGeneric "pauMathLang" res

    it "pau as TS output" $ goldenGeneric "pau" $ do
      ML.toMathLangMw l4i defaultReaderEnv

    it "evaluate pau" do
      case res of
        ([], _) -> mempty
        (expr:_, st) -> do
          let st' = st {
            symtabF = symtabF st <>
              [ ("policyHolder.age", Val (Just "policyHolder.age") 50)
              , ("policy.benADD",  Val (Just "policy.benADD") 50)
              , ("policyHolder.past ADD payouts", Val Nothing 1000)
              , ("risk cap",  Val (Just "risk cap") 10000000)
              , ("total sum assured",  Val (Just "total sum assured") 1000)
              , ("claimable limit", Val (Just "claimable limit") 1000)
              , ("policyHolder's past ADD payouts", Val (Just "policyHolder's past ADD payouts") 0)
              ]
          , symtabP = symtabP st <>
              [ ("user input.accident_claim.selected", PredVal Nothing True)
              , ("ADD is disqualified entirely" , PredVal Nothing False)
              , ("illness.general exclusions apply", PredVal Nothing False)
              , ("accident.general exclusions apply", PredVal Nothing False)
              , ("policy.ended", PredVal Nothing False)
              , ("accident.juvenile limit applies" , PredVal Nothing True)
              , ("accident.triple benefits apply" , PredVal Nothing True)
              , ("accident.double benefits apply" , PredVal Nothing False)
              -- , ("illness.disqualified" , PredVal Nothing False)
              -- this comes from illness,IS,disqualified and not illness's disqualified
              -- gml2ml has been changed in meng's branch and probably will be merged into main?
              -- anyway, this GML->ML transformation seems to be broken in more than one way,
              -- and will be deprecated eventually, so I'm not fixing more stuff right now. /Inari 2024-07
              ]
          }
          (res, _xp, _st, _strs) <- xplainE emptyE st' $ eval expr

          res `shouldBe` 50.0

  describe "mustsing5" do
    let l4i = l4interpret defaultInterpreterOptions mustsing5

    it "should expand hornlikes from Must Sing 5" $ goldenGeneric "mustSingHornlikesExpanded" do
        expandHornlikes l4i (getHornlikes l4i)

    it "should translate Must Sing 5 into GML" $ goldenGeneric "mustSingGML" do
        toMathLangGen l4i

    it "should translate Must Sing 5 into ML without expanding rules" $ goldenGeneric "mustSingMathLang" do
        ML.toMathLang l4i

    it "should translate Must Sing 5 into ML, expanding the rules" $ goldenGeneric "mustSingExpandedMathLang" do
        ML.toMathLangExpand l4i

    it "must sing 5 in typescript, expanding the rules" $ goldenGeneric "mustSing" do
        ML.toMathLangMw l4i defaultReaderEnv

testBaseExpify :: FilePath -> String -> String -> [Rule] -> Spec
testBaseExpify fp = testLCProgram (fmap exp . lcProgram) [i|#{fp}_GML|]

testLCProgram ::
  (Show a, Eq a) =>
  (LCProgram -> a) -> FilePath -> String -> String -> [Rule] -> Spec
testLCProgram f goldPath name desc rules  =
  describe name do
    it desc $ goldenGeneric goldPath $ runToLC $ f <$> l4ToLCProgram rules


-----------------------------------------------------------------------------
-- Test rules

stringsAndBackticks :: Rule
stringsAndBackticks = parseRule [r|DECIDE `name of the book` IS "Perhaps the Stars"|]

rule3predicate :: Rule
rule3predicate = parseRule [r|
GIVEN ind IS A Person
DECIDE "ind" "qualifies a la case 3"
IF ALL ( ind IS "Singapore citizen"
       , ind's "place of residence" IS Singapore
       , ind's age >= 21
       , ind's "property annual value" <= 21000
       , ind  "meets the property eligibility criteria for GSTV-Cash"
       , ind's "annual income" <= 34000
       )|]

rule2givens :: Rule
rule2givens = parseRule [r|
GIVEN `place of residence` ; `age` ; `property annual value` ; `meets the property eligibility criteria for GSTV-Cash` ; `annual income`
DECIDE `case 2 qualifies`
    IF ALL ( `Singapore citizen`
           , `place of residence` IS `Singapore`
           , age >= 21
           , `property annual value` <= 21000
           , `meets the property eligibility criteria for GSTV-Cash`
           , `annual income` <= 34000
           )|]

rule2givens_shl_gold :: BaseHL
rule2givens_shl_gold = OneClause (HeadAndBody MkHornBodyHeadClause {
                   hbHead = RPMT [MTT "case 2 qualifies"],
                   hbBody = AA.All Nothing [AA.Leaf (RPMT [MTT "Singapore citizen"]), AA.Leaf (RPConstraint [MTT "place of residence"] RPis [MTT "Singapore"]), AA.Leaf (RPConstraint [MTT "age"] RPgte [MTI 21]), AA.Leaf (RPConstraint [MTT "property annual value"] RPlte [MTI 21000]), AA.Leaf (RPMT [MTT "meets the property eligibility criteria for GSTV-Cash"]), AA.Leaf (RPConstraint [MTT "annual income"] RPlte [MTI 34000])]
                 })

rule2nogivens :: Rule
rule2nogivens = rule2givens {given = Nothing}

arithRule1 :: Rule
arithRule1 = mkTestRule
               [ MTT "two plus two" ]
               Nothing
               [ HC { hHead = RPConstraint
                    [ MTT "m1" ] RPis
                    [ MTT "2 + 2" ]
                , hBody = Nothing } ]

arithRule2withInitializedValues :: Rule
arithRule2withInitializedValues = parseRule [r|
GIVEN m1 IS A Number ; m2 IS A Number
GIVETH result IS A Number
DECIDE m1 IS 10 ;
       m2 IS 5 ;
       result IS MINUS (m1, m2, 3, 1)
|]

arithRule2 :: Rule
arithRule2 = mkTestRule
                [ MTT "m3a" ]
                (mkGivens [("m1", Just ( SimpleType TOne "Number" )), ("m2", Just ( SimpleType TOne "Number" ))])
                [ HC { hHead = RPConstraint
                                [ MTT "m3a" ] RPis
                                [ MTT "m1"
                                , MTT "*"
                                , MTT "m2"
                                ]
                    , hBody = Nothing}
                , HC { hHead = RPnary RPis
                        [ RPMT
                            [ MTT "m3b" ]
                        , RPnary RPproduct
                            [ RPMT
                                [ MTT "m1" ]
                            , RPMT
                                [ MTT "m2" ]
                            ]
                        ]
                    , hBody = Nothing }
                , HC
                    { hHead = RPConstraint
                        [ MTT "m3c" ] RPis
                        [ MTT "m1 * m2" ]
                    , hBody = Nothing }
                , HC { hHead = RPnary RPis
                        [ RPMT
                            [ MTT "m3d" ]
                        , RPnary RPminus
                            [ RPMT
                                [ MTT "m1" ]
                            , RPMT
                                [ MTT "m2" ]
                            ]
                        ]
                    , hBody = Nothing }
                ]

arithRule3 :: Rule
arithRule3 = mkTestRule'
                [ MTT "o3a" ]
                (mkGivens [("o1", Just ( SimpleType TOne "Number" )), ("o2", Just ( SimpleType TOne "Number" ))])
                (mkGivens [("o3a_plus_times", Just ( SimpleType TOne "Number" ))])
                [ HC { hHead = RPConstraint
                        [ MTT "o3a_plus_times" ] RPis
                                [ MTT "o1 * 0.01"
                                , MTT "+"
                                , MTT "o2 * 0.07"
                                ]
                    , hBody = Nothing }
                , HC { hHead = RPConstraint
                        [ MTT "o3a_times_plus" ] RPis
                        [ MTT "o1 + 0.01"
                        , MTT "*"
                        , MTT "o2 + 0.07"
                        ]
                    , hBody = Nothing }
                , HC { hHead = RPnary RPis
                        [ RPMT
                            [ MTT "o3b" ]
                        , RPnary RPsum
                            [ RPnary RPproduct
                        [ RPMT [ MTT "o1" ]
                        , RPMT [ MTF 1.0e-2 ] ]
                            , RPnary RPproduct
                        [ RPMT [ MTT "o2" ]
                        , RPMT [ MTF 7.0e-2 ] ]
                            ]
                        ]
                    , hBody = Nothing
                    }
                , HC
                    { hHead = RPConstraint
                        [ MTT "o3c" ] RPis
                        [ MTT "o1 * 0.01 + (o2 * 0.03 + o2 * 0.04)" ]
                    , hBody = Nothing
                    }
                ]

-- All these come from TextuaL4
parseRule :: String -> Rule
parseRule = either (const RegBreach) transRule . pRule . myLexer

arithRule4 :: Rule
arithRule4 = parseRule [r|
  GIVEN annualIncome IS A Number ;
  netWorth IS A Number ;
  vivacity IS A Boolean ;
  phaseOfMoon IS ONE OF new , waxing, full, gibbous
  GIVETH  taxesPayable IS A Number
  DECIDE  taxesPayable IS `taxesPayableAlive / 2` IF phaseOfMoon IS gibbous ;

          taxesPayable  IS  taxesPayableAlive IF vivacity ;
          taxesPayable  IS  taxesPayableAlive `/` 3 IF phaseOfMoon IS waxing ;
          taxesPayable  IS  waived IF phaseOfMoon IS full ;
          taxesPayable  IS  0 OTHERWISE ;

          taxesPayableAlive       IS SUM ( `income tax component`
                                         , `asset tax component` ) ;
          `income tax component`  IS PRODUCT  ( annualIncome
                                              , incomeTaxRate
                                              ) ;
          `asset tax component`   IS PRODUCT ( netWorth
                                             , assetTaxRate );
          incomeTaxRate           IS 0.01 ;
          assetTaxRate           IS 0.07|]

nestedGenitives :: [MTExpr]
nestedGenitives = [ MTT "ind's", MTT "friend's", MTT "age"]

nestedGenitivesInFunApp :: [Rule]
nestedGenitivesInFunApp =  testLambda :
  [ mkTestRule'
      [ MTT "test function application" ]
      Nothing
      (mkGivens [("Answer", Just (SimpleType TOne "Number"))])
      [ HC { hHead = RPConstraint
            [ MTT "Answer" ] RPis
            [ MTT "ind's", MTT "friend's", MTT "age"
            , MTT "discounted by"
            , MTT "foo's", MTT "bar's", MTT "baz" ]
        , hBody = Nothing
        }]
  ]

simpleFunApp :: [Rule]
simpleFunApp = testLambda :
  [ mkTestRule'
      [ MTT "simple test function application" ]
      Nothing
      (mkGivens [("Answer", Just (SimpleType TOne "Number"))])
      [ HC { hHead = RPConstraint
            [ MTT "Answer" ] RPis
            [ MTT "firstArg"
            , MTT "discounted by"
            , MTT "secondArg" ]
        , hBody = Nothing
        }]
  ]

complexFunApp :: [Rule]
complexFunApp = testLambdaComplex :
  [ mkTestRule'
      [ MTT "complex test function application" ]
      Nothing
      (mkGivens [("Answer", Just (SimpleType TOne "Number"))])
      [ HC { hHead = RPConstraint
            [ MTT "Answer" ] RPis
            [ MTT "firstArg"
            , MTT "`funThatRepeatsArgs`"
            , MTT "secondArg" ]
        , hBody = Nothing
        }]
  ]

funCallsAnotherFun = [
    defaultHorn
    { name =
        [ MTT "The Answer" ]
    , keyword = Decide
    , given = mkGivens
                [ ("firstArgument" , Just (SimpleType TOne "Number"))
                , ("secondArgument", Just (SimpleType TOne "Number"))
                ]
    , giveth = mkGivens [("The Answer" , Just (SimpleType TOne "Number"))]
    , clauses = [ HC
            { hHead = RPConstraint
                [ MTT "The Answer" ] RPis
                [ MTT "firstArgument", MTT "f", MTT "secondArgument"]
            , hBody = Nothing
            }]
    , rlabel = Just ( "§", 2, "Top-Level" )
    },
    defaultHorn
    { name =
        [ MTT "c", MTT "f", MTT "d"]
    , keyword = Decide
    , given = mkGivens
                [ ("c", Just (SimpleType TOne "Number"))
                , ("d", Just (SimpleType TOne "Number"))
                ]
    , clauses =
        [ HC
            { hHead = RPConstraint
                [ MTT "c", MTT "f", MTT "d"
                ] RPis
                [ MTT "c", MTT "plus", MTT "d"
                ]
            , hBody = Nothing
            }
        ]
    }
    , defaultHorn
    { name =
        [ MTT "a", MTT "plus", MTT "b"
        ]
    , keyword = Decide
    , given = mkGivens
                [ ("a", Just (SimpleType TOne "Number"))
                , ("b", Just (SimpleType TOne "Number"))
                ]
    , clauses =
        [ HC
            { hHead = RPConstraint
                [ MTT "a", MTT "plus", MTT "b"
                ] RPis
                [ MTT "a", MTT "+", MTT "b"
                ]
            , hBody = Nothing
            }
        ]
    }]

mkGivens :: [(T.Text, Maybe TypeSig)] -> Maybe ParamText
mkGivens = fmap mkTypedMulti >>> nonEmpty
  where
    mkTypedMulti (t, typesig) = (MTT t :| [], typesig)

mkTestRule :: RuleName
           -> Maybe ParamText  -- given
           -> [HornClause2]    -- clauses
           -> Rule
mkTestRule n g = mkTestRule' n g Nothing

mkTestRule' :: RuleName
           -> Maybe ParamText  -- given
           -> Maybe ParamText  -- giveth
           -> [HornClause2]    -- clauses
           -> Rule
mkTestRule' name given giveth clauses = defaultHorn
    { name = name
    , keyword = Decide
    , given = given
    , giveth = giveth
    , clauses = clauses
    }

dummyMetadata :: [ExpMetadata]
dummyMetadata = [ MkExpMetadata
                    { srcPos = MkPosition
                        { row = 1, col = 1
                        }, typeLabel = Nothing, explnAnnot = Nothing
                    }
                ]

mkMetadata :: TLabel -> [ExpMetadata]
mkMetadata typelabel = [ MkExpMetadata
                    { srcPos = MkPosition
                        { row = 1, col = 1
                        }, typeLabel = Just typelabel, explnAnnot = Nothing
                    }
                ]


testLambda :: Rule
testLambda = mkTestRule
    [ MTT "discounted by" ]
    (mkGivens [("x", Just (SimpleType TOne "Number")), ("y", Just (SimpleType TOne "Number"))])
    [ HC { hHead = RPConstraint
            [ MTT "x", MTT "discounted by", MTT "y" ] RPis
            [ MTT "x * (1 - y)" ]
        , hBody = Nothing}]

testLambdaComplex :: Rule
testLambdaComplex = mkTestRule
    [ MTT "`funThatRepeatsArgs`" ]
    (mkGivens [("x", Just (SimpleType TOne "Number")), ("y", Just (SimpleType TOne "Number"))])
    [ HC { hHead = RPConstraint
            [ MTT "x", MTT "`funThatRepeatsArgs`", MTT "y" ] RPis
            [ MTT "(x + y) * ((42 - y) + x)" ]
        , hBody = Nothing}]

testCurrency :: [Rule]
testCurrency = [
  mkTestRule
    [ MTT "test currencies" ]
    Nothing
    [ HC { hHead = RPConstraint
            [ MTT "sgdTestSpaceNoComma" ] RPis
            [ MTT "SGD 42" ]
        , hBody = Nothing }
    , HC { hHead = RPConstraint
            [ MTT "sgdTestNoSpaceNoComma" ] RPis
            [ MTT "SGD42" ]
          , hBody = Nothing }
    , HC { hHead = RPConstraint
            [ MTT "eurTestSpaceComma" ] RPis
            [ MTT "€ 500,000" ]
          , hBody = Nothing }
    , HC { hHead = RPConstraint
            [ MTT "eurTestNoSpaceComma" ] RPis
            [ MTT "€500,000" ]
          , hBody = Nothing }
    , HC { hHead = RPConstraint
          [ MTT "notACurrency" ] RPis
          [ MTT "PAU4" ]
        , hBody = Nothing }
    ]
  ]

testFunApp :: [Rule]
testFunApp = testLambda :
  [ mkTestRule'
      [ MTT "test function application" ]
      Nothing
      (mkGivens [("Answer", Just (SimpleType TOne "Number"))])
      [ HC { hHead = RPConstraint
            [ MTT "Answer" ] RPis
            [ MTT "Step 3"
            , MTT "discounted by"
            , MTT "accident's"
            , MTT "risk cap" ]
        , hBody = Nothing
        }]
  ]

listsum :: [Rule]
listsum = [parseRule [r|
GIVEN listThing IS LIST OF Number ;
      singleThing IS A Number
GIVETH listSum IS A Number
DECIDE listSum IS SUM (singleThing, listThing);
listThing IS 1 2 3 ;
singleThing IS 10|]]



paus_ :: [Rule]
paus_ = parseRule <$> [pau0, pau4, pauToplevel, sub1, sub2]
 where
  pau0 = [r|
  § PAU0
  GIVEN	addBenefit IS A Number ;
        otherBenefits IS LIST OF	Number ;
        policy IS A Policy ;
        policyHolder IS A PolicyHolder ;
        accident IS AN Accident ;
        illness IS A Claim ;
        "user input" IS A Dictionary
  GIVETH	"The Answer" IS A Number
  DECIDE	"The Answer" IS	"accident branch" IF "user input's"	accident_claim IS	selected ;
          "The Answer" IS "illness branch"  OTHERWISE

  WHERE	"accident branch" IS	excludedZero  IF "ADD is disqualified entirely" ;
        "accident branch" IS "ADD benefit"  OTHERWISE ;

        "illness branch" IS excludedZero    IF illness IS disqualified ;
        "illness branch" IS policy's benMR  OTHERWISE ;

        "ADD is disqualified entirely"
                        IF ANY ( policyHolder's age	>=	75
                               , accident's "general exclusions apply"
                               , policy's ended ) ;
        excludedZero  IS	0 ;
        "ADD benefit" IS MIN ( SUM ( addBenefit
                                   , otherBenefits )
                              , "risk cap"
                              ) ;

        illness IS disqualified       IF	ANY ( illness's "general exclusions apply"
                                              , policy's ended )
        illness IS "not disqualified" OTHERWISE|]

  pau4 = [r|
  § PAU4
  GIVEN	policy IS A		Policy ;
        policyHolder IS A		PolicyHolder ;
        accident IS AN		Accident
  GIVETH	`Step 4` IS A		Number
  DECIDE	`Step 1` IS	`claimable limited base ADD benefit` IF	`there were past ADD payouts` ;
          `Step 1` IS `base ADD benefit`			             OTHERWISE ;
          `Step 2` IS	`juvenile limited`	IF	accident's `juvenile limit applies` ;
          `Step 2` IS `Step 1`			      OTHERWISE ;
          `Step 3` IS	`multiplied by double triple benefit` ;
          `Step 4` IS	`Step 3` `discounted by`	accident's `risk percentage`

  WHERE	`base ADD benefit` IS	policy's benADD ;
        `there were past ADD payouts` IF	policyHolder's `past ADD payouts`		>	0 ;
        `claimable limited base ADD benefit` IS	MINUS (`claimable limit`,	`policyHolder's past ADD payouts`) ;

        `juvenile limited` IS	MIN ( `Step 1`
                                  , `juvenile limit` ) ;
        `multiplied by double triple benefit` IS	PRODUCT (`Step 2`, 3)
                                              IF	accident's `triple benefits apply` ;
        `multiplied by double triple benefit` IS	PRODUCT (`Step 2`, 2)
                                              IF	accident's `double benefits apply` ;
        `multiplied by double triple benefit` IS	`Step 2` OTHERWISE
  |]
  pauToplevel = [r|
  §	Top-Level
  GIVEN	policy IS A Policy ;
        policyHolder IS A PolicyHolder ;
        accident IS AN Accident ;
        illness IS A Claim ;
        `user input` IS A Dictionary
  GIVETH	`How Much Money Do You Get` IS A Number
  DECIDE	`How Much Money Do You Get` IS PAU0
  WHERE	addBenefit    IS PAU4 ;
        otherBenefits IS 50|]
  sub1 = [r|
  §	`subsidiary computations`
  GIVEN	`total sum assured` IS A		Number
  DECIDE	`claimable limit` IS
              MIN	( PRODUCT ( 1.5
                            , `total sum assured` )
                  , `lifetime claimable limit`
              ) ;


          `lifetime claimable limit`  IS `$4,500,000` ;
          `juvenile limit` IS	`$500,000`|]
  sub2 = [r|
  GIVEN	x IS A	Number ;
        y IS A	Number
  DECIDE	x	`discounted by`	y	IS	`x * (1 - y)`
  |]

paus :: [Rule]
paus = [
  Hornlike
    { name =
        [ MTT "The Answer" ]
    , super = Nothing
    , keyword = Decide
    , given = Just
        (
            ( MTT "addBenefit" :| []
            , Just (SimpleType TOne "Number") )
            :|
            [ ( MTT "otherBenefits" :| []
              , Just (SimpleType TList1 "Number") )
            , ( MTT "policy" :| []
              , Just (SimpleType TOne "Policy") )
            , ( MTT "policyHolder" :| []
              , Just (SimpleType TOne "PolicyHolder") )
            , ( MTT "accident" :| []
              , Just (SimpleType TOne "Accident") )
            , ( MTT "illness" :| []
              , Just (SimpleType TOne "Claim") )
            , ( MTT "user input" :| []
              , Just (SimpleType TOne "Dictionary") )
            ]
        )
    , giveth = Just
        (
            ( MTT "The Answer" :| []
            , Just
                ( SimpleType TOne "Number" )
            ) :| []
        )
    , upon = Nothing
    , clauses =
        [ HC
            { hHead = RPConstraint
                [ MTT "The Answer" ] RPis
                [ MTT "accident branch" ]
            , hBody = Just
                ( Leaf
                    ( RPConstraint
                        [ MTT "user input's"
                        , MTT "accident_claim"] RPis
                        [ MTT "selected" ]
                    )
                )
            }
        , HC
            { hHead = RPConstraint
                [ MTT "The Answer" ] RPis
                [ MTT "illness branch" ]
            , hBody = Just
                ( Leaf
                    ( RPMT
                        [ MTT "OTHERWISE" ]
                    )
                )
            }
        ]
    , rlabel = Just
        ( "§"
        , 2
        , "PAU0"
        )
    , lsource = Nothing
    , wwhere =
        [ Hornlike
            { name =
                [ MTT "accident branch" ]
            , super = Nothing
            , keyword = Where
            , given = Nothing
            , giveth = Nothing
            , upon = Nothing
            , clauses =
                [ HC
                    { hHead = RPConstraint
                        [ MTT "accident branch" ] RPis
                        [ MTT "excludedZero" ]
                    , hBody = Just
                        ( Leaf
                            ( RPMT
                                [ MTT "ADD is disqualified entirely" ]
                            )
                        )
                    }
                , HC
                    { hHead = RPConstraint
                        [ MTT "accident branch" ] RPis
                        [ MTT "ADD benefit" ]
                    , hBody = Just
                        ( Leaf
                            ( RPMT
                                [ MTT "OTHERWISE" ]
                            )
                        )
                    }
                , HC
                    { hHead = RPConstraint
                        [ MTT "illness branch" ] RPis
                        [ MTT "excludedZero" ]
                    , hBody = Just
                        ( Leaf
                            ( RPConstraint
                                [ MTT "illness" ] RPis
                                [ MTT "disqualified" ]
                            )
                        )
                    }
                , HC
                    { hHead = RPConstraint
                        [ MTT "illness branch" ] RPis
                        [ MTT "policy's"
                        , MTT "benMR"
                        ]
                    , hBody = Just
                        ( Leaf
                            ( RPMT
                                [ MTT "OTHERWISE" ]
                            )
                        )
                    }
                , HC
                    { hHead = RPMT
                        [ MTT "ADD is disqualified entirely" ]
                    , hBody = Just
                        ( Any Nothing
                            [ Leaf
                                ( RPConstraint
                                    [ MTT "policyHolder's"
                                    , MTT "age"
                                    ] RPgte
                                    [ MTI 75 ]
                                )
                            , Leaf
                                ( RPMT
                                    [ MTT "accident's"
                                    , MTT "general exclusions apply"
                                    ]
                                )
                            , Leaf
                                ( RPMT
                                    [ MTT "policy's"
                                    , MTT "ended"
                                    ]
                                )
                            ]
                        )
                    }
                , HC
                    { hHead = RPConstraint
                        [ MTT "excludedZero" ] RPis
                        [ MTI 0 ]
                    , hBody = Nothing
                    }
                , HC
                    { hHead = RPnary RPis
                        [ RPMT
                            [ MTT "ADD benefit" ]
                        , RPnary RPmin
                            [ RPnary RPsum
                                [ RPMT
                                    [ MTT "addBenefit" ]
                                , RPMT
                                    [ MTT "otherBenefits" ]
                                ]
                            , RPMT
                                [ MTT "risk cap" ]
                            ]
                        ]
                    , hBody = Nothing
                    }
                , HC
                    { hHead = RPConstraint
                        [ MTT "illness" ] RPis
                        [ MTT "disqualified" ]
                    , hBody = Just
                        ( Any Nothing
                            [ Leaf
                                ( RPMT
                                    [ MTT "illness's"
                                    , MTT "general exclusions apply"
                                    ]
                                )
                            , Leaf
                                ( RPMT
                                    [ MTT "policy's"
                                    , MTT "ended"
                                    ]
                                )
                            ]
                        )
                    }
                ]
            , rlabel = Nothing
            , lsource = Nothing
            , wwhere = []
            , srcref = Just
                ( SrcRef
                    { url = "test/Spec"
                    , short = "test/Spec"
                    , srcrow = 1
                    , srccol = 1
                    , version = Nothing
                    }
                )
            , defaults = []
            , symtab = []
            }
        ]
    , srcref = Just
        ( SrcRef
            { url = "test/PAUs.csv"
            , short = "test/PAUs.csv"
            , srcrow = 4
            , srccol = 52
            , version = Nothing
            }
        )
    , defaults = []
    , symtab = []
    }
  , Hornlike
    { name =
        [ MTT "Step 1" ]
    , super = Nothing
    , keyword = Decide
    , given = Just
        (
            ( MTT "policy" :| []
            , Just
                ( SimpleType TOne "Policy" )
            ) :|
            [
                ( MTT "policyHolder" :| []
                , Just
                    ( SimpleType TOne "PolicyHolder" )
                )
            ,
                ( MTT "accident" :| []
                , Just
                    ( SimpleType TOne "Accident" )
                )
            ]
        )
    , giveth = Just
        (
            ( MTT "Step 4" :| []
            , Just
                ( SimpleType TOne "Number" )
            ) :| []
        )
    , upon = Nothing
    , clauses =
        [ HC
            { hHead = RPConstraint
                [ MTT "Step 1" ] RPis
                [ MTT "claimable limited base ADD benefit" ]
            , hBody = Just
                ( Leaf
                    ( RPMT
                        [ MTT "there were past ADD payouts" ]
                    )
                )
            }
        , HC
            { hHead = RPConstraint
                [ MTT "Step 1" ] RPis
                [ MTT "base ADD benefit" ]
            , hBody = Just
                ( Leaf
                    ( RPMT
                        [ MTT "OTHERWISE" ]
                    )
                )
            }
        , HC
            { hHead = RPConstraint
                [ MTT "Step 2" ] RPis
                [ MTT "juvenile limited" ]
            , hBody = Just
                ( Leaf
                    ( RPMT
                        [ MTT "accident's"
                        , MTT "juvenile limit applies"
                        ]
                    )
                )
            }
        , HC
            { hHead = RPConstraint
                [ MTT "Step 2" ] RPis
                [ MTT "Step 1" ]
            , hBody = Just
                ( Leaf
                    ( RPMT
                        [ MTT "OTHERWISE" ]
                    )
                )
            }
        , HC
            { hHead = RPConstraint
                [ MTT "Step 3" ] RPis
                [ MTT "multiplied by double triple benefit" ]
            , hBody = Nothing
            }
        , HC
            { hHead = RPConstraint
                [ MTT "Step 4" ] RPis
                [ MTT "Step 3"
                , MTT "discounted by"
                , MTT "accident's"
                , MTT "risk percentage"
                ]
            , hBody = Nothing
            }
        ]
    , rlabel = Just
        ( "§"
        , 2
        , "PAU4"
        )
    , lsource = Nothing
    , wwhere =
        [ Hornlike
            { name =
                [ MTT "base ADD benefit" ]
            , super = Nothing
            , keyword = Where
            , given = Nothing
            , giveth = Nothing
            , upon = Nothing
            , clauses =
                [ HC
                    { hHead = RPConstraint
                        [ MTT "base ADD benefit" ] RPis
                        [ MTT "policy's"
                        , MTT "benADD"
                        ]
                    , hBody = Nothing
                    }
                , HC
                    { hHead = RPMT
                        [ MTT "there were past ADD payouts" ]
                    , hBody = Just
                        ( Leaf
                            ( RPConstraint
                                [ MTT "policyHolder's"
                                , MTT "past ADD payouts"
                                ] RPgt
                                [ MTI 0 ]
                            )
                        )
                    }
                , HC
                    { hHead = RPConstraint
                        [ MTT "claimable limited base ADD benefit" ] RPis
                        [ MTT "claimable limit"
                        , MTT "-"
                        , MTT "policyHolder's"
                        , MTT "past ADD payouts"
                        ]
                    , hBody = Nothing
                    }
                , HC
                    { hHead = RPnary RPis
                        [ RPMT
                            [ MTT "juvenile limited" ]
                        , RPnary RPmin
                            [ RPMT
                                [ MTT "Part 1" ]
                            , RPMT
                                [ MTT "juvenile limit" ]
                            ]
                        ]
                    , hBody = Nothing
                    }
                , HC
                    { hHead = RPConstraint
                        [ MTT "multiplied by double triple benefit" ] RPis
                        [ MTT "Step 2"
                        , MTT "*"
                        , MTI 3
                        ]
                    , hBody = Just
                        ( Leaf
                            ( RPMT
                                [ MTT "accident's"
                                , MTT "triple benefits apply"
                                ]
                            )
                        )
                    }
                , HC
                    { hHead = RPConstraint
                        [ MTT "multiplied by double triple benefit" ] RPis
                        [ MTT "Step 2"
                        , MTT "*"
                        , MTI 2
                        ]
                    , hBody = Just
                        ( Leaf
                            ( RPMT
                                [ MTT "accident's"
                                , MTT "double benefits apply"
                                ]
                            )
                        )
                    }
                , HC
                    { hHead = RPConstraint
                        [ MTT "multiplied by double triple benefit" ] RPis
                        [ MTT "Step 2" ]
                    , hBody = Just
                        ( Leaf
                            ( RPMT
                                [ MTT "OTHERWISE" ]
                            )
                        )
                    }
                ]
            , rlabel = Nothing
            , lsource = Nothing
            , wwhere = []
            , srcref = Just
                ( SrcRef
                    { url = "test/Spec"
                    , short = "test/Spec"
                    , srcrow = 1
                    , srccol = 1
                    , version = Nothing
                    }
                )
            , defaults = []
            , symtab = []
            }
        ]
    , srcref = Just
        ( SrcRef
            { url = "test/PAUs.csv"
            , short = "test/PAUs.csv"
            , srcrow = 4
            , srccol = 89
            , version = Nothing
            }
        )
    , defaults = []
    , symtab = []
    }
  , Hornlike
    { name =
        [ MTT "How Much Money Do You Get" ]
    , super = Nothing
    , keyword = Decide
    , given = Just
        (
            ( MTT "policy" :| []
            , Just
                ( SimpleType TOne "Policy" )
            ) :|
            [
                ( MTT "policyHolder" :| []
                , Just
                    ( SimpleType TOne "PolicyHolder" )
                )
            ,
                ( MTT "accident" :| []
                , Just
                    ( SimpleType TOne "Accident" )
                )
            ,
                ( MTT "illness" :| []
                , Just
                    ( SimpleType TOne "Claim" )
                )
            ,
                ( MTT "user input" :| []
                , Just
                    ( SimpleType TOne "Dictionary" )
                )
            ]
        )
    , giveth = Just
        (
            ( MTT "How Much Money Do You Get" :| []
            , Just
                ( SimpleType TOne "Number" )
            ) :| []
        )
    , upon = Nothing
    , clauses =
        [ HC
            { hHead = RPConstraint
                [ MTT "How Much Money Do You Get" ] RPis
                [ MTT "PAU0" ]
            , hBody = Nothing
            }
        ]
    , rlabel = Just
        ( "§"
        , 2
        , "Top-Level"
        )
    , lsource = Nothing
    , wwhere =
        [ Hornlike
            { name =
                [ MTT "addBenefit" ]
            , super = Nothing
            , keyword = Where
            , given = Nothing
            , giveth = Nothing
            , upon = Nothing
            , clauses =
                [ HC
                    { hHead = RPConstraint
                        [ MTT "addBenefit" ] RPis
                        [ MTT "PAU4" ]
                    , hBody = Nothing
                    }
                , HC
                    { hHead = RPConstraint
                        [ MTT "otherBenefits" ] RPis
                        [ MTI 50
                        -- , MTI 2
                        -- , MTI 3
                        -- , MTI 4
                        ]
                    , hBody = Nothing
                    }
                , HC
                    { hHead = RPConstraint
                        [ MTT "policyHolder's", MTT "age" ] RPis
                        [ MTI 50 ]
                    , hBody = Nothing
                    }
                ]
            , rlabel = Nothing
            , lsource = Nothing
            , wwhere = []
            , srcref = Just
                ( SrcRef
                    { url = "test/Spec"
                    , short = "test/Spec"
                    , srcrow = 1
                    , srccol = 1
                    , version = Nothing
                    }
                )
            , defaults = []
            , symtab = []
            }
        ]
    , srcref = Just
        ( SrcRef
            { url = "test/PAUs.csv"
            , short = "test/PAUs.csv"
            , srcrow = 4
            , srccol = 112
            , version = Nothing
            }
        )
    , defaults = []
    , symtab = []
    }
  , Hornlike
    { name =
        [ MTT "claimable limit" ]
    , super = Nothing
    , keyword = Decide
    , given = Just
        (
            ( MTT "total sum assured" :| []
            , Just
                ( SimpleType TOne "Number" )
            ) :| []
        )
    , giveth = Nothing
    , upon = Nothing
    , clauses =
        [ HC
            { hHead = RPnary RPis
                [ RPMT
                    [ MTT "claimable limit" ]
                , RPnary RPmin
                    [ RPnary RPproduct
                        [ RPMT
                            [ MTF 1.5 ]
                        , RPMT
                            [ MTT "total sum assured" ]
                        ]
                    , RPMT
                        [ MTT "lifetime claimable limit" ]
                    ]
                ]
            , hBody = Nothing
            }
        ]
    , rlabel = Just
        ( "§"
        , 3
        , "subsidiary computations"
        )
    , lsource = Nothing
    , wwhere = []
    , srcref = Just
        ( SrcRef
            { url = "test/PAUs.csv"
            , short = "test/PAUs.csv"
            , srcrow = 4
            , srccol = 124
            , version = Nothing
            }
        )
    , defaults = []
    , symtab = []
    }
  , Hornlike
    { name =
        [ MTT "x"
        , MTT "discounted by"
        , MTT "y"
        ]
    , super = Nothing
    , keyword = Decide
    , given = Just
        (
            ( MTT "x" :| []
            , Just
                ( SimpleType TOne "Number" )
            ) :|
            [
                ( MTT "y" :| []
                , Just
                    ( SimpleType TOne "Number" )
                )
            ]
        )
    , giveth = Nothing
    , upon = Nothing
    , clauses =
        [ HC
            { hHead = RPConstraint
                [ MTT "x"
                , MTT "discounted by"
                , MTT "y"
                ] RPis
                [ MTT "x * (1 - y)" ]
            , hBody = Nothing
            }
        ]
    , rlabel = Nothing
    , lsource = Nothing
    , wwhere = []
    , srcref = Just
        ( SrcRef
            { url = "test/PAUs.csv"
            , short = "test/PAUs.csv"
            , srcrow = 4
            , srccol = 131
            , version = Nothing
            }
        )
    , defaults = []
    , symtab = []
    }
  , Hornlike
    { name =
        [ MTT "lifetime claimable limit" ]
    , super = Nothing
    , keyword = Decide
    , given = Nothing
    , giveth = Nothing
    , upon = Nothing
    , clauses =
        [ HC
            { hHead = RPConstraint
                [ MTT "lifetime claimable limit" ] RPis
                [ MTT "$4,500,000" ]
            , hBody = Nothing
            }
        , HC
            { hHead = RPConstraint
                [ MTT "juvenile limit" ] RPis
                [ MTT "$500,000" ]
            , hBody = Nothing
            }
        ]
    , rlabel = Nothing
    , lsource = Nothing
    , wwhere = []
    , srcref = Just
        ( SrcRef
            { url = "test/PAUs.csv"
            , short = "test/PAUs.csv"
            , srcrow = 4
            , srccol = 136
            , version = Nothing
            }
        )
    , defaults = []
    , symtab = []
    }
  ]

pausGlobalVarsGold :: GlobalVars
pausGlobalVarsGold = MkGlobalVars [
  ( MkVar "x", Just ( L4EntityType "Number" ) ),
  ( MkVar "y", Just ( L4EntityType "Number" ) ),
  ( MkVar "user input", Just ( L4EntityType "Dictionary" ) ),
  ( MkVar "accident", Just ( L4EntityType "Accident" ) ),
  ( MkVar "addBenefit", Just ( L4EntityType "Number" ) ),
  ( MkVar "otherBenefits", Just ( L4List ( L4EntityType "Number" ) ) ),
  ( MkVar "total sum assured", Just ( L4EntityType "Number" ) ),
  ( MkVar "policyHolder", Just ( L4EntityType "PolicyHolder" ) ),
  ( MkVar "policy", Just ( L4EntityType "Policy" ) ),
  ( MkVar "illness", Just ( L4EntityType "Claim" ) )
  ]

emptyE :: Map.HashMap () ()
emptyE = mempty

mustsing5 :: [Rule]
mustsing5 = parseRule <$> [reg, drinks, person, qualifies]
 where
  person = [r|DECLARE Person
  HAS drinks IS A Boolean ;
      eats IS A Boolean ;
      walks IS A Boolean ;
      alcoholic IS A Boolean ;
      `non-alcoholic` IS A Boolean ;
      `in part` IS A Boolean ;
      `in whole` IS A Boolean|]

  reg = [r|
  EVERY Person
    WHO Qualifies
    MUST sing|]
  qualifies = "Qualifies MEANS ALL(walks, ANY(Drinks, eats))"
  drinks = [r|
  Drinks MEANS
    ALL(`consumes an` ANY(alcoholic, `non-alcoholic`) beverage
       , whether ANY(`in part`, `in whole`))
    |]
