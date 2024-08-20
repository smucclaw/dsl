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
import LS.Interpreter (Interpreted (..), defaultL4I, l4interpret)
import LS.Rule (Rule (..), defaultReg, defaultHorn, defaultTypeDecl)
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
              ]
          }
          (res, _xp, _st, _strs) <- xplainE emptyE st' $ eval expr

          res `shouldBe` 550.0

  describe "mustsing5" do
    let l4i = l4interpret mustsing5

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

emptyE :: Map.HashMap () ()
emptyE = mempty

-----------------------------------------------------------------------------
-- Gold versions (that are not in files)

rule2givens_shl_gold :: BaseHL
rule2givens_shl_gold = OneClause (HeadAndBody MkHornBodyHeadClause {
                   hbHead = RPMT [MTT "case 2 qualifies"],
                   hbBody = AA.All Nothing [AA.Leaf (RPMT [MTT "Singapore citizen"]), AA.Leaf (RPConstraint [MTT "place of residence"] RPis [MTT "Singapore"]), AA.Leaf (RPConstraint [MTT "age"] RPgte [MTI 21]), AA.Leaf (RPConstraint [MTT "property annual value"] RPlte [MTI 21000]), AA.Leaf (RPMT [MTT "meets the property eligibility criteria for GSTV-Cash"]), AA.Leaf (RPConstraint [MTT "annual income"] RPlte [MTI 34000])]
                 })

-----------------------------------------------------------------------------
-- Test rules

-- | From TextuaL4
parseRule :: String -> Rule
parseRule = either (const RegBreach) transRule . pRule . myLexer

stringsAndBackticks :: Rule
stringsAndBackticks = parseRule [r|DECIDE `name of the book` IS "Perhaps the Stars"|]

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

rule2nogivens :: Rule
rule2nogivens = rule2givens {given = Nothing}

arithRule1 :: Rule
arithRule1 = parseRule [r|DECIDE m1 IS `2 + 2`|]

arithRule2withInitializedValues :: Rule
arithRule2withInitializedValues = parseRule [r|
GIVEN m1 IS A Number ; m2 IS A Number
GIVETH result IS A Number
DECIDE m1 IS 10 ;
       m2 IS 5 ;
       result IS MINUS (m1, m2, 3, 1)
|]

arithRule2 :: Rule
arithRule2 = parseRule [r|
GIVEN m1 IS A Number ;
      m2 IS A Number
DECIDE m3a IS m1 `*` m2 ;
       m3b IS PRODUCT (m1, m2) ;
       m3c IS `m1 * m2` ;
       m3d IS MINUS (m1, m2)|]


arithRule3 :: Rule
arithRule3 = parseRule [r|
GIVEN o1 IS A Number ;
      o2 IS A Number
GIVETH o3a_plus_times IS A Number
DECIDE o3a_plus_times IS `o1 * 0.01` `+` `o2 * 0.07` ;
       o3a_times_plus IS `o1 + 0.01` `*` `o2 + 0.07` ;
       o3b IS SUM( PRODUCT(o1, 0.01) , PRODUCT (o2, 0.07) ) ;
       o3c IS `o1 * 0.01 + (o2 * 0.03 + o2 * 0.04)`|]

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

testLambda :: Rule
testLambda = parseRule [r|
GIVEN x IS A Number ; y IS A Number
DECIDE x `discounted by` y IS `x * (1 - y)`|]


testLambdaComplex :: Rule
testLambdaComplex = parseRule [r|
GIVEN x IS A Number ; y IS A Number
DECIDE x `funThatRepeatsArgs` y IS `(x + y) * ((42 - y) + x)`|]

testFunApp :: [Rule]
testFunApp = testLambda :
  [ parseRule [r|
    GIVETH Answer IS A Number
    DECIDE Answer IS `Step 3` `discounted by` accident's `risk cap`|]
  ]

nestedGenitives :: [MTExpr]
nestedGenitives = [ MTT "ind's", MTT "friend's", MTT "age"]

nestedGenitivesInFunApp :: [Rule]
nestedGenitivesInFunApp = testLambda :
  [ parseRule [r|
    GIVETH Answer IS A Number
    DECIDE Answer IS `ind's` `friend's` `age` `discounted by` `foo's` `bar's` `baz`|]
  ]

simpleFunApp :: [Rule]
simpleFunApp = testLambda :
  [ parseRule [r|
    GIVETH Answer IS A Number
    DECIDE Answer IS firstArg `discounted by` secondArg|]
  ]


complexFunApp :: [Rule]
complexFunApp = testLambdaComplex :
  [ parseRule [r|
    GIVETH Answer IS A Number
    DECIDE Answer IS firstArg `funThatRepeatsArgs` secondArg|]
  ]

funCallsAnotherFun :: [Rule]
funCallsAnotherFun = parseRule <$> [toplevel, f, plus]
 where
  toplevel = [r|
  § Top-Level
  GIVEN firstArgument ; secondArgument
  GIVETH `The Answer` IS A Number
  DECIDE `The Answer` IS firstArgument `f` secondArgument|]
  f = [r|
  GIVEN c ; d
  DECIDE c `f` d IS c `plus` d|]
  plus = [r|
  GIVEN a ; b
  DECIDE a `plus` b IS `a + b`|]

testCurrency :: [Rule]
testCurrency = [parseRule [r|
DECIDE sgdTestSpaceNoComma IS `SGD 42` ;
       sgdTestNoSpaceNoComma IS SGD42 ;
       eurTestSpaceComma IS `€ 500,000` ;
       eurTestNoSpaceComma IS `€500,000` ;
       notACurrency IS PAU4|]]

listsum :: [Rule]
listsum = [parseRule [r|
GIVEN listThing IS LIST OF Number ;
      singleThing IS A Number
GIVETH listSum IS A Number
DECIDE listSum IS SUM (singleThing, listThing);
listThing IS 1 2 3 ;
singleThing IS 10|]]

paus :: [Rule]
paus = parseRule <$> [pau0, pau4, pauToplevel, sub1, sub2]
 where
  pau0 = [r|
  § PAU0
  GIVEN	addBenefit IS A Number ;
        otherBenefits IS LIST OF	Number ;
        policy IS A Policy ;
        policyHolder IS A PolicyHolder ;
        accident IS AN Accident ;
        illness IS A Claim ;
        `user input` IS A Dictionary
  GIVETH	`The Answer` IS A Number
  DECIDE	`The Answer` IS	`accident branch` IF `user input's`	accident_claim IS	selected ;
          `The Answer` IS `illness branch`  OTHERWISE

  WHERE	`accident branch` IS	excludedZero  IF `ADD is disqualified entirely` ;
        `accident branch` IS `ADD benefit`  OTHERWISE ;

        `illness branch` IS excludedZero    IF illness IS disqualified ;
        `illness branch` IS policy's benMR  OTHERWISE ;

        `ADD is disqualified entirely`
                        IF ANY ( policyHolder's age	>=	75
                               , accident's `general exclusions apply`
                               , policy's ended ) ;
        excludedZero  IS	0 ;
        `ADD benefit` IS MIN ( SUM ( addBenefit
                                   , otherBenefits )
                              , `risk cap`
                              ) ;

        illness IS disqualified       IF	ANY ( illness's `general exclusions apply`
                                              , policy's ended )
                                              |]

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
        `claimable limited base ADD benefit` IS	MINUS (`claimable limit`,	policyHolder's `past ADD payouts`) ;

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
        otherBenefits IS 50 ;
        policyHolder's age IS 50|]
  sub1 = [r|
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
