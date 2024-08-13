{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LS.TextuaL4Spec (spec) where

import LS.Rule
import LS.Types
import TextuaL4.Transform
import TextuaL4.LexTextuaL   ( Token )
import TextuaL4.ParTextuaL   ( pRule, pListRule, myLexer )
import Text.Pretty.Simple ( pShowNoColor )
import Text.RawString.QQ ( r )
import Data.List ( intercalate )
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import System.FilePath ( (<.>), (</>) )
import Test.Hspec.Golden
import Test.Hspec (Spec, describe, it, shouldBe)
import Data.Either (fromRight)

goldenGeneric :: Show a => String -> a -> Golden TL.Text
goldenGeneric name output_ = Golden
  { output = pShowNoColor output_
  , encodePretty = TL.unpack
  , writeToFile = TL.writeFile
  , readFromFile = TL.readFile
  , goldenFile =  testPath <.> "expected"
  , actualFile = Just (testPath <.> "actual")
  , failFirstTime = False
  }
  where
    testPath = "test" </> "testdata" </> "golden" </> "TextuaL4Spec" </> name

spec :: Spec
spec = do
  describe "TextuaL4 output" do
    test "GIVETH x ; y DECIDE x IS 5; y IS 4" "hornlike-2-giveths"
    test "GIVEN foo GIVETH bar DECIDE bar IS 1 > 2" "hornlike-given-giveth-1"
    test "GIVEN foo IS A Number GIVETH bar IS A Boolean DECIDE bar IS foo > 2" "hornlike-given-giveth-2"
    test "EVERY tame ANY (Person, Animal) WHO Qualifies MEANS ALL(walks, ANY(eats, drinks), climbs) MUST ANY (sing, dance)" "regulative-any-tame-person-animal"
    test [r|DECIDE `name of the book` IS "Perhaps the Stars"|] "strings-and-backticks"
    test' rodents "rodents and vermin" "rodents-and-vermin"
    test' taxesPayable "taxes payable" "taxes-payable"
    testList pauTypedecls "PAU typedecls only" "pau-typedecls"
    testList pauRules "PAU rules" "pau-bnfc"
  where
    test rule = test' rule rule

    test' rule desc fname = do
      let res :: Rule = fromRight RegBreach $ run rule
      it desc $ goldenGeneric fname res

    testList rule desc fname = do
      let res :: [Rule] = fromRight [] $ runList rule
      it desc $ goldenGeneric fname res

rodents = [r|§ `Rodents and vermin`
DECIDE `Not Covered`
IF
   UNLESS ( `Loss or Damage` IS ANY ( `caused by rodents`
                                    , `caused by insects`
                                    , `caused by vermin`
                                    , `caused by birds`
                                    )

          , ANY ( ALL ( `Loss or Damage` IS `to Contents`
                      , `Loss or Damage` IS `caused by birds`
                      )

                , UNLESS ( `Loss or Damage` IS `ensuing covered loss`

                         , ANY ( `any other exclusion applies`
                               , `an animal caused water to escape from`
                                    ANY ( `a household appliance`
                                        , `a swimming pool`
                                        , `a plumbing, heating, or air conditioning system` )
                               )
                         )
                )
        )|]

taxesPayable = [r|
§ taxesPayable
GIVEN annualIncome IS A Number ;
netWorth IS A Number ;
vivacity IS A Boolean ;
phaseOfMoon IS ONE OF new , waxing, full, gibbous
GIVETH  taxesPayable IS A Number
DECIDE  taxesPayable IS taxesPayableAlive `/` 2 IF phaseOfMoon IS gibbous ;

        taxesPayable  IS  taxesPayableAlive IF vivacity ;
        taxesPayable  IS  DIVIDE (taxesPayableAlive, 3) IF phaseOfMoon IS waxing ;
        taxesPayable  IS  waived IF phaseOfMoon IS full ;
        taxesPayable  IS  0 OTHERWISE ;

        taxesPayableAlive       IS SUM ( `income tax component`
                                       , `asset tax component` ) ;
        `income tax component`  IS PRODUCT	( annualIncome
                                            , PRODUCT (incomeTaxRate
                                                      , MINUS ( 2
                                                              , 1 )
                                                      )
                                            ) ;
        `asset tax component`   IS PRODUCT ( netWorth
                                           , assetTaxRate );
        incomeTaxRate           IS 0.01 ;
        assetTaxRate           IS 0.07|]

pauTypedecls = [r|
DECLARE	PolicyHolder IS A Person
HAS	address        IS A Address	;
    birthdate      IS A Date ;
    `current policy` IS A Policy ;
    `past policies` IS LIST OF Policy ;
    `addSA over all policies`  IS A Money ;
    `past ADD payouts`  IS A Money ;
    `death date`  IS OPTIONAL Date
§
DECLARE	Policy   IS A	Contract
HAS	planAF   IS A	PlanAF ;
    benADD   IS A	Benefit ;
    benMR   IS A	Benefit ;
    benTCM   IS A	Benefit ;
    benRA   IS	OPTIONAL Benefit ;
    benFCPA   IS	OPTIONAL Benefit ;
    accidents IS	LIST OF	Accident ;
    `start date`   IS A	Date ;
    ended   IS A	Boolean
§
DECLARE	Claim
HAS	date IS A	Date ;
    outcome IS	ONE OF	Successful, Unsuccessful ;
    amount IS A	Money ;
    `general exclusions apply` IS A	Boolean
§
DECLARE	Accident
HAS	injuries		IS	SET OF	loss ;
   `general exclusions apply` IS A	Boolean ;
   `juvenile limit applies` IS A	Boolean ;
   `triple benefits apply` IS A	Boolean ;
   `double benefits apply` IS A	Boolean ;
   `risk percentage` IS A	Number
§
DECLARE	Benefit
HAS	`initial sum assured`			IS A	Number ;
    stepUpSumAssured			IS A	Number ;
    name			IS A	String ;
    claim			IS A	Claim
§
DECLARE	PlanAF			IS	ONE OF	planA, planB, planC, planD, planE, planF
§
DECLARE	Plan14			IS	ONE OF	plan1, plan2, plan3, plan4
|]

pauRules = intercalate "§" [pau0, pau4, toplevel, subsidiary]
pau0 = [r|
§ PAU0
GIVEN	addBenefit IS A Number ;
      otherBenefits IS LIST OF	Number ;
      policy IS A Policy ;
      policyHolder IS A PolicyHolder ;
      accident IS AN Accident ;
      illness IS A Claim ;
      `user input` IS A		Dictionary
GIVETH	`The Answer` IS A Number
DECIDE	`The Answer` IS	`accident branch`   IF `user input's`	accident_claim IS	selected ;
        `The Answer` IS `act of god branch`	IF	`user input's`	`act of god` IS	selected ;
        `The Answer` IS `illness branch`		OTHERWISE

WHERE	`accident branch`		IS	excludedZero  IF `ADD is disqualified entirely` ;
      `accident branch` IS `ADD benefit`    OTHERWISE ;

      `illness branch`  IS	excludedZero    IF		illness	IS	disqualified ;
      `illness branch` IS policy's benMR    OTHERWISE ;

	    `ADD is disqualified entirely`
                      IF ANY ( policyHolder's age	>=	75
                            , accident's `general exclusions apply`
                            , policy's ended ) ;
	    excludedZero		IS	0 ;
	    `ADD benefit`		IS MIN ( SUM ( addBenefit
                                   , otherBenefits)
                             , `risk cap`
                             ) ;

	    illness	IS	disqualified
                      IF	ANY ( illness's `general exclusions apply`
                              , policy's ended )
|]

pau4 = [r|
§ PAU4
GIVEN	policy IS A		Policy ;
      policyHolder IS A		PolicyHolder ;
      accident IS AN		Accident
GIVETH	`Step 4` IS A		Number
DECIDE	`Step 1` IS	`claimable limited base ADD benefit`			IF	`there were past ADD payouts` ;
        `Step 1` IS `base ADD benefit`			OTHERWISE ;
        `Step 2` IS	`juvenile limited`			IF	accident's 	`juvenile limit applies` ;
        `Step 2` IS `Step 1`			OTHERWISE ;
        `Step 3` IS	`multiplied by double triple benefit` ;
        `Step 4` IS	`Step 3` `discounted by`	accident's `risk percentage`

WHERE	`base ADD benefit` IS	policy's benADD ;
	    `there were past ADD payouts` IF	policyHolder's `past ADD payouts` >	0 ;
	    `claimable limited base ADD benefit` IS	`claimable limit`	`-` policyHolder's `past ADD payouts` ;

	    `juvenile limited` IS	MIN ( `Step 1`
                                , `juvenile limit` ) ;
	    `multiplied by double triple benefit` IS	`Step 2` `*` 3
                                            IF	accident's `triple benefits apply` ;
      `multiplied by double triple benefit` IS	`Step 2` `*` 2
                                            IF	accident's `double benefits apply` ;
      `multiplied by double triple benefit` IS	`Step 2` OTHERWISE
|]

toplevel = [r|
§	Top-Level
GIVEN	policy IS A Policy ;
      policyHolder IS A PolicyHolder ;
      accident IS AN Accident ;
      illness IS A Claim ;
      `user input` IS A Dictionary
GIVETH	`How Much Money Do You Get` IS A Number
DECIDE	`How Much Money Do You Get` IS PAU0
WHERE	addBenefit    IS PAU4 ;
      otherBenefits IS 1 2 3 4
|]

subsidiary = [r|
§	`subsidiary computations`
GIVEN	`total sum assured` IS A		Number
DECIDE	`claimable limit` IS
            MIN	( PRODUCT ( 1.5
									        , `total sum assured` )
								, `lifetime claimable limit`
            ) ;


	      `lifetime claimable limit`  IS `$4,500,000` ;
        `juvenile limit` IS	`$500,000`
§
GIVEN	x IS A	Number ;
	    y IS A	Number
DECIDE	x	`discounted by`	y	IS	`x * (1 - y)`
|]

type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

run :: String -> Either String Rule
run = fmap transRule . pRule . myLexer

runList :: String -> Either String [Rule]
runList = fmap (fmap transRule) . pListRule . myLexer
