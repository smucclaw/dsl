{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LS.TextuaL4Spec (spec) where

import LS.Rule
import LS.Types
import TextuaL4.AbsTextuaL qualified as TL4
import TextuaL4.Transform
import TextuaL4.LexTextuaL   ( Token )
import TextuaL4.ParTextuaL   ( pRule, myLexer )
import TextuaL4.PrintTextuaL ( Print, printTree )
import Text.Pretty.Simple (pShowNoColor)
import Text.RawString.QQ
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
    testPath = "test" </> "testdata" </> "golden" </> name

spec :: Spec
spec = do
  describe "TextuaL4 output" do
    test "GIVEN foo GIVETH bar DECIDE bar IS 1 > 2" "hornlike-given-giveth-1"
    test "GIVEN foo IS A Number GIVETH bar IS A Boolean DECIDE bar IS foo > 2" "hornlike-given-giveth-2"
    test "EVERY tame ANY (Person, Animal) WHO Qualifies MEANS ALL(walks, ANY(eats, drinks), climbs) MUST ANY (sing, dance)" "regulative-any-tame-person-animal"
    test' rodents "rodents and vermin" "rodents-and-vermin"
    test' taxesPayable "taxes payable" "taxes-payable"
  where
    test rule = test' rule rule

    test' rule desc fname =  do
      let res :: Rule = fromRight RegBreach $ run rule
      it desc $ goldenGeneric fname res


rodents = [r|DECIDE "Not Covered"
IF          "Loss or Damage" IS ANY ( "caused by rodents"
                                    , "caused by insects"
                                    , "caused by vermin"
                                    , "caused by birds"
                                    )
  UNLESS
            ANY ( ALL ( "Loss or Damage" IS "to Contents"
                      , "Loss or Damage" IS "caused by birds"
                      )

                , "Loss or Damage" IS "ensuing covered loss"

                  UNLESS  ANY ( "any other exclusion applies"
                              , "an animal caused water to escape from"
                                    ANY ( "a household appliance"
                                        , "a swimming pool"
                                        , "a plumbing, heating, or air conditioning system" )
                        )
                )|]

taxesPayable = [r|
GIVEN annualIncome IS A Number ;
netWorth IS A Number ;
vivacity IS A Boolean ;
phaseOfMoon IS ONE OF new , waxing, full, gibbous
GIVETH  taxesPayable IS A Number
DECIDE  taxesPayable IS taxesPayableAlive "/" 2 IF phaseOfMoon IS gibbous ;

        taxesPayable  IS  taxesPayableAlive IF vivacity ;
        taxesPayable  IS  DIVIDE (taxesPayableAlive, 3) IF phaseOfMoon IS waxing ;
        taxesPayable  IS  waived IF phaseOfMoon IS full ;
        taxesPayable  IS  0 OTHERWISE ;

        taxesPayableAlive       IS SUM ( "income tax component"
                                       , "asset tax component" ) ;
        "income tax component"  IS PRODUCT	( annualIncome
                                            , PRODUCT (incomeTaxRate
                                                      , MINUS ( 2
                                                              , 1 )
                                                      )
                                            ) ;
        "asset tax component"   IS PRODUCT ( netWorth
                                           , assetTaxRate );
        incomeTaxRate           IS 0.01 ;
        assetTaxRate           IS 0.07|]

type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

run :: String -> Either String Rule
run = fmap transRule . pRule . myLexer
