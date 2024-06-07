{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module LS.XPile.GenericMathLang.TranslateL4Spec (spec) where

import AnyAll qualified as AA
import AnyAll (BoolStruct(..), Label(..))
import Control.Arrow ((>>>))
import Control.Monad (forM_)
import Control.Monad.Trans.Maybe (runMaybeT )
import Control.Monad.Trans.Writer.Lazy (runWriter)
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty (NonEmpty (..), fromList, toList, nonEmpty)
import Data.Text qualified as T
import Explainable
import Explainable.MathLang -- hiding ((|>))
import LS.Rule (Interpreted (..), Rule (..), defaultL4I)
import LS.Types
import LS.XPile.MathLang.GenericMathLang.GenericMathLangAST
import LS.XPile.MathLang.GenericMathLang.GenericMathLangAST qualified as GML
import LS.XPile.MathLang.GenericMathLang.TranslateL4
import LS.XPile.MathLang.MathLang qualified as ML
--import LS.XPile.MathLang.GenericMathLang.ToGenericMathLang (toMathLangGen)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck
  ( Arbitrary (..),
    Property,
    Testable (property),
    verbose,
    genericShrink,
    (===),
    (==>),
  )
import Test.QuickCheck.Arbitrary.Generic
  ( Arbitrary (..),
    genericArbitrary,
    genericShrink,
  )

import Prelude hiding (exp, seq)

instance Arbitrary (Expr Double) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary (ExprList Double) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary (MathSection Double) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary (Pred Double) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary SomeFold where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Comp where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary MathBinOp where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary PredBinOp where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary AndOr where
  arbitrary = genericArbitrary
  shrink = genericShrink

-- prop_changeVars :: (Expr Double, [(String, Expr Double)]) -> Property
-- prop_changeVars (e, ks_vs) = ML.replaceVars [] e fst ks vs === ML.replaceVars' ks vs e
--   where
--     ks = map fst ks_vs
--     vs = map snd ks_vs

spec :: Spec
spec = do
  -- describe "replaceVars different implementations" do
  --   it "should do the same thing" do
  --     --Test.QuickCheck.verbose $
  --     property prop_changeVars


  describe "rule 2 into SimpleHL" do
    it "should become a SimpleHL" do
      let toTest = runToLC $ simplifyL4Hlike rule2givens
      case toTest of
        Left err -> err `shouldBe` MiscError "" ""
        Right res -> baseHL res `shouldBe` rule2givens_shl_gold

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

  testBaseExpify "arithmetics testcase 2"
                  "should parse inside a cell"
                  [arithRule2]
                  [arithRule2_gold]

  testBaseExpify "arithmetics testcase 3"
                  "should parse inside a cell"
                  [arithRule3]
                  [arithRule3_gold]

  testBaseExpify "arithmetics testcase 4"
                  "mengs complex case"
                  [arithRule4]
                  [arithRule4_gold]

  describe "toMathLang" do
    let l4i = defaultL4I {origrules = [arithRule4]}
        res@(exprs,st) = ML.toMathLang l4i

    it "should turn Rules straight to MathLang (via GenericMathLang)" do
      res `shouldBe` mathLangGold4

    it "should evaluate taxesPayable correctly when info is given" do
      let st' = st {
              symtabF = symtabF st <> [
                ("annualIncome", Val (Just "annualIncome") 10000) ]
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

    -- testBaseExpify "foo" "bar" [arithRule2withInitializedValues] EEmpty

  describe "evalComplex" do
    it "should evaluate arithRule2" do
      let l4i = defaultL4I {origrules = [arithRule2withInitializedValues]}
      case ML.toMathLang l4i of
        ([], _) -> mempty
        (expr:_, st) -> do
          (res, _xp, _st, _strs) <- xplainE emptyE st $ eval expr
          res `shouldBe` 1.0

    let l4i_ar3 = defaultL4I {origrules = [arithRule3]}
        res_ar3@(exprs,state) = ML.toMathLang l4i_ar3
    it "toMathLang for arithRule3 should take m3a as the toplevel" do
      symtabF state `shouldBe` arithRule3_gold_symtab

  describe "test lambda expression" do
    it "should become a binary function in the environment" do
      let toTest = runToLC $ l4ToLCProgram testFunApp
      case toTest of
        Right p -> userFuns p `shouldBe` testLambdaGold
        Left err -> err `shouldBe` MiscError "" ""

  testBaseExpify "test function application" "should be recognised as a function" testFunApp testFunAppGold
  testBaseExpify "test function application" "fun app and nested genitives" nestedGenitivesInFunApp nestedGenitivesInFunAppGold

  testBaseExpify "parsing currencies" "should be parsed into various currencies" testCurrency testCurrencyGold

  describe "genitive->record field" do
    let gml = runToLC $ baseExpifyMTEs nestedGenitives
    case gml of
      Left err -> do
        it "unsuccesful, abort" do
          err `shouldBe` MiscError "foo" "bar"
      Right res -> do
        it "should turn xs y into a record x.y" do
          res `shouldBe` nestedGenitivesGold
        it "should turn into correct MathLang" do
          let exp = noExtraMdata res
              res' = ML.runToMathLang Map.empty $ ML.gml2ml exp
          case res' of
            Right ml -> ml `shouldBe` MathVar "ind.friend.age"
            Left err -> err `shouldBe` "something went wrong :("

  describe "simple fun app" do
    let l4i = defaultL4I {origrules = simpleFunApp}
        res = ML.toMathLang l4i
    it "should replace simple variables in a function that uses its arguments once" do
      res `shouldBe` simpleFunAppGold
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
    it "should replace simple variables in a function that uses its arguments twice each" do
      let l4i = defaultL4I {origrules = complexFunApp}
          res = ML.toMathLang l4i
      res `shouldBe` complexFunAppGold

  describe "nested function calls, e.g. `f x y = g x y where g x y = x + y`" do
    it "in GenMathLang, should parse two user functions" do
      let resGML = runToLC $ l4ToLCProgram funCallsAnotherFun
      case resGML of
        Right p -> userFuns p `shouldBe` testNestedFunAppGold
        Left err -> err `shouldBe` MiscError "" ""
    it "in MathLang, should evaluate x + y but keep track of the call of g in state" do
      let l4i = defaultL4I {origrules = funCallsAnotherFun}
          resML = ML.toMathLang l4i
      resML `shouldBe` funCallsAnotherFunGold

  describe "nested genitives + fun app" do
    it "should turn out right" do
      let l4i = defaultL4I {origrules = nestedGenitivesInFunApp}
          res = ML.toMathLang l4i
      res `shouldBe` ([MathSet "Answer" (MathVar "ind.friend.age discounted by foo.bar.baz")], emptyState {symtabF = [("ind.friend.age discounted by foo.bar.baz",MathBin (Just "ind.friend.age discounted by foo.bar.baz") Times (MathVar "ind.friend.age") (MathBin Nothing Minus (Val Nothing 1.0) (MathVar "foo.bar.baz"))),("Answer",MathVar "ind.friend.age discounted by foo.bar.baz")]})

  testLCProgram globalVars
                "extract GIVENs from PAU rules"
                "should include lists and basic types"
                paus
                pausGlobalVarsGold

  testLCProgram globalVars
                "extract GIVENs from taxesPayable rules"
                "should include enums and basic types"
                [arithRule4]
                arithRule4_globalVars_gold

  testBaseExpify "list types" "list has correct type in GML" listsum listsumGMLGold

  describe "list types" do
    let l4i = defaultL4I {origrules = listsum}
        res = ML.toMathLang l4i
    it "can sum a list with a single number" do
      res `shouldBe` listsumMLGold
    it "can evaluate summing a list with a single number" do
      case res of
        ([], _) -> mempty
        (expr:_, st) -> do
          (res, _xp, _st, _strs) <- xplainE emptyE st $ eval expr

          res `shouldBe` 16.0

  describe "testPau" do
    let l4i = defaultL4I {origrules = paus}
        res = ML.toMathLang l4i
    it "actual insurance policy" do
      res `shouldBe` testPauGold

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
              , ("claimable limit", Val (Just "claimable limit") 2000)
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
              , ("illness.disqualified" , PredVal Nothing False)
              ]
          }
          (res, _xp, _st, _strs) <- xplainE emptyE st' $ eval expr

          res `shouldBe` 1050.0

  describe "mustsing5" do
    let l4i = defaultL4I {origrules = mustsing5}
        res = ML.toMathLang l4i
    it "must sing 5 translated into ml" do
        res `shouldBe` mustsing5Gold

testBaseExpify :: String -> String -> [Rule] -> [BaseExp] -> Spec
testBaseExpify = testLCProgram (fmap exp . lcProgram)

testLCProgram ::
  (Show a, Eq a) =>
  (LCProgram -> a) -> String -> String -> [Rule] -> a -> Spec
testLCProgram f name desc rules gold =
  describe name do
    it desc do
      let toTest = runToLC $ l4ToLCProgram rules
      case toTest of
        Right p -> f p `shouldBe` gold
        Left err -> err `shouldBe` MiscError "" ""

-----------------------------------------------------------------------------
-- gold for mathlang transformation
mathLangGold23 :: [Expr Double]
mathLangGold23 = [
    MathSet "m3a"
    ( MathBin (Just "m3a") Times
        ( MathVar "m1" )
        ( MathVar "m2" )
    )
  , MathSet "m3b"
    ( MathBin (Just "m3b") Times
        ( MathVar "m1" )
        ( MathVar "m2" )
    )
  , MathSet "m3c"
    ( MathBin  (Just "m3c") Times
        ( MathVar "m1" )
        ( MathVar "m2" )
    )
  , MathSet "o3a"
    ( MathBin (Just "o3a") Plus
        ( MathBin Nothing Times
            ( MathVar "o1" )
            ( Val Nothing 1.0e-2 )
        )
        ( MathBin Nothing Times
            ( MathVar "o2" )
            ( Val Nothing 7.0e-2 )
        )
    )
  , MathSet "o3b"
    ( MathBin (Just "o3b")  Plus
        ( MathBin Nothing Times
            ( MathVar "o1" )
            ( Val Nothing 1.0e-2 )
        )
        ( MathBin Nothing Times
            ( MathVar "o2" )
            ( Val Nothing 7.0e-2 )
        )
    )
  , MathSet "o3c"
    ( MathBin (Just "o3c") Plus
        ( MathBin Nothing Times
            ( MathVar "o1" )
            ( Val Nothing 1.0e-2 )
        )
        ( MathBin Nothing Plus
            ( MathBin Nothing Times
                ( MathVar "o2" )
                ( Val Nothing 3.0e-2 )
            )
            ( MathBin Nothing Times
                ( MathVar "o2" )
                ( Val Nothing 4.0e-2 )
            )
        )
    )]

mathLangGold4 :: ([Expr Double], MyState)
mathLangGold4 = (
  [ MathSet "taxesPayable"
        ( MathITE
            ( Just "taxesPayable" )
            ( PredVar "phaseOfMoon.gibbous" )
            ( MathBin
                ( Just "taxesPayable" ) Divide
                ( MathVar "taxesPayableAlive" )
                ( Val Nothing 2.0 )
            )
            ( MathITE Nothing
                ( PredVar "vivacity" )
                ( MathVar "taxesPayableAlive" )
                ( MathITE Nothing
                    ( PredVar "phaseOfMoon.waxing" )
                    ( MathBin
                        ( Just "taxesPayable" ) Divide
                        ( MathVar "taxesPayableAlive" )
                        ( Val Nothing 3.0 )
                    )
                    ( MathITE Nothing
                        ( PredVar "phaseOfMoon.full" )
                        ( MathVar "waived" )
                        ( Val
                            ( Just "taxesPayable" ) 0.0
                        )
                    )
                )
            )
        )
    ]
  , emptyState
    { symtabF =
        [
            ( "income tax component"
            , MathBin
                ( Just "income tax component" ) Times
                ( MathVar "annualIncome" )
                ( MathVar "incomeTaxRate" )
            )
        ,
            ( "taxesPayable"
            , MathITE
                ( Just "taxesPayable" )
                ( PredVar "phaseOfMoon.gibbous" )
                ( MathBin
                    ( Just "taxesPayable" ) Divide
                    ( MathVar "taxesPayableAlive" )
                    ( Val Nothing 2.0 )
                )
                ( MathITE Nothing
                    ( PredVar "vivacity" )
                    ( MathVar "taxesPayableAlive" )
                    ( MathITE Nothing
                        ( PredVar "phaseOfMoon.waxing" )
                        ( MathBin
                            ( Just "taxesPayable" ) Divide
                            ( MathVar "taxesPayableAlive" )
                            ( Val Nothing 3.0 )
                        )
                        ( MathITE Nothing
                            ( PredVar "phaseOfMoon.full" )
                            ( MathVar "waived" )
                            ( Val
                                ( Just "taxesPayable" ) 0.0
                            )
                        )
                    )
                )
            )
        ,
            ( "asset tax component"
            , MathBin
                ( Just "asset tax component" ) Times
                ( MathVar "netWorth" )
                ( MathVar "assetTaxRate" )
            )
        ,
            ( "assetTaxRate"
            , Val
                ( Just "assetTaxRate" ) 7.0e-2
            )
        ,
            ( "incomeTaxRate"
            , Val
                ( Just "incomeTaxRate" ) 1.0e-2
            )
        ,
            ( "taxesPayableAlive"
            , MathBin
                ( Just "taxesPayableAlive" ) Plus
                ( MathVar "income tax component" )
                ( MathVar "asset tax component" )
            )
        ]
    }
  )



-----------------------------------------------------------------------------
-- Test rules

rule3predicate :: Rule
rule3predicate =
  let description = [ MTT "ind", MTT "qualifies a la case 3" ]
  in mkTestRule
        description
        (Just (
            ( MTT "ind" :| []
            , Just ( SimpleType TOne "Person" )
            ) :| [] ))
        [ HC { hHead = RPMT description
             , hBody = Just
                ( AA.All Nothing
                    [ AA.Leaf ( RPConstraint
                            [ MTT "ind" ] RPis
                            [ MTT "Singapore citizen" ] )
                    , AA.Leaf ( RPConstraint
                            [ MTT "ind's", MTT "place of residence" ] RPis
                            [ MTT "Singapore" ])
                    , AA.Leaf ( RPConstraint
                            [ MTT "ind's", MTT "age"] RPgte
                            [ MTI 21 ] )
                    , AA.Leaf ( RPConstraint
                            [ MTT "ind's", MTT "property annual value" ] RPlte
                            [ MTI 21000 ])
                    , AA.Leaf ( RPMT
                            [ MTT "ind"
                            , MTT "meets the property eligibility criteria for GSTV-Cash" ])
                    , AA.Leaf ( RPConstraint
                            [ MTT "ind's", MTT "annual income" ] RPlte
                            [ MTI 34000 ])
                    ])}]

-- TODO: Which ones of the following should be replaced by records?
rule3predicatesGold :: BaseExp
rule3predicatesGold = EIfThen
    { condExp = MkExp
        { exp = EAnd
            { left = MkExp
                { exp = EIs
                    { isLeft = MkExp
                        { exp = EVar
                            { var = MkVar "ind" }, md = []
                        }, isRight = MkExp
                        { exp = ELit
                            { lit = EString "Singapore citizen" }, md = []
                        }
                    }, md = []
                }, right = MkExp
                { exp = EAnd
                    { left = MkExp
                        { exp = EIs
                            { isLeft = MkExp
                                { exp = ERec
                                    { fieldName = MkExp
                                        { exp = ELit
                                            { lit = EString "place of residence" }, md = []
                                        }, recName = MkExp
                                        { exp = EVar
                                            { var = MkVar "ind" }, md = []
                                        }
                                    }, md = []
                                }, isRight = MkExp
                                { exp = ELit
                                    { lit = EString "Singapore" }, md = []
                                }
                            }, md = []
                        }, right = MkExp
                        { exp = EAnd
                            { left = MkExp
                                { exp = ECompOp
                                    { compOp = OpGte, compLeft = MkExp
                                        { exp = ERec
                                            { fieldName = MkExp
                                                { exp = ELit
                                                    { lit = EString "age" }, md = []
                                                }, recName = MkExp
                                                { exp = EVar
                                                    { var = MkVar "ind" }, md = []
                                                }
                                            }, md = []
                                        }, compRight = MkExp
                                        { exp = ELit
                                            { lit = EInteger 21 }, md = []
                                        }
                                    }, md = []
                                }, right = MkExp
                                { exp = EAnd
                                    { left = MkExp
                                        { exp = ECompOp
                                            { compOp = OpLte, compLeft = MkExp
                                                { exp = ERec
                                                    { fieldName = MkExp
                                                        { exp = ELit
                                                            { lit = EString "property annual value" }, md = []
                                                        }, recName = MkExp
                                                        { exp = EVar
                                                            { var = MkVar "ind" }, md = []
                                                        }
                                                    }, md = []
                                                }, compRight = MkExp
                                                { exp = ELit
                                                    { lit = EInteger 21000 }, md = []
                                                }
                                            }, md = []
                                        }, right = MkExp
                                        { exp = EAnd
                                            { left = MkExp
                                                { exp = ECompOp
                                                    { compOp = OpBoolEq, compLeft = MkExp
                                                        { exp = EApp
                                                            { func = MkExp
                                                                { exp = ELit
                                                                    { lit = EString "meets the property eligibility criteria for GSTV-Cash" }, md = []
                                                                }, appArg = MkExp (EVar (MkVar "ind")) []
                                                            }, md = []
                                                        }, compRight = MkExp
                                                        { exp = ELit { lit = EBoolTrue }, md = []
                                                        }
                                                    }, md = []
                                                }, right = MkExp
                                                { exp = EAnd
                                                    { left = MkExp
                                                        { exp = ECompOp
                                                            { compOp = OpLte, compLeft = MkExp
                                                                { exp = ERec
                                                                    { fieldName = MkExp
                                                                        { exp = ELit
                                                                            { lit = EString "annual income" }, md = []
                                                                        }, recName = MkExp
                                                                        { exp = EVar
                                                                            { var = MkVar "ind" }, md = []
                                                                        }
                                                                    }, md = []
                                                                }, compRight = MkExp
                                                                { exp = ELit
                                                                    { lit = EInteger 34000 }, md = []
                                                                }
                                                            }, md = []
                                                        }, right = MkExp
                                                        { exp = EEmpty, md = []
                                                        }
                                                    }, md = []
                                                }
                                            }, md = []
                                        }
                                    }, md = []
                                }
                            }, md = []
                        }
                    }, md = []
                }
            }, md = []
        }, thenExp = MkExp
        { exp = EVarSet
            { vsetVar = MkExp
                { exp = EVar
                    { var = MkVar "ind qualifies a la case 3" }, md = dummyMetadata
                }, arg = MkExp
                { exp = ELit { lit = EBoolTrue }, md = [] }
            }, md = []
        }
    }

rule2givens :: Rule
rule2givens =
  let description = [ MTT "case 2 qualifies" ]
  in mkTestRule
        description
        (mkGivens $ map (, Nothing) ["place of residence", "age", "property annual value", "meets the property eligibility criteria for GSTV-Cash", "annual income"])
        [ HC { hHead = RPMT description
             , hBody = Just
                ( AA.All Nothing
                    [ AA.Leaf ( RPMT [ MTT "Singapore citizen" ])
                    , AA.Leaf ( RPConstraint
                            [ MTT "place of residence" ] RPis [ MTT "Singapore" ])
                    , AA.Leaf ( RPConstraint
                            [ MTT "age" ] RPgte [ MTT "21" ])
                    , AA.Leaf ( RPConstraint
                            [ MTT "property annual value" ] RPlte [ MTI 21000 ])
                    , AA.Leaf ( RPMT
                            [ MTT "meets the property eligibility criteria for GSTV-Cash" ])
                    , AA.Leaf ( RPConstraint
                            [ MTT "annual income" ] RPlte [ MTI 34000 ]
                        )
                    ])}]

rule2givens_shl_gold :: BaseHL
rule2givens_shl_gold = OneClause (HeadAndBody MkHnBHC {
                   hbHead = RPMT [MTT "case 2 qualifies"],
                   hbBody = AA.All Nothing [AA.Leaf (RPMT [MTT "Singapore citizen"]), AA.Leaf (RPConstraint [MTT "place of residence"] RPis [MTT "Singapore"]), AA.Leaf (RPConstraint [MTT "age"] RPgte [MTT "21"]), AA.Leaf (RPConstraint [MTT "property annual value"] RPlte [MTI 21000]), AA.Leaf (RPMT [MTT "meets the property eligibility criteria for GSTV-Cash"]), AA.Leaf (RPConstraint [MTT "annual income"] RPlte [MTI 34000])]
                 })

rule2nogivens :: Rule
rule2nogivens = rule2givens {given = Nothing}

rule2nogivens_shl_gold :: BaseHL
rule2nogivens_shl_gold = OneClause (HeadAndBody MkHnBHC {
                   hbHead = RPMT [MTT "case 2 qualifies"],
                   hbBody = AA.All Nothing [AA.Leaf (RPMT [MTT "Singapore citizen"]), AA.Leaf (RPConstraint [MTT "place of residence"] RPis [MTT "Singapore"]), AA.Leaf (RPConstraint [MTT "age"] RPgte [MTT "21"]), AA.Leaf (RPConstraint [MTT "property annual value"] RPlte [MTI 21000]), AA.Leaf (RPMT [MTT "meets the property eligibility criteria for GSTV-Cash"]), AA.Leaf (RPConstraint [MTT "annual income"] RPlte [MTI 34000])]
                 })

-- Simple case? The conditions are all checked with OpBoolEq, OpStringEq and Op[GL]te
-- No Vars, because there were no givens in the original rule
rule2nogivens_gold :: BaseExp
rule2nogivens_gold = EIfThen
    { condExp = MkExp
        { exp = EAnd
            { left = MkExp
                { exp = ECompOp
                    { compOp = OpBoolEq, compLeft = MkExp
                        { exp = ELit
                            { lit = EString "Singapore citizen" }, md = []
                        }, compRight = MkExp
                        { exp = ELit { lit = EBoolTrue }, md = [] }
                    }, md = []
                }, right = MkExp
                { exp = EAnd
                    { left = MkExp
                        { exp = EIs
                            { isLeft = MkExp
                                { exp = ELit
                                    { lit = EString "place of residence" }, md = []
                                }, isRight = MkExp
                                { exp = ELit
                                    { lit = EString "Singapore" }, md = []
                                }
                            }, md = []
                        }, right = MkExp
                        { exp = EAnd
                            { left = MkExp
                                { exp = ECompOp
                                    { compOp = OpGte, compLeft = MkExp
                                        { exp = EVar
                                            { var = MkVar "age" }, md = dummyMetadata
                                        }, compRight = MkExp
                                        { exp = ELit
                                            { lit = EInteger 21 }, md = []
                                        }
                                    }, md = []
                                }, right = MkExp
                                { exp = EAnd
                                    { left = MkExp
                                        { exp = ECompOp
                                            { compOp = OpLte, compLeft = MkExp
                                                { exp = ELit
                                                    { lit = EString "property annual value" }, md = []
                                                }, compRight = MkExp
                                                { exp = ELit
                                                    { lit = EInteger 21000 }, md = []
                                                }
                                            }, md = []
                                        }, right = MkExp
                                        { exp = EAnd
                                            { left = MkExp
                                                { exp = ECompOp
                                                    { compOp = OpBoolEq, compLeft = MkExp
                                                        { exp = ELit
                                                            { lit = EString "meets the property eligibility criteria for GSTV-Cash" }, md = []
                                                        }, compRight = MkExp
                                                        { exp = ELit { lit = EBoolTrue }, md = []
                                                        }
                                                    }, md = []
                                                }, right = MkExp
                                                { exp = EAnd
                                                    { left = MkExp
                                                        { exp = ECompOp
                                                            { compOp = OpLte, compLeft = MkExp
                                                                { exp = ELit
                                                                    { lit = EString "annual income" }, md = []
                                                                }, compRight = MkExp
                                                                { exp = ELit
                                                                    { lit = EInteger 34000 }, md = []
                                                                }
                                                            }, md = []
                                                        }, right = MkExp
                                                        { exp = EEmpty, md = []
                                                        }
                                                    }, md = []
                                                }
                                            }, md = []
                                        }
                                    }, md = []
                                }
                            }, md = []
                        }
                    }, md = []
                }
            }, md = []
        }, thenExp = MkExp
        { exp = EVarSet
            { vsetVar = MkExp
                { exp = EVar
                    { var = MkVar "case 2 qualifies" }, md = dummyMetadata
                }, arg = MkExp
                { exp = ELit { lit = EBoolTrue }, md = [] }
            }, md = []
        }
    }

arithRule1 :: Rule
arithRule1 = mkTestRule
               [ MTT "two plus two" ]
               Nothing
               [ HC { hHead = RPConstraint
                    [ MTT "m1" ] RPis
                    [ MTT "2 + 2" ]
                , hBody = Nothing } ]

arithRule2withInitializedValues :: Rule
arithRule2withInitializedValues = mkTestRule'
                [ MTT "result" ]
                (mkGivens [("m1", Just ( SimpleType TOne "Number" )), ("m2", Just ( SimpleType TOne "Number" ))])
                (mkGivens [("result", Just ( SimpleType TOne "Number"))])
                [ HC { hHead = RPConstraint
                        [ MTT "m1" ] RPis
                        [ MTT "10" ]
                    , hBody = Nothing }
                , HC { hHead = RPConstraint
                        [ MTT "m2" ] RPis
                        [ MTT "5" ]
                    , hBody = Nothing }
                , HC { hHead = RPnary RPis
                        [ RPMT
                            [ MTT "result" ]
                        , RPnary RPminus
                            [ RPMT
                                [ MTT "m1" ]
                            , RPMT
                                [ MTT "m2" ]
                            , RPMT
                                [ MTI 3 ]
                            , RPMT
                                [ MTI 1 ]
                            ]
                        ]
                    , hBody = Nothing }
                ]


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

arithRule2_gold :: BaseExp
arithRule2_gold = ESeq {seq = SeqExp [MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "m3a"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ENumOp {numOp = OpMul, nopLeft = MkExp {exp = EVar {var = MkVar "m1"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (FromUser (L4EntType "Number")), explnAnnot = Nothing}]}, nopRight = MkExp {exp = EVar {var = MkVar "m2"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (FromUser (L4EntType "Number")), explnAnnot = Nothing}]}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []},MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "m3b"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ENumOp {numOp = OpMul, nopLeft = MkExp {exp = EVar {var = MkVar "m1"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (FromUser (L4EntType "Number")), explnAnnot = Nothing}]}, nopRight = MkExp {exp = EVar {var = MkVar "m2"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (FromUser (L4EntType "Number")), explnAnnot = Nothing}]}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []},MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "m3c"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ENumOp {numOp = OpMul, nopLeft = MkExp {exp = EVar {var = MkVar "m1"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, nopRight = MkExp {exp = EVar {var = MkVar "m2"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []},MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "m3d"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ENumOp {numOp = OpMinus, nopLeft = MkExp {exp = EVar {var = MkVar "m1"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (FromUser (L4EntType "Number")), explnAnnot = Nothing}]}, nopRight = MkExp {exp = EVar {var = MkVar "m2"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (FromUser (L4EntType "Number")), explnAnnot = Nothing}]}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []}]}

arithExpr3_gold :: Exp
arithExpr3_gold =
  MkExp { exp = ENumOp
    { numOp = OpPlus, nopLeft = MkExp
        { exp = ENumOp
            { numOp = OpMul, nopLeft = MkExp
                { exp = EVar
                    { var = MkVar "o1" }, md = mkMetadata ( FromUser ( L4EntType "Number" ))
                }, nopRight = MkExp
                { exp = ELit
                    { lit = EFloat 1.0e-2 }, md = mkMetadata ( Inferred "Number" )
                }
            }, md = mkMetadata ( Inferred "Number" )
        }, nopRight = MkExp
        { exp = ENumOp
            { numOp = OpMul, nopLeft = MkExp
                { exp = EVar
                    { var = MkVar "o2" }, md = mkMetadata ( FromUser ( L4EntType "Number" ))
                }, nopRight = MkExp
                { exp = ELit
                    { lit = EFloat 7.0e-2 }, md = mkMetadata ( Inferred "Number" )
                }
            }, md = mkMetadata (Inferred "Number")
        }
    }, md = mkMetadata (Inferred "Number")
  }

arithRule3_gold_symtab :: Explainable.MathLang.SymTab (Expr Double)
arithRule3_gold_symtab =
  [ ( "o3b", MathBin
        ( Just "o3b" ) Plus
        ( MathBin Nothing Times ( MathVar "o1" ) ( Val Nothing 1.0e-2 ) )
        ( MathBin Nothing Times ( MathVar "o2" ) ( Val Nothing 7.0e-2 ) )
    ),
    ( "o3a_plus_times", MathBin
        ( Just "o3a_plus_times" ) Plus
        ( MathBin Nothing Times
            ( MathVar "o1" )
            ( Val Nothing 1.0e-2 )
        )
        ( MathBin Nothing Times
            ( MathVar "o2" )
            ( Val Nothing 7.0e-2 )
        )
    ),
    ( "o3c", MathBin
        ( Just "o3c" ) Plus
        ( MathBin Nothing Times ( MathVar "o1" ) ( Val Nothing 1.0e-2 ) )
        ( MathBin Nothing Plus
            ( MathBin Nothing Times ( MathVar "o2" ) ( Val Nothing 3.0e-2 ) )
            ( MathBin Nothing Times ( MathVar "o2" ) ( Val Nothing 4.0e-2 ) )
        )
    ),
    ( "o3a_times_plus", MathBin
        ( Just "o3a_times_plus" ) Times
        ( MathBin Nothing Plus
            ( MathVar "o1" )
            ( Val Nothing 1.0e-2 )
        )
        ( MathBin Nothing Plus ( MathVar "o2" ) ( Val Nothing 7.0e-2 ) )
    )
  ]

arithRule3_gold :: BaseExp
arithRule3_gold = ESeq {seq = SeqExp [MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "o3a_plus_times"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ENumOp {numOp = OpPlus, nopLeft = MkExp {exp = ENumOp {numOp = OpMul, nopLeft = MkExp {exp = EVar {var = MkVar "o1"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, nopRight = MkExp {exp = ELit {lit = EFloat 1.0e-2}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []}, nopRight = MkExp {exp = ENumOp {numOp = OpMul, nopLeft = MkExp {exp = EVar {var = MkVar "o2"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, nopRight = MkExp {exp = ELit {lit = EFloat 7.0e-2}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []},MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "o3a_times_plus"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ENumOp {numOp = OpMul, nopLeft = MkExp {exp = ENumOp {numOp = OpPlus, nopLeft = MkExp {exp = EVar {var = MkVar "o1"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, nopRight = MkExp {exp = ELit {lit = EFloat 1.0e-2}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []}, nopRight = MkExp {exp = ENumOp {numOp = OpPlus, nopLeft = MkExp {exp = EVar {var = MkVar "o2"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, nopRight = MkExp {exp = ELit {lit = EFloat 7.0e-2}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []},MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "o3b"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ENumOp {numOp = OpPlus, nopLeft = MkExp {exp = ENumOp {numOp = OpMul, nopLeft = MkExp {exp = EVar {var = MkVar "o1"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (FromUser (L4EntType "Number")), explnAnnot = Nothing}]}, nopRight = MkExp {exp = ELit {lit = EFloat 1.0e-2}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, nopRight = MkExp {exp = ENumOp {numOp = OpMul, nopLeft = MkExp {exp = EVar {var = MkVar "o2"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (FromUser (L4EntType "Number")), explnAnnot = Nothing}]}, nopRight = MkExp {exp = ELit {lit = EFloat 7.0e-2}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []},MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "o3c"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ENumOp {numOp = OpPlus, nopLeft = MkExp {exp = ENumOp {numOp = OpMul, nopLeft = MkExp {exp = EVar {var = MkVar "o1"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, nopRight = MkExp {exp = ELit {lit = EFloat 1.0e-2}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, nopRight = MkExp {exp = ENumOp {numOp = OpPlus, nopLeft = MkExp {exp = ENumOp {numOp = OpMul, nopLeft = MkExp {exp = EVar {var = MkVar "o2"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, nopRight = MkExp {exp = ELit {lit = EFloat 3.0e-2}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, nopRight = MkExp {exp = ENumOp {numOp = OpMul, nopLeft = MkExp {exp = EVar {var = MkVar "o2"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, nopRight = MkExp {exp = ELit {lit = EFloat 4.0e-2}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []}]}

arithRule4 :: Rule
arithRule4 = mkTestRule'
                [ MTT "taxesPayable" ]
                (Just (
                    ( MTT "annualIncome" :| [] , Just ( SimpleType TOne "Number" )) :|
                    [ ( MTT "netWorth" :| [], Just ( SimpleType TOne "Number" ))
                    , ( MTT "vivacity" :| [], Just ( SimpleType TOne "Boolean" ))
                    , ( MTT "phaseOfMoon" :| []
                      , Just ( InlineEnum TOne (
                                ( MTT "new" :| [ MTT "waxing", MTT "full", MTT "gibbous"] , Nothing ) :| []
                                )
                             )
                      )
                    ]
                ))
                (mkGivens [("taxesPayable", Just ( SimpleType TOne "Number"))])
                [ HC { hHead = RPConstraint
                                [ MTT "taxesPayable" ] RPis [ MTT "taxesPayableAlive / 2" ]
                     , hBody = Just ( AA.Leaf ( RPConstraint
                                [ MTT "phaseOfMoon" ] RPis [ MTT "gibbous" ]))}
                , HC { hHead = RPConstraint
                                [ MTT "taxesPayable" ] RPis [ MTT "taxesPayableAlive" ]
                     , hBody = Just ( AA.Leaf ( RPMT [ MTT "vivacity" ]))}
                , HC { hHead = RPConstraint [ MTT "taxesPayable" ] RPis [ MTT "taxesPayableAlive"
                                                                        , MTT "/"
                                                                        , MTI 3
                                                                        ]
                     , hBody = Just ( AA.Leaf ( RPConstraint
                                [ MTT "phaseOfMoon" ] RPis [ MTT "waxing" ]))}
                , HC { hHead = RPConstraint [ MTT "taxesPayable" ] RPis [ MTT "waived" ]
                     , hBody = Just ( AA.Leaf ( RPConstraint
                                [ MTT "phaseOfMoon" ] RPis [ MTT "full" ]))}
                , HC { hHead = RPConstraint [ MTT "taxesPayable" ] RPis [ MTI 0 ]
                    , hBody = Just ( AA.Leaf ( RPMT [ MTT "OTHERWISE" ]))}
                , HC { hHead = RPnary RPis [ RPMT [ MTT "taxesPayableAlive" ]
                                          , RPnary RPsum [ RPMT [ MTT "income tax component" ]
                                                         , RPMT [ MTT "asset tax component" ]]]
                     , hBody = Nothing }
                , HC { hHead = RPnary RPis [ RPMT [ MTT "income tax component" ]
                                           , RPnary RPproduct [ RPMT [ MTT "annualIncome" ]
                                                              , RPMT [ MTT "incomeTaxRate" ]]]
                     , hBody = Nothing }
                , HC { hHead = RPnary RPis [ RPMT [ MTT "asset tax component" ]
                                           , RPnary RPproduct [ RPMT [ MTT "netWorth" ]
                                                              , RPMT [ MTT "assetTaxRate" ]]]
                     , hBody = Nothing }
                , HC { hHead = RPConstraint [ MTT "incomeTaxRate" ] RPis [ MTF 1.0e-2 ]
                     , hBody = Nothing }
                , HC { hHead = RPConstraint [ MTT "assetTaxRate" ] RPis [ MTF 7.0e-2 ]
                     , hBody = Nothing }
            ]

arithRule4_gold :: BaseExp
arithRule4_gold = ESeq {seq = SeqExp [MkExp {exp = EIfThen {condExp = MkExp {exp = EIs {isLeft = MkExp {exp = EVar {var = MkVar "phaseOfMoon"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (FromUser (L4Enum ["new","waxing","full","gibbous"])), explnAnnot = Nothing}]}, isRight = MkExp {exp = ELit {lit = EENum "gibbous"}, md = []}}, md = []}, thenExp = MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "taxesPayable"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ENumOp {numOp = OpDiv, nopLeft = MkExp {exp = EVar {var = MkVar "taxesPayableAlive"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, nopRight = MkExp {exp = ELit {lit = EInteger 2}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []}}, md = []},MkExp {exp = EIfThen {condExp = MkExp {exp = EVar {var = MkVar "vivacity"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (FromUser (L4EntType "Boolean")), explnAnnot = Nothing}]}, thenExp = MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "taxesPayable"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}, arg = MkExp {exp = EVar {var = MkVar "taxesPayableAlive"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}}, md = []}}, md = []},MkExp {exp = EIfThen {condExp = MkExp {exp = EIs {isLeft = MkExp {exp = EVar {var = MkVar "phaseOfMoon"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (FromUser (L4Enum ["new","waxing","full","gibbous"])), explnAnnot = Nothing}]}, isRight = MkExp {exp = ELit {lit = EENum "waxing"}, md = []}}, md = []}, thenExp = MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "taxesPayable"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ENumOp {numOp = OpDiv, nopLeft = MkExp {exp = EVar {var = MkVar "taxesPayableAlive"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}, nopRight = MkExp {exp = ELit {lit = EInteger 3}, md = []}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []}}, md = []},MkExp {exp = EIfThen {condExp = MkExp {exp = EIs {isLeft = MkExp {exp = EVar {var = MkVar "phaseOfMoon"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (FromUser (L4Enum ["new","waxing","full","gibbous"])), explnAnnot = Nothing}]}, isRight = MkExp {exp = ELit {lit = EENum "full"}, md = []}}, md = []}, thenExp = MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "taxesPayable"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}, arg = MkExp {exp = EVar {var = MkVar "waived"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}}, md = []}}, md = []},MkExp {exp = EIfThen {condExp = MkExp {exp = ELit {lit = EBoolTrue}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Bool"), explnAnnot = Nothing}]}, thenExp = MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "taxesPayable"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ELit {lit = EInteger 0}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []}}, md = []},MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "taxesPayableAlive"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ENumOp {numOp = OpPlus, nopLeft = MkExp {exp = EVar {var = MkVar "income tax component"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, nopRight = MkExp {exp = EVar {var = MkVar "asset tax component"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []},MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "income tax component"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ENumOp {numOp = OpMul, nopLeft = MkExp {exp = EVar {var = MkVar "annualIncome"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (FromUser (L4EntType "Number")), explnAnnot = Nothing}]}, nopRight = MkExp {exp = EVar {var = MkVar "incomeTaxRate"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []},MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "asset tax component"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ENumOp {numOp = OpMul, nopLeft = MkExp {exp = EVar {var = MkVar "netWorth"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (FromUser (L4EntType "Number")), explnAnnot = Nothing}]}, nopRight = MkExp {exp = EVar {var = MkVar "assetTaxRate"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []},MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "incomeTaxRate"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ELit {lit = EFloat 1.0e-2}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []},MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "assetTaxRate"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ELit {lit = EFloat 7.0e-2}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []}]}

arithRule4_globalVars_gold :: GlobalVars
arithRule4_globalVars_gold = MkGlobalVars [
  ( MkVar "netWorth", Just ( L4EntType "Number" ) ),
  ( MkVar "annualIncome", Just ( L4EntType "Number" ) ),
  ( MkVar "phaseOfMoon", Just
      ( L4Enum [ "new", "waxing", "full", "gibbous" ] )
  ),
  ( MkVar "vivacity", Just ( L4EntType "Boolean" ) )
  ]


nestedGenitives :: [MTExpr]
nestedGenitives = [ MTT "ind's", MTT "friend's", MTT "age"]

nestedGenitivesGold :: BaseExp
nestedGenitivesGold = ERec {fieldName = MkExp {exp = ERec {fieldName = MkExp {exp = EVar {var = MkVar "age"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}, recName = MkExp {exp = EVar {var = MkVar "friend"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}}, md = []}, recName = MkExp {exp = EVar {var = MkVar "ind"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}}

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

simpleFunAppGold :: ([Expr Double], MyState)
simpleFunAppGold =  (
        [ MathSet "Answer"
            ( MathVar "firstArg discounted by secondArg" )
        ]
    , emptyState
        { symtabF =
            [
                ( "firstArg discounted by secondArg"
                , MathBin
                    ( Just "firstArg discounted by secondArg" ) Times
                    ( MathVar "firstArg" )
                    ( MathBin Nothing Minus
                        ( Val Nothing 1.0 )
                        ( MathVar "secondArg" )
                    )
                )
            ,
                ( "Answer"
                , MathVar "firstArg discounted by secondArg"
                )
            ]
        }
    )
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

complexFunAppGold :: ([Expr a], MyState)
complexFunAppGold = (
    [ MathSet "Answer"
        ( MathVar "firstArg `funThatRepeatsArgs` secondArg" )
    ]
    , emptyState
    { symtabF =
        [
            ( "firstArg `funThatRepeatsArgs` secondArg"
            , MathBin
                ( Just "firstArg `funThatRepeatsArgs` secondArg" ) Times
                ( MathBin Nothing Plus
                    ( MathVar "firstArg" )
                    ( MathVar "secondArg" )
                )
                ( MathBin Nothing Plus
                    ( MathBin Nothing Minus
                        ( Val Nothing 42.0 )
                        ( MathVar "secondArg" )
                    )
                    ( MathVar "firstArg" )
                )
            )
        ,
            ( "Answer"
            , MathVar "firstArg `funThatRepeatsArgs` secondArg"
            )
        ]
    })

funCallsAnotherFun = [
    Hornlike
    { name =
        [ MTT "The Answer" ]
    , super = Nothing
    , keyword = Decide
    , given = mkGivens
                [ ("firstArgument" , Just (SimpleType TOne "Number"))
                , ("secondArgument", Just (SimpleType TOne "Number"))
                ]
    , giveth = mkGivens [("The Answer" , Just (SimpleType TOne "Number"))]
    , upon = Nothing
    , clauses = [ HC
            { hHead = RPConstraint
                [ MTT "The Answer" ] RPis
                [ MTT "firstArgument", MTT "f", MTT "secondArgument"]
            , hBody = Nothing
            }]
    , rlabel = Just ( "§", 2, "Top-Level" )
    , lsource = Nothing
    , wwhere = []
    , srcref = Just
        ( SrcRef
            { url = "test/localFuns.csv"
            , short = "test/localFuns.csv"
            , srcrow = 4
            , srccol = 8
            , version = Nothing
            }
        )
    , defaults = []
    , symtab = []
    },
    Hornlike
    { name =
        [ MTT "c", MTT "f", MTT "d"]
    , super = Nothing
    , keyword = Decide
    , given = mkGivens
                [ ("c", Just (SimpleType TOne "Number"))
                , ("d", Just (SimpleType TOne "Number"))
                ]
    , giveth = Nothing
    , upon = Nothing
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
    , rlabel = Nothing
    , lsource = Nothing
    , wwhere = []
    , srcref = Just
        ( SrcRef
            { url = "test/localFuns.csv"
            , short = "test/localFuns.csv"
            , srcrow = 4
            , srccol = 15
            , version = Nothing
            }
        )
    , defaults = []
    , symtab = []
    }
    , Hornlike
    { name =
        [ MTT "a", MTT "plus", MTT "b"
        ]
    , super = Nothing
    , keyword = Decide
    , given = mkGivens
                [ ("a", Just (SimpleType TOne "Number"))
                , ("b", Just (SimpleType TOne "Number"))
                ]
    , giveth = Nothing
    , upon = Nothing
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
    , rlabel = Nothing
    , lsource = Nothing
    , wwhere = []
    , srcref = Just
        ( SrcRef
            { url = "test/localFuns.csv"
            , short = "test/localFuns.csv"
            , srcrow = 4
            , srccol = 23
            , version = Nothing
            }
        )
    , defaults = []
    , symtab = []
    }]

funCallsAnotherFunGold :: ([Expr Double], MyState)
funCallsAnotherFunGold = ([MathSet "The Answer" (MathVar "firstArgument f secondArgument")], emptyState {symtabF = [("firstArgument f secondArgument",MathBin (Just "firstArgument f secondArgument") Plus (MathVar "firstArgument") (MathVar "secondArgument")),("Top-Level",MathSet "The Answer" (MathVar "firstArgument f secondArgument"))]})

nestedGenitivesInFunAppGold :: [BaseExp]
nestedGenitivesInFunAppGold = [EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "Answer"}, md = []}, arg = MkExp {exp = EApp {func = MkExp {exp = EApp {func = MkExp {exp = EVar {var = MkVar "discounted by"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}, appArg = MkExp {exp = ERec {fieldName = MkExp {exp = ERec {fieldName = MkExp {exp = EVar {var = MkVar "age"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}, recName = MkExp {exp = EVar {var = MkVar "friend"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}}, md = []}, recName = MkExp {exp = EVar {var = MkVar "ind"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}}, md = []}}, md = []}, appArg = MkExp {exp = ERec {fieldName = MkExp {exp = ERec {fieldName = MkExp {exp = EVar {var = MkVar "baz"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}, recName = MkExp {exp = EVar {var = MkVar "bar"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}}, md = []}, recName = MkExp {exp = EVar {var = MkVar "foo"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}}, md = []}}, md = []}}]

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
mkTestRule' name given giveth clauses = Hornlike
    { name = name
    , super = Nothing
    , keyword = Decide
    , given = given
    , giveth = giveth
    , upon = Nothing
    , clauses = clauses
    , rlabel = Nothing
    , lsource = Nothing
    , wwhere = []
    , srcref = Just ( SrcRef { url = "dummy" , short = "dummy"
             , srcrow = 0, srccol = 0, version = Nothing })
    , defaults = []
    , symtab = []
    }

simplifiedFib :: Rule
simplifiedFib = mkTestRule
            [ MTT "fib n" ]
            (mkGivens [("n", Just (SimpleType TOne "Number" ))])
            [ HC { hHead = RPConstraint
                    [ MTT "fib n" ] RPis [ MTI 0 ]
            , hBody = Nothing
            }]

dummyMetadata :: [ExpMetadata]
dummyMetadata = [ MkExpMetadata
                    { srcPos = MkPositn
                        { row = 0, col = 0
                        }, typeLabel = Nothing, explnAnnot = Nothing
                    }
                ]

mkMetadata :: TLabel -> [ExpMetadata]
mkMetadata typelabel = [ MkExpMetadata
                    { srcPos = MkPositn
                        { row = 0, col = 0
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

testLambdaGold :: Map.HashMap String ([GML.Var], Exp)
testLambdaGold = [("discounted by",([MkVar "x",MkVar "y"],MkExp {exp = ENumOp {numOp = OpMul, nopLeft = MkExp {exp = EVar {var = MkVar "x"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, nopRight = MkExp {exp = ENumOp {numOp = OpMinus, nopLeft = MkExp {exp = ELit {lit = EInteger 1}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, nopRight = MkExp {exp = EVar {var = MkVar "y"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []}))]

testNestedFunAppGold :: Map.HashMap String ([GML.Var], Exp)
testNestedFunAppGold = [("f",([MkVar "c",MkVar "d"],MkExp {exp = EApp {func = MkExp {exp = EApp {func = MkExp {exp = EVar {var = MkVar "plus"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}, appArg = MkExp {exp = EVar {var = MkVar "c"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}}, md = []}, appArg = MkExp {exp = EVar {var = MkVar "d"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}}, md = []})),("plus",([MkVar "a",MkVar "b"],MkExp {exp = ENumOp {numOp = OpPlus, nopLeft = MkExp {exp = EVar {var = MkVar "a"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}, nopRight = MkExp {exp = EVar {var = MkVar "b"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}}, md = []}))]

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

testCurrencyGold :: [BaseExp]
testCurrencyGold =
  [ESeq {seq = SeqExp [MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "sgdTestSpaceNoComma"}, md = []}, arg = MkExp {exp = ELit {lit = ECurrency "SGD" 42.0}, md = []}}, md = []}, MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "sgdTestNoSpaceNoComma"}, md = []}, arg = MkExp {exp = ELit {lit = ECurrency "SGD" 42.0}, md = []}}, md = []}, MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "eurTestSpaceComma"}, md = []}, arg = MkExp {exp = ELit {lit = ECurrency "EUR" 500000.0}, md = []}}, md = []}, MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "eurTestNoSpaceComma"}, md = []}, arg = MkExp {exp = ELit {lit = ECurrency "EUR" 500000.0}, md = []}}, md = []}, MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "notACurrency"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}, arg = MkExp {exp = EVar {var = MkVar "PAU4"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}}, md = []}]}]

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


testFunAppGold :: [BaseExp]
testFunAppGold = [EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "Answer"}, md = []}, arg = MkExp {exp = EApp {func = MkExp {exp = EApp {func = MkExp {exp = EVar {var = MkVar "discounted by"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}, appArg = MkExp {exp = EVar {var = MkVar "Step 3"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}}, md = []}, appArg = MkExp {exp = ERec {fieldName = MkExp {exp = EVar {var = MkVar "risk cap"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}, recName = MkExp {exp = EVar {var = MkVar "accident"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}}, md = []}}, md = []}}]

listsum :: [Rule]
listsum = [mkTestRule'
    [ MTT "test for lists" ]
    (mkGivens [
        ( "listThing", Just (SimpleType TList1 "Number") )
      , ( "singleThing", Just (SimpleType TOne "Number") ) ])
    (mkGivens [("listSum", Just (SimpleType TOne "Number") )])
    [ HC { hHead = RPConstraint
            [ MTT "listSum" ] RPis
            [ MTT "singleThing", MTT "+", MTT "listThing" ]
         , hBody = Nothing }
    , HC { hHead = RPConstraint
            [ MTT "listThing" ] RPis
            [ MTI 1, MTI 2, MTI 3 ]
         , hBody = Nothing }
    , HC { hHead = RPConstraint
            [ MTT "singleThing" ] RPis
            [ MTI 10 ]
         , hBody = Nothing }
    ]]

listsumGMLGold :: [BaseExp]
listsumGMLGold = [ESeq {seq = SeqExp [MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "listSum"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ENumOp {numOp = OpPlus, nopLeft = MkExp {exp = EVar {var = MkVar "singleThing"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (FromUser (L4EntType "Number")), explnAnnot = Nothing}]}, nopRight = MkExp {exp = EVar {var = MkVar "listThing"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (FromUser (L4List (L4EntType "Number"))), explnAnnot = Nothing}]}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []},MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "listThing"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (FromUser (L4List (L4EntType "Number"))), explnAnnot = Nothing}]}, arg = MkExp {exp = ESeq {seq = SeqExp [MkExp {exp = ELit {lit = EInteger 1}, md = []},MkExp {exp = ELit {lit = EInteger 2}, md = []},MkExp {exp = ELit {lit = EInteger 3}, md = []}]}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (FromUser (L4List (L4EntType "Number"))), explnAnnot = Nothing}]}}, md = []},MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "singleThing"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (FromUser (L4EntType "Number")), explnAnnot = Nothing}]}, arg = MkExp {exp = ELit {lit = EInteger 10}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (FromUser (L4EntType "Number")), explnAnnot = Nothing}]}}, md = []}]}]

listsumMLGold :: ([Expr Double], MyState)
listsumMLGold =
  ( [ MathSet
        "listSum"
        ( MathBin
            (Just "listSum")
            Plus
            (MathVar "singleThing")
            (MathVar "listThing")
        )
    ],
    emptyState
      { symtabF =
          [ ("singleThing", Val (Just "singleThing") 10.0),
            ( "listSum",
              MathBin
                (Just "listSum")
                Plus
                (MathVar "singleThing")
                (MathVar "listThing")
            )
          ],
        symtabL =
          [ ( "listThing",
              MathList
                Nothing
                [Val Nothing 1.0, Val Nothing 2.0, Val Nothing 3.0]
            )
          ]
      }
  )

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

testPauGold :: ([Expr a], MyState)
testPauGold = (
  [ MathSet "How Much Money Do You Get" ( MathVar "PAU0" ) ], emptyState
  { symtabF =
      [
          ( "Step 1", MathITE
              ( Just "Step 1" )
              ( PredVar "there were past ADD payouts" )
              ( MathVar "claimable limited base ADD benefit" )
              ( MathVar "base ADD benefit" )
          ),
          ( "claimable limited base ADD benefit", MathBin
              ( Just "claimable limited base ADD benefit" ) Minus
              ( MathVar "claimable limit" )
              ( MathVar "policyHolder.past ADD payouts" )
          ),
          ( "Step 3", MathVar "multiplied by double triple benefit" ),
          ( "juvenile limited", MathMin
              ( Just "juvenile limited" )
              ( MathVar "Part 1" )
              ( MathVar "juvenile limit" )
          ),
          ( "ADD benefit", MathMin
              ( Just "ADD benefit" )
              ( MathBin Nothing Plus
                  ( MathVar "addBenefit" )
                  ( MathVar "otherBenefits" )
              )
              ( MathVar "risk cap" )
          ),
          ( "addBenefit", MathVar "PAU4" ),
          ( "The Answer", MathITE
              ( Just "The Answer" )
              ( PredVar "user input.accident_claim.selected" )
              ( MathVar "accident branch" )
              ( MathVar "illness branch" )
          ),
          ( "accident branch", MathITE
              ( Just "accident branch" )
              ( PredVar "ADD is disqualified entirely" )
              ( MathVar "excludedZero" )
              ( MathVar "ADD benefit" )
          ),
          ( "illness", MathITE
              ( Just "illness" )
              ( PredFold Nothing PLOr
                  [ PredVar "illness.general exclusions apply", PredVar "policy.ended"
                  ]
              )
              ( MathVar "disqualified" )
              ( Undefined ( Just "No otherwise case" ) )
          ),
          ("policyHolder.age",Val (Just "policyHolder.age") 50.0),
          ( "PAU4", MathVar "Step 1" ),
          ( "Step 2", MathITE
              ( Just "Step 2" )
              ( PredVar "accident.juvenile limit applies" )
              ( MathVar "juvenile limited" )
              ( MathVar "Step 1" )
          ),
          ( "multiplied by double triple benefit", MathITE
              ( Just "multiplied by double triple benefit" )
              ( PredVar "accident.triple benefits apply" )
              ( MathBin
                  ( Just "multiplied by double triple benefit" ) Times
                  ( MathVar "Step 2" )
                  ( Val Nothing 3.0 )
              )
              ( MathITE Nothing
                  ( PredVar "accident.double benefits apply" )
                  ( MathBin
                      ( Just "multiplied by double triple benefit" ) Times
                      ( MathVar "Step 2" )
                      ( Val Nothing 2.0 )
                  )
                  ( MathVar "Step 2" )
              )
          ),
          ( "PAU0", MathVar "The Answer" ),
          ( "Step 4", MathVar "Step 3 discounted by accident.risk percentage"
          ),
          ( "subsidiary computations", MathSet "claimable limit"
              ( MathMin
                  ( Just "claimable limit" )
                  ( MathBin Nothing Times
                      ( Val Nothing 1.5 )
                      ( MathVar "total sum assured" )
                  )
                  ( MathVar "lifetime claimable limit" )
              )
          ),
          ( "base ADD benefit", MathVar "policy.benADD" ),
          ( "Step 3 discounted by accident.risk percentage", MathBin
              ( Just "Step 3 discounted by accident.risk percentage" ) Times
              ( MathVar "Step 3" )
              ( MathBin Nothing Minus
                  ( Val Nothing 1.0 )
                  ( MathVar "accident.risk percentage" )
              )
          ),
          ( "otherBenefits", Val ( Just "otherBenefits" ) 50.0 ),
          ( "excludedZero", Val ( Just "excludedZero" ) 0.0 ),
          ( "lifetime claimable limit", Val
              ( Just "lifetime claimable limit" ) 4500000.0
          ),
          ( "juvenile limit", Val ( Just "juvenile limit" ) 500000.0 ),
          ( "Top-Level", MathSet "How Much Money Do You Get"
              ( MathVar "PAU0" )
          ),
          ( "illness branch", MathITE
              ( Just "illness branch" )
              ( PredVar "illness.disqualified" )
              ( MathVar "excludedZero" )
              ( MathVar "policy.benMR" )
          )
      ], symtabP =
      [
          ( "ADD is disqualified entirely", PredFold
              ( Just "ADD is disqualified entirely" ) PLOr
              [ PredComp Nothing CGTE
                  ( MathVar "policyHolder.age" )
                  ( Val Nothing 75.0 ), PredVar "accident.general exclusions apply", PredVar "policy.ended"
              ]
          ),
          ( "there were past ADD payouts", PredComp
              ( Just "there were past ADD payouts" ) CGT
              ( MathVar "policyHolder.past ADD payouts" )
              ( Val Nothing 0.0 )
          )
      ]
    }
  )

pausGlobalVarsGold :: GlobalVars
pausGlobalVarsGold = MkGlobalVars [
  ( MkVar "x", Just ( L4EntType "Number" ) ),
  ( MkVar "y", Just ( L4EntType "Number" ) ),
  ( MkVar "user input", Just ( L4EntType "Dictionary" ) ),
  ( MkVar "accident", Just ( L4EntType "Accident" ) ),
  ( MkVar "addBenefit", Just ( L4EntType "Number" ) ),
  ( MkVar "otherBenefits", Just ( L4List ( L4EntType "Number" ) ) ),
  ( MkVar "total sum assured", Just ( L4EntType "Number" ) ),
  ( MkVar "policyHolder", Just ( L4EntType "PolicyHolder" ) ),
  ( MkVar "policy", Just ( L4EntType "Policy" ) ),
  ( MkVar "illness", Just ( L4EntType "Claim" ) )
  ]

emptyE :: Map.HashMap () ()
emptyE = mempty

mustsing5 :: [Rule]
mustsing5 = [ Regulative
  { subj = Leaf
      (
          ( MTT "Person" :| []
          , Nothing
          ) :| []
      )
  , rkeyword = REvery
  , who = Just
      ( Leaf
          ( RPMT
              [ MTT "Qualifies" ]
          )
      )
  , cond = Nothing
  , deontic = DMust
  , action = Leaf
      (
          ( MTT "sing" :| []
          , Nothing
          ) :| []
      )
  , temporal = Nothing
  , hence = Nothing
  , lest = Nothing
  , rlabel = Nothing
  , lsource = Nothing
  , srcref = Just
      ( SrcRef
          { url = "/var/nl4/e4e823bb-5b45-4e90-8fd7-0b32fa4d94ca/1ZtuEY2DAMh5sZ16o7Lgp8RH9dVdamZgfQrU0sklmvrw/259009466/20240605T075302.515555Z.csv"
          , short = "/var/nl4/e4e823bb-5b45-4e90-8fd7-0b32fa4d94ca/1ZtuEY2DAMh5sZ16o7Lgp8RH9dVdamZgfQrU0sklmvrw/259009466/20240605T075302.515555Z.csv"
          , srcrow = 2
          , srccol = 1
          , version = Nothing
          }
      )
  , upon = Nothing
  , given = Nothing
  , having = Nothing
  , wwhere = []
  , defaults = []
  , symtab = []
  }
  , Hornlike
  { name =
      [ MTT "Drinks" ]
  , super = Nothing
  , keyword = Means
  , given = Nothing
  , giveth = Nothing
  , upon = Nothing
  , clauses =
    [ HC
        { hHead = RPBoolStructR
            [ MTT "Drinks" ] RPis
            ( All Nothing
                [ Any
                    ( Just
                        ( Pre "consumes" )
                    )
                    [ Leaf
                        ( RPMT
                            [ MTT "an alcoholic beverage" ]
                        )
                    , Leaf
                        ( RPMT
                            [ MTT "a non-alcoholic beverage" ]
                        )
                    ]
                , Any
                    ( Just
                        ( Pre "whether" )
                    )
                    [ Leaf
                        ( RPMT
                            [ MTT "in part" ]
                        )
                    , Leaf
                        ( RPMT
                            [ MTT "in whole" ]
                        )
                    ]
                ]
              )
          , hBody = Nothing
          }
      ]
  , rlabel = Just
      ( "§"
      , 1
      , "What does it mean to drink?"
      )
  , lsource = Nothing
  , wwhere = []
  , srcref = Just
      ( SrcRef
          { url = "/var/nl4/e4e823bb-5b45-4e90-8fd7-0b32fa4d94ca/1ZtuEY2DAMh5sZ16o7Lgp8RH9dVdamZgfQrU0sklmvrw/259009466/20240605T075302.515555Z.csv"
          , short = "/var/nl4/e4e823bb-5b45-4e90-8fd7-0b32fa4d94ca/1ZtuEY2DAMh5sZ16o7Lgp8RH9dVdamZgfQrU0sklmvrw/259009466/20240605T075302.515555Z.csv"
          , srcrow = 3
          , srccol = 15
          , version = Nothing
          }
      )
  , defaults = []
  , symtab = []
  }
  , TypeDecl
  { name =
      [ MTT "Person" ]
  , super = Nothing
  , has =
      [ TypeDecl
          { name =
              [ MTT "drinks" ]
          , super = Just
              ( SimpleType TOne "Boolean" )
          , has = []
          , enums = Nothing
          , given = Nothing
          , upon = Nothing
          , rlabel = Nothing
          , lsource = Nothing
          , srcref = Nothing
          , defaults = []
          , symtab = []
          }
      , TypeDecl
          { name =
              [ MTT "eats" ]
          , super = Just
              ( SimpleType TOne "Boolean" )
          , has = []
          , enums = Nothing
          , given = Nothing
          , upon = Nothing
          , rlabel = Nothing
          , lsource = Nothing
          , srcref = Nothing
          , defaults = []
          , symtab = []
          }
      , TypeDecl
          { name =
              [ MTT "walks" ]
          , super = Just
              ( SimpleType TOne "Boolean" )
          , has = []
          , enums = Nothing
          , given = Nothing
          , upon = Nothing
          , rlabel = Nothing
          , lsource = Nothing
          , srcref = Nothing
          , defaults = []
          , symtab = []
          }
      , TypeDecl
          { name =
              [ MTT "an alcoholic beverage" ]
          , super = Just
              ( SimpleType TOne "Boolean" )
          , has = []
          , enums = Nothing
          , given = Nothing
          , upon = Nothing
          , rlabel = Nothing
          , lsource = Nothing
          , srcref = Nothing
          , defaults = []
          , symtab = []
          }
      , TypeDecl
          { name =
              [ MTT "a non-alcoholic beverage" ]
          , super = Just
              ( SimpleType TOne "Boolean" )
          , has = []
          , enums = Nothing
          , given = Nothing
          , upon = Nothing
          , rlabel = Nothing
          , lsource = Nothing
          , srcref = Nothing
          , defaults = []
          , symtab = []
          }
      , TypeDecl
          { name =
              [ MTT "in part" ]
          , super = Just
              ( SimpleType TOne "Boolean" )
          , has = []
          , enums = Nothing
          , given = Nothing
          , upon = Nothing
          , rlabel = Nothing
          , lsource = Nothing
          , srcref = Nothing
          , defaults = []
          , symtab = []
          }
      , TypeDecl
          { name =
              [ MTT "in whole" ]
          , super = Just
              ( SimpleType TOne "Boolean" )
          , has = []
          , enums = Nothing
          , given = Nothing
          , upon = Nothing
          , rlabel = Nothing
          , lsource = Nothing
          , srcref = Nothing
          , defaults = []
          , symtab = []
          }
      ]
  , enums = Nothing
  , given = Nothing
  , upon = Nothing
  , rlabel = Just
      ( "§"
      , 1
      , "People consume food and drink"
      )
  , lsource = Nothing
  , srcref = Just
      ( SrcRef
          { url = "/var/nl4/e4e823bb-5b45-4e90-8fd7-0b32fa4d94ca/1ZtuEY2DAMh5sZ16o7Lgp8RH9dVdamZgfQrU0sklmvrw/259009466/20240605T075302.515555Z.csv"
          , short = "/var/nl4/e4e823bb-5b45-4e90-8fd7-0b32fa4d94ca/1ZtuEY2DAMh5sZ16o7Lgp8RH9dVdamZgfQrU0sklmvrw/259009466/20240605T075302.515555Z.csv"
          , srcrow = 3
          , srccol = 25
          , version = Nothing
          }
      )
  , defaults = []
  , symtab = []
  }
  , Hornlike
  { name =
      [ MTT "Qualifies" ]
  , super = Nothing
  , keyword = Means
  , given = Nothing
  , giveth = Nothing
  , upon = Nothing
  , clauses =
      [ HC
          { hHead = RPBoolStructR
              [ MTT "Qualifies" ] RPis
              ( All Nothing
                  [ Leaf
                      ( RPMT
                          [ MTT "walks" ]
                      )
                  , Any Nothing
                      [ Leaf
                          ( RPMT
                              [ MTT "Drinks" ]
                          )
                      , Leaf
                          ( RPMT
                              [ MTT "eats" ]
                          )
                      ]
                  ]
              )
          , hBody = Nothing
          }
      ]
  , rlabel = Nothing
  , lsource = Nothing
  , wwhere = []
  , srcref = Just
      ( SrcRef
          { url = "/var/nl4/e4e823bb-5b45-4e90-8fd7-0b32fa4d94ca/1ZtuEY2DAMh5sZ16o7Lgp8RH9dVdamZgfQrU0sklmvrw/259009466/20240605T075302.515555Z.csv"
          , short = "/var/nl4/e4e823bb-5b45-4e90-8fd7-0b32fa4d94ca/1ZtuEY2DAMh5sZ16o7Lgp8RH9dVdamZgfQrU0sklmvrw/259009466/20240605T075302.515555Z.csv"
          , srcrow = 6
          , srccol = 7
          , version = Nothing
          }
      )
  , defaults = []
  , symtab = []
  }
  ]

mustsing5Gold = ([MathPred (PredFold (Just "Qualifies") PLAnd [PredVar "walks",PredFold Nothing PLAnd [PredFold Nothing PLOr [PredVar "Drinks",PredVar "eats"]]]),MathPred (PredFold (Just "Drinks") PLAnd [PredFold Nothing PLOr [PredVar "an alcoholic beverage",PredVar "a non-alcoholic beverage"],PredFold Nothing PLAnd [PredFold Nothing PLOr [PredVar "in part",PredVar "in whole"]]])], emptyState {symtabF = Map.fromList [("What does it mean to drink?",MathPred (PredSet "Drinks" (PredFold (Just "Drinks") PLAnd [PredFold Nothing PLOr [PredVar "an alcoholic beverage",PredVar "a non-alcoholic beverage"],PredFold Nothing PLAnd [PredFold Nothing PLOr [PredVar "in part",PredVar "in whole"]]])))], symtabP = Map.fromList [("Qualifies",PredFold (Just "Qualifies") PLAnd [PredVar "walks",PredFold Nothing PLAnd [PredFold Nothing PLOr [PredVar "Drinks",PredVar "eats"]]]),("Drinks",PredFold (Just "Drinks") PLAnd [PredFold Nothing PLOr [PredVar "an alcoholic beverage",PredVar "a non-alcoholic beverage"],PredFold Nothing PLAnd [PredFold Nothing PLOr [PredVar "in part",PredVar "in whole"]]])]})