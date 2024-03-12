{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module LS.XPile.GenericMathLang.TranslateL4Spec (spec) where

import AnyAll qualified as AA
import AnyAll (BoolStruct(..))
import Control.Arrow ((>>>))
import Control.Monad (forM_)
import Control.Monad.Trans.Maybe (runMaybeT )
import Control.Monad.Trans.Writer.Lazy (runWriter)
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty (NonEmpty (..), fromList, nonEmpty)
import Data.Text qualified as T
import Explainable
import Explainable.MathLang -- hiding ((|>))
import LS.Rule (Interpreted (..), Rule (..), defaultL4I)
import LS.Types
import LS.XPile.MathLang.GenericMathLang.GenericMathLangAST
import LS.XPile.MathLang.GenericMathLang.TranslateL4
import LS.XPile.MathLang.MathLang qualified as ML
--import LS.XPile.MathLang.GenericMathLang.ToGenericMathLang (toMathLangGen)
import Test.Hspec (Spec, describe, it, shouldBe, xit, pending)
import Prelude hiding (exp, seq)

spec :: Spec
spec = do
    describe "rule 2 into SimpleHL" do
      it "should become a SimpleHL" do
        let toTest = runToLC $ simplifyL4Hlike rule2givens
        case toTest of
          Left err -> err `shouldBe` MiscError "" ""
          Right res -> baseHL res `shouldBe` rule2givens_shl_gold

    -- Postpone
    -- testBaseExpify "rule 2 (basic, simple arithmetic) into BaseExp"
    --                "should become something that makes sense"
    --                 rule2nogivens
    --                 EEmpty

    -- testBaseExpify "rule 2 with givens into BaseExp"
    --                 "should become (eventually) something where the givens are... ??? at least not SetVar"
    --                 rule2givens
    --                 rule2givens_gold

    -- testBaseExpify "rule 3 into BaseExp"
    --                 "should become something with records?"
    --                 rule3predicate
    --                 rule3predicate_gold

    -- testBaseExpify "arithmetics testcase 2"
    --                 "should parse inside a cell"
    --                 [arithRule2]
    --                 [arithRule2_gold]

    -- testBaseExpify "arithmetics testcase 3"
    --                 "should parse inside a cell"
    --                 [arithRule3]
    --                 [arithRule3_gold]

    -- testBaseExpify "arithmetics testcase 4"
    --                 "meng's complex case"
    --                 [arithRule4]
    --                 [arithRule4_gold]

    describe "toMathLang" do
      let l4i = defaultL4I {origrules = [arithRule4]}
          res@(exprs,st) = ML.toMathLang l4i

      it "should turn Rules straight to MathLang (via GenericMathLang)" do
        res `shouldBe` mathLangGold4

      it "should evaluate taxesPayable correctly when info is lacking" do
        case exprs of
          [expr] -> do
            (taxes, _xp, _st, _strs) <-
              xplainE (mempty :: Map.HashMap () ()) st $ eval expr
            taxes `shouldBe` 0.0 -- because we don't know what the phase of the moon is, defaults to 0.0
          _ -> mempty

      it "should evaluate taxesPayable correctly when more info is given" do
        let st' = st {
            symtabP = Map.singleton "vivacity" (PredVal (Just "vivacity") True),
            symtabF = symtabF st <> Map.singleton "annualIncome" (Val (Just "annualIncome") 10000)}
        case exprs of
          [expr] -> do
            (taxes, _xp, _st, _strs) <- xplainE (mempty :: Map.HashMap () ()) st' $ eval expr
            taxes `shouldBe` 50.0 -- now we should know vivacity and annual income
          _ -> mempty

    describe "evalSimple" do
      it "should evaluate 2+2" do
        let l4i = defaultL4I {origrules = [arithRule1]}
        case ML.toMathLang l4i of
          ([],_) -> mempty
          (expr:_,st) -> do
            (e, _xp, _st, _strs) <-
              xplainE (mempty :: Map.HashMap () ()) st $ eval expr
            e `shouldBe` 4.0

    -- testBaseExpify "foo" "bar" [arithRule2withInitializedValues] EEmpty

    describe "evalComplex" do
      it "should evaluate arithRule2" do
        let l4i = defaultL4I {origrules = [arithRule2withInitializedValues]}
        case ML.toMathLang l4i of
          ([], _) -> mempty
          (expr:_, st) -> do
            (res, _xp, _st, _strs) <-
              xplainE (mempty :: Map.HashMap () ()) st $ eval expr
            res `shouldBe` 45.14

      let l4i_ar3 = defaultL4I {origrules = [arithRule3]}
          res_ar3@(exprs,state) = ML.toMathLang l4i_ar3
      it "toMathLang for arithRule3 should take m3a as the toplevel" do
        symtabF state `shouldBe` arithRule3_gold_symtab

    describe "extractVariables" do
      xit "extracts variables and their values from rules" do
        let rl = arithRule4
            expected = Map.fromList
              [ ("taxesPayable", "taxesPayableAlive"),
                ("taxesPayable", "taxesPayableAlive / 2"),
                ("incomeTaxRate", "1.0e-2"),
                ("assetTaxRate", "7.0e-2")
              ]
        getVarVals rl `shouldBe` expected

    testBaseExpify "test lambda expression" "should become a binary function" [testLambda] [testLambda_gold]

    testBaseExpify "test function application" "should be recognised as a function" testFunApp testFunApp_gold

    describe "genitive->record field" do
      let gml = runToLC $ baseExpifyMTEs nestedGenitives
      case gml of
        Left err -> do
          it "unsuccesful, abort" do
            err `shouldBe` MiscError "foo" "bar"
        Right res -> do
          it "should turn xs y into a record x.y" do
            res `shouldBe` nestedGenitives_gold
          it "should turn into correct MathLang" do
            let exp = noExtraMdata res
                (ml, _st) = runWriter $ runMaybeT $ ML.gml2ml exp
            ml `shouldBe` Just (MathVar "ind.friend.age")

testBaseExpify :: String -> String -> [Rule] -> [BaseExp] -> Spec
testBaseExpify name desc rule gold =
  describe name do
    it desc do
      let toTest = runToLC $ l4ToLCProgram rule
--      let toTest = runToLC $ l4sHLsToLCExp  =<< simplifyL4Hlike rule
      case toTest of
        Right p -> fmap exp (lcProgram p) `shouldBe` gold
        Left err -> err `shouldBe` MiscError "" ""

-----------------------------------------------------------------------------
-- gold for mathlang transformation
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
    [ MathSet "taxesPayable" ( MathITE Nothing
        ( PredComp Nothing CEQ
            ( MathVar "phaseOfMoon" )
            ( MathVar "gibbous" )
        )
        ( MathBin
            ( Just "taxesPayable" ) Divide
            ( MathVar "taxesPayableAlive" )
            ( Val Nothing 2.0 )
        )
        ( MathITE Nothing
            ( PredVar "vivacity" )
            ( MathVar "taxesPayableAlive" )
            ( MathITE Nothing
                ( PredComp Nothing CEQ
                    ( MathVar "phaseOfMoon" )
                    ( MathVar "waxing" )
                )
                ( MathBin
                    ( Just "taxesPayable" ) Divide
                    ( MathVar "taxesPayableAlive" )
                    ( Val Nothing 3.0 )
                )
                ( MathITE Nothing
                    ( PredComp Nothing CEQ
                        ( MathVar "phaseOfMoon" )
                        ( MathVar "full" )
                    )
                    ( MathVar "waived" )
                    ( Val ( Just "taxesPayable" ) 0.0 )
                )
            )
        )
    )
    ], MyState
    { symtabF = Map.fromList
        [
            ( "income tax component", MathBin
                ( Just "income tax component" ) Times
                ( MathVar "annualIncome" )
                ( MathVar "incomeTaxRate" )
            ),
            ( "taxesPayable", MathITE Nothing
                ( PredComp Nothing CEQ
                    ( MathVar "phaseOfMoon" )
                    ( MathVar "gibbous" )
                )
                ( MathBin
                    ( Just "taxesPayable" ) Divide
                    ( MathVar "taxesPayableAlive" )
                    ( Val Nothing 2.0 )
                )
                ( MathITE Nothing
                    ( PredVar "vivacity" )
                    ( MathVar "taxesPayableAlive" )
                    ( MathITE Nothing
                        ( PredComp Nothing CEQ
                            ( MathVar "phaseOfMoon" )
                            ( MathVar "waxing" )
                        )
                        ( MathBin
                            ( Just "taxesPayable" ) Divide
                            ( MathVar "taxesPayableAlive" )
                            ( Val Nothing 3.0 )
                        )
                        ( MathITE Nothing
                            ( PredComp Nothing CEQ
                                ( MathVar "phaseOfMoon" )
                                ( MathVar "full" )
                            )
                            ( MathVar "waived" )
                            ( Val ( Just "taxesPayable" ) 0.0 )
                        )
                    )
                )
            ),
            ( "asset tax component", MathBin
                ( Just "asset tax component" ) Times
                ( MathVar "netWorth" )
                ( MathVar "assetTaxRate" )
            ),
            ( "assetTaxRate", Val ( Just "assetTaxRate" ) 7.0e-2 ),
            ( "incomeTaxRate", Val ( Just "incomeTaxRate" ) 1.0e-2 ),
            ( "taxesPayableAlive", MathBin
                ( Just "taxesPayableAlive" ) Plus
                ( MathVar "income tax component" )
                ( MathVar "asset tax component" )
            )
        ], symtabP = Map.empty , symtabL = Map.empty , symtabS = Map.empty
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
rule3predicate_gold :: BaseExp
rule3predicate_gold = EIfThen
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
                                                                }, appArg = (MkExp (EVar (MkVar "ind")) [])
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
                        [ MTT "42.0" ]
                    , hBody = Nothing }
                , HC { hHead = RPConstraint
                        [ MTT "m2" ] RPis
                        [ MTT "3.14" ]
                    , hBody = Nothing }
                , HC { hHead = RPnary RPis
                        [ RPMT
                            [ MTT "result" ]
                        , RPnary RPsum
                            [ RPMT
                                [ MTT "m1" ]
                            , RPMT
                                [ MTT "m2" ]
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
                        , HC
                            { hHead = RPnary RPis
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

arithExpr2_gold :: T.Text -> Exp
arithExpr2_gold name = MkExp
                    { exp = EVarSet
                        { vsetVar = MkExp
                            { exp = EVar
                                { var = MkVar name }
                            , md = mkMetadata (Inferred "Number")
                            }
                        , arg = MkExp
                            { exp = ENumOp
                                { numOp = OpMul
                                , nopLeft = MkExp
                                    { exp = EVar
                                        { var = MkVar "m1" }
                                    , md = mkMetadata (FromUser (MkL4EntType "Number"))
                                    }
                                , nopRight = MkExp
                                    { exp = EVar
                                        { var = MkVar "m2" }
                                    , md = mkMetadata (FromUser (MkL4EntType "Number"))
                                    }
                                }, md = dummyMetadata
                            }
                        }
                        , md = []
                    }

arithRule2_gold :: BaseExp
arithRule2_gold = ESeq
    { seq = consSE
        (arithExpr2_gold "m3a")
        ( consSE
            (arithExpr2_gold "m3b")
            ( consSE
                (arithExpr2_gold "m3c")
                mempty )
        )
    }

arithExpr3_gold :: Exp
arithExpr3_gold =
  MkExp { exp = ENumOp
    { numOp = OpPlus, nopLeft = MkExp
        { exp = ENumOp
            { numOp = OpMul, nopLeft = MkExp
                { exp = EVar
                    { var = MkVar "o1" }, md = mkMetadata ( FromUser ( MkL4EntType "Number" ))
                }, nopRight = MkExp
                { exp = ELit
                    { lit = EFloat 1.0e-2 }, md = mkMetadata ( Inferred "Number" )
                }
            }, md = mkMetadata ( Inferred "Number" )
        }, nopRight = MkExp
        { exp = ENumOp
            { numOp = OpMul, nopLeft = MkExp
                { exp = EVar
                    { var = MkVar "o2" }, md = mkMetadata ( FromUser ( MkL4EntType "Number" ))
                }, nopRight = MkExp
                { exp = ELit
                    { lit = EFloat 7.0e-2 }, md = mkMetadata ( Inferred "Number" )
                }
            }, md = mkMetadata (Inferred "Number")
        }
    }, md = mkMetadata (Inferred "Number")
  }

arithRule3_gold_symtab :: Explainable.MathLang.SymTab (Expr Double)
arithRule3_gold_symtab = Map.fromList
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
arithRule3_gold = ESeq
    { seq = consSE
        ( MkExp
            { exp = EVarSet
                { vsetVar = MkExp
                    { exp = EVar
                        { var = MkVar "o3a" }, md = mkMetadata ( Inferred "Number" )
                    }, arg = arithExpr3_gold }
            , md = [] }
        )
        ( consSE
            ( MkExp
                { exp = EVarSet
                    { vsetVar = MkExp
                        { exp = EVar
                            { var = MkVar "o3b" }, md = mkMetadata ( Inferred "Number" )
                        }, arg = arithExpr3_gold }
                , md = [] }
            )
            ( consSE
                ( MkExp
                    { exp = EVarSet
                        { vsetVar = MkExp
                            { exp = EVar { var = MkVar "o3c" }, md = mkMetadata ( Inferred "Number" )}
                            , arg = MkExp { exp = ENumOp
                                { numOp = OpPlus, nopLeft = MkExp
                                    { exp = ENumOp
                                        { numOp = OpMul, nopLeft = MkExp
                                            { exp = EVar
                                                { var = MkVar "o1" }, md = mkMetadata ( FromUser ( MkL4EntType "Number" ))
                                            }, nopRight = MkExp
                                            { exp = ELit
                                                { lit = EFloat 0.01 }, md = mkMetadata ( Inferred "Number" )
                                            }
                                        }, md = []
                                    }, nopRight = MkExp
                                    { exp = ENumOp
                                        { numOp = OpPlus, nopLeft = MkExp
                                            { exp = ENumOp
                                                { numOp = OpMul, nopLeft = MkExp
                                                    { exp = EVar
                                                        { var = MkVar "o2" }, md = mkMetadata ( FromUser ( MkL4EntType "Number" ))
                                                    }, nopRight = MkExp
                                                    { exp = ELit
                                                        { lit = EFloat 0.03 }, md = mkMetadata ( Inferred "Number" )
                                                    }
                                                }, md = []
                                            }, nopRight = MkExp
                                            { exp = ENumOp
                                                { numOp = OpMul, nopLeft = MkExp
                                                    { exp = EVar
                                                        { var = MkVar "o2" }, md = mkMetadata ( FromUser ( MkL4EntType "Number" ))
                                                    }, nopRight = MkExp
                                                    { exp = ELit
                                                        { lit = EFloat 0.04 }, md = mkMetadata ( Inferred "Number" )
                                                    }
                                                }, md = mkMetadata ( Inferred "Number" )
                                            }
                                        }, md = mkMetadata ( Inferred "Number" )
                                    }
                                }, md = mkMetadata ( Inferred "Number" )
                            }
                        }, md = []
                    }
                ) mempty
            )
        )
    }

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
arithRule4_gold = ESeq {seq = consSE (MkExp {exp = ESeq {seq = consSE (MkExp {exp = EIfThen {condExp = MkExp {exp = EIs {isLeft = MkExp {exp = EVar {var = MkVar "phaseOfMoon"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}, isRight = MkExp {exp = EVar {var = MkVar "gibbous"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}}, md = []}, thenExp = MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "taxesPayable"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ENumOp {numOp = OpDiv, nopLeft = MkExp {exp = EVar {var = MkVar "taxesPayableAlive"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, nopRight = MkExp {exp = ELit {lit = EInteger 2}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []}}, md = []}) (consSE (MkExp {exp = EIfThen {condExp = MkExp {exp = EVar {var = MkVar "vivacity"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (FromUser (MkL4EntType "Boolean")), explnAnnot = Nothing}]}, thenExp = MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "taxesPayable"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}, arg = MkExp {exp = EVar {var = MkVar "taxesPayableAlive"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}}, md = []}}, md = []}) (consSE (MkExp {exp = EIfThen {condExp = MkExp {exp = EIs {isLeft = MkExp {exp = EVar {var = MkVar "phaseOfMoon"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}, isRight = MkExp {exp = EVar {var = MkVar "waxing"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}}, md = []}, thenExp = MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "taxesPayable"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ENumOp {numOp = OpDiv, nopLeft = MkExp {exp = EVar {var = MkVar "taxesPayableAlive"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, nopRight = MkExp {exp = ELit {lit = EInteger 3}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []}}, md = []}) (consSE (MkExp {exp = EIfThen {condExp = MkExp {exp = EIs {isLeft = MkExp {exp = EVar {var = MkVar "phaseOfMoon"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}, isRight = MkExp {exp = EVar {var = MkVar "full"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}}, md = []}, thenExp = MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "taxesPayable"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}, arg = MkExp {exp = EVar {var = MkVar "waived"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}}, md = []}}, md = []}) (consSE (MkExp {exp = EIfThen {condExp = MkExp {exp = ELit {lit = EBoolTrue}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Bool"), explnAnnot = Nothing}]}, thenExp = MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "taxesPayable"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ELit {lit = EInteger 0}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []}}, md = []}) (consSE (MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "taxesPayableAlive"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ENumOp {numOp = OpPlus, nopLeft = MkExp {exp = ELit {lit = EString "income tax component"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, nopRight = MkExp {exp = ELit {lit = EString "asset tax component"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []}) (consSE (MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "income tax component"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ENumOp {numOp = OpMul, nopLeft = MkExp {exp = EVar {var = MkVar "annualIncome"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (FromUser (MkL4EntType "Number")), explnAnnot = Nothing}]}, nopRight = MkExp {exp = EVar {var = MkVar "incomeTaxRate"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []}) (consSE (MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "asset tax component"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ENumOp {numOp = OpMul, nopLeft = MkExp {exp = EVar {var = MkVar "netWorth"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (FromUser (MkL4EntType "Number")), explnAnnot = Nothing}]}, nopRight = MkExp {exp = EVar {var = MkVar "assetTaxRate"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []}) (consSE (MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "incomeTaxRate"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ELit {lit = EFloat 1.0e-2}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []}) (consSE (MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "assetTaxRate"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ELit {lit = EFloat 7.0e-2}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []}) mempty)))))))))}, md = []}) mempty}

nestedGenitives = [ MTT "ind's", MTT "friend's", MTT "age"]

nestedGenitives_gold = ERec {fieldName = MkExp {exp = ERec {fieldName = MkExp {exp = EVar {var = MkVar "age"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}, recName = MkExp {exp = EVar {var = MkVar "friend"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}}, md = []}, recName = MkExp {exp = EVar {var = MkVar "ind"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}}

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
testLambda = Hornlike
    { name =
        [ MTT "discounted by" ]
    , super = Nothing
    , keyword = Decide
    , given = mkGivens [("x", Just (SimpleType TOne "Number")), ("y", Just (SimpleType TOne "Number"))]
    , giveth = Nothing
    , upon = Nothing
    , clauses =
        [ HC
            { hHead = RPConstraint
                [ MTT "x", MTT "discounted by", MTT "y" ] RPis
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
            , srccol = 121
            , version = Nothing
            }
        )
    , defaults = []
    , symtab = []
    }

testLambda_gold = ELam
    { param = MkVar "x", body = MkExp
        { exp = ELam
            { param = MkVar "y", body = MkExp
                { exp = ENumOp
                    { numOp = OpMul, nopLeft = MkExp
                        { exp = EVar
                            { var = MkVar "x" }, md = []
                        }, nopRight = MkExp
                        { exp = ENumOp
                            { numOp = OpMinus, nopLeft = MkExp
                                { exp = ELit
                                    { lit = EInteger 1 }, md = []
                                }, nopRight = MkExp
                                { exp = EVar { var = MkVar "y" }, md = [] }
                            }, md = []
                        }
                    }, md = []
                }
            }, md = []
        }
    }

testFunApp :: [Rule]
testFunApp = testLambda :
  [Hornlike
    { name =
        [ MTT "Step 1" ]
    , super = Nothing
    , keyword = Decide
    , given = Nothing
    , giveth = mkGivens [("Answer", Just (SimpleType TOne "Number"))]
    , upon = Nothing
    , clauses =
        [ HC
            { hHead = RPConstraint
                [ MTT "Answer" ] RPis
                [ MTT "foo"
                , MTT "discounted by"
--                , MTT "accident's"
                , MTT "bar"
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
    , wwhere = []
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
    }]

testFunApp_gold = [ELam {param = MkVar "x", body = MkExp {exp = ELam {param = MkVar "y", body = MkExp {exp = ENumOp {numOp = OpMul, nopLeft = MkExp {exp = EVar {var = MkVar "x"}, md = []}, nopRight = MkExp {exp = ENumOp {numOp = OpMinus, nopLeft = MkExp {exp = ELit {lit = EInteger 1}, md = []}, nopRight = MkExp {exp = EVar {var = MkVar "y"}, md = []}}, md = []}}, md = []}}, md = []}},EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "Answer"}, md = []}, arg = MkExp {exp = EApp {func = MkExp {exp = EApp {func = MkExp {exp = EVar {var = MkVar "discounted by"}, md = []}, appArg = MkExp {exp = EVar {var = MkVar "foo"}, md = []}}, md = []}, appArg = MkExp {exp = EVar {var = MkVar "bar"}, md = []}}, md = []}}]
