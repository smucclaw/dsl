
{-# LANGUAGE OverloadedStrings #-}
module LS.XPile.GenericMathLang.TranslateL4Spec (spec) where

import Test.Hspec ( describe, it, xit, shouldBe, Spec )
import LS.Types
import LS.Rule (Rule(..), Interpreted(..), defaultL4I)
import LS.XPile.MathLang.GenericMathLang.GenericMathLangAST
import LS.XPile.MathLang.GenericMathLang.TranslateL4
import qualified LS.XPile.MathLang.MathLang as ML
import Explainable.MathLang
import Data.List.NonEmpty (NonEmpty(..), fromList)
import AnyAll qualified as AA
import qualified Data.Text as T
import Data.Maybe (fromJust)
import Prelude hiding (exp, seq)


spec :: Spec
spec = do
    describe "rule 2 into SimpleHL" $ do
      it "should become a SimpleHL" $ do
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

    testBaseExpify "arithmetics testcase 2"
                    "should parse inside a cell"
                    arithRule2
                    arithRule2_gold

    testBaseExpify "arithmetics testcase 3"
                    "should parse inside a cell"
                    arithRule3
                    arithRule3_gold

    testBaseExpify "arithmetics testcase 4"
                    "meng's complex case"
                    arithRule4
                    arithRule4_gold

    describe "toMathLang" $ do
      it "should turn Rules straight to MathLang (via GenericMathLang)" $ do
        let l4i = defaultL4I {origrules = [arithRule2, arithRule3, arithRule4]}
        ML.toMathLang l4i `shouldBe` mathLangGold

testBaseExpify :: String -> String -> Rule -> BaseExp -> Spec
testBaseExpify name desc rule gold =
  describe name $ do
    it desc $ do
      let toTest = runToLC $ l4ToLCProgram [rule]
--      let toTest = runToLC $ l4sHLsToLCExp  =<< simplifyL4Hlike rule
      case toTest of
        Right p -> exp (lcProgram p) `shouldBe` gold
        Left err -> err `shouldBe` MiscError "" ""

-----------------------------------------------------------------------------
-- gold for mathlang transformation

mathLangGold = [ MathSet "m3a"
    ( MathBin Nothing Times
        ( MathVar "m1" )
        ( MathVar "m2" )
    )
  , MathSet "m3b"
    ( MathBin Nothing Times
        ( MathVar "m1" )
        ( MathVar "m2" )
    )
  , MathSet "m3c"
    ( MathBin Nothing Times
        ( MathVar "m1" )
        ( MathVar "m2" )
    )
  , MathSet "o3a"
    ( MathBin Nothing Plus
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
    ( MathBin Nothing Plus
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
    ( MathBin Nothing Plus
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
    )
  , MathITE Nothing
    ( PredComp Nothing CEQ
        ( MathVar "phaseOfMoon" )
        ( MathVar "gibbous" )
    )
    ( MathSet "taxesPayable"
        ( MathBin Nothing Divide
            ( MathVar "taxesPayableAlive" )
            ( Val Nothing 2.0 )
        )
    ) ( Undefined Nothing )
  , MathITE Nothing
    ( PredVar "vivacity" )
    ( MathSet "taxesPayable"
        ( MathVar "taxesPayableAlive" )
    ) ( Undefined Nothing )
  , MathITE Nothing
    ( PredComp Nothing CEQ
        ( MathVar "phaseOfMoon" )
        ( MathVar "waxing" )
    )
    ( MathSet "taxesPayable"
        ( MathBin Nothing Divide
            ( MathVar "taxesPayableAlive" )
            ( Val Nothing 3.0 )
        )
    ) ( Undefined Nothing )
  , MathITE Nothing
    ( PredComp Nothing CEQ
        ( MathVar "phaseOfMoon" )
        ( MathVar "full" )
    )
    ( MathSet "taxesPayable"
        ( MathVar "waived" )
    ) ( Undefined Nothing )
  , MathITE Nothing
    ( PredVar "TODO: not implemented yet" )
    ( MathSet "taxesPayable"
        ( Val Nothing 0.0 )
    ) ( Undefined Nothing )
  , MathSet "taxesPayableAlive"
    ( MathBin Nothing Plus
        ( MathVar "income tax component" )
        ( MathVar "asset tax component" )
    )
  , MathSet "income tax component"
    ( MathBin Nothing Times
        ( MathVar "annualIncome" )
        ( MathVar "incomeTaxRate" )
    )
  , MathSet "asset tax component"
    ( MathBin Nothing Times
        ( MathVar "netWorth" )
        ( MathVar "assetTaxRate" )
    )
  , MathSet "incomeTaxRate"
    ( Val Nothing 1.0e-2 )
  , MathSet "assetTaxRate"
    ( Val Nothing 7.0e-2 )
  ]

-----------------------------------------------------------------------------
-- Test rules

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
                                                                }, appArg = MkVar "ind"
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


rule2givens =
  let description = [ MTT "case 2 qualifies" ]
  in mkTestRule
        description
        (mkGivens $ map (\x -> (x, Nothing)) ["place of residence", "age", "property annual value", "meets the property eligibility criteria for GSTV-Cash", "annual income"])
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

rule2givens_shl_gold = OneClause (HeadAndBody MkHnBHC {
                   hbHead = RPMT [MTT "case 2 qualifies"],
                   hbBody = AA.All Nothing [AA.Leaf (RPMT [MTT "Singapore citizen"]), AA.Leaf (RPConstraint [MTT "place of residence"] RPis [MTT "Singapore"]), AA.Leaf (RPConstraint [MTT "age"] RPgte [MTT "21"]), AA.Leaf (RPConstraint [MTT "property annual value"] RPlte [MTI 21000]), AA.Leaf (RPMT [MTT "meets the property eligibility criteria for GSTV-Cash"]), AA.Leaf (RPConstraint [MTT "annual income"] RPlte [MTI 34000])]
                 })

rule2nogivens = rule2givens {given = Nothing}
rule2nogivens_shl_gold = OneClause (HeadAndBody MkHnBHC {
                   hbHead = RPMT [MTT "case 2 qualifies"],
                   hbBody = AA.All Nothing [AA.Leaf (RPMT [MTT "Singapore citizen"]), AA.Leaf (RPConstraint [MTT "place of residence"] RPis [MTT "Singapore"]), AA.Leaf (RPConstraint [MTT "age"] RPgte [MTT "21"]), AA.Leaf (RPConstraint [MTT "property annual value"] RPlte [MTI 21000]), AA.Leaf (RPMT [MTT "meets the property eligibility criteria for GSTV-Cash"]), AA.Leaf (RPConstraint [MTT "annual income"] RPlte [MTI 34000])]
                 })

-- Simple case? The conditions are all checked with OpBoolEq, OpStringEq and Op[GL]te
-- No Vars, because there were no givens in the original rule
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

arithRule3 = mkTestRule
                [ MTT "o3a" ]
                (mkGivens [("o1", Just ( SimpleType TOne "Number" )), ("o2", Just ( SimpleType TOne "Number" ))])
                [ HC
                            { hHead = RPConstraint
                                [ MTT "o3a" ] RPis
                                [ MTT "o1 * 0.01"
                                , MTT "+"
                                , MTT "o2 * 0.07"
                                ]
                            , hBody = Nothing
                            }
                        , HC
                            { hHead = RPnary RPis
                                [ RPMT
                                    [ MTT "o3b" ]
                                , RPnary RPsum
                                    [ RPnary RPproduct
                                        [ RPMT
                                            [ MTT "o1" ]
                                        , RPMT
                                            [ MTF 1.0e-2 ]
                                        ]
                                    , RPnary RPproduct
                                        [ RPMT
                                            [ MTT "o2" ]
                                        , RPMT
                                            [ MTF 7.0e-2 ]
                                        ]
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

arithRule2_gold = ESeq
    { seq = ConsSE
        (arithExpr2_gold "m3a")
        ( ConsSE
            (arithExpr2_gold "m3b")
            ( ConsSE
                (arithExpr2_gold "m3c")
                EmptySeqE )
        )
    }

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

arithRule3_gold = ESeq
    { seq = ConsSE
        ( MkExp
            { exp = EVarSet
                { vsetVar = MkExp
                    { exp = EVar
                        { var = MkVar "o3a" }, md = mkMetadata ( Inferred "Number" )
                    }, arg = arithExpr3_gold }
            , md = [] }
        )
        ( ConsSE
            ( MkExp
                { exp = EVarSet
                    { vsetVar = MkExp
                        { exp = EVar
                            { var = MkVar "o3b" }, md = mkMetadata ( Inferred "Number" )
                        }, arg = arithExpr3_gold }
                , md = [] }
            )
            ( ConsSE
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
                ) EmptySeqE
            )
        )
    }

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
arithRule4_gold = ESeq {seq = ConsSE (MkExp {exp = ESeq {seq = ConsSE (MkExp {exp = EIfThen {condExp = MkExp {exp = EIs {isLeft = MkExp {exp = EVar {var = MkVar "phaseOfMoon"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}, isRight = MkExp {exp = EVar {var = MkVar "gibbous"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}}, md = []}, thenExp = MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "taxesPayable"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ENumOp {numOp = OpDiv, nopLeft = MkExp {exp = EVar {var = MkVar "taxesPayableAlive"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, nopRight = MkExp {exp = ELit {lit = EInteger 2}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []}}, md = []}) (ConsSE (MkExp {exp = EIfThen {condExp = MkExp {exp = EVar {var = MkVar "vivacity"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (FromUser (MkL4EntType "Boolean")), explnAnnot = Nothing}]}, thenExp = MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "taxesPayable"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}, arg = MkExp {exp = EVar {var = MkVar "taxesPayableAlive"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}}, md = []}}, md = []}) (ConsSE (MkExp {exp = EIfThen {condExp = MkExp {exp = EIs {isLeft = MkExp {exp = EVar {var = MkVar "phaseOfMoon"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}, isRight = MkExp {exp = EVar {var = MkVar "waxing"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}}, md = []}, thenExp = MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "taxesPayable"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ENumOp {numOp = OpDiv, nopLeft = MkExp {exp = EVar {var = MkVar "taxesPayableAlive"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, nopRight = MkExp {exp = ELit {lit = EInteger 3}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []}}, md = []}) (ConsSE (MkExp {exp = EIfThen {condExp = MkExp {exp = EIs {isLeft = MkExp {exp = EVar {var = MkVar "phaseOfMoon"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}, isRight = MkExp {exp = EVar {var = MkVar "full"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}}, md = []}, thenExp = MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "taxesPayable"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}, arg = MkExp {exp = EVar {var = MkVar "waived"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Nothing, explnAnnot = Nothing}]}}, md = []}}, md = []}) (ConsSE (MkExp {exp = EIfThen {condExp = MkExp {exp = ELit {lit = EBoolTrue}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Bool"), explnAnnot = Nothing}]}, thenExp = MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "taxesPayable"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ELit {lit = EInteger 0}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []}}, md = []}) (ConsSE (MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "taxesPayableAlive"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ENumOp {numOp = OpPlus, nopLeft = MkExp {exp = ELit {lit = EString "income tax component"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, nopRight = MkExp {exp = ELit {lit = EString "asset tax component"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []}) (ConsSE (MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "income tax component"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ENumOp {numOp = OpMul, nopLeft = MkExp {exp = EVar {var = MkVar "annualIncome"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (FromUser (MkL4EntType "Number")), explnAnnot = Nothing}]}, nopRight = MkExp {exp = EVar {var = MkVar "incomeTaxRate"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []}) (ConsSE (MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "asset tax component"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ENumOp {numOp = OpMul, nopLeft = MkExp {exp = EVar {var = MkVar "netWorth"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (FromUser (MkL4EntType "Number")), explnAnnot = Nothing}]}, nopRight = MkExp {exp = EVar {var = MkVar "assetTaxRate"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []}) (ConsSE (MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "incomeTaxRate"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ELit {lit = EFloat 1.0e-2}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []}) (ConsSE (MkExp {exp = EVarSet {vsetVar = MkExp {exp = EVar {var = MkVar "assetTaxRate"}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}, arg = MkExp {exp = ELit {lit = EFloat 7.0e-2}, md = [MkExpMetadata {srcPos = MkPositn {row = 0, col = 0}, typeLabel = Just (Inferred "Number"), explnAnnot = Nothing}]}}, md = []}) EmptySeqE)))))))))}, md = []}) EmptySeqE}

mkGivens :: [(T.Text, Maybe TypeSig)] -> Maybe ParamText
mkGivens [] = Nothing
mkGivens xs = Just $ fromList $ map mkTypedMulti xs
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


simplifiedFib = mkTestRule
            [ MTT "fib n" ]
            (mkGivens [("n", Just (SimpleType TOne "Number" ))])
            [ HC { hHead = RPConstraint
                    [ MTT "fib n" ] RPis [ MTI 0 ]
            , hBody = Nothing
            }]

dummyMetadata = [ MkExpMetadata
                    { srcPos = MkPositn
                        { row = 0, col = 0
                        }, typeLabel = Nothing, explnAnnot = Nothing
                    }
                ]
mkMetadata typelabel = [ MkExpMetadata
                    { srcPos = MkPositn
                        { row = 0, col = 0
                        }, typeLabel = Just typelabel, explnAnnot = Nothing
                    }
                ]