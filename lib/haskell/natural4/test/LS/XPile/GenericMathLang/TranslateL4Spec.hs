
{-# LANGUAGE OverloadedStrings #-}
module LS.XPile.GenericMathLang.TranslateL4Spec (spec) where

import Test.Hspec ( describe, it, xit, shouldBe, Spec )
import LS.Types
import LS.Rule (Rule(..))
import LS.XPile.MathLang.GenericMathLang.GenericMathLangAST
import LS.XPile.MathLang.GenericMathLang.TranslateL4
import Data.List.NonEmpty (NonEmpty(..), fromList)
import AnyAll qualified as AA
import qualified Data.Text as T
import Data.Maybe (fromJust)
import Prelude hiding (exp, seq)


spec :: Spec
spec = do
    describe "rule 2 into SimpleHL" $ do
      it "should become a SimpleHL" $ do
        let Right toTest = runToLC $ simplifyL4Hlike rule2givens
        baseHL toTest `shouldBe` rule2givens_shl_gold

    testBaseExpify "rule 2 (basic, simple arithmetic) into BaseExp"
                   "should become something that makes sense"
                    rule2nogivens
                    rule2nogivens_gold

    -- Postpone
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
    --                 arithRule2
    --                 arithRule2_gold

    -- testBaseExpify "arithmetics testcase 3"
    --                 "should parse inside a cell"
    --                 arithRule3
    --                 arithRule3_gold


testBaseExpify name desc rule gold =
  describe name $ do
    it desc $ do
      let toTest = runToLC $ baseExpify =<< simplifyL4Hlike rule
      case toTest of
        Right exp -> exp `shouldBe` gold
        Left err -> err `shouldBe` MiscError "" ""




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

-- NB: this is weird/wrong right now, just recording it to track its evolution
-- What should happen if GIVEN includes stuff that looks like predicates?
rule2givens_gold = EIfThen
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
                        { exp = EPred1
                            { predExp = MkExp
                                { exp = ELit
                                    { lit = EString "Singapore" }, md = []
                                }, predArg = MkVar "place of residence"
                            }, md = []
                        }, right = MkExp
                        { exp = EAnd
                            { left = MkExp
                                { exp = ECompOp
                                    { compOp = OpGte, compLeft = MkExp
                                        { exp = EVar
                                            { var = MkVar "age" }, md = []
                                        }, compRight = MkExp
                                        { exp = ELit
                                            { lit = EString "21" }, md = []
                                        }
                                    }, md = []
                                }, right = MkExp
                                { exp = EAnd
                                    { left = MkExp
                                        { exp = ECompOp
                                            { compOp = OpLte, compLeft = MkExp
                                                { exp = EVar
                                                    { var = MkVar "property annual value" }, md = []
                                                }, compRight = MkExp
                                                { exp = ELit
                                                    { lit = EInteger 21000 }, md = []
                                                }
                                            }, md = []
                                        }, right = MkExp
                                        { exp = EAnd
                                            { left = MkExp
                                                { exp = EVarSet
                                                    { vsetVar = MkExp
                                                        { exp = EVar
                                                            { var = MkVar "meets the property eligibility criteria for GSTV-Cash" }
                                                            , md = dummyMetadata

                                                        }, arg = MkExp
                                                        { exp = ELit { lit = EBoolTrue }, md = []
                                                        }
                                                    }, md = []
                                                }, right = MkExp
                                                { exp = EAnd
                                                    { left = MkExp
                                                        { exp = ECompOp
                                                            { compOp = OpLte, compLeft = MkExp
                                                                { exp = EVar
                                                                    { var = MkVar "annual income" }, md = []
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
                            , md = dummyMetadata
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
            { numOp = OpPlus
            , nopLeft = MkExp
                { exp = ENumOp
                    { numOp = OpMul, nopLeft = MkExp
                        { exp = ELit
                            { lit = EString "o1" }, md = []
                        }, nopRight = MkExp
                        { exp = ELit
                            { lit = EString "0.01" }, md = []
                        }
                    }, md = [] }
            , nopRight = MkExp
                { exp = ENumOp
                    { numOp = OpMul, nopLeft = MkExp
                        { exp = ELit
                            { lit = EString "o2" }, md = []
                        }, nopRight = MkExp
                        { exp = ELit
                            { lit = EString "0.07" }, md = []
                        }
                    }, md = []}
            }, md = [] }

arithRule3_gold = ESeq
    { seq = ConsSE
        ( MkExp
            { exp = EVarSet
                { vsetVar = MkExp
                    { exp = EVar
                        { var = MkVar "o3a" }, md = dummyMetadata
                    }, arg = arithExpr3_gold }
            , md = dummyMetadata

            }
        )
        ( ConsSE
            ( MkExp
                { exp = EEmpty, md = dummyMetadata } -- TODO: arithExpr3_gold here too
            )
            ( ConsSE
                ( MkExp
                    { exp = EVarSet
                        { vsetVar = MkExp
                            { exp = EVar { var = MkVar "o3c" }, md = dummyMetadata}
                            , arg = MkExp { exp = ENumOp
                                { numOp = OpPlus, nopLeft = MkExp
                                    { exp = ENumOp
                                        { numOp = OpMul, nopLeft = MkExp
                                            { exp = ELit
                                                { lit = EString "o1" }, md = []
                                            }, nopRight = MkExp
                                            { exp = ELit
                                                { lit = EString "0.01" }, md = []
                                            }
                                        }, md = []
                                    }, nopRight = MkExp
                                    { exp = ENumOp
                                        { numOp = OpPlus, nopLeft = MkExp
                                            { exp = ENumOp
                                                { numOp = OpMul, nopLeft = MkExp
                                                    { exp = ELit
                                                        { lit = EString "o2" }, md = []
                                                    }, nopRight = MkExp
                                                    { exp = ELit
                                                        { lit = EString "0.03" }, md = []
                                                    }
                                                }, md = []
                                            }, nopRight = MkExp
                                            { exp = ENumOp
                                                { numOp = OpMul, nopLeft = MkExp
                                                    { exp = ELit
                                                        { lit = EString "o2" }, md = []
                                                    }, nopRight = MkExp
                                                    { exp = ELit
                                                        { lit = EString "0.04" }, md = []
                                                    }
                                                }, md = []
                                            }
                                        }, md = []
                                    }
                                }, md = []
                            }
                        }, md = dummyMetadata
                    }
                ) EmptySeqE
            )
        )
    }

mkGivens :: [(T.Text, Maybe TypeSig)] -> Maybe ParamText
mkGivens [] = Nothing
mkGivens xs = Just $ fromList $ map mkTypedMulti xs
  where
    mkTypedMulti (t, typesig) = (MTT t :| [], typesig)

mkTestRule :: RuleName
           -> Maybe ParamText  -- given
           -> [HornClause2]    -- clauses
           -> Rule
mkTestRule name given clauses = Hornlike
    { name = name
    , super = Nothing
    , keyword = Decide
    , given = given
    , giveth = Nothing
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