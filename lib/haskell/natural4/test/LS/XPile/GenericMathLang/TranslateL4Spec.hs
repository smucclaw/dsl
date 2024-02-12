
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
import Prelude hiding (exp)


spec :: Spec
spec = do
    describe "rule 2 into SimpleHL" $ do
      it "should become a SimpleHL" $ do
        let Right toTest = runToLC $ simplifyL4Hlike rule2givens
        baseHL toTest `shouldBe` rule2givens_shl_gold

    describe "rule 2 into BaseExp" $ do
      it "should become a BaseExp" $ do
        let Right toTest = runToLC $ baseExpify =<< simplifyL4Hlike rule2givens
        toTest `shouldBe` rule2givens_gold


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

rule2givens =
  let description = [ MTT "case 2 qualifies" ]
  in mkTestRule
        description
        (mkGivens ["place of residence", "age", "property annual value", "meets the property eligibility criteria for GSTV-Cash", "annual income"])
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
                                                            { var = MkVar "meets the property eligibility criteria for GSTV-Cash" }, md =
                                                            [ MkExpMetadata
                                                                { srcPos = MkPositn
                                                                    { row = 0, col = 0
                                                                    }, typeLabel = Nothing, explnAnnot = Nothing
                                                                }
                                                            ]
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
                    { var = MkVar "case 2 qualifies" }, md =
                    [ MkExpMetadata
                        { srcPos = MkPositn
                            { row = 0, col = 0
                            }, typeLabel = Nothing, explnAnnot = Nothing
                        }
                    ]
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

mkGivens :: [T.Text] -> Maybe (NonEmpty (NonEmpty MTExpr, Maybe TypeSig)) --ParamText
mkGivens [] = Nothing
mkGivens xs = Just $ fromList $ map mkTypedMulti xs
  where
    mkTypedMulti :: T.Text -> TypedMulti
    mkTypedMulti t = (MTT t :| [], Nothing)

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
            (Just (( MTT "n" :| []
                  , Just( SimpleType TOne "Number" )) :| []))
            [ HC { hHead = RPConstraint
                    [ MTT "fib n" ] RPis [ MTI 0 ]
            , hBody = Nothing
            }]
