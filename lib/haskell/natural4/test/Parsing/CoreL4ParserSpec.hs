{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Parsing.CoreL4ParserSpec (spec) where

-- import qualified Test.Hspec.Megaparsec as THM

import AnyAll (BoolStruct (All), mkLeaf)
import Data.ByteString.Lazy qualified as BS
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String.Interpolate (i)
import Data.Text qualified as T
import LS.BasicTypes (MyStream, MyToken (Decide))
import LS.Interpreter (l4interpret, classHierarchy, getCTkeys)
import LS.Lib
  ( exampleStreams,
    pRules,
    pToplevel,
    pTypeDeclaration,
  )
import LS.Rule
  ( Rule (clauses, given, has, keyword, name, rlabel, srcref, super),
    defaultHorn,
    defaultTypeDecl,
    mkTestSrcRef,
    runMyParser,
  )
import LS.Types
  ( HornClause (HC, hBody, hHead),
    MTExpr (MTI, MTT),
    MyStream,
    MyToken (Decide),
    ParamType (TOne),
    RPRel (RPgt, RPis),
    RelationalPredicate (RPConstraint),
    RunConfig (debug, sourceURL),
    TypeSig (InlineEnum, SimpleType),
    clsParent,
    defaultRC,
    extendedAttributes,
    mkLeafR,
    thisAttributes,
  )
import LS.XPile.CoreL4
import LS.XPile.Logging (fromxpLogE)
import System.FilePath ((-<.>), (</>))
import Test.Hspec
  ( HasCallStack,
    Spec,
    SpecWith,
    describe,
    it,
    xit,
  )
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec
  ( ParseErrorBundle,
    ShowErrorComponent,
    some,
  )

filetest :: (HasCallStack, ShowErrorComponent e, Show b, Eq b) => String -> String -> (String -> MyStream -> Either (ParseErrorBundle MyStream e) b) -> b -> SpecWith ()
filetest testfile desc parseFunc expected =
  it [i|#{testfile}: #{desc}|] do
  testcsv <- BS.readFile $ "test" </> "Parsing" </> "corel4" </> testfile -<.> "csv"
  parseFunc testfile `traverse` exampleStreams testcsv
    `shouldParse` [ expected ]

spec :: Spec
spec  = do
    let runConfig = defaultRC { sourceURL = T.pack $ "test" </> "Spec" }
        runConfigDebug = runConfig { debug = True }
    let  combine = uncurry (<>)
    let _parseWith1 f x y s = f <$> runMyParser combine runConfigDebug x y s
    let  parseR       x y s = runMyParser combine runConfig x y s
    let _parseR1      x y s = runMyParser combine runConfigDebug x y s
    let _parseOther1  x y s = runMyParser id      runConfigDebug x y s
    let  parseOther   x y s = runMyParser id      runConfig x y s

    -- [TODO] it'd be nice to get this working as a filetest rather than the manual way
    describe "transpiler to CoreL4" do
      xit "should output a class declaration for seca.csv" do
        let testfile = "seca"
        testcsv <- BS.readFile $ "test" </> testfile -<.> "csv"
        let rules  = parseR pRules "" `traverse` exampleStreams testcsv
        interpreted <- (traverse . traverse) l4interpret rules
        (fmap (fromxpLogE . sfl4ToCorel4) <$> interpreted) `shouldParse` ["\n#\n# outputted directly from XPile/CoreL4.hs\n#\n\n\n\n-- [SecA_RecoverPassengersVehicleAuthorizedOp]\ndecl s: Situation\n\n--facts\n\nfact <SecA_RecoverPassengersVehicleAuthorizedOp> fromList [([\"s\"],((Just (SimpleType TOne \"Situation\"),[]),[]))]\n\n\n# directToCore\n\n\nrule <SecA_RecoverPassengersVehicleAuthorizedOp>\nfor s: Situation\nif (secA_Applicability && currentSit_s && s == missingKeys)\nthen coverProvided s recoverPassengersVehicleAuthorizedOp SecA_RecoverPassengersVehicleAuthorizedOp\n\n\n"]

      filetest "class-1" "type definitions"
        (parseR pRules)
        [ defaultTypeDecl
            { name = [MTT "Class1"],
              super = Just (SimpleType TOne "Object"),
              has =
                [ defaultTypeDecl
                    { name = [MTT "id"],
                      super = Just (SimpleType TOne "Integer")
                    }
                ],
              srcref = mkTestSrcRef 1 1
            },
          defaultTypeDecl
            { name = [MTT "Class2"],
              super = Just (SimpleType TOne "Class1"),
              has =
                [ defaultTypeDecl
                    { name = [MTT "firstname"],
                      super = Just (SimpleType TOne "String")
                    },
                  defaultTypeDecl
                    { name = [MTT "lastname"],
                      super = Just (SimpleType TOne "String")
                    },
                  defaultTypeDecl
                    { name = [MTT "office address"],
                      super = Nothing,
                      has =
                        [ defaultTypeDecl
                            { name = [MTT "line1"],
                              super = Just (SimpleType TOne "String")
                            },
                          defaultTypeDecl
                            { name = [MTT "line2"],
                              super = Just (SimpleType TOne "String")
                            }
                        ]
                    },
                  defaultTypeDecl
                    { name = [MTT "bar address"],
                      super = Just (SimpleType TOne "address")
                    },
                  defaultTypeDecl
                    { name = [MTT "work address"],
                      super = Just (SimpleType TOne "address"),
                      has =
                        [ defaultTypeDecl
                            { name = [MTT "floor"],
                              super = Just (SimpleType TOne "String")
                            },
                          defaultTypeDecl
                            { name = [MTT "company name"],
                              super = Just (SimpleType TOne "String")
                            }
                        ]
                    }
                ],
              srcref = mkTestSrcRef 1 4
            },
          defaultTypeDecl
            { name = [MTT "address"],
              has =
                [ defaultTypeDecl
                    { name = [MTT "line1"],
                      super = Just (SimpleType TOne "String")
                    },
                  defaultTypeDecl
                    { name = [MTT "line2"],
                      super = Just (SimpleType TOne "String")
                    }
                ],
              srcref = mkTestSrcRef 1 15
            }
        ]


      filetest "class-1" "parent-class identification"
        (parseOther do
                        rules <- some pTypeDeclaration
                        let classH = classHierarchy rules
                            parent = clsParent classH "Class2"
                        return $ parent)
        (Just "Class1",[])

      filetest "class-1" "attribute enumeration"
        (parseOther do
                        rules <- some pTypeDeclaration
                        let classH = classHierarchy rules
                            tA = sort . getCTkeys <$> thisAttributes classH "Class2"
                        return $ tA)
        (Just ["bar address","firstname","lastname","office address","work address"], [])

      filetest "class-1" "extended attribute enumeration"
        (parseOther do
                        rules <- some pTypeDeclaration
                        let classH = classHierarchy rules
                            eA = sort . getCTkeys <$> extendedAttributes classH "Class2"
                        return $ eA)
        (Just ["bar address","firstname","id","lastname","office address","work address"], [])

      filetest "class-fa-1" "financial advisor data modelling"
        (parseR pToplevel)
        [ defaultTypeDecl
            { name = [MTT "FinancialStatus"],
              super = Just (InlineEnum TOne ((MTT <$> "adequate" :| ["inadequate"], Nothing) :| [])),
              srcref = mkTestSrcRef 2 1
            },
          defaultTypeDecl
            { name = [MTT "EarningsStatus"],
              super = Just (InlineEnum TOne ((MTT <$> "steady" :| ["unsteady"], Nothing) :| [])),
              srcref = mkTestSrcRef 2 2
            },
          defaultTypeDecl
            { name = [MTT "InvestmentStrategy"],
              super = Just (InlineEnum TOne ((MTT <$> "savings" :| ["stocks", "combination"], Nothing) :| [])),
              srcref = mkTestSrcRef 2 3
            },
          defaultTypeDecl
            { name = [MTT "Person"],
              has =
                [ defaultTypeDecl
                    { name = [MTT "dependents"],
                      super = Just (SimpleType TOne "Number")
                    },
                  defaultTypeDecl
                    { name = [MTT "amountSaved"],
                      super = Just (SimpleType TOne "Number")
                    },
                  defaultTypeDecl
                    { name = [MTT "earnings"],
                      super = Just (SimpleType TOne "Number")
                    },
                  defaultTypeDecl
                    { name = [MTT "steadiness"],
                      super = Just (SimpleType TOne "EarningsStatus")
                    },
                  defaultTypeDecl
                    { name = [MTT "income"],
                      super = Just (SimpleType TOne "FinancialStatus")
                    },
                  defaultTypeDecl
                    { name = [MTT "savingsAccount"],
                      super = Just (SimpleType TOne "FinancialStatus")
                    },
                  defaultTypeDecl
                    { name = [MTT "isDead"],
                      super = Just (SimpleType TOne "Boolean")
                    },
                  defaultTypeDecl
                    { name = [MTT "spendthrift"],
                      super = Just (SimpleType TOne "Boolean")
                    },
                  defaultTypeDecl
                    { name = [MTT "investment"],
                      super = Just (SimpleType TOne "InvestmentStrategy")
                    }
                ],
              rlabel = Just ("\167", 2, "person type"),
              srcref = mkTestSrcRef 2 5
            }
        ]

      filetest "class-fa-2" "financial advisor decision modelling"
        (parseR pToplevel)
        [ defaultHorn {
          name = MTT <$> ["p", "investment"],
          keyword = Decide,
          given = Just ((MTT "p" :| [], Just (SimpleType TOne "Person")) :| []),
          clauses =
          [ HC
              { hHead = RPConstraint (MTT <$> ["p", "investment"]) RPis [MTT "savings"],
                hBody = Just (mkLeaf (RPConstraint (MTT <$> ["p", "savingsAccount"]) RPis [MTT "inadequate"]))
              },
            HC
              { hHead = RPConstraint (MTT <$> ["p", "investment"]) RPis [MTT "stocks"],
                hBody =
                  Just
                    ( All
                        Nothing
                        [ mkLeaf (RPConstraint (MTT <$> ["p", "savingsAccount"]) RPis [MTT "adequate"]),
                          mkLeaf (RPConstraint (MTT <$> ["p", "income"]) RPis [MTT "adequate"])
                        ]
                    )
              },
            HC
              { hHead = RPConstraint (MTT <$> ["p", "investment"]) RPis [MTT "combination"],
                hBody = Just (mkLeafR "OTHERWISE")
              },
            HC
              { hHead = RPConstraint (MTT <$> ["p", "minSavings"]) RPis [MTT "p's dependents", MTT "*", MTI 5000],
                hBody = Nothing
              },
            HC
              { hHead = RPConstraint (MTT <$> ["p", "savingsAccount"]) RPis [MTT "adequate"],
                hBody = Just (mkLeaf (RPConstraint (MTT <$> ["p", "amountSaved"]) RPgt [MTT "p", MTT "minSavings"]))
              },
            HC
              { hHead = RPConstraint (MTT <$> ["p", "savingsAccount"]) RPis [MTT "inadequate"],
                hBody = Just (mkLeafR "OTHERWISE")
              },
            HC
              { hHead = RPConstraint (MTT <$> ["p", "minIncome"]) RPis [MTT "15000 + 4000 * p's dependents"],
                hBody = Nothing
              },
            HC
              { hHead = RPConstraint (MTT <$> ["p", "income"]) RPis [MTT "adequate"],
                hBody =
                  Just
                    ( All
                        Nothing
                        [ mkLeaf (RPConstraint (MTT <$> ["p", "earnings"]) RPgt (MTT <$> ["p", "minIncome"])),
                          mkLeaf (RPConstraint (MTT <$> ["p", "steadiness"]) RPis [MTT "steady"])
                        ]
                    )
              },
            HC
              { hHead = RPConstraint (MTT <$> ["p", "blah"]) RPis [MTI 42],
                hBody = Nothing
              }
          ]
        }
        ]
