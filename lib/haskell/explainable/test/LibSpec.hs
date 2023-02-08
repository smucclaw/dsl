module LibSpec where

import Test.Hspec
import qualified Data.Map as Map
import Control.Monad.RWS
import Data.Tree
import Data.Map ((!))

import Explainable
import Explainable.MathLang
import Explainable.TaxDSL
import Explainable.Lib

scenarios :: Map.Map String Scenario
scenarios = Map.fromList
    [ "1a" ~=>
        [ "ordinary income"      <-~ ["Rents" ~== 72150, "Agriculture" ~== 30000  , "Exempt Capital" ~== 100]
        , "extraordinary income" <-~ [                   "Agriculture" ~== 270000 , "Exempt Capital" ~== 100]
        , "ordinary expenses"    <-~ ["Rents" ~== 2150 , "Independent" ~== 6000   , "Other"          ~== 100000 ]
        ]
    , "1b" ~=>
        [ "ordinary income"      <-~ ["Rents" ~== 72150, "Agriculture" ~== 20000  , "Exempt Capital" ~== 100 ]
        , "ordinary expenses"    <-~ ["Rents" ~== 2150,  "Independent" ~== 6000   , "Other"          ~== 60000 ]
        ]
    , "2" ~=>
        [ "ordinary income"      <-~ [ "Rents" ~== 72150, "Agriculture" ~== 30000 , "Exempt Capital" ~== 100 ]
        , "extraordinary income" <-~ [                    "Agriculture" ~== 270000, "Exempt Capital" ~== 100 ]
        , "ordinary expenses"    <-~ [ "Rents" ~== 2150 , "Independent" ~== 6000 ] ]

    , "test case 1" ~=> [ "ordinary income"      <-~ [ "Rents"   ~== 72150 ]
                        , "extraordinary income" <-~ [ "Rents"   ~== 25000 ] ]
    , "test case 2" ~=> [ "ordinary income"      <-~ [                       "Trade"   ~== 5350   ]
                        , "ordinary expenses"    <-~ [ "Rents"   ~== 45000  ]
                        , "special expenses"     <-~ [ "Rents"   ~== 3200   ]
                        , "extraordinary income" <-~ [                                               "Capital" ~== 225000 ] ]
    , "test case 3" ~=> [ "ordinary income"      <-~ [                       "Trade"   ~== 5350  ]
                        , "ordinary expenses"    <-~ [ "Rents"   ~== 45000 ]
                        , "special expenses"     <-~ [ "Rents"   ~== 3200  ]
                        , "extraordinary income" <-~ [                                               "Capital" ~== 225000 ] ]

    , "test case 3 - fired"   ~=> [ "ordinary income"      <-~ [ "Employment" ~==  22000 ]
                                    , "extraordinary income" <-~ [ "Employment" ~== 130000 ] ]
    , "test case 3 - unfired" ~=> [ "ordinary income"      <-~ [ "Employment" ~==  22000 ] ]
    ]

spec :: Spec
spec = do
  describe "labelFirst" $ do
    let someScenario = LibSpec.scenarios ! "1a"

    it "the sum of all negative elements" $ do
        ((val, xpl), stab, wlog) <- runRWST
                                        (eval $ sumOf $ negativeElementsOf [-2, -1, 0, 1, 2, 3])
                                        (([],["toplevel"]), someScenario)
                                        (MyState Map.empty Map.empty Map.empty)
        val `shouldBe` -3.0
        xpl `shouldBe` sumOfNegativeExplanation
        stab  `shouldBe` MyState Map.empty Map.empty Map.empty
        wlog `shouldBe` []

    it "the product of the doubles of all positive elements, ignoring negative and zero elements" $ do
        ((val, xpl), stab, wlog) <- runRWST
                                        (eval $ productOf $ timesEach 2 $ positiveElementsOf [-2, -1, 0, 1, 2, 3])
                                        (([],["toplevel"]), someScenario)
                                        (MyState Map.empty Map.empty Map.empty)
        val `shouldBe` 48.0
        xpl `shouldBe` productOfDoublesOfPositivesExplanation
        stab  `shouldBe` MyState Map.empty Map.empty Map.empty
        wlog `shouldBe` []

    it "the sum of the doubles of all positive elements and the unchanged original values of all negative elements" $ do
        ((val, xpl), stab, wlog) <- runRWST
                                        (eval $ sumOf $ timesPositives 2 [-2, -1, 0, 1, 2, 3])
                                        (([],["toplevel"]), someScenario)
                                        (MyState Map.empty Map.empty Map.empty)
        val `shouldBe` 9.0
        xpl `shouldBe` sumOfDoublesOfPositivesAndNegativesExplanation
        stab  `shouldBe` MyState Map.empty Map.empty Map.empty
        wlog `shouldBe` []


sumOfNegativeExplanation :: Tree ([String], [String])
sumOfNegativeExplanation = Node
  { rootLabel = ([], ["-3.0 = sum of 2 elements", "- -2.0", "- -1.0"]),
    subForest =
      [ Node
          { rootLabel = ([], ["2 elements were reduced from an original 6", "- -2.0", "- -1.0", "- 0.0", "- 1.0", "- 2.0", "- 3.0"]),
            subForest =
              [ Node
                  { rootLabel = (["Val (-2.0)"], ["included Val (-2.0) due to passing comparison test"]),
                    subForest =
                      [ Node
                          { rootLabel = ([], ["True is the result of comparing (>)"]),
                            subForest =
                              [ Node
                                  { rootLabel = (["toplevel / listfold sum / comparison >: 0.0"], ["0.0: a leaf value"]),
                                    subForest = []
                                  },
                                Node
                                  { rootLabel = ([], ["with"]),
                                    subForest = []
                                  },
                                Node
                                  { rootLabel = (["toplevel / listfold sum / comparison >: -2.0"], ["-2.0: a leaf value"]),
                                    subForest = []
                                  }
                              ]
                          }
                      ]
                  },
                Node
                  { rootLabel = (["Val (-1.0)"], ["included Val (-1.0) due to passing comparison test"]),
                    subForest =
                      [ Node
                          { rootLabel = ([], ["True is the result of comparing (>)"]),
                            subForest =
                              [ Node
                                  { rootLabel = (["toplevel / listfold sum / comparison >: 0.0"], ["0.0: a leaf value"]),
                                    subForest = []
                                  },
                                Node
                                  { rootLabel = ([], ["with"]),
                                    subForest = []
                                  },
                                Node
                                  { rootLabel = (["toplevel / listfold sum / comparison >: -1.0"], ["-1.0: a leaf value"]),
                                    subForest = []
                                  }
                              ]
                          }
                      ]
                  },
                Node
                  { rootLabel = (["Val 0.0"], ["excluded Val 0.0 due to failing comparison test"]),
                    subForest =
                      [ Node
                          { rootLabel = ([], ["False is the result of comparing (>)"]),
                            subForest =
                              [ Node
                                  { rootLabel = (["toplevel / listfold sum / comparison >: 0.0"], ["0.0: a leaf value"]),
                                    subForest = []
                                  },
                                Node
                                  { rootLabel = ([], ["with"]),
                                    subForest = []
                                  },
                                Node
                                  { rootLabel = (["toplevel / listfold sum / comparison >: 0.0"], ["0.0: a leaf value"]),
                                    subForest = []
                                  }
                              ]
                          }
                      ]
                  },
                Node
                  { rootLabel = (["Val 1.0"], ["excluded Val 1.0 due to failing comparison test"]),
                    subForest =
                      [ Node
                          { rootLabel = ([], ["False is the result of comparing (>)"]),
                            subForest =
                              [ Node
                                  { rootLabel = (["toplevel / listfold sum / comparison >: 0.0"], ["0.0: a leaf value"]),
                                    subForest = []
                                  },
                                Node
                                  { rootLabel = ([], ["with"]),
                                    subForest = []
                                  },
                                Node
                                  { rootLabel = (["toplevel / listfold sum / comparison >: 1.0"], ["1.0: a leaf value"]),
                                    subForest = []
                                  }
                              ]
                          }
                      ]
                  },
                Node
                  { rootLabel = (["Val 2.0"], ["excluded Val 2.0 due to failing comparison test"]),
                    subForest =
                      [ Node
                          { rootLabel = ([], ["False is the result of comparing (>)"]),
                            subForest =
                              [ Node
                                  { rootLabel = (["toplevel / listfold sum / comparison >: 0.0"], ["0.0: a leaf value"]),
                                    subForest = []
                                  },
                                Node
                                  { rootLabel = ([], ["with"]),
                                    subForest = []
                                  },
                                Node
                                  { rootLabel = (["toplevel / listfold sum / comparison >: 2.0"], ["2.0: a leaf value"]),
                                    subForest = []
                                  }
                              ]
                          }
                      ]
                  },
                Node
                  { rootLabel = (["Val 3.0"], ["excluded Val 3.0 due to failing comparison test"]),
                    subForest =
                      [ Node
                          { rootLabel = ([], ["False is the result of comparing (>)"]),
                            subForest =
                              [ Node
                                  { rootLabel = (["toplevel / listfold sum / comparison >: 0.0"], ["0.0: a leaf value"]),
                                    subForest = []
                                  },
                                Node
                                  { rootLabel = ([], ["with"]),
                                    subForest = []
                                  },
                                Node
                                  { rootLabel = (["toplevel / listfold sum / comparison >: 3.0"], ["3.0: a leaf value"]),
                                    subForest = []
                                  }
                              ]
                          }
                      ]
                  }
              ]
          },
        Node
          { rootLabel = (["toplevel / listfold sum: -2.0"], ["-2.0: a leaf value"]),
            subForest = []
          },
        Node
          { rootLabel = (["toplevel / listfold sum: -1.0"], ["-1.0: a leaf value"]),
            subForest = []
          }
      ]
  }

productOfDoublesOfPositivesExplanation :: Tree ([String], [String])
productOfDoublesOfPositivesExplanation = Node
  { rootLabel = ([], ["48.0 = product of 3 elements", "- 2.0", "- 4.0", "- 6.0"]),
    subForest =
      [ Node
          { rootLabel = ([], ["fmap mathsection TimesVal 2.0 over 3 elements"]),
            subForest =
              [ Node
                  { rootLabel = ([], ["3 elements were reduced from an original 6", "- -2.0", "- -1.0", "- 0.0", "- 1.0", "- 2.0", "- 3.0"]),
                    subForest =
                      [ Node
                          { rootLabel = (["Val (-2.0)"], ["excluded Val (-2.0) due to failing comparison test"]),
                            subForest =
                              [ Node
                                  { rootLabel = ([], ["False is the result of comparing (<)"]),
                                    subForest =
                                      [ Node
                                          { rootLabel = (["toplevel / listfold product / fmap mathsection / comparison <: 0.0"], ["0.0: a leaf value"]),
                                            subForest = []
                                          },
                                        Node
                                          { rootLabel = ([], ["with"]),
                                            subForest = []
                                          },
                                        Node
                                          { rootLabel = (["toplevel / listfold product / fmap mathsection / comparison <: -2.0"], ["-2.0: a leaf value"]),
                                            subForest = []
                                          }
                                      ]
                                  }
                              ]
                          },
                        Node
                          { rootLabel = (["Val (-1.0)"], ["excluded Val (-1.0) due to failing comparison test"]),
                            subForest =
                              [ Node
                                  { rootLabel = ([], ["False is the result of comparing (<)"]),
                                    subForest =
                                      [ Node
                                          { rootLabel = (["toplevel / listfold product / fmap mathsection / comparison <: 0.0"], ["0.0: a leaf value"]),
                                            subForest = []
                                          },
                                        Node
                                          { rootLabel = ([], ["with"]),
                                            subForest = []
                                          },
                                        Node
                                          { rootLabel = (["toplevel / listfold product / fmap mathsection / comparison <: -1.0"], ["-1.0: a leaf value"]),
                                            subForest = []
                                          }
                                      ]
                                  }
                              ]
                          },
                        Node
                          { rootLabel = (["Val 0.0"], ["excluded Val 0.0 due to failing comparison test"]),
                            subForest =
                              [ Node
                                  { rootLabel = ([], ["False is the result of comparing (<)"]),
                                    subForest =
                                      [ Node
                                          { rootLabel = (["toplevel / listfold product / fmap mathsection / comparison <: 0.0"], ["0.0: a leaf value"]),
                                            subForest = []
                                          },
                                        Node
                                          { rootLabel = ([], ["with"]),
                                            subForest = []
                                          },
                                        Node
                                          { rootLabel = (["toplevel / listfold product / fmap mathsection / comparison <: 0.0"], ["0.0: a leaf value"]),
                                            subForest = []
                                          }
                                      ]
                                  }
                              ]
                          },
                        Node
                          { rootLabel = (["Val 1.0"], ["included Val 1.0 due to passing comparison test"]),
                            subForest =
                              [ Node
                                  { rootLabel = ([], ["True is the result of comparing (<)"]),
                                    subForest =
                                      [ Node
                                          { rootLabel = (["toplevel / listfold product / fmap mathsection / comparison <: 0.0"], ["0.0: a leaf value"]),
                                            subForest = []
                                          },
                                        Node
                                          { rootLabel = ([], ["with"]),
                                            subForest = []
                                          },
                                        Node
                                          { rootLabel = (["toplevel / listfold product / fmap mathsection / comparison <: 1.0"], ["1.0: a leaf value"]),
                                            subForest = []
                                          }
                                      ]
                                  }
                              ]
                          },
                        Node
                          { rootLabel = (["Val 2.0"], ["included Val 2.0 due to passing comparison test"]),
                            subForest =
                              [ Node
                                  { rootLabel = ([], ["True is the result of comparing (<)"]),
                                    subForest =
                                      [ Node
                                          { rootLabel = (["toplevel / listfold product / fmap mathsection / comparison <: 0.0"], ["0.0: a leaf value"]),
                                            subForest = []
                                          },
                                        Node
                                          { rootLabel = ([], ["with"]),
                                            subForest = []
                                          },
                                        Node
                                          { rootLabel = (["toplevel / listfold product / fmap mathsection / comparison <: 2.0"], ["2.0: a leaf value"]),
                                            subForest = []
                                          }
                                      ]
                                  }
                              ]
                          },
                        Node
                          { rootLabel = (["Val 3.0"], ["included Val 3.0 due to passing comparison test"]),
                            subForest =
                              [ Node
                                  { rootLabel = ([], ["True is the result of comparing (<)"]),
                                    subForest =
                                      [ Node
                                          { rootLabel = (["toplevel / listfold product / fmap mathsection / comparison <: 0.0"], ["0.0: a leaf value"]),
                                            subForest = []
                                          },
                                        Node
                                          { rootLabel = ([], ["with"]),
                                            subForest = []
                                          },
                                        Node
                                          { rootLabel = (["toplevel / listfold product / fmap mathsection / comparison <: 3.0"], ["3.0: a leaf value"]),
                                            subForest = []
                                          }
                                      ]
                                  }
                              ]
                          }
                      ]
                  }
              ]
          },
        Node
          { rootLabel = (["toplevel / listfold product / multiplication: 2.0", "2.0\ntoplevel / listfold product / multiplication: 1.0"], ["2.0: which we obtain by multiplying"]),
            subForest =
              [ Node
                  { rootLabel = (["toplevel / listfold product / multiplication: 2.0"], ["2.0: a leaf value"]),
                    subForest = []
                  },
                Node
                  { rootLabel = ([], ["by"]),
                    subForest = []
                  },
                Node
                  { rootLabel = (["2.0\ntoplevel / listfold product / multiplication: 1.0"], ["1.0: a leaf value"]),
                    subForest = []
                  }
              ]
          },
        Node
          { rootLabel = (["toplevel / listfold product / multiplication: 2.0", "2.0\ntoplevel / listfold product / multiplication: 2.0"], ["4.0: which we obtain by multiplying"]),
            subForest =
              [ Node
                  { rootLabel = (["toplevel / listfold product / multiplication: 2.0"], ["2.0: a leaf value"]),
                    subForest = []
                  },
                Node
                  { rootLabel = ([], ["by"]),
                    subForest = []
                  },
                Node
                  { rootLabel = (["2.0\ntoplevel / listfold product / multiplication: 2.0"], ["2.0: a leaf value"]),
                    subForest = []
                  }
              ]
          },
        Node
          { rootLabel = (["toplevel / listfold product / multiplication: 2.0", "2.0\ntoplevel / listfold product / multiplication: 3.0"], ["6.0: which we obtain by multiplying"]),
            subForest =
              [ Node
                  { rootLabel = (["toplevel / listfold product / multiplication: 2.0"], ["2.0: a leaf value"]),
                    subForest = []
                  },
                Node
                  { rootLabel = ([], ["by"]),
                    subForest = []
                  },
                Node
                  { rootLabel = (["2.0\ntoplevel / listfold product / multiplication: 3.0"], ["3.0: a leaf value"]),
                    subForest = []
                  }
              ]
          }
      ]
  }

sumOfDoublesOfPositivesAndNegativesExplanation :: Tree ([String], [String])
sumOfDoublesOfPositivesAndNegativesExplanation = Node {
                   rootLabel = ([], ["9.0 = sum of 6 elements", "- -2.0", "- -1.0", "- 0.0", "- 2.0", "- 4.0", "- 6.0"]),
                   subForest = [Node {
                   rootLabel = ([], ["fmap mathsection TimesVal 2.0 over 3 relevant elements (who pass Val 0.0 CLT)"]),
                   subForest = [Node {
                   rootLabel = (["Val (-2.0)", "Val (-1.0)", "Val 0.0", "Val 1.0", "Val 2.0", "Val 3.0"], ["base MathList with 6 elements"]),
                   subForest = []
                 }, Node {
                   rootLabel = ([], ["selection of relevant elements"]),
                   subForest = [Node {
                   rootLabel = ([], ["False is the result of comparing (<)"]),
                   subForest = [Node {
                   rootLabel = (["toplevel / listfold sum / fmap mathsection if / comparison <: 0.0"], ["0.0: a leaf value"]),
                   subForest = []
                 }, Node {
                   rootLabel = ([], ["with"]),
                   subForest = []
                 }, Node {
                   rootLabel = (["toplevel / listfold sum / fmap mathsection if / comparison <: -2.0"], ["-2.0: a leaf value"]),
                   subForest = []
                 }]
                 }, Node {
                   rootLabel = ([], ["False is the result of comparing (<)"]),
                   subForest = [Node {
                   rootLabel = (["toplevel / listfold sum / fmap mathsection if / comparison <: 0.0"], ["0.0: a leaf value"]),
                   subForest = []
                 }, Node {
                   rootLabel = ([], ["with"]),
                   subForest = []
                 }, Node {
                   rootLabel = (["toplevel / listfold sum / fmap mathsection if / comparison <: -1.0"], ["-1.0: a leaf value"]),
                   subForest = []
                 }]
                 }, Node {
                   rootLabel = ([], ["False is the result of comparing (<)"]),
                   subForest = [Node {
                   rootLabel = (["toplevel / listfold sum / fmap mathsection if / comparison <: 0.0"], ["0.0: a leaf value"]),
                   subForest = []
                 }, Node {
                   rootLabel = ([], ["with"]),
                   subForest = []
                 }, Node {
                   rootLabel = (["toplevel / listfold sum / fmap mathsection if / comparison <: 0.0"], ["0.0: a leaf value"]),
                   subForest = []
                 }]
                 }, Node {
                   rootLabel = ([], ["True is the result of comparing (<)"]),
                   subForest = [Node {
                   rootLabel = (["toplevel / listfold sum / fmap mathsection if / comparison <: 0.0"], ["0.0: a leaf value"]),
                   subForest = []
                 }, Node {
                   rootLabel = ([], ["with"]),
                   subForest = []
                 }, Node {
                   rootLabel = (["toplevel / listfold sum / fmap mathsection if / comparison <: 1.0"], ["1.0: a leaf value"]),
                   subForest = []
                 }]
                 }, Node {
                   rootLabel = ([], ["True is the result of comparing (<)"]),
                   subForest = [Node {
                   rootLabel = (["toplevel / listfold sum / fmap mathsection if / comparison <: 0.0"], ["0.0: a leaf value"]),
                   subForest = []
                 }, Node {
                   rootLabel = ([], ["with"]),
                   subForest = []
                 }, Node {
                   rootLabel = (["toplevel / listfold sum / fmap mathsection if / comparison <: 2.0"], ["2.0: a leaf value"]),
                   subForest = []
                 }]
                 }, Node {
                   rootLabel = ([], ["True is the result of comparing (<)"]),
                   subForest = [Node {
                   rootLabel = (["toplevel / listfold sum / fmap mathsection if / comparison <: 0.0"], ["0.0: a leaf value"]),
                   subForest = []
                 }, Node {
                   rootLabel = ([], ["with"]),
                   subForest = []
                 }, Node {
                   rootLabel = (["toplevel / listfold sum / fmap mathsection if / comparison <: 3.0"], ["3.0: a leaf value"]),
                   subForest = []
                 }]
                 }]
                 }]
                 }, Node {
                   rootLabel = (["toplevel / listfold sum: -2.0"], ["-2.0: a leaf value"]),
                   subForest = []
                 }, Node {
                   rootLabel = (["toplevel / listfold sum: -1.0"], ["-1.0: a leaf value"]),
                   subForest = []
                 }, Node {
                   rootLabel = (["toplevel / listfold sum: 0.0"], ["0.0: a leaf value"]),
                   subForest = []
                 }, Node {
                   rootLabel = (["toplevel / listfold sum / multiplication: 2.0", "2.0\ntoplevel / listfold sum / multiplication: 1.0"], ["2.0: which we obtain by multiplying"]),
                   subForest = [Node {
                   rootLabel = (["toplevel / listfold sum / multiplication: 2.0"], ["2.0: a leaf value"]),
                   subForest = []
                 }, Node {
                   rootLabel = ([], ["by"]),
                   subForest = []
                 }, Node {
                   rootLabel = (["2.0\ntoplevel / listfold sum / multiplication: 1.0"], ["1.0: a leaf value"]),
                   subForest = []
                 }]
                 }, Node {
                   rootLabel = (["toplevel / listfold sum / multiplication: 2.0", "2.0\ntoplevel / listfold sum / multiplication: 2.0"], ["4.0: which we obtain by multiplying"]),
                   subForest = [Node {
                   rootLabel = (["toplevel / listfold sum / multiplication: 2.0"], ["2.0: a leaf value"]),
                   subForest = []
                 }, Node {
                   rootLabel = ([], ["by"]),
                   subForest = []
                 }, Node {
                   rootLabel = (["2.0\ntoplevel / listfold sum / multiplication: 2.0"], ["2.0: a leaf value"]),
                   subForest = []
                 }]
                 }, Node {
                   rootLabel = (["toplevel / listfold sum / multiplication: 2.0", "2.0\ntoplevel / listfold sum / multiplication: 3.0"], ["6.0: which we obtain by multiplying"]),
                   subForest = [Node {
                   rootLabel = (["toplevel / listfold sum / multiplication: 2.0"], ["2.0: a leaf value"]),
                   subForest = []
                 }, Node {
                   rootLabel = ([], ["by"]),
                   subForest = []
                 }, Node {
                   rootLabel = (["2.0\ntoplevel / listfold sum / multiplication: 3.0"], ["3.0: a leaf value"]),
                   subForest = []
                 }]
                 }]
                 }
