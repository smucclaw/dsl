{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}

module Explainable.Lib where

import qualified Data.Map as Map
import Control.Monad.IO.Class (MonadIO(..))
import Text.Megaparsec ( choice, some, Parsec, MonadParsec(try) )
import Text.Megaparsec.Char ( numberChar )
import Text.PrettyPrint.Boxes ( Box, nullBox )
import qualified Text.PrettyPrint.Boxes as BX

import Explainable
import Explainable.MathLang
import Explainable.TaxDSL

import Control.Monad.Trans.RWS
import Data.Tree
import Control.Monad

someFunc :: IO ()
someFunc = putStrLn "someFunc"

instance Semigroup Box where
  (<>) = (BX.<>)

instance Monoid Box where
  mempty = nullBox

-- | if we generalize a 2 dimensional Data.Matrix to higher dimensions, we have ... a Tensor! but let's reinvent the wheel and not use one of the available tensor libraries.
-- some guidance from IRC suggested just making vector of vectors of vectors etc.

type Parser = Parsec () String

-- we'll do the mathlang parser using control.onad.combinators.expr later.

tryChoice :: [Parser a] -> Parser a
tryChoice = choice . fmap try

int :: Parser Int
int = read <$> some numberChar



-- * Set up some ExplainableIO computations

runTests_1 :: Scenario -> IO ()
runTests_1 sc = do
  -- (t1v, t1x, t1s, t1w) <- xplainF someScenario (MathITE (PredComp CLT (Val 1) (Val 2)) (Val 100) (Val 200))

  -- you may ask: how is this better than just doing things natively in haskell? The answer: evaluation is decorated with explanations, and that's valuable, because XAI.

  putStrLn "* the sum of all positive elements, ignoring negative elements"
  _ <- xplainF sc $ sumOf     $ positiveElementsOf [-2, -1, 0, 1, 2, 3]

  putStrLn "* the product of the doubles of all positive elements, ignoring negative and zero elements"
  _ <- xplainF sc $ productOf $ timesEach 2 $ positiveElementsOf [-2, -1, 0, 1, 2, 3]

  putStrLn "* the sum of the doubles of all positive elements and the unchanged original values of all negative elements"
  _ <- xplainF sc $ sumOf $ timesPositives 2 [-2, -1, 0, 1, 2, 3]

  let someScenario = scenarios Map.! "1a"
  runTests_1 someScenario

  putStrLn "* we start with a simple scenario"
  putStrLn $ asExample someScenario
  putStrLn "** we calculate net income"
  newSome <- netIncome someScenario
  putStrLn $ asExample newSome 

  let nets = Map.elems $ newSome Map.! "net income" 
  putStrLn $ "** what is the positive sum of the incomes?"
  (positiveSum,_,_,_)  <- xplainF newSome $ ListFold FoldSum $ Val 0 <| MathList (Val <$> nets)
  putStrLn $ "** what is the negative sum of the incomes?"
  (negativeSum,_,_,_)  <- xplainF newSome $ ListFold FoldSum $ Val 0 |> MathList (Val <$> nets)

  putStrLn $ "** if the positive sum is greater than 100000, the maximum reduction is half of the negative sum; otherwise it is the entire negative sum"
  (maxReductPos,_,_,_) <- xplainF newSome (MathITE
                                            (PredComp CGT (Val positiveSum) (Val 100000))
                                            (Val negativeSum |/ Val 2)
                                            (Val negativeSum))

  putStrLn $ "* the maximum amount by which we can reduce the positive sum is " ++ show maxReductPos
  (maxReductNeg,_,_,_) <- xplainF newSome $ MathMax (Val 0) (Val negativeSum |+ MathMin (Val 0) (Val positiveSum))

  putStrLn $ "* the amount by which we can shrink the negative sum is " ++ show maxReductNeg
  putStrLn $ "* now we prorata reduce both the positive and the negative incomes, by type"

  (fromMathList,_,_,_) <- let sc = newSome
                              ml = getColAsMathList "net income" sc
                          in xplainL sc $
        ListMapIf (MathSection Times (Val $ 1 - maxReductNeg / negativeSum)) (Val 0) CGT $ -- [TODO] wrap this into a prorata combinator inside Explanation with its own expl
        ListMapIf (MathSection Times (Val $ 1 - maxReductPos / positiveSum)) (Val 0) CLT $
        ml

  let newSome3 = Map.union newSome $
                 Map.singleton "reduction" $
                 Map.fromList (zip (rowNames newSome) fromMathList)
  putStrLn "* we have a new column \"reduction\""
  putStrLn $ asExample newSome3

  return ()

netIncome :: Scenario -> IO Scenario
netIncome =
  addCol "net income" [ getCol "ordinary income"
                      , deplus "extraordinary income"
                      , deminus "ordinary expenses"
                      , deminus "special expenses"
                      ] 

runTax :: Scenario -> Focus -> IO Float
runTax sc taxcomp = do
  sc2 <- netIncome sc
  (val, xpl, stab, wlog) <- xplainE sc2 emptyState taxcomp
  return val
 
tax_2_3 :: Focus
tax_2_3 = do
  (sc1,xpl) <- squashToTotals =<< asks origReader
  let income = cell sc1 "net income" "total"
  (val,xpl2) <- progDirectM 2023 income
  return (val, Node ([],["tax_2_3 computation determines net income is " ++ show val])
               [Node ([],["progDirectM 2023"]) [xpl2]
               ,Node ([],["squashToTotals"]) [xpl]
               ])

(>>=>) :: MonadIO m => m a -> (a -> IO b) -> m b
x >>=> y = x >>= liftIO . y

taxPayableFor :: Float -> ExplainableIO r MyState Float
taxPayableFor = progDirectM 2023

tax_34 :: Focus
tax_34 = do
  sc <- asks origReader
        >>=> addCol "extraordinary pre-net" [ getCol "extraordinary income"
                                            , deminus "special expenses" ]
        >>=> addCol "ordinary pre-net" [ getCol "ordinary income"
                                       , deminus "ordinary expenses" ]
        >>=> addCol "pre-net" [ getCol "extraordinary pre-net"
                              , deplus "ordinary pre-net" ]
  let extPreNet = col2mathList (sc Map.! "extraordinary pre-net")
      ordPreNet = col2mathList (sc Map.! "ordinary pre-net")
      negPreNets    = evalList     (negativeElementsOf (Map.elems $ sc Map.! "pre-net"))
      posPreNets    = evalList     (positiveElementsOf (Map.elems $ sc Map.! "pre-net"))
      negPreNetSum  = eval $ sumOf (negativeElementsOf (Map.elems $ sc Map.! "pre-net"))
      posPreNetSum  = eval $ sumOf (positiveElementsOf (Map.elems $ sc Map.! "pre-net"))
  
  -- offset losses

  (sc1,xpl) <- squashToTotals =<< asks origReader
  let income = cell sc1 "net income" "total"
  (val,xpl2) <- progDirectM 2023 income
  return (val, Node ([],["tax_34 computation determines net income is " ++ show val])
               [Node ([],["progDirectM 2023"]) [xpl2]
               ,Node ([],["squashToTotals"]) [xpl]
               ])

squashToTotals :: Scenario -> ExplainableIO r MyState Scenario
squashToTotals sc = do
  (total,xpl) <- eval $ sumOf . col2mathList $ sc Map.! "net income"
  return (Map.singleton "net income" (Map.singleton "total" total)
         ,xpl)
  
runTests :: IO ()
runTests = do
 
  putStrLn "* Scenarios"

  -- [TODO] write a simple parser to set up the scenario
  -- scenario 1: income exceeds expenses thanks to extraordinary; taxable income is > 100000; some negative income in certain categories

  print scenarios
  
  forM_ (Map.toList scenarios) $ \(sctitle,sc) -> do
    putStrLn $ "* running scenario: " <> sctitle

    putStrLn $ "** executing tax_2_3: " <> sctitle
    result_2_3 <- runTax sc tax_2_3
    putStrLn $ "result = " ++ show result_2_3

    putStrLn $ "** executing section_34_1: " <> sctitle
    (result_341, expl_341) <- runExplainIO $ section_34_1 sc
    unless (null expl_341) $ putStrLn "** explaining section_34_1:" >> printExplanation expl_341

  let testcase3_fired   = ("test case3 - fired",   scenarios Map.! "test case 3 - fired")
      testcase3_unfired = ("test case3 - unfired", scenarios Map.! "test case 3 - unfired")

  putStrLn "* which tax method shall we use to deal with extraordinary income in test case 3?"
  (effMethod, expl) <- runExplainIO $ chooseEffectiveEOTaxMethod testcase3_fired testcase3_unfired
  putStrLn $ "* we choose " ++ show effMethod
  printExplanation expl

  return ()

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

  , "test case 1" ~=> [ "ordinary income"      <-~ [ "Rents"   ~== 72150 - 25000 ]
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
    
