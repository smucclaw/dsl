{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}

module Lib where

import qualified Data.Map as Map
import Control.Monad.Trans.State ( gets, State, StateT, evalState, runStateT, modify )
import Control.Monad.State (liftIO)
import Control.Monad (forM_, when, unless, (>=>))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Function ((&))
import Text.Megaparsec ( choice, many, some, Parsec, MonadParsec(try) )
import Text.Megaparsec.Char ( numberChar, hspace )
import Data.List ( sort, isPrefixOf, nub )
import Text.PrettyPrint.Boxes
    ( emptyBox, hsep, nullBox, render, vcat, Box )
import qualified Text.PrettyPrint.Boxes as BX
import Control.Monad.Combinators.Expr
import Data.Ord
import Explainable
import qualified Data.Tree as DT
import Control.Monad.Trans.RWS
import Data.Tree
import Data.Bifunctor

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

-- | we get right to business with a shallow embedding that gets into expr and eval and expl without further ado.

defaultScenario :: Scenario
defaultScenario =
  mkMap [ (i, defaultStream)
        | i <- [ "ordinary income"
               , "extraordinary income"
               , "ordinary expenses"
               , "special expenses"
--               , "lump sum deductions"
               ]
        , let defaultStream :: IncomeStreams
              defaultStream =
                mkMap [ (ic,0)
                      | ic <- [ "Agriculture"
                              , "Trade"
                              , "Independent"
                              , "Employment"
                              , "Exempt Capital"
                              , "Capital"
                              , "Rents"
                              , "Other"
                              ] ]
        ]
  where mkMap = Map.fromList

(~==) :: String -> Float -> IncomeStreams -> IncomeStreams
s ~== n = Map.update (const $ pure n) s

(<-~) :: String -> [IncomeStreams -> IncomeStreams] -> Scenario -> Scenario
x <-~ ys = Map.update (pure . foldl1 (.) ys) x

(~=>) :: String -> [Scenario -> Scenario] -> (String,Scenario)
title ~=> js = (title, foldl (.) id js defaultScenario)

infix 4 ~==
infixl 8 ~=>
infixl 1 <-~

--      type Explainable r                 a     = RWST (HistoryPath,r) [String] MyState IO (a,XP)
type Focus = Explainable Scenario Float
           
deplus,deminus,detimes,getCol :: String -> String -> (Float,XP) -> Focus
deplus colName row (x,xpl) = do
  scn <- asks snd
  let mycell = cell scn colName row
      toreturn = x + mycell
  return (toreturn, xpl { rootLabel = second (++ ["- plus " ++ colName ++ " (" ++ show mycell ++ ") = " ++ show toreturn]) (rootLabel xpl) })
deminus colName row (x,xpl) = do
  scn <- asks snd
  let mycell = cell scn colName row
      toreturn = x - mycell
  return (toreturn, xpl { rootLabel = second (++ ["- less " ++ colName ++ " (" ++ show mycell ++ ") = " ++ show toreturn]) (rootLabel xpl) })
detimes colName row (x,xpl) = do
  scn <- asks snd
  let mycell = cell scn colName row
      toreturn = x * mycell
  return (toreturn, xpl { rootLabel = second (++ ["- times " ++ colName ++ " (" ++ show mycell ++ ") = " ++ show toreturn]) (rootLabel xpl) })
getCol colName row (_,xpl) = do
  scn <- asks snd
  let toreturn = cell scn colName row
  return (toreturn, reRootLbl xpl (startingWith colName toreturn))

startingWith :: Show a => [Char] -> a -> [[Char]] -> [[Char]]
startingWith s1 s2 = (++ ["- starting with " ++ s1 ++ " (" ++ show s2 ++ ")"])
reRootLbl :: Bifunctor p => Tree (p a b) -> (b -> b) -> Tree (p a b)
reRootLbl xpl f = xpl { rootLabel = second f (rootLabel xpl) }

constCell :: Float -> String -> (Float,XP) -> Focus
constCell n _ (_,xpl) = do
  return (n, xpl { rootLabel = second (++ ["- hardcoding " ++ show n]) (rootLabel xpl) })

runRow :: [ String -> (Float,XP) -> Focus ] -> String -> Focus
runRow fs rowname = do
  (f, xp) <- foldr (>=>) pure (($ rowname) <$> fs) (0,mkNod $ "row " ++ rowname)
  return (f,Node ([], [rowname ++ " = " ++ show f]) [xp])

colNames :: Scenario -> [String]
colNames = Map.keys

rowNames :: Scenario -> [String]
rowNames sc = Map.keys $ head $ Map.elems sc

-- [TODO] -- upgrade this to just return Focus
addCol :: String -> [ String -> (Float,XP) -> Focus ] -> Scenario -> IO Scenario
addCol newcol fs sc = do
  (fxp,_st,_wlog) <- runRWST (mapM (runRow fs) (rowNames sc))
                                (([],[]),sc) emptyState
  putStrLn $ "*** addCol " ++ newcol ++ " = " ++ show (fst <$> fxp)
  putStrLn $ drawTreeOrg 4 (Node ([],["adding 1 new column (\"" ++ newcol ++ "\") with " ++ show (length fxp) ++ " rows"]) (snd <$> fxp))

  return $ Map.union sc $ Map.singleton newcol $ Map.fromList (zip (rowNames sc) (fst <$> fxp))

col2mathList :: IncomeStreams -> ExprList Float
col2mathList is = MathList ( Val <$> Map.elems is )

mathList2col :: ExprList Float -> Explainable r [Float]
mathList2col ml = deepEvalList (ml,mkNod "mathList2col")

getColAsMathList :: String -> Scenario -> ExprList Float
getColAsMathList colname sc = col2mathList $ sc Map.! colname

getColAsMathListM :: String -> Explainable Scenario (ExprList Float)
getColAsMathListM colname = do
  sc <- asks snd
  return (getColAsMathList colname sc
         , mkNod $ "retrieve column " ++ colname ++ " from scenario")


-- * Set up some Explainable computations

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
  (val, xpl, stab, wlog) <- xplainE sc2 taxcomp
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

taxPayableFor :: Float -> Explainable r Float
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

squashToTotals :: Scenario -> Explainable r Scenario
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
    

-- | choose a tax computation method
-- > extraordinary income may only be taxed at a reduced rate if it
-- > leads to an aggregation of income for the tax year in question.

-- the "leads to" is confusing above: there is no implication; it is a biconditional.
--
--          | Aggregation of Income (IN)   |     Tax Computation Method (OUT)
--          |---------------------------------------------------------------
--          |             True             |             One-Fifths
--          |            False             |               Normal

-- > According to a 1997 ruling by the Federal Court of Finance (BFH,
-- > Bundesfinanzhof), extraordinary income may only be taxed at a
-- > reduced rate if it leads to an aggregation of income for the tax
-- > year in question.
-- > 
-- > For this purpose, both your actual total annual income (including
-- > severance pay and other income earned after termination) and the
-- > income you would have earned without termination of employment are
-- > compared (you can base this on your income from the previous year).
-- > 
-- > If your actual income is higher than the income you would have
-- > earned, there is an aggregation of income and the one-fifth method
-- > [the method of computing tax on extraordinary income used earlier]
-- > can be applied.

data EOTaxMethod = EONormal | EOFifth
  deriving (Eq, Show)

chooseEffectiveEOTaxMethod :: (String,Scenario) -- ^ actual
                           -> (String,Scenario) -- ^ hypothetical
                           -> ExplainIO EOTaxMethod
chooseEffectiveEOTaxMethod (titleA,scActual) (titleB,scHypo) = do
  explain "effectiveEOTaxMethod" $ "we need to determine if there is aggregation of income. let's see if the different treatments matter. we will compare 34.1 with 2.3, considering the actual (" ++ titleA ++ ") and hypothetical (" ++ titleB ++ ") scenarios"
  treatmentA <- ("section 34.1",) <$> thereIs_aggregation_of_income "when" "total taxable income" "according to" section_34_1 "is larger in" scActual "than in" scHypo
  treatmentB <- ("section 2.3",)  <$> thereIs_aggregation_of_income "when" "total taxable income" "according to" section_2_3 "is larger in" scActual "than in" scHypo
  explain "effectiveEOTaxMethod" $ "under treatment " <> fst treatmentA <> ", there is" ++ (if snd treatmentA then " " else " not ") ++ "aggregation of income."
  explain "effectiveEOTaxMethod" $ "under treatment " <> fst treatmentB <> ", there is" ++ (if snd treatmentB then " " else " not ") ++ "aggregation of income."
  if snd treatmentA
  then explain "effectiveEOTaxMethod" "there is aggregation of income, so the one-fifths method is indicated." >> return EOFifth
  else explain "effectiveEOTaxMethod" "there is not aggregation of income, so the normal method is indicated." >> return EONormal

-- | there is aggregation of income if the actual income is higher than the income you would have earned without termination of employment.
-- shallow DSL phrasing:
thereIs_aggregation_of_income :: (Show a, Ord a)
                              => String
                              -> String
                              -> String
                              -> (Scenario -> StateT [(String, String)] IO (Map.Map String (Map.Map String a)))
                              -> String
                              -> Scenario
                              -> String
                              -> Scenario
                              -> StateT [(String, String)] IO Bool
thereIs_aggregation_of_income
  "when"    col
  "according to" section
  "is larger in" scenario1
  "than in" scenario2
  = do
  actual <- section scenario1
  hypo   <- section scenario2
  let tti_actual = cell actual col "total"
      tti_hypo   = cell hypo   col "total"
      ttp_actual = cell actual "total tax payable"    "total"
      ttp_hypo   = cell hypo   "total tax payable"    "total"
  explain "aggregationOfIncome" $ "in the actual scenario, " ++ col ++" is " ++ show tti_actual ++
    " (and total tax payable is = " ++ show ttp_actual ++ ")"
  explain "aggregationOfIncome" $ "in the hypo   scenario, total taxable income is " ++ show tti_hypo ++
    " (and total tax payable is = " ++ show ttp_hypo ++ ")"
  case tti_actual `compare` tti_hypo of
    GT -> explain "aggregationOfIncome" "actual > hypo, returning true"  >> return True
    EQ -> explain "aggregationOfIncome" "actual = hypo, returning false" >> return False
    LT -> explain "aggregationOfIncome" "actual < hypo, returning false" >> return False
thereIs_aggregation_of_income _ _ _ _ _ _ _ _ = error "incorrect call to thereIs_aggregation_of_income"  


-- | We set up an "explanation monad" which is basically a State+IO
-- monad. The state component is a Data.Tree. We wrap child executions
-- within a mkNod wrapper which behaves mostly as a Writer monad,
-- except we want to give each computation a history of prior
-- executions that just might affect it, so we make it a State. In the
-- future it would be nice to wrap the IO computations so that any
-- @putStrLn@s get automatically wrapped with a @#+begin_example / #+end_example@ block.

type Explanation   = [(String, String)]
type ExplainIO = StateT Explanation IO

explain :: String -> String -> StateT [(String, String)] IO ()
explain foo bar = do
  Control.Monad.Trans.State.modify (++ [(foo,bar)])
  -- liftIO (putStrLn $ "- " ++ foo ++ " :: " ++ bar)

runExplainIO :: StateT [([Char], [Char])] IO a -> IO (a, Explanation)
runExplainIO x = runStateT x []
  
printExplanation :: [([Char], [Char])] -> IO ()
printExplanation xs = do
  putStrLn . unlines . fmap (\(a,b) -> "- " ++ a ++ " :: " ++ b) $ xs
  
--  let parsed = runParser pMathLang "" "( 5 + 3 * 2 )"
--  print parsed

type Scenario      = Map.Map String         IncomeStreams
type IncomeStreams = Map.Map IncomeCategory Float

type IncomeCategory = String

type NetIncome     = Int
type TaxableIncome = Int

-- | render a Scenario as a table -- something like this:

asTable :: Scenario -> Box
asTable sc =
  hsep 2 BX.left (
  -- row headers at left
  vcat BX.left (emptyBox 1 1 : (BX.text <$> sort (Map.keys (head (Map.elems sc)))))
    : [ vcat BX.left (
          -- column headers at top
          BX.text streamName
            : [ BX.text (show (round numval :: Int))
              | numval <- Map.elems streamVal -- is auto sorted by incomeCategory i think
              ] )
      | (streamName, streamVal) <- Map.toAscList sc
      ]
  )

-- | render a single column as a mini table
asColumn :: String -> IncomeStreams -> Box
asColumn str istream = asTable (Map.singleton str istream)

-- | "natural language" friendly way of phrasing a transformation that changes some elements around
data Replace = Replace { elems_  :: [String] , with_ :: [String] }

-- | run the generic replacement for Scenarios
runReplaceSc :: Scenario -> Replace -> ExplainIO Scenario
runReplaceSc m (Replace ks fs) =
  if all (`Map.member` m) ks
  then do
    nu <- sequence $ metaFsc <$> fs <*> [m]
    return $ Map.unions $ nu ++ [foldl (flip Map.delete) m ks]
  else do
    liftIO $ putStrLn $ "ERROR - runReplaceSc: sanity check failed on keys " ++ unwords ks
    return m

runReplaceIs :: IncomeStreams -> Replace -> ExplainIO IncomeStreams
runReplaceIs m (Replace ks fs) =
  if all (`Map.member` m) ks
  then Map.unions . (++ [foldl (flip Map.delete) m ks]) <$> sequence ((metaFis <$> fs) <*> [m]) 
  else do
    liftIO $ putStrLn $ "ERROR - runReplaceIs: sanity check failed on keys " ++ unwords ks
    return m

-- | syntactic sugar for setting up a Replace transformation
(~->) :: [String] -> [String] -> Replace
(~->) k ka = Replace { elems_ = k, with_ = ka }

-- | generic section wrapper
runSection :: String -> Scenario -> [Replace] -> ExplainIO Scenario
runSection name initialScenario transformations = do
  liftIO $ putStrLn $ "** executing " <> name
  steps <- scanlM runReplaceSc initialScenario transformations
  _ <- liftIO $ sequence [ putStrLn ("*** step " <> show n) >> putStrLn (asExample step)
                         | (n, step) <- zip [1::Int ..] steps ]
  return $ last steps

-- | section 2(3) EStG
section_2_3 :: Scenario -> ExplainIO Scenario
section_2_3 sc = do
  runSection "section 2.3" sc
        [ []                                             ~-> []
        , ["ordinary income"
          ,"extraordinary income"
          ,"ordinary expenses"]                          ~-> ["squashIncomes"]
        , ["combined income"
          ,"special expenses"]                           ~-> ["netIncome"]          -- 2_3_2
        , ["net income"]                                 ~-> ["offsetLosses_2_3_3"] -- 2_3_3
        , []                                             ~-> ["squashCats"]
        , []                                             ~-> ["ordinaryPayable"]
      -- marital adjustments
      -- carryover loss, if net negative then leave some negative?
        ]

-- | run through a specific set of transformations defined in section 34_1.
-- 
-- A particular transformation runs only if all its LHS columns are found in the scenario table,
-- in which case those columns are replaced by the output of the transformer.
section_34_1 :: Scenario -> ExplainIO Scenario
section_34_1 sc = do
  runSection "section 34.1" sc
        [ []                                            ~-> []
        , ["ordinary income", "ordinary expenses"
          , "special expenses"]                         ~-> ["preNetIncome"]
        , ["pre-net income"]                            ~-> ["offsetLosses"]
        , []                                            ~-> ["squashCats"]
        , []                                            ~-> ["extraordinary"]
        , []                                            ~-> ["sentence3"]
        , ["1 revised RTI taxation due to sentence 3"]  ~-> ["sentence3_b"]
        , [ "1 RTI taxation"
          , "2 RTI plus one fifth"
          , "3 tax on RTI+.2"
          , "4 difference"
          , "5 extraordinary taxation"
          , "extraordinary income"
          , "remaining taxable income"]                 ~-> ["totalPayable"]
        ]


colOp :: Scenario -> (Float -> Float -> Float) -> String -> String -> IncomeStreams
colOp sc op lhs rhs = mapAp op (sc Map.! lhs) (sc Map.! rhs)

colOpR :: Scenario -> (Float -> Float -> Float) -> String -> IncomeStreams -> IncomeStreams
colOpR sc op lhs = mapAp op (sc Map.! lhs)

colOpL :: Scenario -> (Float -> Float -> Float) -> IncomeStreams -> String -> IncomeStreams
colOpL sc op lhs rhs = mapAp op lhs (sc Map.! rhs)

colOpLR :: (Float -> Float -> Float) -> IncomeStreams -> IncomeStreams -> IncomeStreams
colOpLR = mapAp

(~+~), (~-~) :: Scenario -> String -> String -> IncomeStreams
(~+~) sc = colOp sc (+)
(~-~) sc = colOp sc (-)

(~+), (~-) :: Scenario -> String -> IncomeStreams -> IncomeStreams
(~+) sc = colOpR sc (+)
(~-) sc = colOpR sc (-)

(+~), (-~) :: Scenario -> IncomeStreams -> String -> IncomeStreams
(+~) sc = colOpL sc (+)
(-~) sc = colOpL sc (-)

(<+>),(<->) :: IncomeStreams -> IncomeStreams -> IncomeStreams
(<+>) = colOpLR (+)
(<->) = colOpLR (-)

infix 4 ~-, <+>, <->

(~=) :: String -> IncomeStreams -> Scenario
s ~= is = Map.singleton s is

infix 3 ~=

(~|) :: Scenario -> String -> IncomeStreams
(~|) sc s = sc Map.! s
infix 5 ~|



  
-- | a meta-function constructor for use by our meta-interpreter.
-- Instead of just a direct function definition @f = whatever@
-- we have (generally) @metaF "f" = whatever@ which gets called later.
-- here, the @whatever@ is a Scenario, so we say `metaFsc`.
metaFsc :: String -> Scenario -> ExplainIO Scenario

metaFsc "preNetIncome" sc = do
  liftIO $ putStrLn "RUNNING - metaFsc preNetIncome"
  return $ "pre-net income" ~= sc ~| "ordinary income" <-> ( sc ~| "special expenses" <+> sc ~| "ordinary expenses")

metaFsc "netIncome" sc = do
  liftIO $ putStrLn "RUNNING - metaFsc netIncome"
  let toreturn = Map.singleton "net income" $
        mapAp (-) (sc Map.! "combined income") (sc Map.! "special expenses")
  liftIO $ print toreturn
  liftIO $ putStrLn "RETURNING - metaFsc netIncome"
  return toreturn

-- | squash extraordinary into pre-net income; only used for section 2
metaFsc title@"squashIncomes" sc = do
  liftIO $ putStrLn "RUNNING - metaFsc squashIncomes"
  sequence_ [ do
                liftIO $ print (sc Map.! "extraordinary income" Map.! k)
                explain title $ "for " <> k <> ", " <>
                  "extraordinary " ++      scell sc "extraordinary income"       k ++
                  " + ordinary "   ++      scell sc "ordinary income"            k ++
                  " - expenses "   ++      scell sc "ordinary expenses"          k ++
                  " = pre-net "    ++ show (cell sc "extraordinary income"       k +
                                            cell sc "ordinary income"            k -
                                            cell sc "ordinary expenses"          k)
            | k <- Map.keys $ sc Map.! "extraordinary income"
            , sc Map.! "extraordinary income" Map.! k /= 0
            ]
  liftIO $ putStrLn "RETURNING - metaFsc squashIncomes"
  return $
    Map.singleton "combined income" $
    mapAp (-) 
    ( mapAp (+) (sc Map.! "ordinary income") (sc Map.! "extraordinary income") )
    ( sc Map.! "ordinary expenses" )


-- | loss offsets, based on section 34.
-- losses from one income category can be used to offset earnings in another.
-- the offsetting is done on a per-category basis, against the single "pre-net income" column, pro rata
--
metaFsc "offsetLosses" sc = do
  liftIO $ putStrLn "RUNNING - offsetLosses"
  let orig = sc Map.! "pre-net income"
      (negatives, positives) = Map.partition (< 0) orig
      totalNeg = sum $ Map.elems negatives
      totalPos = sum $ Map.elems positives
  return $
    Map.singleton "remaining taxable income" $
    (\x -> if x < 0
           then 0 -- [TODO] correctly handle a situation where the negatives exceed the positives
           else x + totalNeg * (x / totalPos))
    <$> orig 

-- | loss offsets, based on section 2(3) para 3 & 4
-- the reduction is done on a per-category basis, against the single "pre-net income" column, pro rata
--
metaFsc title@"offsetLosses_2_3_3" sc = do
  liftIO $ putStrLn "RUNNING - offsetLosses_2_3_3"
  liftIO $ print sc
  liftIO $ putStrLn "RUNNING - offsetLosses_2_3_3 printed sc"
  let orig = sc Map.! "net income"
      (negatives, positives) = Map.partition (< 0) orig
      totalNeg = sum $ Map.elems negatives
      totalPos = sum $ Map.elems positives
      maxReduction = if   totalPos > 100000
                     then totalNeg / 2
                     else totalNeg
      reductio = 1 + max (-1) (maxReduction / totalPos)
  when (totalPos > 100000) $ do
    explain title $ "sum of the positive incomes " ++ show totalPos ++ " exceeds 100000"
    explain title $ "so we will limit deductions to half of the sum of the negative incomes " ++ show totalNeg ++ " = " ++ show maxReduction
    explain title $ "and apply them pro rata to the positive incomes"
  when (totalPos <= 100000) $ do
    explain title $ "sum of the positive incomes " ++ show totalPos ++ " is less than 100000"
    explain title $ "so we will not limit deductions to half of the sum of the negative incomes; the deductible amount will be " ++ show maxReduction
    explain title $ "we will apply deductions pro rata to the positive incomes"
  explain title $ "reductio = " <> show reductio
  rti <- sequence [ (cat,) <$> offsetLosses_2_3_4 cat reductio n
                  | (cat,n) <- Map.toList orig ]
  -- viaprorata <- prorateF (* reductio) (>0) 0 (Map.elems orig)
  -- liftIO $ putStrLn $ "via pro rata, we would have " ++ show viaprorata
  return $ Map.singleton "total taxable income" (Map.fromList rti)
    
  where 
    -- | based on section 2(3) para 4
    -- 
    -- 4. The reduction is to be made in proportion to
    --    1. the positive totals of income
    --       1. from different types of income
    --    2. to the total of positive income.
    offsetLosses_2_3_4 :: String -> Float -> Float -> ExplainIO Float
    offsetLosses_2_3_4 categoryName reduction x
      | x < 0     = pure 0 <* explain title (categoryName <> " is negative, resetting to 0")
      | x == 0    = pure 0
      | otherwise = do
          let out = x * reduction
          explain title (categoryName <> " " <> show x <> " is positive, multiplying by " ++ show reduction ++ " = " ++ show out)
          return out

-- | extraordinary income is taxed
metaFsc "extraordinary" sc = do
  liftIO $ putStrLn "RUNNING - extraordinary"
  let ordinary     = sc Map.! "remaining taxable income"
      zeroOrdinary = max 0 <$> ordinary
      extraI       = sc Map.! "extraordinary income"
      ordtax       = taxFor zeroOrdinary
      totalI       = mapAp (+) zeroOrdinary extraI
      rti5         = mapAp (+) zeroOrdinary ((/5) <$> extraI)
      rti5tax      = max 0 <$> taxFor rti5
      delta        = abs   <$> mapAp (-) ordtax rti5tax
      rti5tax5     = (5*)  <$> delta
      
  return $ Map.fromList [("1 RTI taxation",           ordtax)
                        ,("2 RTI plus one fifth",     rti5)
                        ,("3 tax on RTI+.2",          rti5tax)
                        ,("4 difference",             delta)
                        ,("5 extraordinary taxation", rti5tax5)
                        ,("total taxable income",     totalI)
                        ]
  
-- | sentence 3
metaFsc "sentence3" sc = do
  liftIO $ putStrLn "RUNNING - sentence3"
  let ordinary = sc Map.! "remaining taxable income"
      extraI   = sc Map.! "extraordinary income"
      negRTI   = (\o -> if o < 0 then 1 else 0) <$> ordinary
      fiveOnFifth = mapAp (\o e -> if o < 0 && e + o > 0
                                   then 5 * progDirect 2023 ( (o + e) / 5 )
                                   else 0)
                    ordinary extraI
  return $
    if any (>0) (Map.elems negRTI)
    then Map.fromList [("0 RTI is negative",                        negRTI)
                      ,("1 revised RTI taxation due to sentence 3", fiveOnFifth)
                      ]
    else Map.empty

metaFsc "sentence3_b" sc = return $
  Map.fromList [("1 RTI taxation", sc Map.! "1 revised RTI taxation due to sentence 3")]


metaFsc "totalPayable" sc = do
  liftIO $ putStrLn "RUNNING - totalPayable"
  let rtiTax   = sc Map.! "1 RTI taxation"
      extraTax = sc Map.! "5 extraordinary taxation"
  return $ Map.fromList [("total tax payable", mapAp (+) rtiTax extraTax)]

metaFsc "ordinaryPayable" sc = do
  liftIO $ putStrLn "RUNNING - ordinaryPayable"
  let ttiTax   = taxFor $ sc Map.! "total taxable income"
  return $ Map.fromList [("total tax payable", ttiTax)]

-- | squash all non-exempt income categories together
metaFsc "squashCats" sc = do
  liftIO $ putStrLn "RUNNING - squashCats"
  mapM (\istream -> runReplaceIs istream (categoryKeys ~-> ["squashCats'"])) sc
  where
    categoryKeys :: [String]
    categoryKeys = nub (concatMap Map.keys (Map.elems sc))

metaFsc fname _ = return $ error $ "metaFsc called for undefined function name " <> fname

metaFis :: Num a => String -> Map.Map String a -> ExplainIO (Map.Map String a)
metaFis "squashCats'" ns = return $
  Map.singleton "total" $ sum $ Map.elems $ Map.filterWithKey (\k _ -> not ("Exempt " `isPrefixOf` k)) ns

metaFis fname _ = return $ error $ "metaFis called for undefined function name " <> fname

-- | what does pro rata mean?
-- it means we map some function across a functor, where each application is scaled to that value's fraction of the whole
--
-- example: @ prorate (* 1000) [5,3,2] == [500.0,300.0,200.0] @ 
prorate :: (Fractional a, Functor f, Foldable f) => (a -> a) -> f a -> ExplainIO (f a)
prorate f xs = return $ f . (/ sum xs) <$> xs

-- | what does filtered pro rata mean?
-- it means we map some function across a functor, where each application is scaled to that value's fraction of some partitioned subset of the functor, which passes a filter
-- elements which fail that filter are reset to mempty
-- example: divide 1000 new shares among all the debtholders who hold a positive balance, while zeroing the allotment for any who hold a negative balance
-- @
--   prorateF (const 1000) (>0) [5,3,2,-1] ==> [500.0,300.0,200.0,0]
-- @
-- 
-- example: halve the value of all elements which are positive, while zeroing anything negative
-- @
--   prorateF (/ 2) (>0) [5,3,2,-1] ==> [500.0,300.0,200.0,0]
-- @
-- 
prorateF :: (Fractional a, Functor f, Foldable f) => (a -> a) -> (a -> Bool) -> a -> f a -> ExplainIO (f a)
prorateF f filt zeroval xs = do
  let passes = sum $ (\x -> if filt x then x else zeroval) <$> xs
  return $ (\x -> if filt x
                  then f x
                  else zeroval) <$> xs

-- map application
mapAp :: Ord k => (a -> a -> a) -> Map.Map k a -> Map.Map k a -> Map.Map k a
mapAp = Map.unionWith

-- | fmap only those elements that qualify, leaving the others alone
mapOnly :: Functor t => (a -> Bool) -> (a -> a) -> t a -> t a
mapOnly test transform items = (\i -> if test i then transform i else i) <$> items

-- | when we start paying attention to marital status, we will want to know about different tax classes 
data TaxClass
  = TC1 -- ^ Applies to single, widowed, divorced, permanently separated couples and married couples with one spouse living abroad.
  | TC2 -- ^ Applies to single parents who can claim the relief for single parents
  | TC3 -- ^ - Applies to married persons whose spouse is either not employed or belongs to tax class V as an employee.
        --   - Applies to married persons whose spouse lives in an EU country.
        --   - Applies to widowers in the first year after the deceased spouse's death.
  | TC4 -- ^ - Applies to married spouses who live together and both are subject to unlimited tax liability.
        --   - The tax category combination IV/IV is particularly worthwhile if both spouses earn approximately the same amount.
        --   - In the case of the tax category combination IV with factor, tax-exempt amounts are taken into account in the wage tax calculation from the outset. As a result, the difference between the amount of income tax paid and the actual tax liability at the end of the year is lower.
  | TC5 -- ^ Applies to married persons, if the other spouse is in tax class III (provided the spouses live together).
  | TC6 -- ^ Applies to single and married persons with another employment, if no employment tax card has been issued by the tax office.
  deriving (Ord, Eq, Show)

data Marital = Single | Married
  deriving (Eq, Show)

-- | rate table from the "Tariff history" PDF downloaded from https://www.bmf-steuerrechner.de/
--
-- progressive individual tax rate table, by year.
-- note that these rates do not include:
-- - solidarity tax 5.5% of the normal rate payable; single taxpayers < 62127 / couples < 124255 exempt
-- - church tax 8 -- 9%
-- - business income
--   - corporate tax 15%, reduced for part
--   - municipal business tax of 14 to 17%
--   - municipal trade tax of 7 -- 17%

rateTable :: (Fractional a) => Int -> [(Ordering, a, a -> a)]
-- | ratetable for 2022 -- use only with progStack
rateTable 2022 = [(LT,   9409, const  0)
                 ,(GT,   9408, const 14)
                 ,(GT,  57051, const 42)
                 ,(GT, 270500, const 45)
                 ]
-- | ratetable for 2023 -- use only with progDirect
rateTable 2023 = [(LT,  10909, const  0)
                 ,(GT,  10908, \zvE -> let y = (zvE - 10908) / 10000
                                       in (979.18 * y + 1400) * y)
                 ,(GT,  15999, \zvE -> let y = (zvE - 15999) / 10000
                                       in (192.59 * y + 2397) * y + 966.53)
                 ,(GT,  62810, \zvE -> 0.42 * zvE - 9972.98)
                 ,(GT, 277825, \zvE -> 0.45 * zvE - 18307.73)
                 ]
rateTable _    = rateTable 2023



mkParent title children = Node ([],[title]) [children]

-- | an Explainable version
rateTableM :: Int -> [(Ordering, Float, Float -> Explainable r Float)]
rateTableM 2023 =
  [(LT,  10909, const (return (0, mkNod "below tax threshold")))
  ,(GT,  10908, \zvE -> do
       (y,yxpl) <- eval $ (Val zvE |- Val 10908) |/ Val 10000
       (x,xxpl) <- eval $ (Val 979.18 |* Val y |+ Val 1400) |* Val y
       return (x, Node ([],["taxable income exceeds 10908"]) [mkParent "x computation 1" xxpl ,mkParent "y computation 1" yxpl]))
  ,(GT,  15999, \zvE -> do
       (y,yxpl) <- eval $ (Val zvE |- Val 15999) |/ Val 10000
       (x,xxpl) <- eval $ (Val 192.59 |* Val y |+ Val 2397) |* Val y |+ Val 966.53
       return (x, Node ([],["taxable income exceeds 15999"]) [mkParent "x computation 2" xxpl ,mkParent "y computation 1" yxpl]))
  ,(GT,  62810, \zvE -> do
       (x,xxpl) <- eval $ Val 0.42 |* Val zvE |- Val 9972.98
       return (x, Node ([],["taxable income exceeds 62810"]) [mkParent "x computation 3" xxpl]))
  ,(GT, 277825, \zvE -> do
       (x,xxpl) <- eval $ Val 0.45 |* Val zvE |- Val 18307.73
       return (x, Node ([],["taxable income exceeds 277825"]) [mkParent "x computation 4" xxpl]))
  ]
rateTableM _ = error "rateTableM: only 2023 supported"

-- | we'll use the 2023 table when computing taxes.
taxFor :: Map.Map IncomeCategory Float -> Map.Map IncomeCategory Float
taxFor       = mapOnly (>0) (progDirect 2023)

-- | Direct computation of progressive tax, without recursing to lower tiers of the stack.
-- this is for countries which just give a direct formula that already takes into account
-- the lower tiers of the stack. the @go@ function here just looks up the correct formula
-- based on the taxable income tier.
progDirect :: (Ord a, Fractional a) => Int -> a -> a
progDirect year income =
  let rt = reverse $ rateTable year
  in go rt income
  where
    go ((o,n,f):rts) income'
      | income' `compare` n == o = f income'
      | otherwise                = go rts income'
    go [] _ = 0

progDirectM :: Int -> Float -> Explainable r Float
progDirectM year income =
  let rt = reverse $ rateTableM year
  in go rt income
  where
    go :: [(Ordering, Float, Float -> Explainable r Float)] -> Float -> Explainable r Float
    go ((o,n,f):rts) income'
      | income' `compare` n == o = f income'
      | otherwise                = go rts income'
    go [] _ = error "progDirectM: arrived at unreachable state"


-- | Stacked computation of progressive tax, by recursively adding lower tiers.
-- this is suitable for countries that announce their progressive tiers in terms of
-- "for the nth dollar you make, pay X in taxes on it".
-- This is not to be actually used, it's just here because we wrote it before progDirect.
progStack :: (Ord a, Fractional a) => Int -> a -> a
progStack year income =
  let rt = reverse $ rateTable year
  in progRate rt income
  where
    progRate ((o,n,r):rts) income'
      | income' `compare` n == o = r income' / 100 * (income' - n) + progRate rts n
      | otherwise                = progRate rts income'
    progRate [] _ = 0

-- | for org-mode purposes
asExample :: Scenario -> String
asExample sc = orgExample $ render (asTable sc)

orgExample :: String -> String
orgExample str = "\n#+begin_example\n" <> str <> "\n#+end_example\n"

-- | what is the effective tax rate? against `progStack`
effectiveRateStacked :: (Ord a, Fractional a) => Int -> a -> a
effectiveRateStacked year income = progStack year income / income * 100

-- | what is the effective tax rate? against `progDirect`
effectiveRateDirect :: (Ord a, Fractional a) => Int -> a -> a
effectiveRateDirect year income = progDirect year income / income * 100

-- | the solidarity surcharge is 5.5% /of/ the rest of the taxes
solidaritySurcharge :: (Fractional a, Ord a) => Int -> a -> a
solidaritySurcharge year income = progDirect year income / income * 0.055 * income

{- Also:
Profits from the sale of private real estate that has been held for more than 10 years, or from the sale of other assets that have been held for more than 12 months is exempt from tax. For shorter holding periods the general tax rates apply.
Sale of a shares when the percentage of the investment is less than 1% is subject to a flat 25% tax. On the other hand, when the percentage of the holding is in excess of 1%, tax is payable on 60% of the profit at normal rates.
-}

-- | monadic scanl, but not very performant:
-- 
-- > 00:12 < c_wraith> But it matters a lot less with than with scanl because this will probably fall apart on large
-- >                   lists anyway
-- > 00:12 < c_wraith> it's already not capable of streaming results like scanl in general
-- > 00:14 < c_wraith> this one is tough to do a more detailed strictness analysis on, because it depends on (>>=)
-- > 00:15 < c_wraith> I think there is *a* case where forcing t before the recursive call can matter.
-- > 00:16 < c_wraith> yeah, if the monad's (>>=) is sufficiently lazy, like Identity, it can matter if you're
-- >                   skipping elements of the result list.
-- > 00:16 < [exa]> c_wraith: e.g. for Identity this becomes a normal scanl, but I'd say weird stuff may start
-- >                happening with say State.Lazy
-- > 00:16 < c_wraith> [exa]: that stuff is the responsibility of (>>=) to handle.
-- > 00:16 < freeside> i am very much hoping to engage in weird activity in the near future, but not today. I mean
-- >                   LogicT.
-- > 00:17 < [exa]> LogicT <3
-- > 00:17 < [exa]> anyway yeah a bit of `seq` never hurt nobody, right?
-- > 00:18 < c_wraith> definitely false.  I've had people think they were optimizing libraries I was using and break
-- >                   them.
-- > 00:18 < c_wraith> like...  bytestring
-- > 00:18  * [exa] was joking, yeah
-- > 00:19 < c_wraith> still, given the recursive structure, I think scanlM' (forcing t before the recursive call), is
-- >                   almost always more appropriate.
-- > 
scanlM :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m [b]
scanlM _ _ []     = return []
scanlM f q (x:xs) = do
  t  <- f q x
  ts <- scanlM f t xs
  return $ t `seq` (t : ts)

scell :: (Ord k, Ord j, Show a, Show k, Show j) => Map.Map k (Map.Map j a) -> k -> j -> String
scell k j a = show (cell k j a)

-- column first, then row
cell :: (Show k, Show j, Show a, Ord k, Ord j) => Map.Map k (Map.Map j a) -> k -> j -> a
cell m k j =
  case Map.lookup k m of
    Nothing -> error $ "can't find key " ++ show k ++ " in outer map with keys " ++ show (Map.keys m)
    Just jx -> case Map.lookup j jx of
      Nothing -> error $ "can't find key " ++ show j ++ " in inner map with keys " ++ show (Map.keys jx) ++
                 "\ninput params:" ++ show (k,j) ++
                 "\nfull dump:" ++ show m
      Just a  -> a
