module Explainable.Lib where

import Text.Megaparsec ( choice, some, Parsec, MonadParsec(try) )
import Text.Megaparsec.Char ( numberChar )

import Explainable.MathLang

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | if we generalize a 2 dimensional Data.Matrix to higher dimensions, we have ... a Tensor! but let's reinvent the wheel and not use one of the available tensor libraries.
-- some guidance from IRC suggested just making vector of vectors of vectors etc.

type Parser = Parsec () String

-- we'll do the mathlang parser using control.onad.combinators.expr later.

tryChoice :: [Parser a] -> Parser a
tryChoice = choice . fmap try

int :: Parser Int
int = read <$> some numberChar



-- * Set up some ExplainableIO computations

runTests_1 :: IO ()
runTests_1 = do
  -- (t1v, t1x, t1s, t1w) <- xplainF someScenario (MathITE (PredComp CLT (Val 1) (Val 2)) (Val 100) (Val 200))

  -- you may ask: how is this better than just doing things natively in haskell? The answer: evaluation is decorated with explanations, and that's valuable, because XAI.

  putStrLn "* the sum of all positive elements, ignoring negative elements"
  dumpExplanation 2 $ sumOf     $ positiveElementsOf [-2, -1, 0, 1, 2, 3]

  putStrLn "* the product of the doubles of all positive elements, ignoring negative and zero elements"
  dumpExplanation 2 $ productOf $ timesEach 2 $ positiveElementsOf [-2, -1, 0, 1, 2, 3]

  putStrLn "* the sum of the doubles of all positive elements and the unchanged original values of all negative elements"
  dumpExplanation 2 $ sumOf $ timesPositives 2 [-2, -1, 0, 1, 2, 3]



runTests_Mathlang :: IO ()
runTests_Mathlang = do
  putStrLn "* mathlang tests"

  putStrLn "** two plus two equals four"
  let four = Val (Just "two") 2 |+ Val (Just "two") 2
  dumpExplanation 3 four
  
  putStrLn "* output to typescript"

dumpExplanation :: Int -> Expr Float -> IO ()
dumpExplanation depth f = do
  (val, xpl, stab, wlog) <- xplainF () f
  putStrLn (stars ++ " val" ); print val
  putStrLn (stars ++ " xpl" ); print xpl
  putStrLn (stars ++ " stab"); print stab
  putStrLn (stars ++ " wlog"); print wlog

  where stars = replicate depth '*'
  
