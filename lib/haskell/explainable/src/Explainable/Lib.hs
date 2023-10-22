module Explainable.Lib where

import Text.Megaparsec ( choice, some, Parsec, MonadParsec(try) )
import Text.Megaparsec.Char ( numberChar )
import qualified Data.Map as Map

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
  -- (t1v, t1x, t1s, t1w) <- xplainF someScenario varstate (MathITE (PredComp CLT (Val 1) (Val 2)) (Val 100) (Val 200))

  -- you may ask: how is this better than just doing things natively in haskell? The answer: evaluation is decorated with explanations, and that's valuable, because XAI.

  putStrLn "* the sum of all positive elements, ignoring negative elements"
  dumpExplanationF 2 defaultState $ sumOf     $ positiveElementsOf [-2, -1, 0, 1, 2, 3]

  putStrLn "* the product of the doubles of all positive elements, ignoring negative and zero elements"
  dumpExplanationF 2 defaultState $ productOf $ timesEach 2 $ positiveElementsOf [-2, -1, 0, 1, 2, 3]

  putStrLn "* the sum of the doubles of all positive elements and the unchanged original values of all negative elements"
  dumpExplanationF 2 defaultState $ sumOf $ timesPositives 2 [-2, -1, 0, 1, 2, 3]


defaultState :: MyState
defaultState = emptyState { symtabF = Map.fromList [("dow", "dow" @|. 7)] }
  

runTests_Mathlang :: IO ()
runTests_Mathlang = do
  putStrLn "* mathlang tests"

  putStrLn "** two plus (two on sundays, and one every other day) equals three or four"
  let iceCreams =
        "iceCreams" @|=
        "two usually" @|. 2
        |+
        ("more on Sundays" @|=
         ("it is sunday" @|= ( (MathVar "dow") |=== ("sunday is day seven" @|. 7)) )
         @|? ("two on Sundays" @|. 2)
         @|: ("one otherwise"  @|. 1)
        )
  dumpExplanationF 3 defaultState iceCreams


  

