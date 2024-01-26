module Explainable.Lib (runTests_Mathlang) where

import Data.HashMap.Strict qualified as Map
import Explainable.MathLang
  ( Expr (MathVar),
    MyState (symtabF),
    dumpExplanationF,
    emptyState,
    positiveElementsOf,
    timesEach,
    timesPositives,
    (*||),
    (+||),
    (@|.),
    (@|:),
    (@|=),
    (@|?),
    (|+),
    (|===),
  )
import Text.Megaparsec (MonadParsec (try), Parsec, choice, some)
import Text.Megaparsec.Char (numberChar)

someFunc :: IO ()
someFunc = print "someFunc"

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

  print "* the sum of all positive elements, ignoring negative elements"
  dumpExplanationF 2 defaultState $ (+||)     $ positiveElementsOf [-2, -1, 0, 1, 2, 3]

  print "* the product of the doubles of all positive elements, ignoring negative and zero elements"
  dumpExplanationF 2 defaultState $ (*||) $ timesEach 2 $ positiveElementsOf [-2, -1, 0, 1, 2, 3]

  print "* the sum of the doubles of all positive elements and the unchanged original values of all negative elements"
  dumpExplanationF 2 defaultState $ (+||) $ timesPositives 2 [-2, -1, 0, 1, 2, 3]


defaultState :: MyState
defaultState = emptyState { symtabF = Map.fromList [("dow", "dow" @|. 7)] }
  

runTests_Mathlang :: IO ()
runTests_Mathlang = do
  print "* mathlang tests"

  print "** two plus (two on sundays, and one every other day) equals three or four"
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


