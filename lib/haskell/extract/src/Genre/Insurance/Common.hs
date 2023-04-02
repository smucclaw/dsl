{-| This module provides common functionality regarding modifiers and scenarios. -}

{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Genre.Insurance.Common where

import Control.Monad.Reader
import Text.RawString.QQ
import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ratio ( (%) )

-- | If a modifier applies
data IfThen scenario = IfThen ScenarioEval (Modifier scenario)
  deriving (Eq, Show)

-- * Each benefit can be modified in various ways.

-- | Each benefit contains an attribute listing multiple modifiers.
-- We define  a FEEL-like expression language which helps to operationalize the policies.
-- The schedule is a list of percentages, the greatest of which is chosen to determine the coefficient to the sum assured ...
-- but actually, it's not entirely a list of percentages; sometimes it's a percentage or a dollar amount multiplied by the number of extremities lost
-- and so the choice of "greatest" actually needs to be made among products, not percentages.
-- So, we represent the schedule as something like a FEELy 

type BenefitAmount scenario = Reader (scenario) (Maybe Int)

evalfexp :: Fexp -> BenefitAmount scenario
evalfexp fexp = do
  sc <- ask
  return $ case fexp of
             (Fconst fexp')     -> evalfexp fexp'
             (Fmax f1 f2)       -> max <$> evalfexp f1 <*> evalfexp f2
             (Fmaxs fexps)      -> maximum . evalfexp <$> fexps
             (First fexps)      -> listToMaybe (evalfexp <$> fexps)
             (Ftimes n m)       -> (*) <$> n <*> sa
             (Fproduct fexps)   -> product <$> mapMaybe evalfexp fexps
             (Fif fexp sceval)  -> if evalScenario sc sceval then evalfexp fexp' else Nothing

fproduct :: [Fexp] -> Fexp
fproduct 
             
-- * Our limited FEEL-like language
-- | our version of FEEL expressions ... basically a mathlang, operating relative to a certain variable
data Fexp = Fconst Fexp          -- ^ return a certain expression
          | Fmax   Fexp Fexp     -- ^ the maximum of two expressions, each of which is provided the variable
          | Fmaxs  [Fexp]        -- ^ the maximum of a list of expressions, each of which is provided the variable
          | First  [Fexp]        -- ^ the first element in the list of expressions which is non-null
          | Ftimes Fexp Fexp     -- ^ multiply two expressions
          | Fproduct [Fexp]      -- ^ product of a list of expressions
          | Fexp :- ScenarioEval -- ^ return this expression if the ScenarioEval is true vs a given scenario
  deriving (Eq, Show)

-- | modifiers. [TODO] tweak so the coefficients can take the actual scenario number from ScR into account
allMods :: BenefitAmount Scenario SumAssured

allMods =
  [ Ftimes (300%100) :- appliesTo "Taxi"
  , Ftimes (300%100) :- appliesTo "public transportation"
  , Ftimes (200%100) :- appliesTo "school"
  , Ftimes (200%100) :- appliesTo "pedestrian"
  , Ftimes (200%100) :- appliesTo "fire"
  , Ftimes (  5%100) :- "step-ups" `ScEqN` 1
  , Ftimes ( 10%100) :- "step-ups" `ScEqN` 2
  , Ftimes ( 15%100) :- "step-ups" `ScEqN` 3
  , Ftimes ( 20%100) :- "step-ups" `ScEqN` 4
  , Ftimes ( 25%100) :- "step-ups" `ScEqN` 5
  , Ftimes ( 50%100) :- appliesTo "rock climbing"
  ]




-- * Scenarios
-- | A user will typically come to us with a certain scenario in mind.
-- The scenario helps inform how the reductions and double/triple benefits apply.
type Scenario = Map ScL ScR
-- | For our prototype, a scenario is a map of String to Int.
type ScL = String
type ScR = Int

-- | A little minilang so we can deal with the scenario logic in a Showable fashion.
data ScenarioEval
  = ScEqN ScL ScR    -- ^ numeric equality
  | ScGt  ScL ScR    -- ^ greater than
  | ScGtE ScL ScR    -- ^ greater than or equal to
  | ScLt  ScL ScR    -- ^    less than
  | ScLtE ScL ScR    -- ^    less than or equal to
  | ScAnd ScenarioEval ScenarioEval -- ^ logical and
  | ScOr  ScenarioEval ScenarioEval -- ^ logical or
  | ScGrp ScenarioEval  -- ^ parentheses
  deriving (Eq, Show)

-- | The valuation routine will add explainability in future
evalScenario :: Scenario -> ScenarioEval -> Bool
evalScenario sc (ScEqN k n)     = maybe False (== n) $ k `Map.lookup` sc
evalScenario sc (ScGt  k n)     = maybe False (>  n) $ k `Map.lookup` sc
evalScenario sc (ScGtE k n)     = maybe False (>= n) $ k `Map.lookup` sc
evalScenario sc (ScLt  k n)     = maybe False (<  n) $ k `Map.lookup` sc
evalScenario sc (ScLtE k n)     = maybe False (<= n) $ k `Map.lookup` sc
evalScenario sc (ScAnd sc1 sc2) = evalScenario sc sc1 && evalScenario sc sc2
evalScenario sc (ScOr  sc1 sc2) = evalScenario sc sc1 || evalScenario sc sc2
evalScenario sc (ScGrp sc0)     = evalScenario sc sc0

-- * Some Made-Up Examples
-- | Let's pretend we took an Uber.
exampleScs :: [Scenario]
exampleScs = Map.fromList <$>
  [
    [ ("Scenario ID",          1)
    , ("NQQ sum consumed", 50000)
    , ("age",                 50)
    , ("Uber",                 1)
    , ("Taxi",                 0)
    , ("step-ups",             0)
    , ("death",                1)
    ]
  , [ ("Scenario ID",          2)
    , ("NQQ sum consumed", 50000)
    , ("age",                 50)
    , ("Uber",                 0)
    , ("Taxi",                 1)
    , ("step-ups",             2)
    , ("minor toe",            1)
    ]
  , [ ("Scenario ID",          3)
    , ("NQQ sum consumed", 50000)
    , ("age",                 14)
    , ("school",               1)
    , ("step-ups",             2)
    , ("death",                1)
    ]
  , [ ("Scenario ID",          3)
    , ("NQQ sum consumed", 50000)
    , ("age",                 50)
    , ("fire",                 1)
    , ("step-ups",             4)
    , ("death",                1)
    , ("tooth",                1)
    ]
  , [ ("Scenario ID",          4)
    , ("NQQ sum consumed", 50000)
    , ("age",                 50)
    , ("fire",                 1)
    , ("step-ups",             4)
    , ("death",                1)
    , ("tooth",                2)
    ]
  , [ ("Scenario ID",          5)
    , ("NQQ sum consumed", 50000)
    , ("age",                 50)
    , ("fire",                 1)
    , ("step-ups",             4)
    , ("death",                1)
    , ("tooth",                3)
    ]
  ]

scenarioID :: Scenario -> String
scenarioID = show . (! "Scenario ID")

appliesTo x = x `ScGtE` 1


schedule_NQQ =
  [ Coefficient (p%100) $ appliesTo k
  | (p,k) <- read . splitOn "% " <$> filter (not . null) $ lines [r|
100% death
150% totally and permanently disabled
150% PTL sight in both eyes
100% PTL sight in one eye
 50% PTL the lens of one eye
150% PTL speech and hearing
 50% PTL speech
 75% PTL all hearing in both ears all hearing in one ear
150% LoPTL two limbs
125% LoPTL one limb
150% LoPTL one limb and sight of one eye
150% LoPTL two hands or two feet
150% LoPTL one hand and one foot
100% LoPTL one hand or one foot

100% LoPTL both thumbs and all fingers
 70% LoPTL four fingers and thumb of one hand
 60% LoPTL four fingers of one hand
 30% LoPTL thumb (both phalanges
 25% LoPTL thumb (one phalanx
 10% LoPTL index finger (three phalanges
  8% LoPTL index finger (two phalanges
  6% LoPTL index finger (one phalanx
 10% LoPTL middle finger (three phalanges
  8% LoPTL middle finger (two phalanges
  6% LoPTL middle finger (one phalanx
 10% LoPTL ring finger (three phalanges
  8% LoPTL ring finger (two phalanges
  6% LoPTL ring finger (one phalanx
 10% LoPTL little finger (three phalanges
  8% LoPTL little finger (two phalanges
  6% LoPTL little finger (one phalanx

 15% LoPTL all the toes of one foot
  5% LoPTL great toe -- two phalanges
  3% LoPTL great toe -- one phalanx
  1% LoPTL other than the great toe, each toe
 10% LoPTL Fractured leg or patella with established non-union
  5% LoPTL Shortening of leg by at least 5cm
 50% Burns - Head - equal to or greater than 2% but less than 5%
 75% Burns - Head - equal to or greater than 5% but less than 8%
100% Burns - Head - equal to or greater than 8%
 50% Burns - Body - equal to or greater than 10% but less than 15%
 75% Burns - Body - equal to or greater than 15% but less than 20%
100% Burns - Body - equal to or greater than 20%
100% Permanent and incurable insanity

 50% Removal of the lower jaw by surgical operation
|] ++ (FEELy
       (Fmax 5000)
        (FEELy
         (Fmax(FEELy $ Ftimes 4)
  2% or $500 per tooth, up to a max of $5,000 per Accident

-- max 5000 (max (2   * tooth)
                 (500 * tooth))

-- * Evaluating Modifiers

-- | In a particular scenario, a modifier will decide to multiple the Sum Assured by something like 50% or 200% or 300%... or 0
evalMod :: Scenario -> Modifier Scenario -> Maybe Rational
evalMod sc (Coefficient rat sceval) = if evalScenario sc sceval then Just rat else Nothing
evalMod sc (FirstOf mods)           = listToMaybe $ catMaybes [ evalMod sc md | md <- mods ]

-- | Run all the modifiers and come out with a result. Usually there's really only one modifier that fires, but if there are multiples, we need to be careful. One way out is to use the FirstOf logic.
evalMods :: Scenario -> [Modifier Scenario] -> Rational
evalMods sc mods =
  let coefficients = catMaybes [ evalMod sc md | md <- mods ]
  in product coefficients

