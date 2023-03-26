module Genre.Insurance.Common where

{-| This module provides common functionality regarding modifiers and scenarios. -}

import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ratio ( (%) )

-- * Each benefit can be modified in various ways.
-- | Each benefit contains an attribute listing multiple modifiers.
data Modifier scenario =
  Coefficient Rational ScenarioEval
  -- ^ naively, we multiply all the modifier coefficients in the list, if they apply
  | FirstOf [Modifier scenario]
  -- ^ sometimes there is a group of modifiers which is first-to-fire,
  -- similar to the DMN F hit policy
  deriving (Eq, Show)

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
evalScenario sc (ScEqN k n)     = sc ! k == n
evalScenario sc (ScGt  k n)     = sc ! k >  n
evalScenario sc (ScGtE k n)     = sc ! k >= n
evalScenario sc (ScLt  k n)     = sc ! k <  n
evalScenario sc (ScLtE k n)     = sc ! k <= n
evalScenario sc (ScAnd sc1 sc2) = evalScenario sc sc1 && evalScenario sc sc2
evalScenario sc (ScOr  sc1 sc2) = evalScenario sc sc1 || evalScenario sc sc2
evalScenario sc (ScGrp sc0)     = evalScenario sc sc0

-- | Let's pretend we took an Uber.
exampleSc1 :: Scenario
exampleSc1 = Map.fromList
  [ ("NQQ sum consumed", 50000)
  , ("age",                 50)
  , ("Uber",                 1)
  , ("Taxi",                 0) ]

-- | Let's pretend we took a taxi.
exampleSc2 :: Scenario
exampleSc2 = Map.fromList
  [ ("NQQ sum consumed", 50000)
  , ("age",                 50)
  , ("Uber",                 0)
  , ("Taxi",                 1) ]

-- | example modifiers
modTaxi, modUber :: Modifier Scenario
-- | pay-out doubles if we're in a taxi
modTaxi = Coefficient (200%100) $ "Taxi" `ScGtE` 1
-- | pay-out halves if we're in an Uber
modUber = Coefficient ( 50%100) $ "Uber" `ScGtE` 1

-- | In a particular scenario, a modifier will decide to multiple the Sum Assured by something like 50% or 200% or 300%... or 0
evalMod :: Scenario -> Modifier Scenario -> Maybe Rational
evalMod sc (Coefficient rat sceval) = if evalScenario sc sceval then Just rat else Nothing
evalMod sc (FirstOf mods)           = listToMaybe $ catMaybes [ evalMod sc md | md <- mods ]

-- | Run all the modifiers and come out with a result. Usually there's really only one modifier that fires, but if there are multiples, we need to be careful. One way out is to use the FirstOf logic.
evalMods :: Scenario -> [Modifier Scenario] -> Rational
evalMods sc mods =
  let coefficients = catMaybes [ evalMod sc md | md <- mods ]
  in product coefficients
                     
