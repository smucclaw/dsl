module Genre.Insurance.Common where

import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.Ratio

-- * Each benefit can be modified in various ways.
-- So each benefit contains an attribute listing modifiers.
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

exampleSc1 :: Scenario
exampleSc1 = Map.fromList
  [ ("NQQ sum consumed", 50000)
  , ("age",                 50)
  , ("Grab",                 1)
  , ("Taxi",                 0) ]

exampleSc2 :: Scenario
exampleSc2 = Map.fromList
  [ ("NQQ sum consumed", 50000)
  , ("age",                 50)
  , ("Grab",                 0)
  , ("Taxi",                 1) ]

modTaxi, modGrab :: Modifier Scenario
modTaxi = Coefficient (200%100) $ ScGtE "Taxi" 1
modGrab = Coefficient ( 50%100) $ ScGtE "Grab" 1
