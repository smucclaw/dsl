{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-| This module provides an internal convenience DSL for representing
insurance, as a sketchbook for representing the semantics. Expressions
in this DSL are meant to be used as a point of comparison with the L4
rendering.
-}

module Genre.Insurance ( module Genre.Insurance.Policy2020 -- | re-exported
                       , module Genre.Insurance.Common     -- | re-exported
                       , claimable
                       ) where

import Genre.Insurance.Policy2020
import Genre.Insurance.Common

-- | Given a particular policy template, instance, scenario, and choice of benefit, what amount is claimable?
claimable :: PolicyTemplate -> PolicyInstance -> Scenario
          -> (PolicyInstance -> SumAssured)
          -> (PolicyTemplate -> [Modifier Scenario])
          -> Int
claimable pt pI sc claimBenefit getMods =
  let sumAssured    = claimBenefit pI
      modifiers     = getMods pt
      adjustment    = evalMods sc modifiers
  in round $ fromIntegral sumAssured * fromRational adjustment
