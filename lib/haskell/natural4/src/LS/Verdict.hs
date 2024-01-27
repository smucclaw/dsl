{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-|

The Verdict module provides a runtime evaluation engine to deliver
answers based on available input.

Typical usage: some application executable obtains input data from the
end-user, sets up a query, and returns an answer, with accompanying
explanation.

-}

module LS.Verdict
  ()
where

import AnyAll qualified as AA
-- import Explainable
-- import Explainable.MathLang
import LS.Rule (Interpreted)

-- | User input takes the form of a Marking -- see AnyAll for more on what that means.
type RuntimeInput = AA.TextMarking

-- | What is the output of partial evaluation? a residual ruleset?
-- Let's start simple with just a true or false.
type RuntimeOutput =
  Bool
  -- [Rule]

-- | Let's compute a verdict, along with explanations.
l4verdict :: Interpreted
          -> RuntimeInput
          -> (RuntimeOutput, [String])
l4verdict l4i rti =
  (True, ["Explainability goes here"])
