{-# LANGUAGE BlockArguments #-}

module LS.XPile.LogicalEnglishSpec (spec) where

import LS.XPile.LogicalEnglish (toLE)
import Test.Hspec (Spec)
import TestLib (mkSpec, TestConfig (..))

-- | The 'Spec' used to test the Logical English transpiler.
spec :: Spec
spec =
  mkSpec
    TestConfig
      { description = "Logical English",
        dir = "LogicalEnglish",
        fileExt = "le",
        xpileFn = toLE
      }