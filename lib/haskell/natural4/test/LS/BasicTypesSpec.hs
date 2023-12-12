{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module LS.BasicTypesSpec (spec) where

import AnyAll
import Data.List.NonEmpty
import Data.Text qualified as T
import Data.Text.Arbitrary
import LS.Types
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic

instance Arbitrary MyToken where
  arbitrary = genericArbitrary
  shrink = genericShrink

notOther :: MyToken -> Bool
notOther (Other _) = False
notOther (RuleMarker _ _) = False
notOther _ = True

prop_rendertoken :: MyToken -> Property
prop_rendertoken mytok =
  mytok `notElem` [Distinct, TokTrue, TokFalse, As, EOL, GoDeeper, UnDeeper, Empty, SOF, EOF, TypeSeparator, Other "", RuleMarker 0 ""] && notOther mytok ==>
  toToken (T.pack $ renderToken mytok) === [mytok]

spec :: Spec
spec = do
    describe "renderToken" do
      it "is the inverse of toToken" do
        property prop_rendertoken
