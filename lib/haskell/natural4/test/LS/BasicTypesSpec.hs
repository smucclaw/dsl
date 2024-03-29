{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module LS.BasicTypesSpec (spec) where

import Data.HashSet qualified as Set
import Data.Text qualified as T
import Data.Text.Arbitrary ()
import LS.Types
  ( MyToken
      ( As,
        Distinct,
        EOF,
        EOL,
        Empty,
        GoDeeper,
        Other,
        RuleMarker,
        SOF,
        TokFalse,
        TokTrue,
        TypeSeparator,
        UnDeeper
      ),
    renderToken,
    toTokens,
  )
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck
  ( Arbitrary (..),
    Property,
    Testable (property),
    genericShrink,
    (===),
    (==>),
  )
import Test.QuickCheck.Arbitrary.Generic
  ( Arbitrary (..),
    genericArbitrary,
    genericShrink,
  )

instance Arbitrary MyToken where
  arbitrary = genericArbitrary
  shrink = genericShrink

notOther :: MyToken -> Bool
notOther (Other _) = False
notOther (RuleMarker _ _) = False
notOther _ = True

prop_rendertoken :: MyToken -> Property
prop_rendertoken mytok =
  mytok `notElem` tokens && notOther mytok ==>
    toTokens (T.pack $ renderToken mytok) === [mytok]
  where
    tokens :: Set.HashSet MyToken =
      [ Distinct,
        TokTrue,
        TokFalse,
        As,
        EOL,
        GoDeeper,
        UnDeeper,
        Empty,
        SOF,
        EOF,
        TypeSeparator
      ]

spec :: Spec
spec = do
    describe "renderToken" do
      it "is the inverse of toToken" do
        property prop_rendertoken
