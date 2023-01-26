{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

{-
  Work-in-progress transpiler to Maude.
  Note that since we do all the parsing and transpilation within Maude itself,
  all we do here is convert the list of rules to a textual, string
  representation that Maude can parse.
-}
module LS.XPile.Maude where

import Data.List ( intercalate )
import Data.Text qualified as T

import LS.Types
import LS.Rule

import Prettyprinter
import Flow ( (|>) )

-- This function is still a work in progress.
rule2maude :: Rule -> String
rule2maude
  Regulative
    { rlabel = Just (_, _, ruleName),
      rkeyword = RParty,
      deontic,
      action,
      temporal = Just tempConstr@(TemporalConstraint _ _ _),
      hence,
      lest
    } =
    mconcat
      [ "RULE '",
        T.unpack ruleName,
        deontic',
        "DO '",
        show action,
        show tempConstr,
        "'",
        show hence,
        "'",
        show lest
      ]
    where
      -- Thanks monomorphism restriction.
      -- show2text :: forall a. Show a => a -> String
      -- show2text = show .> T.pack
      deontic' = deontic |> deontic2str
      deontic2str DMust = "MUST"
      deontic2str DMay = "MAY"
      deontic2str DShant = "SHANT"

rules2maude :: [Rule] -> String
rules2maude rules = rules
  |$> rule2maude
  |> intercalate ", "

-- Utilities.

infixl 0 |$>

(|$>) :: Functor f => f a -> (a -> b) -> f b
x |$> f = fmap f x