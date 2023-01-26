{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-| example of a simple transpiler

-}

module LS.XPile.NaturalLanguage where

import LS
import AnyAll as AA
import qualified Data.Map as Map
import qualified Data.Text as T
import LS.Rule
-- import Debug.Trace (trace)

toNatLang :: Interpreted -> String
toNatLang l4i =
  unlines [ show names
          | (names, _bs) <- qaHornsT l4i
          ]

