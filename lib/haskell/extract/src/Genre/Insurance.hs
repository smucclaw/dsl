{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- * This module provides an internal convenience DSL for representing insurance, as a sketchbook for representing the semantics.
-- Expressions in this DSL are meant to be used as a point of comparison with the L4 rendering.

module Genre.Insurance (module Genre.Insurance.Policy2020
                       ,module Genre.Insurance.Common
                       ) where

import Genre.Insurance.Policy2020
import Genre.Insurance.Common
