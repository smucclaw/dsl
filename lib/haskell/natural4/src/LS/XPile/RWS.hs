{-| module for wrapping transpilation errors and STDERR trace mumbling. -}

module LS.XPile.RWS (XPileRWS, xpRWS, tell) where

import Control.Monad.RWS
import Data.Map as Map

-- | typical usage (see `LS.XPile.Purescript.translate2PS` for an example):
--
-- @module Transpiler@ defines an @asOutput :: XPileRWS T.Text@
--
-- the @asOoutput@ function can @tell [String]@
--
-- and it returns its output using pure or returne
--
-- the reader and state usually go unused, but if you want
-- to jot down your scribbles, you can use a Map String String
-- or define your own type along these lines
type XPileRWS = RWS
                (Map String String) -- ^ reader
                [String]            -- ^ writer
                (Map String String) -- ^ state

-- | the caller calls @(output, err) = xpRWS asOutput@
--
-- The err is a list of strings morally equivalent to STDERR
-- the output is passed through.
--
-- This function is usually called in `app/Main.hs`
xpRWS :: XPileRWS a -> (a, [String])
xpRWS x = evalRWS x mempty mempty


  
