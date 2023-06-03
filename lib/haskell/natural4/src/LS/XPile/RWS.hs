{-| module for wrapping transpilation errors and STDERR trace mumbling. -}

module LS.XPile.RWS (XPileRWS, XPileRWSE, xpRWS, tell, xpReturn, xpError) where

import Control.Monad.RWS ( evalRWS, MonadWriter(tell), RWS
                         , evalRWST, RWST )
import Data.Map as Map ( Map )

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

-- | There are two kinds of errors available.
-- In the course of normal operation, you can mumble to the equivalent of STDERR
-- by saying `tell` to output to the timestap.err file.
-- 
-- But if the error is unrecoverable, then return a Left value, by using `xpLeft`.
-- And that error will appear in the output file, commented.
--
-- Normal output then gets returned via `xpReturn`.
type 87XPileRWSE a = XPileRWS (Either String a)
                
-- | the caller calls @(output, err) = xpRWS asOutput@
--
-- The err is a list of strings morally equivalent to STDERR
-- the output is passed through.
--
-- This function is usually called in `app/Main.hs`
xpRWS :: XPileRWS a -> (a, [String])
xpRWS x = evalRWS x mempty mempty

xpReturn, xpRight :: a -> XPileRWS (Either String a)
xpReturn = xpRight
xpRight = pure . Right

xpError, xpLeft :: String -> XPileRWS (Either String a)
xpError = xpLeft
xpLeft = pure . Left


