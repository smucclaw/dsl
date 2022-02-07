{-# LANGUAGE OverloadedStrings #-}

module LS.XPile.VueJSON where

import LS
import Options.Generic

-- https://en.wikipedia.org/wiki/Ground_expression
grounds :: Opts Unwrapped -> [Rule] -> [MultiTerm]
grounds opts rs = concatMap (grue opts) rs

grue :: Opts Unwrapped -> Rule -> [MultiTerm]
grue opts r = [ ["Are we", "happy?" ]
              , ["Can you hear", "me now?"]
              ]




