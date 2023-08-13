{-# LANGUAGE OverloadedStrings #-}

module LS.XPile.LogicalEnglish.Common 
  (
  L4Prog,
  Rule,
  (|>)
  ) 
  where

import Flow ((|>))
import Text.Pretty.Simple   ( pShowNoColor )
import Control.Monad.Identity ( Identity )

import LS.Rule              ( Rule )


type L4Prog = [Rule]  
-- TODO: consider making this a newtype if the type ends up being useful for transpilation
