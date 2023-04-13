{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DuplicateRecordFields #-}

module LS.XPile.Maude.Maude
  ( rules2maudeStr,
  )
where

{-
  Note that since we do all the parsing and transpilation within Maude itself,
  all we do here is convert the list of rules to a textual, string
  representation that Maude can parse.
-}

import LS.XPile.Maude.Rules (rules2maudeStr)