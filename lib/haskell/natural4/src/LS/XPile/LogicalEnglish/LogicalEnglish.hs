{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-|

We're trying to work with the rules / AST instead, 
in part because we don't need most of the stuff Interpreter.hs provides,
and in part to avoid breaking if the spec / API for Interpreter.hs changes.
After all, the design intentions for this short-term LE transpiler aren't the same as those for the analzyer (which is part of what will hopefully become an effort in longer-term, more principled language development).
-}

module LS.XPile.LogicalEnglish.LogicalEnglish (toLE) where

import LS.PrettyPrinter
    ( myrender, vvsep, (</>), tildes, (<//>), srchs )
import Prettyprinter
    ( vsep, viaShow, hsep, emptyDoc, (<+>), Pretty(pretty), Doc, indent, line )
import Text.Pretty.Simple   ( pShowNoColor )
import Data.Text qualified as Text
import Data.Bifunctor       ( first )
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.HashMap.Strict qualified as Map
import Control.Monad.Identity ( Identity )

import LS.Rule (Rule(..))
import LS.XPile.LogicalEnglish.Common (
    L4Prog,
    (|>)
    )


{- 
TODO: After we get a v simple end-to-end prototype out, 
we'll add functionality for checking the L4 input rules __upfront__ for things like whether it's using unsupported keywords, whether the input is well-formed by the lights of the translation rules, and so forth. 
The thought is that if the upfront checks fail, we'll be able to exit gracefully and provide more helpful diagnostics / error messages.

But for now, we will help ourselves, undeservedly, to the assumption that the L4 input is wellformed.
-}


toLE :: L4Prog -> String
toLE = undefined

