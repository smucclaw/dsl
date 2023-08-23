{-# OPTIONS_GHC -W #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields, RecordWildCards #-}
-- {-# LANGUAGE OverloadedRecordDot #-}
-- {-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}

module LS.XPile.LogicalEnglish.GenTemplateInsts
where
-- TODO: Make export list


-- import Data.Text qualified as T
import Data.HashSet qualified as HS
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import qualified Data.List as L hiding (head, tail)
-- import Debug.Trace (trace)
import Data.Coerce (coerce)
-- import Data.String.Interpolate ( i )
import Control.Monad.State
import Data.Traversable
import Control.Monad.Identity (Identity)

import LS.XPile.LogicalEnglish.Types



leHCFromLAbsHC :: LamAbsHC -> LEhcPrint
leHCFromLAbsHC = \case
  LAhcF labsfact -> undefined
  LAhcR labsrule -> undefined


leFactFromLAbsFact :: LamAbsFact -> LEFactForPrint
leFactFromLAbsFact = undefined





--TODO: Think abt whether to make this an inner fn in leFactFromLAbsFact 
leFactIntrmdFromLAbsFact :: LamAbsFact -> LEFactIntrmd
leFactIntrmdFromLAbsFact = undefined

leRulePrintFromleRuleIntrmd :: LERuleIntrmd -> LERuleForPrint
leRulePrintFromleRuleIntrmd = undefined
--TODO: mconcat, intersperse. refactor helper function into a common util module

leRuleIntrmdFromLAbsRule :: LamAbsRule -> LERuleIntrmd
leRuleIntrmdFromLAbsRule = undefined
