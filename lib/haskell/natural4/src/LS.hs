-- | This is the importable interface to the Legal Spreadsheets parser.

module LS
  ( module LS.Lib,
    module LS.Types,
    module LS.Rule,
    module LS.RelationalPredicates,
    module LS.Interpreter,
    module LS.Error,
    module LS.PrettyPrinter,
    myTraceM,
  )
where

import Control.Monad (when)
import Debug.Trace (traceM)
import LS.Error
import LS.Interpreter
import LS.Lib
import LS.PrettyPrinter
import LS.RelationalPredicates
import LS.Rule
import LS.Types

myTraceM :: Monad m => String -> m ()
myTraceM x = when True $ traceM x

