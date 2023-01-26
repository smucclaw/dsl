-- | This is the importable interface to the Legal Spreadsheets parser.

module LS (module LS.Lib
          ,module LS.Types
          ,module LS.Rule
          ,module LS.RelationalPredicates
          ,module LS.Interpreter
          ,module LS.Error
          ,module LS.PrettyPrinter
          , myTraceM
          ) where

import LS.Lib
import LS.Types
import LS.Rule
import LS.RelationalPredicates
import LS.Interpreter
import LS.Error
import LS.PrettyPrinter
import Debug.Trace (traceM)
import Control.Monad (when)
import qualified DBNF as LS

myTraceM :: Monad m => String -> m ()
myTraceM x = when False $ traceM x

