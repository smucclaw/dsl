-- | This is the importable interface to the Legal Spreadsheets parser.

module LS (module LS.Lib
          ,module LS.Types
          ,module LS.RelationalPredicates
          ,module LS.Interpreter
          ,module LS.Error
          ,module LS.PrettyPrinter) where

import LS.Lib
import LS.Types
import LS.RelationalPredicates
import LS.Interpreter
import LS.Error
import LS.PrettyPrinter

