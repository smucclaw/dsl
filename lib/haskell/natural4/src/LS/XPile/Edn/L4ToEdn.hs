module LS.XPile.Edn.L4ToEdn (l4rulesToEdn) where

import Control.Arrow ((>>>))
import LS.Rule (Rule)
import LS.XPile.Edn.AstToEdn (astNodeToEdn)
import LS.XPile.Edn.AstToEdn.CPSTranspileM (TranspileResult)
import LS.XPile.Edn.L4ToAst (l4rulesToProgram)

l4rulesToEdn :: [Rule] -> TranspileResult metadata
l4rulesToEdn = l4rulesToProgram >>> astNodeToEdn
