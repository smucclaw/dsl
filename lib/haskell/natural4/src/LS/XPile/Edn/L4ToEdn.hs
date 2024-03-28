module LS.XPile.Edn.L4ToEdn
  ( l4rulesToEdn,
    l4rulesToEdnText,
    l4rulesToEdnStr,
  )
where

import Control.Arrow ((>>>))
import Data.Text qualified as T
import LS.Rule (Rule)
import LS.XPile.Edn.AstToEdn (astNodeToEdn)
import LS.XPile.Edn.AstToEdn.CPSTranspileM (TranspileResult, ednText)
import LS.XPile.Edn.L4ToAst (l4rulesToProgram)
import Optics.Getter qualified as Optics

l4rulesToEdn :: [Rule] -> TranspileResult metadata
l4rulesToEdn = l4rulesToProgram >>> astNodeToEdn

l4rulesToEdnText :: [Rule] -> T.Text
l4rulesToEdnText = l4rulesToEdn >>> Optics.view ednText

l4rulesToEdnStr :: [Rule] -> String
l4rulesToEdnStr = l4rulesToEdnText >>> T.unpack