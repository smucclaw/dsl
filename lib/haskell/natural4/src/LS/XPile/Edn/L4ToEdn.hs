module LS.XPile.Edn.L4ToEdn
  ( l4rulesToEdn,
    l4rulesToEdnText,
    l4rulesToEdnStr,
  )
where

import Control.Arrow ((>>>))
import Data.EDN qualified as EDN
import Data.Text qualified as T
import LS.Rule (Rule)
import LS.XPile.Edn.AstToEdn (astNodeToEdn)
import LS.XPile.Edn.L4ToAst (l4rulesToProgram)
import Optics.Getter qualified as Optics

l4rulesToEdn :: [Rule] -> EDN.TaggedValue
l4rulesToEdn = l4rulesToProgram >>> astNodeToEdn

l4rulesToEdnText :: [Rule] -> T.Text
l4rulesToEdnText = l4rulesToEdn >>> EDN.renderText

l4rulesToEdnStr :: [Rule] -> String
l4rulesToEdnStr = l4rulesToEdnText >>> T.unpack