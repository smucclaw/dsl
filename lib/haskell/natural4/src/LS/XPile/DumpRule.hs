module LS.XPile.DumpRule
  ( rules2String
  )
where

import qualified Data.Foldable as DF
import qualified Data.Text.Lazy as TL
import LS.Interpreter
  ( expandClauses,
    getAndOrTree,
    itemsByRule,
    onlyTheItems,
  )
import LS.Rule
  ( Interpreted (classtable, scopetable),
    Rule (clauses),
    hasClauses,
    ruleLabelName,
  )
import Text.Pretty.Simple (pShowNoColor)

rules2String :: Interpreted -> [Rule] -> String
rules2String l4i rules =
  unlines
    [ "-- original rules:\n",
      TL.unpack (pShowNoColor rules),
      "-- variable-substitution expanded AnyAll rules\n",
      TL.unpack
        ( pShowNoColor $
            [ r {clauses = expandClauses l4i 1 (clauses r)}
              | r <- rules,
                hasClauses r
            ]
        ),
      "\n\n-- class hierarchy:\n",
      TL.unpack (pShowNoColor (classtable l4i)),
      "\n\n-- symbol table:\n",
      TL.unpack (pShowNoColor (scopetable l4i)),
      "-- getAndOrTrees",
      unlines $
        ( \r ->
            "\n-- "
              <> show (ruleLabelName r)
              <> "\n"
              <> TL.unpack (pShowNoColor $ getAndOrTree l4i 1 r)
        )
          <$> rules,
      "-- traverse toList of the getAndOrTrees",
      unlines $ TL.unpack . pShowNoColor . traverse DF.toList . getAndOrTree l4i 1 <$> rules,
      "-- onlyTheItems",
      TL.unpack $ pShowNoColor (onlyTheItems l4i),
      "-- ItemsByRule",
      TL.unpack $ pShowNoColor (itemsByRule l4i rules)
    ]