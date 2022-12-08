{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-| transpiler to ProB \/ EventB \/ Rodin

Some decisions we make along the way:
- what is the timing model -- discrete-time or real-time?
- do we aim to generate a restricted set of assertions?
- are we using LTL or CTL? Let's try both to start?

One of the concrete assertions we want to generate is for PDPA:

== CTL output

@
  EF EX[notify_indiv_happened(org)]
  EF EX[prohibit_notify_indiv_happened(pdpc)]
  EG {breached_by = None}
@

== LTL output

@
  (G {breached_by = {}}) &
  G ([prohibit_notify_indiv_happened(_)] =>
    H not [notify_indiv_happened(_)])
@

-}

module LS.XPile.B where

import LS
import AnyAll.BoolStruct (alwaysLabeled)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Prettyprinter
import Text.Pretty.Simple (pShowNoColor)
import qualified AnyAll as AA
import qualified Data.Map as Map

-- | extract the tree-structured rules from Interpreter

asB :: Interpreted -> String
asB l4i =
     show (vsep
           [ "toplevelDecisions :: Map.Map (String) (Item String)"
           , "toplevelDecisions = Map.fromFoldable " <>
             (pretty $ TL.unpack (
                 pShowNoColor
                   [( T.intercalate " / " (T.unwords <$> names)
                    , alwaysLabeled bs)
                   | (names,bs) <- qaHornsT l4i
                   ]
                 )
             )
           , "toplevelDefaultMarking :: Marking"
           , "toplevelDefaultMarking = Marking $ Map.fromFoldable " <>
             (pretty . TL.unpack
              . TL.replace "False" "false"
              . TL.replace "True" "true"
              . pShowNoColor $ 
              Map.toList . AA.getMarking $
              getMarkings l4i
             )
           ]
          )
