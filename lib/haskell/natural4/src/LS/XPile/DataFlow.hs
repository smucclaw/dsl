{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-| transpiler to show dataflow for both arithmetic and boolean logic -}

module LS.XPile.DataFlow where

import LS.XPile.Logging
import LS.Interpreter

-- fgl
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)

-- graphviz
import Data.GraphViz
import Data.GraphViz.Printing (renderDot)

type FlowNodeName = String
type DataFlowGraph = Gr FlowNodeName ()

dataFlowAsDot :: Interpreted -> XPileLogE String
dataFlowAsDot l4i = do
  let graph = mkGraph
              (nodesRepresentingRules ++ nodesRepresentingLeaves)
              (edgesBetweenRules      ++ edgesFromLeavesToRules)
  return "/*  coming soon: this will be a data flow diagram  */"
  where
    nodesRepresentingRules  = []
    nodesRepresentingLeaves = []
    edgesBetweenRules       = []
    edgesFromLeavesToRules  = []

