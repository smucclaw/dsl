{-# LANGUAGE OverloadedStrings #-}

{-| transpiler to show dataflow for both arithmetic and boolean logic -}

module LS.XPile.DataFlow () where

-- if you want to upgrade this to Hashmap, go ahead

-- fgl
import Data.Graph.Inductive.Graph (Graph (mkGraph), Node)
import Data.Graph.Inductive.PatriciaTree (Gr)
-- graphviz
import Data.GraphViz
  ( DotGraph,
    GlobalAttributes (GraphAttrs, NodeAttrs),
    GraphID (Str),
    GraphvizParams (..),
    NodeCluster (C, N),
    Shape (Circle),
    graphToDot,
    shape,
  )
import Data.GraphViz.Attributes.Complete (Attribute (Compound))
import Data.GraphViz.Printing (renderDot)
import Data.HashMap.Strict qualified as Map
import LS
import LS.XPile.Logging (XPileLog)

-- * Conceptually, a FlowNode is either a rule or a leaf/ground term referenced by a rule.
--
-- Suppose the top-level rule is:
--
--    @DECIDE happy IF warm AND sunny@
--
-- The top-level node corresponds to "happy". And there are two dependencies: @warm@ and @sunny@.
--
-- Let's say @warm@ is a ground term: we have no further information
-- about @warm@. We need a human to provide a valuation of that fact.
-- In our graph it is a leaf node.
--
-- Let's say @sunny@ is itself a rule: @sunny IF daytime AND NOT cloudy@.
--
-- And it bottoms out there: both @daytime@ and @cloudy@ are ground terms.
--
-- So our dataflow graph has:
--
-- @
--     warm    -> happy
--     sunny   -> happy
--     daytime -> sunny
--     cloudy  -> sunny
-- @
--
-- Note that we are throwing away the logic -- we have lost the "NOT" on the "cloudy".
--
-- The same idea applies to arithmetic rules: we might have @DECIDE happiness IS baselineHappiness + situationalHappiness@, where the terms are numeric-typed; the same shape of graph obtains.
--
-- Our task is to return that graph.

-- | we use RuleNames to label the nodes in the rule diagram.
type FlowNode = RuleName

-- | it is our responsibility to maintain a mapping between node label and node number for use with fgl.
type FlowMap = Map.HashMap FlowNode Int

-- | We don't need edge labels. In the future, if we really wanted to, we could encode the control logic into an edge label, such that NOT is a 1, AND is a 2, etc etc.
type DataFlowGraph = Gr FlowNode ()

-- | This is the top-level entry point for this file; we produce a dotfile and rely on other elements of the L4 runtime to produce SVG from the Dot.
dataFlowAsDot :: Interpreted -> XPileLog String
dataFlowAsDot l4i = do

  -- https://hackage.haskell.org/package/fgl-5.8.1.1/docs/Data-Graph-Inductive-Graph.html#v:mkGraph
  let dfg :: DataFlowGraph
      dfg = mkGraph
            (Map.toList ruleNodes <> Map.toList leafNodes)
            ruleEdges

  let dot :: DotGraph Node
      dot = graphToDot (flowParams dfg) dfg

  -- if you look at Petri.hs you will see its graph construction delves deep into the logical relationship between rules.
  -- That code was written before we had the Intepreter available to analyze rules for us.
  -- So, we grab one tree of rules at a time from the RuleGraph provided by the interpreter, and dump those;
  -- then we dump the ground term leaves in those rules.

  return "/*  coming soon: this will be a data flow diagram  */"

  where
    ruleNodes = Map.fromList ( zip [1..] [ [MTT "pretend rule R1" ] -- 1
                                         , [MTT "pretend rule R2" ] -- 2
                                         ]  )
    leafNodes = Map.fromList ( zip [1..] [ [MTT "pretend leaf L1" ] -- 1
                                         , [MTT "pretend leaf L2" ] -- 2
                                         , [MTT "pretend leaf L3" ] -- 3
                                         , [MTT "pretend leaf L4" ] -- 4
                                           ] )
    ruleEdges = [ (1, 1, ()),
                  (1, 2, ()),
                  (2, 3, ()),
                  (2, 4, ()) ]

    -- see Petri.hs for a more complex example of styling the graphviz output
    flowParams :: DataFlowGraph -> GraphvizParams Int FlowNode () Int FlowNode
    flowParams g = Params
      { isDirected       = True
      , globalAttributes = [GraphAttrs [Compound True]]
      , clusterBy        = C 1 . N -- in future we may want to partition all leaf nodes into a separate cluster to better identify them
      , isDotCluster     = const False
      , clusterID        = const (Str "clusterId")
      , fmtCluster       = const [NodeAttrs [ shape Circle ] ]
      , fmtNode          = const []
      , fmtEdge          = const []
      }
