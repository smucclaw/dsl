{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-| transpiler to show dataflow for both arithmetic and boolean logic -}

module LS.DataFlow where

import LS.XPile.Logging
import LS
import LS.Interpreter
import LS.Rule
import Data.HashMap.Strict qualified as Map -- if you want to upgrade this to Hashmap, go ahead
import Data.Text qualified as Text
import Flow ((.>), (|>))

-- fgl
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)

-- graphviz
import Data.GraphViz
    ( shape,
      graphToDot,
      Shape(Circle),
      GraphID(Str),
      NodeCluster(N, C),
      GlobalAttributes(NodeAttrs, GraphAttrs),
      GraphvizParams(..),
      PrintDot (unqtDot),
      DotGraph,
      toLabel,
    )
import Data.Text.Lazy qualified as LT
import Data.GraphViz.Printing (renderDot)
import Data.GraphViz.Attributes.Complete

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
--
-- We get most of it out of the Interpreter, with the original Rules preserved, so we work with that.

-- | This is the top-level entry point for this file; we produce a dotfile and rely on other elements of the L4 runtime to produce SVG from the Dot.
dataFlowAsDot :: Interpreted -> XPileLog String
dataFlowAsDot l4i = do
  -- https://hackage.haskell.org/package/fgl-5.8.1.1/docs/Data-Graph-Inductive-Graph.html#v:mkGraph
  let dfg :: RuleGraph
      dfg = ruleGraph l4i

  let dot :: DotGraph Node
      dot = graphToDot flowParams dfg

  -- if you look at Petri.hs you will see its graph construction delves deep into the logical relationship between rules.
  -- That code was written before we had the Intepreter available to analyze rules for us.
  -- So, we grab one tree of rules at a time from the RuleGraph provided by the interpreter, and dump those;
  -- then we dump the ground term leaves in those rules.

  let rG = ruleGraph l4i
  mutterd 2 "dataFlowasDot: retrieving ruleGraph"
  mutterdhsf 3 "dataFlowasDot: first, let's dump the rulegraph" pShowNoColorS rG

  mutterd 3 "dataFlowasDot: heeere's ruleGraphErr"
  mutters (ruleGraphErr l4i)

  let toreturn = dfg
                  |> graphToDot flowParams
                  |> unqtDot
                  |> renderDot
                  |> LT.toStrict
                  |> Text.unpack

  mutterdhsf 3 "and now we should get some dot goodness" pShowNoColorS toreturn
  return toreturn


  where
    ruleNodes = Map.fromList ( zip [(1 :: Int)..] [ [MTT "pretend rule R1" ] -- 1
                                         , [MTT "pretend rule R2" ] -- 2
                                         ]  )
    leafNodes = Map.fromList ( zip [(1 :: Int)..] [ [MTT "pretend leaf L1" ] -- 1
                                         , [MTT "pretend leaf L2" ] -- 2
                                         , [MTT "pretend leaf L3" ] -- 3
                                         , [MTT "pretend leaf L4" ] -- 4
                                           ] )
    ruleEdges = [ (1, 1, ()),
                  (1, 2, ()),
                  (2, 3, ()),
                  (2, 4, ()) ]

    -- see Petri.hs for a more complex example of styling the graphviz output
    flowParams :: GraphvizParams Int Rule () Int Rule
    flowParams = Params
      { isDirected       = True
      , globalAttributes = [GraphAttrs [Compound True]]
      , clusterBy        = C 1 . N -- in future we may want to partition all leaf nodes into a separate cluster to better identify them
      , isDotCluster     = const False
      , clusterID        = const (Str "clusterId")
      , fmtCluster       = const [NodeAttrs [ shape Circle ] ]
      , fmtNode          = fmtRuleNode
      , fmtEdge          = const []
      }

fmtRuleNode :: (Node, Rule) -> [Attribute]
fmtRuleNode (n, r) = pure $ toLabel (Text.pack (show n) <> "\\n" <> mt2text (ruleName r))



