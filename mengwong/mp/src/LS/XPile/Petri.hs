{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module LS.XPile.Petri(module LS.XPile.Petri) where

import qualified Data.Text.Lazy as Text
import Data.Text.Lazy (Text)

import Data.GraphViz
import Data.GraphViz.Attributes
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz.Printing (renderDot)
import Data.GraphViz.Attributes.Complete (Attribute(Height, Style, FontName, Compound)
                                         , StyleItem(..), StyleName(..))

--------------------------------------------------------------------------------
-- fgl
--------------------------------------------------------------------------------

data NType = Place | Trans | Decis deriving (Eq, Show)
type PLabel = Attributes -- used later for graphviz, basically things like (color,blue), (label,somestring)
data PNode = PN { ntype  :: NType
                , ntext  :: Text
                , nlabel :: Attributes }
             deriving (Eq, Show)

type Petri = Gr PNode PLabel

mkPlace,mkTrans,mkDecis :: Text -> PNode
mkPlace x = PN Place x []
mkTrans x = PN Trans x []
mkDecis x = PN Decis x []

mySecondFGL :: Gr PNode PLabel
mySecondFGL = mkGraph
              [ (1, PN Place "start"   [])
              , (2, PN Trans "compute" [])
              , (3, PN Place "end"     [color Black]) ]
              [ (1,2, [])
              , (2,3, []) ]

--------------------------------------------------------------------------------
-- graphviz
--------------------------------------------------------------------------------

petriGP :: DotGraph Node
petriGP = graphToDot (petriParams mySecondFGL) mySecondFGL

-- main = putStrLn . Text.unpack $ renderDot $ unqtDot petriGP

petriParams :: Gr PNode PLabel -> GraphvizParams Int PNode PLabel Int PNode
petriParams g = Params
  { isDirected       = True
  , globalAttributes = [GraphAttrs [Compound True]]
  , clusterBy        = clusterby
  , isDotCluster     = const False
  , clusterID        = clusterid
  , fmtCluster       = fmtcluster
  , fmtNode          = fmtPetriNode
  , fmtEdge          = fmtPetriEdge g
  }

clusterby :: (Int,PNode) -> LNodeCluster Int PNode
clusterby a@(_n,(PN Place _ _)) = C 1 (N a)
clusterby a@(_n,(PN Trans _ _)) = C 2 (N a)
clusterby a@(_n,(PN Decis _ _)) = C 1 (N a)

clusterid :: Node -> GraphID
clusterid 1 = Str "places"
clusterid 2 = Str "transitions"
clusterid _ = Str "other"

fmtcluster :: Node -> [GlobalAttributes]
fmtcluster 1 = [NodeAttrs [ shape Circle ] ]
fmtcluster 2 = [NodeAttrs [ shape BoxShape
                          , Height 0.2
                          , Style [ SItem Filled [] ]
                          , fillColor Black
                          , fontColor White
                          , FontName "Monaco" ] ]
fmtcluster _ = []

fmtPetriNode :: (Node, PNode) -> [Attribute]
fmtPetriNode (_n,(PN Place txt@"FULFILLED" lbls)) = toLabel txt : color Green : lbls 
fmtPetriNode (_n,(PN Place txt@"BREACH"    lbls)) = toLabel txt : color Brown : lbls 
fmtPetriNode (_n,(PN Place txt lbls)) = toLabel txt : lbls 
fmtPetriNode (_n,(PN Trans txt lbls)) = toLabel txt : lbls
fmtPetriNode (_n,(PN Decis txt lbls)) = toLabel txt : shape DiamondShape : lbls

fmtPetriEdge :: Graph gr => gr PNode PLabel -> (Node, Node, PLabel) -> [Attribute]
fmtPetriEdge g (_s,e,el) -- if the edge goes to BREACH then we paint the edge brown
  | (ntext <$> lab g e) == Just "BREACH"    = color Brown : el
  | (ntext <$> lab g e) == Just "FULFILLED" = color Green : el
  | otherwise                               = el
