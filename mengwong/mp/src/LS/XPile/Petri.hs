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
import Data.GraphViz.Attributes.Complete (Attribute(Height, Style, FontName, Compound, Comment, Rank)
                                         , StyleItem(..), StyleName(..), RankType(..))

--------------------------------------------------------------------------------
-- fgl
--------------------------------------------------------------------------------

data NType = Place | Trans | Decis deriving (Eq, Show)
type PLabel = Attributes -- used later for graphviz, basically things like (color,blue), (label,somestring)
data PNode a = PN { ntype  :: NType
                  , ntext  :: Text
                  , nlabel :: Attributes
                  , ndeets :: [a]
                  }
             deriving (Eq, Show)

type Petri a = Gr (PNode a) PLabel

mkPlace,mkTrans,mkDecis :: Text -> (PNode a)
mkPlace x = PN Place x [] []
mkTrans x = PN Trans x [] []
mkDecis x = PN Decis x [] []

-- usage: mkPlaceC "start node" "comment label for start node"
mkXC :: NType -> [a] -> Text -> PNode a
mkXC x a l = PN x l [] a

mkPlaceA,mkTransA,mkDecisA :: [a] -> Text -> PNode a
mkPlaceA = mkXC Place
mkTransA = mkXC Trans
mkDecisA = mkXC Decis

pAddAttribute :: Attribute -> PNode a -> PNode a
pAddAttribute x pn = pn {nlabel = x : nlabel pn}

mySecondFGL :: Gr (PNode ()) PLabel
mySecondFGL = mkGraph
              [ (1, PN Place "start"   [] [])
              , (2, PN Trans "compute" [] [])
              , (3, PN Place "end"     [color Black] []) ]
              [ (1,2, [])
              , (2,3, []) ]

--------------------------------------------------------------------------------
-- graphviz
--------------------------------------------------------------------------------

petriGP :: DotGraph Node
petriGP = graphToDot (petriParams mySecondFGL) mySecondFGL

-- main = putStrLn . Text.unpack $ renderDot $ unqtDot petriGP

petriParams :: (Show a) => Gr (PNode a) PLabel -> GraphvizParams Int (PNode a) PLabel Int (PNode a)
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

clusterby :: (Int,PNode a) -> LNodeCluster Int (PNode a)
clusterby a@( 0,PN Place _ _ _) = C 3 (N a) -- breach
clusterby a@( 1,PN Place _ _ _) = C 3 (N a) -- fulfilled
clusterby a@(_n,PN Place _ _ _) = C 1 (N a)
clusterby a@(_n,PN Trans _ _ _) = C 2 (N a)
clusterby a@(_n,PN Decis _ _ _) = C 1 (N a)

clusterid :: Node -> GraphID
clusterid 1 = Str "places"
clusterid 2 = Str "transitions"
clusterid 3 = Str "breachfulfilled"
clusterid _ = Str "other"

fmtcluster :: Node -> [GlobalAttributes]
fmtcluster 1 = [NodeAttrs [ shape Circle ] ]
fmtcluster 2 = [NodeAttrs [ shape BoxShape
                          , Height 0.2
                          , Style [ SItem Filled [] ]
                          , fillColor Black
                          , fontColor White
                          , FontName "Monaco" ] ]
fmtcluster 3 = [GraphAttrs [ Rank SameRank ] ]
fmtcluster _ = []

tcsd :: (Show a) => [a] -> [Attribute]
tcsd = fmap $ Comment . Text.pack . show

fmtPetriNode :: Show a => (Node, PNode a) -> [Attribute]
fmtPetriNode (_n,PN Place txt@"FULFILLED" lbls ds) = toLabel txt : color Green        : tcsd ds ++ lbls 
fmtPetriNode (_n,PN Place txt@"BREACH"    lbls ds) = toLabel txt : color Brown        : tcsd ds ++ lbls 
fmtPetriNode (_n,PN Place txt lbls ds)             = toLabel txt                      : tcsd ds ++ lbls 
fmtPetriNode (_n,PN Trans txt lbls ds)             = toLabel txt                      : tcsd ds ++ lbls
fmtPetriNode (_n,PN Decis txt lbls ds)             = toLabel txt : shape DiamondShape : tcsd ds ++ lbls

fmtPetriEdge :: Graph gr => gr (PNode a) PLabel -> (Node, Node, PLabel) -> [Attribute]
fmtPetriEdge g (s,e,el) -- if the edge goes to BREACH then we paint the edge brown
  | (ntext <$> lab g e) == Just "BREACH"                = color Brown : el
  | (ntext <$> lab g e) == Just "FULFILLED" && s /= 0   = color Green : el
  | otherwise                                           = el
