{-# LANGUAGE OverloadedStrings #-}

module LS.XPile.Petri where

import qualified Data.Text.Lazy as Text
import Data.Text.Lazy (Text)

import Data.GraphViz
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

data NType = Place | Transition | Decision
type PLabel = [(Text,Text)]
data PNode = PN { ntype :: NType
                , ntext :: Text
                , nlabel :: PLabel }
             deriving (Eq, Show)

type Petri = Gr PNode


petriGP = GraphvizParams
  { isDirected = True
  , globalAttributes = []
  , clusterBy = clusterFunc
  , 
  }

clusterFunc :: (MyNode, nl) -> NodeCluster cl (MyNode, nl)
clusterFunc x = NodeCluster 0 x

