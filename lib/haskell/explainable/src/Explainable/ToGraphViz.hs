{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Explainable.ToGraphViz where

import Explainable.MathLang

import Data.Graph.Inductive.Graph
  ( Context,
    Graph (mkGraph, nodeRange),
    Node,
    delEdge,
    delNode,
    insEdge,
    insNode,
    lab,
    labfilter,
    nodes,
    pre,
    suc,
  )
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.GraphViz
  ( Attribute,
    Attributes,
    DotGraph,
    GlobalAttributes (GraphAttrs, NodeAttrs),
    GraphID (Str),
    GraphvizParams (..),
    LNodeCluster,
    NodeCluster (C, N),
    PrintDot (unqtDot),
    RankType (SameRank),
    Shape (BoxShape, Circle, DiamondShape),
    X11Color (Black, Brown, Green, White),
    color,
    fillColor,
    fontColor,
    graphToDot,
    shape,
    toLabel,
  )
import Data.GraphViz.Attributes.Complete
  ( Attribute (Comment, Compound, FontName, Height, Rank, Style, TailPort),
    CompassPoint (..),
    PortPos (..),
    StyleItem (..),
    StyleName (..),
  )
import Data.GraphViz.Printing (renderDot)


data VarDeets = VD { vname :: String }
              deriving (Eq, Show)

-- let's do this inductively / recursively.
data NType = Operation POperator
           | Variable VarDeets
           deriving (Eq, Show)
data PNode = PN { ntype :: NType
                , ntext :: String
                , nlabel :: Attributes
                }
             deriving (Eq, Show)
type PEdge = Attributes

type MyFGL = Gr PNode PEdge

data POperator = Plus | Minus
               deriving (Eq, Show)
