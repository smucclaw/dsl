{-# LANGUAGE DerivingStrategies #-}

module Explainable.ToGraphViz () where
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.GraphViz (Attributes)


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
