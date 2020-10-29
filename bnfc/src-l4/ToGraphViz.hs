
module ToGraphViz where

import Data.Text.Lazy as T (unpack)
import Data.Either ( fromRight )
import Control.Applicative ()
import qualified Data.Map as Map
import           Data.Map ((!))
import Data.List (find)
import Debug.Trace ()

import Text.Pretty.Simple ()
import AbsL ( Rule )
import L4
    ( Exit(NoExit, Solo, Choice, Close),
      MyRuleName,
      BoolGroup(..),
      ruleExits,
      showRuleName )

import Data.Graph.Inductive.Graph (lab,  buildGr, prettyPrint )
import Data.Graph.Inductive.PatriciaTree ( Gr )

import Data.GraphViz
import Data.GraphViz.Printing
import Data.Maybe (fromMaybe, fromJust)

import Data.GraphViz.Attributes.Complete

--------------------------------------------------------------------------------
type RuleMap = Map.Map MyRuleName Rule
--------------------------------------------------------------------------------

nameList :: [Rule] -> [(MyRuleName, Rule)]
nameList rs = zip (showRuleName <$> rs) rs

--------------------------------------------------------------------------------
type RuleGraph = Gr MyRuleName EdgeLabel
--------------------------------------------------------------------------------

data EdgeLabel = Next  -- solo
               | Sin   -- choice left
               | Dex   -- choice right
               | Hup   -- close ... "hangup"
            deriving (Eq, Ord, Show, Read)

-- children
children :: Rule -> [MyRuleName]
children r = case ruleExits r of
               Left  _      -> mempty
               Right NoExit -> mempty
               Right (Solo  rules)    -> ungroup rules
               Right (Close rules)    -> ungroup rules
               Right (Choice sin dex) -> ungroup sin ++ ungroup dex

ungroup :: BoolGroup a -> [a]
ungroup (AndGroup xs) = xs
ungroup ( OrGroup xs) = xs
ungroup ( IdGroup x)  = pure x

-- transitive closure -- note this assumes no loops in the input. if the program fails to halt we need to introduce an accumulator of seen nodes
connectedTo :: [(MyRuleName,Rule)] -> [Rule] -> [Rule]
connectedTo rnr rs = do
  r <- rs
  let cs = (lbang rnr) <$> children r
  cs ++ connectedTo rnr cs

haveExits = filter (\r -> case ruleExits r of
                             Left  _      -> False
                             Right NoExit -> False
                             Right _      -> True
                         )

-- for optimization later we can switch this to Data.Map or Data.HashMap if the number of rules ever grows very large
ruleName2Rule :: [Rule] -> [(String, Rule)]
ruleName2Rule rs = zip (showRuleName <$> rs) rs

ruleName2Num :: [Rule] -> [(String, Int)] -- association list to Node number in graph
ruleName2Num ofInterest  = zip (showRuleName <$> ofInterest) [1..]

asGraph :: [Rule] -> RuleGraph
asGraph rs =
  let rulesWithExits = haveExits rs
      ofInterest     = rulesWithExits ++ connectedTo (ruleName2Rule rs) rulesWithExits
  in
    buildGr $
    (\(ruleName,n) ->
        let rule     = ruleName2Rule rs `lbang` ruleName
            exitto   = fromRight NoExit $ ruleExits rule
        in ([], n, ruleName,
             (case exitto of
                 Solo ens       ->   (\exitnode -> (Next, ruleName2Num rs `lbang` exitnode)) <$> ungroup ens
                 Choice sin dex ->  ((\exitnode -> (Sin,  ruleName2Num rs `lbang` exitnode)) <$> ungroup sin)
                                     <>
                                    ((\exitnode -> (Dex,  ruleName2Num rs `lbang` exitnode)) <$> ungroup dex)
                 Close ens      ->   (\exitnode -> (Hup,  ruleName2Num rs `lbang` exitnode)) <$> ungroup ens
                 NoExit         -> []
             ))
    ) <$> ruleName2Num ofInterest

lbang db k = fromJust $ lookup k db
    
printGraph :: [Rule] -> IO ()
printGraph = prettyPrint . asGraph

-- https://stackoverflow.com/questions/20849893/how-to-plot-a-graph-using-haskell-graphviz
-- http://hackage.haskell.org/package/graphviz-2999.20.1.0/docs/Data-GraphViz.html

showDot :: [Rule] -> String
showDot rs = T.unpack $ renderDot $ toDot $ graphToDot params $ myGraph
  where
    myGraph = asGraph rs
    params = blankParams { globalAttributes = []
                         , clusterBy        = clustBy
                         , clusterID        = Num . Int
                         , fmtCluster       = clFmt
                         , fmtNode          = nodeFmt
                         , fmtEdge          = edgeFmt
                         , isDirected       = True
                         , isDotCluster     = const True
                         }
    clustBy (n,l) = C (if lab myGraph n `elem` [Just "FULFILLED", Just "BREACH"]
                       then 1
                       else 0) $ N (n,l)
    clFmt m = [GraphAttrs [toLabel $ ["IN","OUT"] !! m]]
    nodeFmt (node, clusterLabel) = [toLabel (fromMaybe "(unlabeled)" $ lab myGraph node) ]
    edgeFmt (tailN, headN, edgeLabel) = ports tailN headN edgeLabel
    -- to choose a tailport, we need to know:
    -- which exit of the tail rule does the edge belong ti? Solo, Choice(sin/dex), or Close?
    ports tailN headN edgeLabel =
      let tailRule = fromJust $ find (\r -> fromJust (lab myGraph tailN) == showRuleName r) rs
          exitto = fromRight NoExit $ ruleExits tailRule
      in (HeadPort (CompassPoint North)) :
         case exitto of
           Solo _  -> [TailPort (CompassPoint SouthEast)]
           Close _ -> [TailPort (CompassPoint SouthEast)]
           Choice sin dex -> if (fromJust $ lab myGraph headN) `elem` sin
             then [TailPort (CompassPoint SouthWest)]
             else [TailPort (CompassPoint SouthEast)]

