
module ToGraphViz where

import Control.Monad      ( when, guard )
import Data.List (intercalate)
import Data.Tree
import Data.Maybe
import Data.Either
import Control.Applicative
import qualified Data.Map as Map
import           Data.Map ((!))
import Debug.Trace

import Text.Pretty.Simple
import qualified Data.Text.Lazy as T
import qualified SkelL
import PrintL  ( Print, printTree )
import AbsL
import L4

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

import Data.Graph.Inductive.Monad
import Data.Graph.Inductive.Monad.IOArray

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
connectedTo :: (Map.Map MyRuleName Rule) -> [Rule] -> [Rule]
connectedTo rnr rs = do
  r <- rs
  let cs = (rnr !) <$> children r
  cs ++ connectedTo rnr cs

haveExits = filter (\r -> case ruleExits r of
                             Left  _      -> False
                             Right NoExit -> False
                             otherwise    -> True
                         )

asGraph :: [Rule] -> RuleGraph
asGraph rs =
  let ruleName2Rule  = Map.fromList $ (\r -> (showRuleName r, r)) <$> rs
      rulesWithExits = haveExits rs
      ofInterest     = rulesWithExits ++ connectedTo ruleName2Rule rulesWithExits
      ruleName2Num   = Map.fromList $ zip (showRuleName <$> ofInterest) [1..]
  in
    buildGr $
    (\(ruleName,n) ->
        let rule     = ruleName2Rule ! ruleName
            exitto   = fromRight NoExit $ ruleExits rule
        in ([], n, ruleName,
             (case exitto of
                 Solo ens       ->   (\exitnode -> (Next, ruleName2Num ! exitnode)) <$> ungroup ens
                 Choice sin dex ->  ((\exitnode -> (Sin,  ruleName2Num ! exitnode)) <$> ungroup sin)
                                     <>
                                    ((\exitnode -> (Dex,  ruleName2Num ! exitnode)) <$> ungroup dex)
                 Close ens      ->   (\exitnode -> (Hup,  ruleName2Num ! exitnode)) <$> ungroup ens
                 NoExit         -> []
             ))
    ) <$> Map.toList ruleName2Num

    
printGraph :: [Rule] -> IO ()
printGraph = prettyPrint . asGraph

