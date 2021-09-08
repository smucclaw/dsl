module AnyAll.Types where

import Data.Tree
import Data.Maybe
import qualified Data.Map.Strict as Map

data Label a =
    Pre a
  | PrePost a a
  deriving (Eq, Show)

data Item a =
    Leaf a
  | All (Label a) [Item a]
  | Any (Label a) [Item a]
  deriving (Eq, Show)

data AndOr a = And | Or | Simply a deriving (Eq, Show)

type AsTree a = Tree (AndOr a, Maybe (Label a))
native2tree :: Item a -> AsTree a
native2tree (Leaf a) = Node (Simply a, Nothing) []
native2tree (All l items) = Node (And, Just l) (native2tree <$> items)
native2tree (Any l items) = Node ( Or, Just l) (native2tree <$> items)

tree2native :: AsTree a -> Item a
tree2native (Node (Simply a, _) children) = Leaf a
tree2native (Node (And, lbl) children) = All (fromJust lbl) (tree2native <$> children)
tree2native (Node ( Or, lbl) children) = Any (fromJust lbl) (tree2native <$> children)

type Default a = Either (Maybe a) (Maybe a)

type Marking a = Map.Map a (Default Bool)
data ShouldView = View | Hide | Ask deriving (Eq, Show)

type ShouldAsk a = Map.Map a ShouldView
-- or should this be Item ShouldView, lol

