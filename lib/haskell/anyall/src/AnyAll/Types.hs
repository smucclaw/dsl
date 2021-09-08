module AnyAll.Types where

import Data.Tree
import qualified Data.Map.Strict as Map

data Label a =
    Pre a
  | PrePost a a
  deriving Show

data Item a =
    Leaf a
  | All (Label a) [Item a]
  | Any (Label a) [Item a]
  deriving Show

type AsTree a = Tree (Item a)

-- tree2native :: AsTree a -> Item a
-- native2tree :: Item a -> AsTree a


type Default a = Either (Maybe a) (Maybe a)

type Marking a = Map.Map a (Default Bool)
data ShouldView = View | Hide | Ask deriving (Eq, Show)
type ShouldAsk a = Map.Map a ShouldView

