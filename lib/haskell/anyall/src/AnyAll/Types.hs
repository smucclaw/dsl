{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AnyAll.Types where

import Data.Tree
import Data.Maybe
import qualified Data.Map.Strict      as Map
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy       as TL

import Data.Aeson
import GHC.Generics

data Label a =
    Pre a
  | PrePost a a
  deriving (Eq, Show, Generic)
instance ToJSON a => ToJSON (Label a); instance FromJSON a => FromJSON (Label a)

data Hardness = Soft -- use Left defaults
              | Hard -- require Right input
  deriving (Eq, Show, Generic)

type AnswerToExplain = Bool

data Item a =
    Leaf a
  | All (Label a) [Item a]
  | Any (Label a) [Item a]
  deriving (Eq, Show, Generic)

data AndOr a = And | Or | Simply a deriving (Eq, Show, Generic)
instance ToJSON a => ToJSON (AndOr a); instance FromJSON a => FromJSON (AndOr a)

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
data ShouldView = View | Hide | Ask deriving (Eq, Show, Generic)
instance ToJSON ShouldView; instance FromJSON ShouldView

data Q a = Q { shouldView :: ShouldView
             , andOr      :: AndOr a
             , prePost    :: Maybe (Label a)
             , mark       :: Default Bool
             } deriving (Eq, Show, Generic)

type QTree a = Tree (Q a)

instance ToJSON a => ToJSON (Q a) where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON a => FromJSON (Q a)

asJSON :: ToJSON a => QTree a -> B.ByteString
asJSON = encode

data DisplayPref = DPTerse | DPNormal | DPVerbose
  deriving (Eq, Show, Generic)

getSV :: ShouldView -> Q a -> Maybe (a, Default Bool)
getSV sv1 (Q sv2 (Simply x) pp m)
  | sv1 == sv2 = Just (x, m)
  | otherwise  = Nothing
getSV _ _ = Nothing

getAsks :: (ToJSONKey a, Ord a) => QTree a -> Map.Map a (Default Bool)
getAsks qt = Map.fromList $ catMaybes $ getSV Ask <$> flatten qt

getAsksJSON :: (ToJSONKey a, Ord a) => QTree a -> B.ByteString
getAsksJSON = encode . getAsks

getViews :: (ToJSONKey a, Ord a) => QTree a -> Map.Map a (Default Bool)
getViews qt = Map.fromList $ catMaybes $ getSV View <$> flatten qt

getViewsJSON :: (ToJSONKey a, Ord a) => QTree a -> B.ByteString
getViewsJSON = encode . getViews

getForUI :: (ToJSONKey a, Ord a) => QTree a -> B.ByteString
getForUI qt = encode (Map.fromList [("view" :: TL.Text, getViews qt)
                                   ,("ask" :: TL.Text, getAsks qt)])
