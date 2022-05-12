{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module AnyAll.Types where

import Debug.Trace (traceM)
import Data.Tree
import Data.Maybe
import Data.String (IsString)
import qualified Data.Map.Strict      as Map
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy       as TL
import qualified Data.Text.Internal   as DTI
import qualified Data.Vector          as V

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import GHC.Generics
import GHC.Exts (toList)

data Label a =
    Pre a
  | PrePost a a
  deriving (Eq, Show, Generic)
instance ToJSON a => ToJSON (Label a); instance FromJSON a => FromJSON (Label a)

allof, anyof :: Maybe (Label TL.Text)
allof = Just $ Pre "all of:"
anyof = Just $ Pre "any of:"

data Hardness = Soft -- use Left defaults
              | Hard -- require Right input
  deriving (Eq, Show, Generic)

type AnswerToExplain = Bool

type Item a = Item' (Label TL.Text) a

data Item' lbl a =
    Leaf                       a
  | All {itemLbl :: (Maybe lbl), itemsAll :: [Item' lbl a]}
  | Any {itemLbl :: (Maybe lbl), itemsAny :: [Item' lbl a]}
  | Not             (Item' lbl a)
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

instance Semigroup (Item' a b) where
  (<>)   (All x xs)   (All y ys) = All x (xs <> ys)

  (<>) l@(Not  x)   r@(All y ys) = All y (l:ys)
  (<>) l@(All x xs) r@(Not y)    = r <> l

  (<>) l@(Leaf x)   r@(All y ys) = All y (l:ys)
  (<>) l@(All x xs) r@(Leaf y)   = r <> l

  (<>) l@(Leaf x)   r@(Any y ys) = All Nothing [l,r]
  (<>) l@(Any x xs) r@(Leaf y)   = r <> l

  (<>) l@(Any x xs)   (All y ys) = All y (l:ys)
  (<>) l@(All x xs) r@(Any y ys) = r <> l

  -- all the other cases get ANDed together in the most straightforward way.
  (<>) l            r            = All Nothing [l, r]


-- | prepend something to the Pre/Post label, shallowly
shallowPrependBSR :: (IsString a, Semigroup a) => a -> Item' (Label a) a -> Item' (Label a) a
x `shallowPrependBSR` Leaf z    = Leaf (x <> " " <> z)
x `shallowPrependBSR` All ml zs = All (prependToLabel x ml) zs
x `shallowPrependBSR` Any ml zs = Any (prependToLabel x ml) zs
x `shallowPrependBSR` Not z     = Not (x `shallowPrependBSR` z)

-- | prepend something to the Pre/Post label, deeply
deepPrependBSR :: (IsString a, Semigroup a) => a -> Item' (Label a) a -> Item' (Label a) a
x `deepPrependBSR` Leaf z    = x `shallowPrependBSR` Leaf z
x `deepPrependBSR` All ml zs = All (prependToLabel x ml) (deepPrependBSR x <$> zs)
x `deepPrependBSR` Any ml zs = Any (prependToLabel x ml) (deepPrependBSR x <$> zs)
x `deepPrependBSR` Not z     = Not (x `deepPrependBSR` z)

-- | utility function to assist with shallowPrependBSR
prependToLabel :: (IsString a, Semigroup a) => a -> Maybe (Label a) -> Maybe (Label a)
prependToLabel x Nothing              = Just $ Pre      x
prependToLabel x (Just (Pre     y  )) = Just $ Pre     (x <> " " <> y)
prependToLabel x (Just (PrePost y z)) = Just $ PrePost (x <> " " <> y) z


data StdinSchema a = StdinSchema { marking   :: Marking a
                                 , andOrTree :: Item a }
  deriving (Eq, Show, Generic)
instance (ToJSON a, ToJSONKey a) => ToJSON (StdinSchema a)
instance FromJSON (StdinSchema TL.Text) where
  parseJSON = withObject "StdinSchema" $ \o -> do
    markingO <- o .: "marking"
    aotreeO  <- o .: "andOrTree"
    let marking = parseMaybe parseJSON markingO
        aotree  = parseMaybe parseJSON aotreeO
    return $ StdinSchema (fromJust marking :: Marking TL.Text) (fromJust aotree)

instance   ToJSON a =>   ToJSON (Item a)
  -- where toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }
instance (Data.String.IsString a, FromJSON a) => FromJSON (Item a) where
  parseJSON = withObject "andOrTree" $ \o -> do
    leaf      <- o .:? "leaf"
    nodetype  <- o .:? "nodetype"
    pre       <- o .:? "pre"
    prepost   <- o .:? "prepost"
    childrenA <- o .:? "children"
    let label = if isJust prepost
                then PrePost (fromJust pre) (fromJust prepost)
                else Pre     (fromJust pre)
        children = maybe [] (mapMaybe (parseMaybe parseJSON) . V.toList) childrenA
    return $ if isJust leaf
             then Leaf (fromJust leaf)
             else case (nodetype :: Maybe String) of
                    Just "any" -> Any (Just label) children
                    Just "all" -> All (Just label) children
                    Just "not" -> Not $ head children
                    _          -> error "error in parsing JSON input"

data AndOr a = And | Or | Simply a | Neg deriving (Eq, Show, Generic)
instance ToJSON a => ToJSON (AndOr a); instance FromJSON a => FromJSON (AndOr a)

type AsTree a = Tree (AndOr a, Maybe (Label TL.Text))
native2tree :: Item a -> AsTree a
native2tree (Leaf a) = Node (Simply a, Nothing) []
native2tree (Not a)  = Node (Neg, Nothing) (native2tree <$> [a])
native2tree (All l items) = Node (And, l) (native2tree <$> items)
native2tree (Any l items) = Node ( Or, l) (native2tree <$> items)

tree2native :: AsTree a -> Item a
tree2native (Node (Simply a, _) children) = Leaf a
tree2native (Node (Neg, _) children) = Not (tree2native $ head children) -- will this break? maybe we need list nonempty
tree2native (Node (And, lbl) children) = All lbl (tree2native <$> children)
tree2native (Node ( Or, lbl) children) = Any lbl (tree2native <$> children)

newtype Default a = Default { getDefault :: Either (Maybe a) (Maybe a) }
  deriving (Eq, Show, Generic)
instance (ToJSON a, ToJSONKey a) => ToJSON (Default a)
instance (FromJSON a) => FromJSON (Default a)
asJSONDefault :: (ToJSON a, ToJSONKey a) => Default a -> B.ByteString
asJSONDefault = encode

newtype Marking a = Marking { getMarking :: Map.Map a (Default Bool) }
  deriving (Eq, Show, Generic)

instance (ToJSON a, ToJSONKey a) => ToJSON (Marking a)
instance FromJSON (Marking TL.Text) where
  -- the keys in the object correspond to leaf contents, so we have to process them "manually"
  parseJSON = parseMarking

parseMarking = withObject "marking" $ \o -> do
    let asList = toList o
    return $ Marking $ Map.fromList $ mapMaybe (\(k,v) ->
                                                  case parseMaybe parseJSON v :: Maybe (Default Bool) of
                                                    Just ma -> Just (TL.fromStrict k, ma)
                                                    Nothing -> Nothing) asList


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

fromJSON :: FromJSON a => B.ByteString -> Maybe (QTree a)
fromJSON = decode

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
