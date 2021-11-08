{-# LANGUAGE DeriveGeneric #-}
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

data Hardness = Soft -- use Left defaults
              | Hard -- require Right input
  deriving (Eq, Show, Generic)

type AnswerToExplain = Bool

data Item a =
    Leaf a
  | All [Item a]
  | Any [Item a]
  | Not (Item a)
  deriving (Eq, Show, Generic)

-- in which we use <> to mean &&
instance (Semigroup a) => Semigroup (Item a) where
  (<>)   (All xs)   (All ys) = All (xs ++ ys)

  (<>) l@(Not  x)   r@(All ys) = All (l:ys)
  (<>) l@(All xs) r@(Not y)    = r <> l

  (<>) l@(Leaf x)   r@(All ys) = All (l:ys)
  (<>) l@(All xs) r@(Leaf y)   = r <> l

  (<>) l@(Leaf x)   r@(Any ys) = All [l,r]
  (<>) l@(Any xs) r@(Leaf y)   = r <> l

  (<>) l@(Any xs)   (All ys) = All (l:ys) -- this is slightly wrong if the Pre text is "both"
  (<>) l@(All xs) r@(Any ys) = r <> l

  -- all the other cases get ANDed together in the most straightforward way.
  (<>) l            r            = All [l, r]

instance (Monoid a) => Monoid (Item a) where
  mempty = Leaf mempty
  
data StdinSchema a = StdinSchema { marking :: Marking a
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
instance (Data.String.IsString a, FromJSON a) => FromJSON (Item a) where
  parseJSON = withObject "andOrTree" $ \o -> do
    leaf      <- o .:? "leaf"
    nodetype  <- o .:? "nodetype"
    childrenA <- o .:? "children"
    let children = maybe [] (mapMaybe (parseMaybe parseJSON) . V.toList) childrenA
    return $ if isJust leaf
             then Leaf (fromJust leaf)
             else case (nodetype :: Maybe String) of
                    Just "any" -> Any children
                    Just "all" -> All children
                    Just "not" -> Not $ head children
                    _          -> error "error in parsing JSON input"

data AndOr a = And | Or | Simply a | Neg deriving (Eq, Show, Generic)
instance ToJSON a => ToJSON (AndOr a); instance FromJSON a => FromJSON (AndOr a)

type AsTree a = Tree (AndOr a)
native2tree :: Item a -> AsTree a
native2tree (Leaf a) = Node (Simply a) []
native2tree (Not a)  = Node Neg (native2tree <$> [a])
native2tree (All items) = Node And (native2tree <$> items)
native2tree (Any items) = Node  Or (native2tree <$> items)

tree2native :: AsTree a -> Item a
tree2native (Node (Simply a) children) = Leaf a
tree2native (Node Neg children) = Not (tree2native $ head children) -- will this break? maybe we need list nonempty
tree2native (Node And children) = All (tree2native <$> children)
tree2native (Node  Or children) = Any (tree2native <$> children)

newtype Default a = Default { getDefault :: Either (Maybe a) (Maybe a) }
  deriving (Eq, Show, Generic)
instance (ToJSON a, ToJSONKey a) => ToJSON (Default a)
instance (FromJSON a) => FromJSON (Default a)
-- instance (FromJSON a) => FromJSON (Default a) where
--   parseJSON = withObject "withDefaultBool" $ \o -> do
--     byDefault <- o .:? "byDefault"
--     fromUser  <- o .:? "fromUser"
--     if isJust fromUser
--       then return $ Default (Right fromUser)
--       else if isJust byDefault
--            then return $ Default (Left byDefault)
--            else return $ Default (Left Nothing)
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
getSV sv1 (Q sv2 (Simply x) m)
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
