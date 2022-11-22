{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module AnyAll.Types where

import Debug.Trace (traceM, trace)
import Data.Tree
import Data.Maybe
import Data.String (IsString)
import qualified Data.Map.Strict      as Map
import qualified Data.ByteString.Lazy as B
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import qualified Data.Vector          as V
import Text.Pretty.Simple (pShowNoColor)

import Data.Aeson
import Data.Aeson.Types (parseMaybe, parse, Parser)
import GHC.Generics
import Data.Aeson.KeyMap hiding (mapMaybe)
import Data.Aeson.Key (toText)

data Label a =
    Pre a
  | PrePost a a
  deriving (Eq, Show, Generic, Ord)
instance ToJSON a => ToJSON (Label a)
instance FromJSON a => FromJSON (Label a)

labelFirst :: Label p -> p
labelFirst (Pre x      ) = x
labelFirst (PrePost x _) = x

maybeFirst :: Label p -> Maybe p
maybeFirst = Just . labelFirst

maybeSecond :: Label p -> Maybe p
maybeSecond (PrePost _ x) = Just x
maybeSecond _ = Nothing

allof, anyof :: Maybe (Label T.Text)
allof = Just $ Pre "all of:"
anyof = Just $ Pre "any of:"

data Hardness = Soft -- use Left defaults
              | Hard -- require Right input
  deriving (Eq, Ord, Show, Generic)

type AnswerToExplain = Bool

data BinExpr a b =
    BELeaf a
  | BEAll b [BinExpr a b]
  | BEAny b [BinExpr a b]
  | BENot (BinExpr a b)
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

instance (ToJSON a, ToJSON b) => ToJSON (BinExpr a b)

instance Semigroup t => Semigroup (Label t) where
  (<>)  (Pre pr1) (Pre pr2) = Pre (pr1 <> pr2) -- this is semantically incorrect, can we improve it?
  (<>)  (Pre pr1) (PrePost pr2 po2) = PrePost (pr1 <> pr2) po2
  (<>)  (PrePost pr1 po1) (Pre pr2) = PrePost (pr1 <> pr2) po1
  (<>)  (PrePost pr1 po1) (PrePost pr2 po2) = PrePost (pr1 <> pr2) (po1 <> po2)

strPrefix p txt = TL.unlines $ (p <>) <$> TL.lines txt


data AndOr a = And | Or | Simply a | Neg deriving (Eq, Ord, Show, Generic)
instance ToJSON a => ToJSON (AndOr a); instance FromJSON a => FromJSON (AndOr a)

-- | Left: no user input; default value from system.
--
--   Right: user input. received value from user.

-- using data instead of newtype because it makes it easier to prettyprint to Purescript via Show
newtype Default a = Default (Either (Maybe a) (Maybe a))
  deriving (Eq, Ord, Show, Generic)

getDefault :: Default a -> Either (Maybe a) (Maybe a)
getDefault (Default x) = x

instance (ToJSON a, ToJSONKey a) => ToJSON (Default a)
instance (FromJSON a) => FromJSON (Default a)
asJSONDefault :: (ToJSON a, ToJSONKey a) => Default a -> B.ByteString
asJSONDefault = encode

newtype Marking a = Marking { getMarking :: Map.Map a (Default Bool) }
  deriving (Eq, Ord, Show, Generic)

type TextMarking = Marking T.Text

instance (ToJSON a, ToJSONKey a) => ToJSON (Marking a)
instance FromJSON (Marking T.Text) where
  -- the keys in the object correspond to leaf contents, so we have to process them "manually"
  parseJSON = parseMarking

parseMarkingKV :: ( Key, Value) -> Maybe (T.Text, Default Bool)
parseMarkingKV (k,v) =
  case parseMaybe parseJSON v :: Maybe (Default Bool) of
    Just ma -> Just (toText k, ma)
    Nothing -> Nothing

parseMarking :: Value -> Parser (Marking T.Text)
parseMarking = withObject "marking" $ \o -> do
    return $ Marking $ Map.fromList $ mapMaybe parseMarkingKV (toList o)

data ShouldView = View | Hide | Ask deriving (Eq, Ord, Show, Generic)
instance ToJSON ShouldView; instance FromJSON ShouldView

data Q a = Q { shouldView :: ShouldView
             , andOr      :: AndOr a
             , prePost    :: Maybe (Label a)
             , mark       :: Default Bool
             } deriving (Eq, Ord, Show, Generic)

ask2hide :: Q a -> Q a
ask2hide (Q Ask x y z) = Q Hide x y z
ask2hide x = x

ask2view :: Q a -> Q a
ask2view (Q Ask x y z) = Q View x y z
ask2view x = x

type QTree a = Tree (Q a)

instance ToJSON a => ToJSON (Q a) where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON a => FromJSON (Q a)

asJSON :: ToJSON a => QTree a -> B.ByteString
asJSON = encode

fromJSON :: FromJSON a => B.ByteString -> Maybe (QTree a)
fromJSON = decode

data DisplayPref = DPTerse | DPNormal | DPVerbose
  deriving (Eq, Ord, Show, Generic)

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
getForUI qt = encode (Map.fromList [("view" :: T.Text, getViews qt)
                                   ,("ask" :: T.Text, getAsks qt)])
