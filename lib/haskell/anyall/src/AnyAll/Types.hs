{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module AnyAll.Types
  ( AndOr (And, Neg, Or, Simply),
    Default (..),
    Hardness (..),
    Label (..),
    Marking (..),
    Q (..),
    QTree,
    ShouldView (..),
    TextMarking (..),
    asJSON,
    asJSONDefault,
    ask2hide,
    ask2view,
    fromJSON,
    getDefault,
    getForUI,
    labelFirst,
    maybeSecond,
    mkDefault,
  )
where

import Data.Aeson
  ( FromJSON (parseJSON),
    Key,
    ToJSON (toEncoding),
    ToJSONKey,
    Value,
    decode,
    defaultOptions,
    encode,
    genericToEncoding,
    withObject,
  )
import Data.Aeson.Key (toText)
import Data.Aeson.KeyMap (Key, toList)
import Data.Aeson.Types (Parser, parse, parseMaybe)
import Data.ByteString.Lazy qualified as B
import Data.Coerce (coerce)
import Data.HashMap.Strict qualified as Map
import Data.Hashable (Hashable)
import Data.Maybe (mapMaybe)
import Data.String (IsString)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Tree (Tree, flatten)
import Data.Vector qualified as V
import Debug.Trace (trace, traceM)
import GHC.Generics (Generic)
import Text.Pretty.Simple (pShowNoColor)

data Label a =
    Pre a
  | PrePost a a
  deriving (Eq, Show, Generic, Ord)
instance ToJSON a => ToJSON (Label a)
instance FromJSON a => FromJSON (Label a)
instance Hashable a => Hashable (Label a)

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

instance Hashable Hardness

type AnswerToExplain = Bool

data BinExpr a b =
    BELeaf a
  | BEAll b [BinExpr a b]
  | BEAny b [BinExpr a b]
  | BENot (BinExpr a b)
  deriving (Eq, Ord, Show, Generic, Hashable, Functor, Foldable, ToJSON, Traversable)

instance Semigroup t => Semigroup (Label t) where
  (<>)  (Pre pr1) (Pre pr2) = Pre (pr1 <> pr2) -- this is semantically incorrect, can we improve it?
  (<>)  (Pre pr1) (PrePost pr2 po2) = PrePost (pr1 <> pr2) po2
  (<>)  (PrePost pr1 po1) (Pre pr2) = PrePost (pr1 <> pr2) po1
  (<>)  (PrePost pr1 po1) (PrePost pr2 po2) = PrePost (pr1 <> pr2) (po1 <> po2)

strPrefix p txt = TL.unlines $ (p <>) <$> TL.lines txt


data AndOr a = And | Or | Simply a | Neg deriving (Eq, Ord, Show, Generic)
instance ToJSON a => ToJSON (AndOr a); instance FromJSON a => FromJSON (AndOr a)
instance Hashable a => Hashable (AndOr a)

-- | Left: no user input; default value from system.
--
--   Right: user input. received value from user.

-- using data instead of newtype because it makes it easier to prettyprint to Purescript via Show
newtype Default a = Default (Either (Maybe a) (Maybe a))
  deriving (Eq, Ord, Show, Generic)

instance Hashable a => Hashable (Default a)

mkDefault :: Either (Maybe a) (Maybe a) -> Default a
mkDefault = coerce
{-# INLINE mkDefault #-}

getDefault :: Default a -> Either (Maybe a) (Maybe a)
getDefault = coerce
{-# INLINE getDefault #-}

instance (ToJSON a, ToJSONKey a) => ToJSON (Default a)
instance (FromJSON a) => FromJSON (Default a)
asJSONDefault :: (ToJSON a, ToJSONKey a) => Default a -> B.ByteString
asJSONDefault = encode

newtype Marking a = Marking { getMarking :: Map.HashMap a (Default Bool) }
  deriving (Eq, Ord, Show, Generic)

instance Hashable a => Hashable (Marking a)

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
parseMarking = withObject "marking" \o -> do
  pure $ coerce $ Map.fromList $ mapMaybe parseMarkingKV (toList o)

data ShouldView = View | Hide | Ask deriving (Eq, Ord, Show, Generic)
instance ToJSON ShouldView; instance FromJSON ShouldView
instance Hashable ShouldView

data Q a = Q { shouldView :: ShouldView
             , andOr      :: AndOr a
             , prePost    :: Maybe (Label a)
             , mark       :: Default Bool
             } deriving (Eq, Ord, Show, Generic)

instance Hashable a => Hashable (Q a)

ask2hide :: Q a -> Q a
ask2hide q@Q{shouldView=Ask} = q{shouldView=Hide}
ask2hide x = x

ask2view :: Q a -> Q a
ask2view q@Q{shouldView=Ask} = q{shouldView=View}
ask2view x = x

-- add identifier to Q
-- populated identifier with whatever generates the Q

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

getAsks :: (ToJSONKey a, Hashable a) => QTree a -> Map.HashMap a (Default Bool)
getAsks qt = Map.fromList $ mapMaybe (getSV Ask) (flatten qt)

getAsksJSON :: (ToJSONKey a, Hashable a) => QTree a -> B.ByteString
getAsksJSON = encode . getAsks

getViews :: (ToJSONKey a, Hashable a) => QTree a -> Map.HashMap a (Default Bool)
getViews qt = Map.fromList $ mapMaybe (getSV View) (flatten qt)

getViewsJSON :: (ToJSONKey a, Hashable a) => QTree a -> B.ByteString
getViewsJSON = encode . getViews

getForUI :: (ToJSONKey a, Hashable a) => QTree a -> B.ByteString
getForUI qt = encode (Map.fromList [("view" :: T.Text, getViews qt)
                                   ,("ask" :: T.Text, getAsks qt)])
