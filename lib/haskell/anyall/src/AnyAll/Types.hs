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
import qualified Data.Text            as T
import qualified Data.Vector          as V

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import GHC.Generics
import GHC.Exts (toList)

data Label a =
    Pre a
  | PrePost a a
  deriving (Eq, Show, Generic)
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
  deriving (Eq, Show, Generic)

type AnswerToExplain = Bool

data BinExpr a b =
    BELeaf a
  | BEAll b [BinExpr a b]
  | BEAny b [BinExpr a b]
  | BENot (BinExpr a b)
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

instance (ToJSON a, ToJSON b) => ToJSON (BinExpr a b)

data Item lbl a =
    Leaf                       a
  | All lbl [Item lbl a]
  | Any lbl [Item lbl a]
  | Not             (Item lbl a)
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

type ItemMaybeLabel a = Item (Maybe (Label T.Text)) a

type ItemJSONText = Item (Label T.Text) T.Text


instance Semigroup t => Semigroup (Label t) where 
  (<>)  (Pre pr1) (Pre pr2) = Pre (pr1 <> pr2)
  (<>)  (Pre pr1) (PrePost pr2 po2) = PrePost (pr1 <> pr2) po2
  (<>)  (PrePost pr1 po1) (Pre pr2) = PrePost (pr1 <> pr2) po1
  (<>)  (PrePost pr1 po1) (PrePost pr2 po2) = PrePost (pr1 <> pr2) (po1 <> po2)


instance Monoid lbl => Semigroup (Item lbl a) where
  (<>)   (All x xs)   (All y ys) = All x (xs <> ys)

  (<>) l@(Not  x)   r@(All y ys) = All y (l:ys)
  (<>) l@(All x xs) r@(Not y)    = r <> l

  (<>) l@(Leaf x)   r@(All y ys) = All y (l:ys)
  (<>) l@(All x xs) r@(Leaf y)   = r <> l

  (<>) l@(Leaf x)   r@(Any y ys) = All mempty [l,r]
  (<>) l@(Any x xs) r@(Leaf y)   = r <> l

  (<>) l@(Any x xs)   (All y ys) = All y (l:ys)
  (<>) l@(All x xs) r@(Any y ys) = r <> l

  -- all the other cases get ANDed together in the most straightforward way.
  (<>) l            r            = All mempty [l, r]


-- | prepend something to the Pre/Post label, shallowly
-- shallowPrependBSR :: (IsString a, Semigroup a) => a -> Item a -> Item a
-- x `shallowPrependBSR` Leaf z    = Leaf (x <> " " <> z)
-- x `shallowPrependBSR` All ml zs = All (prependToLabel x ml) zs
-- x `shallowPrependBSR` Any ml zs = Any (prependToLabel x ml) zs
-- x `shallowPrependBSR` Not z     = Not (x `shallowPrependBSR` z)

-- | prepend something to the Pre/Post label, deeply
-- deepPrependBSR :: (IsString a, Semigroup a) => a -> Item (Label a) a -> Item (Label a) a
-- x `deepPrependBSR` Leaf z    = x `shallowPrependBSR` Leaf z
-- x `deepPrependBSR` All ml zs = All (prependToLabel x ml) (deepPrependBSR x <$> zs)
-- x `deepPrependBSR` Any ml zs = Any (prependToLabel x ml) (deepPrependBSR x <$> zs)
-- x `deepPrependBSR` Not z     = Not (x `deepPrependBSR` z)

-- | utility function to assist with shallowPrependBSR
-- prependToLabel :: (IsString a, Semigroup a) => a -> Maybe (Label a) -> Maybe (Label a)
-- prependToLabel x Nothing              = Just $ Pre      x
-- prependToLabel x (Just (Pre     y  )) = Just $ Pre     (x <> " " <> y)
-- prependToLabel x (Just (PrePost y z)) = Just $ PrePost (x <> " " <> y) z


-- | The andOrTree is defined in L4; we think of it as an "immutable" given.
--   The marking comes from user input, and it "changes" at runtime,
--   which is to say that whenever we get new input from the user we regenerate everything.
--   This is eerily consistent with modern web dev React architecture. Coincidence?
data StdinSchema a = StdinSchema { marking   :: Marking a
                                 , andOrTree :: ItemMaybeLabel a }
  deriving (Eq, Show, Generic)
instance (ToJSON a, ToJSONKey a) => ToJSON (StdinSchema a)
instance FromJSON (StdinSchema T.Text) where
  parseJSON = withObject "StdinSchema" $ \o -> do
    markingO <- o .: "marking"
    aotreeO  <- o .: "andOrTree"
    let marking = parseMaybe parseJSON markingO
        aotree  = parseMaybe parseJSON aotreeO
    return $ StdinSchema (fromJust marking :: Marking T.Text) (fromJust aotree)


instance   (ToJSON lbl, ToJSON a) =>  ToJSON (Item lbl a)
-- TODO: Superfluous because covered by the above?
-- instance   ToJSON ItemJSON

instance   (FromJSON lbl, FromJSON a) =>  FromJSON (Item lbl a)

{-
-- Is all that really necessary?
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
-}


data AndOr a = And | Or | Simply a | Neg deriving (Eq, Show, Generic)
instance ToJSON a => ToJSON (AndOr a); instance FromJSON a => FromJSON (AndOr a)

type AsTree a = Tree (AndOr a, Maybe (Label T.Text))
native2tree :: ItemMaybeLabel a -> AsTree a
native2tree (Leaf a) = Node (Simply a, Nothing) []
native2tree (Not a)  = Node (Neg, Nothing) (native2tree <$> [a])
native2tree (All l items) = Node (And, l) (native2tree <$> items)
native2tree (Any l items) = Node ( Or, l) (native2tree <$> items)

tree2native :: AsTree a -> ItemMaybeLabel a
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
instance FromJSON (Marking T.Text) where
  -- the keys in the object correspond to leaf contents, so we have to process them "manually"
  parseJSON = parseMarking

parseMarking = withObject "marking" $ \o -> do
    let asList = toList o
    return $ Marking $ Map.fromList $ mapMaybe (\(k,v) ->
                                                  case parseMaybe parseJSON v :: Maybe (Default Bool) of
                                                    Just ma -> Just (k, ma)
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
getForUI qt = encode (Map.fromList [("view" :: T.Text, getViews qt)
                                   ,("ask" :: T.Text, getAsks qt)])


{-
-- Is that function used anywhere?
markingLabel :: Item T.Text -> T.Text
markingLabel (Not x)  = markingLabel x
markingLabel (Leaf x) = x
markingLabel (Any (Just (Pre     p1   )) _) = p1
markingLabel (All (Just (Pre     p1   )) _) = p1
markingLabel (Any (Just (PrePost p1 p2)) _) = p1
markingLabel (All (Just (PrePost p1 p2)) _) = p1
markingLabel (Any Nothing                _) = "any of" -- to do -- add a State autoincrement to distinguish
markingLabel (All Nothing                _) = "all of" -- to do -- add a State autoincrement to distinguish
-}

