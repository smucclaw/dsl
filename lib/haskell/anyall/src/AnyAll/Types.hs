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
import GHC.Exts (toList)

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

data BoolStruct lbl a =
    Leaf                       a
  | All lbl [BoolStruct lbl a]
  | Any lbl [BoolStruct lbl a]
  | Not             (BoolStruct lbl a)
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

data Formula a =
    FAtom a
  | FAll
  | FAny
  | FNot
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

type BoolStructDT lbl a = Tree (Maybe lbl, Formula a)

type OptionallyLabeledBoolStruct a = BoolStruct (Maybe (Label T.Text)) a
type BoolStructLT = BoolStruct (Label T.Text) T.Text

nnf :: BoolStruct lbl a -> BoolStruct lbl a
nnf (Not (Not p)) = nnf p
nnf (Not (All l ps)) = Any l $ (nnf . Not) <$> ps
nnf (Not (Any l ps)) = All l $ (nnf . Not) <$> ps
nnf (All l ps) = All l (nnf <$> ps)
nnf (Any l ps) = Any l (nnf <$> ps)
nnf x = x

notDt :: BoolStructDT lbl a -> BoolStructDT lbl a
notDt at = Node (Nothing, FNot) [at]

nnfDT :: BoolStructDT lbl a -> BoolStructDT lbl a
nnfDT (Node (_,FNot) [Node (_, FNot) [st]] ) = nnfDT st
nnfDT (Node (_,FNot) [Node (l, FAll) fs] ) = Node (l, FAny) (nnfDT . notDt <$> fs)
nnfDT (Node (_,FNot) [Node (l, FAny) fs] ) = Node (l, FAll) (nnfDT . notDt <$> fs)
nnfDT (Node (l, FAll) fs) = Node (l, FAll) (nnfDT <$> fs)
nnfDT (Node (l, FAny) fs) = Node (l, FAny) (nnfDT <$> fs)
nnfDT x = x

extractLeaves :: BoolStruct lbl a -> [a]
extractLeaves (Leaf x) = [x]
extractLeaves (Not x)  = extractLeaves x
extractLeaves (All _ xs) = concatMap extractLeaves xs
extractLeaves (Any _ xs) = concatMap extractLeaves xs

addJust :: BoolStructLT -> OptionallyLabeledBoolStruct T.Text
addJust (Any lbl xs) = Any (Just lbl) (addJust <$> xs)
addJust (All lbl xs) = All (Just lbl) (addJust <$> xs)
addJust (Leaf x)     = Leaf x
addJust (Not x)      = Not (addJust x)

alwaysLabeled :: OptionallyLabeledBoolStruct T.Text -> BoolStructLT
alwaysLabeled (Any Nothing    xs) = Any (Pre "any of:") (alwaysLabeled <$> xs)
alwaysLabeled (All Nothing    xs) = All (Pre "all of:") (alwaysLabeled <$> xs)
alwaysLabeled (Any (Just lbl) xs) = Any lbl (alwaysLabeled <$> xs)
alwaysLabeled (All (Just lbl) xs) = All lbl (alwaysLabeled <$> xs)
alwaysLabeled (Leaf x)            = Leaf x
alwaysLabeled (Not x)             = Not (alwaysLabeled x)

aaFilter :: (BoolStruct lbl a -> Bool) -> BoolStruct lbl a -> BoolStruct lbl a
aaFilter f (Any lbl xs) = Any lbl (filter f (aaFilter f <$> xs))
aaFilter f (All lbl xs) = All lbl (filter f (aaFilter f <$> xs))
aaFilter f x = if f x then x else x -- not super great, should really replace the else with True or False or something?

instance Semigroup t => Semigroup (Label t) where
  (<>)  (Pre pr1) (Pre pr2) = Pre (pr1 <> pr2) -- this is semantically incorrect, can we improve it?
  (<>)  (Pre pr1) (PrePost pr2 po2) = PrePost (pr1 <> pr2) po2
  (<>)  (PrePost pr1 po1) (Pre pr2) = PrePost (pr1 <> pr2) po1
  (<>)  (PrePost pr1 po1) (PrePost pr2 po2) = PrePost (pr1 <> pr2) (po1 <> po2)


instance Monoid lbl => Semigroup (BoolStruct lbl a) where
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

-- | flatten redundantly nested structure
-- example:
-- input:
--        All [All [x1, x2], Any [y1, y2], Leaf z]
-- output:
--        All [x1, x2,       Any [y1, y2], Leaf z]
-- but only if the labels match

simplifyItem :: (Eq lbl, Monoid lbl) => BoolStruct lbl a -> BoolStruct lbl a
-- reverse not-nots
simplifyItem (Not (Not x)) = simplifyItem x
-- extract singletons
simplifyItem (All _ [x]) = simplifyItem x
simplifyItem (Any _ [x]) = simplifyItem x
-- flatten parent-child and flatten siblings
simplifyItem (All l1 xs) = All l1 $ concatMap (\case { (All l2 cs) | l1 == l2 -> cs; x -> [x] }) (siblingfyItem $ simplifyItem <$> xs)
simplifyItem (Any l1 xs) = Any l1 $ concatMap (\case { (Any l2 cs) | l1 == l2 -> cs; x -> [x] }) (siblingfyItem $ simplifyItem <$> xs)
simplifyItem orig = orig

-- | utility for simplifyItem: flatten sibling (Any|All) elements that have the same (Any|All) Label prefix into the same group
-- example:
-- input:
--        All [Any [x1, x2], Any [y1, y2], Leaf z]
-- output:
--        All [Any [x1, x2, y1, y2], Leaf z]
siblingfyItem :: (Eq lbl, Monoid lbl) => [BoolStruct lbl a] -> [BoolStruct lbl a]
siblingfyItem xs =
  let grouped =
        mergeMatch
        [ ((anyall,lbl), ys)
        | x <- xs
        , let ((anyall,lbl),ys) = case x of
                                    (Any lbl ys) -> (("any", lbl),     ys)
                                    (All lbl ys) -> (("all", lbl),     ys)
                                    (Leaf    y ) -> (("leaf",mempty), [Leaf y])
                                    (Not     y ) -> (("not", mempty), [Not  y])
        ]
      after = concat $ flip fmap grouped $ \case
        (("any",lbl),ys) -> [Any lbl ys]
        (("all",lbl),ys) -> [All lbl ys]
        ((_,_)      ,ys) -> ys
  in -- (trace $ TL.unpack $ strPrefix "siblingfyItem: before: " (pShowNoColor xs)) $
     -- (trace $ TL.unpack $ strPrefix "siblingfyItem: during: " (pShowNoColor grouped)) $
     -- (trace $ TL.unpack $ strPrefix "siblingfyItem: after:  " (pShowNoColor after)) $
     after
  where
    -- combine sequential ("foo", [x,y]) , ("foo", [z]) into ("foo", [x,y,z]) but only if the "foo" parts match
    mergeMatch :: (Eq a, Semigroup b) => [(a,b)] -> [(a,b)]
    mergeMatch []  = []
    mergeMatch [k] = [k]
    mergeMatch ((x1,y1) : (x2,y2) : zs)
      | x1 == x2  =           mergeMatch ((x1, y1 <> y2) : zs)
      | otherwise = (x1,y1) : mergeMatch ((x2,       y2) : zs)

strPrefix p txt = TL.unlines $ (p <>) <$> TL.lines txt

-- | The andOrTree is defined in L4; we think of it as an "immutable" given.
--   The marking comes from user input, and it "changes" at runtime,
--   which is to say that whenever we get new input from the user we regenerate everything.
--   This is eerily consistent with modern web dev React architecture. Coincidence?
data StdinSchema a = StdinSchema { marking   :: Marking a
                                 , andOrTree :: OptionallyLabeledBoolStruct T.Text }
  deriving (Eq, Ord, Show, Generic)
instance (ToJSON a, ToJSONKey a) => ToJSON (StdinSchema a)
instance FromJSON (StdinSchema T.Text) where
  parseJSON = withObject "StdinSchema" $ \o -> do
    markingO <- o .: "marking"
    aotreeO  <- o .: "andOrTree"
    let marking = parseMaybe parseJSON markingO
        aotree  = parseMaybe parseJSON aotreeO
    return $ StdinSchema
      (fromJust marking :: Marking T.Text)
      (fromMaybe (Leaf "ERROR: unable to parse andOrTree from input") aotree)


instance   (ToJSON lbl, ToJSON a) =>  ToJSON (BoolStruct lbl a)
-- TODO: Superfluous because covered by the above?
-- instance   ToJSON ItemJSON

instance   (FromJSON lbl, FromJSON a) =>  FromJSON (BoolStruct lbl a)

data AndOr a = And | Or | Simply a | Neg deriving (Eq, Ord, Show, Generic)
instance ToJSON a => ToJSON (AndOr a); instance FromJSON a => FromJSON (AndOr a)

type AsTree a = Tree (AndOr a, Maybe (Label T.Text))
native2tree :: OptionallyLabeledBoolStruct a -> AsTree a
native2tree (Leaf a) = Node (Simply a, Nothing) []
native2tree (Not a)  = Node (Neg, Nothing) (native2tree <$> [a])
native2tree (All l items) = Node (And, l) (native2tree <$> items)
native2tree (Any l items) = Node ( Or, l) (native2tree <$> items)

tree2native :: AsTree a -> OptionallyLabeledBoolStruct a
tree2native (Node (Simply a, _) children) = Leaf a
tree2native (Node (Neg, _) children) = Not (tree2native $ head children) -- will this break? maybe we need list nonempty
tree2native (Node (And, lbl) children) = All lbl (tree2native <$> children)
tree2native (Node ( Or, lbl) children) = Any lbl (tree2native <$> children)

newtype Default a = Default { getDefault :: Either (Maybe a) (Maybe a) }
  deriving (Eq, Ord, Show, Generic)
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

parseMarkingKV :: ( T.Text, Value) -> Maybe (T.Text, Default Bool)
parseMarkingKV (k,v) =
  case parseMaybe parseJSON v :: Maybe (Default Bool) of
    Just ma -> Just (k, ma)
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
