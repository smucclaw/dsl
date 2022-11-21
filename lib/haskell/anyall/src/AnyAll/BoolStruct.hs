{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module AnyAll.BoolStruct where
import qualified Data.Text as T
import AnyAll.Types
import Data.Aeson
import Data.Tree
import GHC.Generics
import Data.Aeson.Types
import Data.Maybe

data BoolStruct lbl a =
    Leaf                       a
  | All lbl [BoolStruct lbl a]
  | Any lbl [BoolStruct lbl a]
  | Not             (BoolStruct lbl a)
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

mkLeaf :: a -> BoolStruct lbl a
mkLeaf = Leaf

mkAll :: lbl -> [BoolStruct lbl a] -> BoolStruct lbl a
mkAll = All

mkAny :: lbl -> [BoolStruct lbl a] -> BoolStruct lbl a
mkAny = Any

mkNot :: BoolStruct lbl a -> BoolStruct lbl a
mkNot = Not

type OptionallyLabeledBoolStruct a = BoolStruct (Maybe (Label T.Text)) a
type BoolStructLT = BoolStruct (Label T.Text) T.Text

-- | negation normal form applies De Morgan's laws to push the Nots down to the Leaves.

nnf :: BoolStruct lbl a -> BoolStruct lbl a
nnf (Not (Not p)) = nnf p
nnf (Not (All l ps)) = Any l $ (nnf . Not) <$> ps
nnf (Not (Any l ps)) = All l $ (nnf . Not) <$> ps
nnf (All l ps) = All l (nnf <$> ps)
nnf (Any l ps) = Any l (nnf <$> ps)
nnf x = x

-- | sometimes we're only interested in the leaves of a Boolstruct.

extractLeaves :: BoolStruct lbl a -> [a]
extractLeaves (Leaf x) = [x]
extractLeaves (Not x)  = extractLeaves x
extractLeaves (All _ xs) = concatMap extractLeaves xs
extractLeaves (Any _ xs) = concatMap extractLeaves xs

-- | more or less the inverse of `alwaysLabeled` below.

addJust ::  BoolStruct lbl a -> BoolStruct (Maybe lbl) a
addJust (Any lbl xs) = Any (Just lbl) (addJust <$> xs)
addJust (All lbl xs) = All (Just lbl) (addJust <$> xs)
addJust (Leaf x)     = Leaf x
addJust (Not x)      = Not (addJust x)

-- | used in the conversion to Purescript output.
-- the Purescript types require a `Label T.Text`, so we convert a `Label (Maybe T.Text)` accordingly.

alwaysLabeled :: OptionallyLabeledBoolStruct a -> BoolStruct (Label T.Text) a
alwaysLabeled (Any Nothing    xs) = Any (Pre "any of:") (alwaysLabeled <$> xs)
alwaysLabeled (All Nothing    xs) = All (Pre "all of:") (alwaysLabeled <$> xs)
alwaysLabeled (Any (Just lbl) xs) = Any lbl (alwaysLabeled <$> xs)
alwaysLabeled (All (Just lbl) xs) = All lbl (alwaysLabeled <$> xs)
alwaysLabeled (Leaf x)            = Leaf x
alwaysLabeled (Not x)             = Not (alwaysLabeled x)

instance Monoid lbl => Semigroup (BoolStruct lbl a) where
  (All x xs)  <> (All y ys) = All (x<>y) (xs <> ys)
  l           <> (All y ys) = All y (l:ys)
  l@(All _ _) <> r          = r <> l
  l           <> r          = All mempty [l, r]

-- | flatten redundantly nested structure
-- example:
-- input:
--        All [All [x1, x2], Any [y1, y2], Leaf z]
-- output:
--        All [x1, x2,       Any [y1, y2], Leaf z]
-- but only if the labels match
simplifyBoolStruct :: (Eq lbl, Monoid lbl) => BoolStruct lbl a -> BoolStruct lbl a
simplifyBoolStruct (Not (Not x)) = simplifyBoolStruct x
simplifyBoolStruct (All _ [x])   = simplifyBoolStruct x
simplifyBoolStruct (Any _ [x])   = simplifyBoolStruct x
simplifyBoolStruct (All l1 xs)   = All l1 $ concatMap (\case { (All l2 cs) | l1 == l2 -> cs; x -> [x] }) (siblingfyBoolStruct $ simplifyBoolStruct <$> xs)
simplifyBoolStruct (Any l1 xs)   = Any l1 $ concatMap (\case { (Any l2 cs) | l1 == l2 -> cs; x -> [x] }) (siblingfyBoolStruct $ simplifyBoolStruct <$> xs)
simplifyBoolStruct orig = orig

data MergeResult a = Merged a | Unmerged a a

attemptMergeHeads :: Eq lbl => BoolStruct lbl a -> BoolStruct lbl a -> MergeResult (BoolStruct lbl a)
attemptMergeHeads  x@(All xl xs)  y@(All yl ys)
  | xl == yl = Merged (All xl (xs ++ ys))
  | otherwise = Unmerged x y
attemptMergeHeads  x@(Any xl xs)  y@(Any yl ys)
  | xl == yl = Merged $ Any xl (xs ++ ys)
  | otherwise = Unmerged x y
attemptMergeHeads  x  y = Unmerged x y

mergeMatch :: (Eq lbl, Monoid lbl) => [BoolStruct lbl a] -> [BoolStruct lbl a]
mergeMatch []  = []
mergeMatch [k] = [k]
mergeMatch (bs1 : bs2 : zs) = case x of
  (Merged m) -> mergeMatch (m:zs)
  (Unmerged x y) -> x : mergeMatch (y:zs)
  where
    x = attemptMergeHeads bs1 bs2

-- | utility for simplifyBoolStruct: flatten sibling (Any|All) elements that have the same (Any|All) Label prefix into the same group
-- example:
-- input:
--        All [Any [x1, x2], Any [y1, y2], Leaf z]
-- output:
--        All [Any [x1, x2, y1, y2], Leaf z]
siblingfyBoolStruct :: (Eq lbl, Monoid lbl) => [BoolStruct lbl a] -> [BoolStruct lbl a]
siblingfyBoolStruct = mergeMatch

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
