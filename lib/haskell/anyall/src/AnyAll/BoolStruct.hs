{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module AnyAll.BoolStruct
  ( BoolStruct (..),
    BoolStructLT,
    OptionallyLabeledBoolStruct,
    StdinSchema (..),
    addJust,
    alwaysLabeled,
    boolStructChildren,
    extractLeaves,
    mkLeaf,
    mkAll,
    mkAny,
    mkNot,
    nnf,
    siblingfyBoolStruct,
    simplifyBoolStruct,
  )
where

import AnyAll.Types (Label (Pre), Marking (..))
import Control.Arrow ((>>>))
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON,
    ToJSONKey,
    withObject,
    (.:),
  )
import Data.Aeson.Types (parseEither)
import Data.Hashable (Hashable)
import Data.List (sort, unfoldr)
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    Testable (property),
    oneof,
    sized,
    vectorOf,
  )
import Test.QuickCheck.Checkers (EqProp (..))

data BoolStruct lbl a =
    Leaf                       a
  | All lbl [BoolStruct lbl a] -- and
  | Any lbl [BoolStruct lbl a] --  or
  | Not             (BoolStruct lbl a)
  deriving (Eq, Ord, Show, Generic, Hashable, Functor, Foldable, Traversable)

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
nnf (Not (All l ps)) = Any l $ nnf . Not <$> ps
nnf (Not (Any l ps)) = All l $ nnf . Not <$> ps
nnf (All l ps) = All l $ nnf <$> ps
nnf (Any l ps) = Any l $ nnf <$> ps
nnf x = x

boolStructChildren :: BoolStruct lbl a -> [BoolStruct lbl a]
boolStructChildren (Leaf _) = []
boolStructChildren (Any _ c) = c
boolStructChildren (All _ c) = c
boolStructChildren (Not c) = [c]

extractLeaves :: BoolStruct lbl a -> [a]
extractLeaves (Leaf x) = [x]
extractLeaves (Not x)  = extractLeaves x
extractLeaves (All _ xs) = foldMap extractLeaves xs
extractLeaves (Any _ xs) = foldMap extractLeaves xs

-- | more or less the inverse of `alwaysLabeled` below.

addJust ::  BoolStruct lbl a -> BoolStruct (Maybe lbl) a
addJust (Any lbl xs) = Any (Just lbl) $ addJust <$> xs
addJust (All lbl xs) = All (Just lbl) $ addJust <$> xs
addJust (Leaf x)     = Leaf x
addJust (Not x)      = Not $ addJust x

-- | used in the conversion to Purescript output.
-- the Purescript types require a `Label T.Text`, so we convert a `Label (Maybe T.Text)` accordingly.

alwaysLabeled :: OptionallyLabeledBoolStruct a -> BoolStruct (Label T.Text) a
alwaysLabeled (Any Nothing    xs) = Any (Pre "any of:") $ alwaysLabeled <$> xs
alwaysLabeled (All Nothing    xs) = All (Pre "all of:") $ alwaysLabeled <$> xs
alwaysLabeled (Any (Just lbl) xs) = Any lbl $ alwaysLabeled <$> xs
alwaysLabeled (All (Just lbl) xs) = All lbl $ alwaysLabeled <$> xs
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
simplifyBoolStruct (All l1 xs)   = All l1 $ foldMap ((\case { (All l2 cs) | l1 == l2 -> cs; x -> [x] }) . simplifyBoolStruct) xs
simplifyBoolStruct (Any l1 xs)   = Any l1 $ foldMap ((\case { (Any l2 cs) | l1 == l2 -> cs; x -> [x] }) . simplifyBoolStruct) xs
simplifyBoolStruct orig = orig

data MergeResult a = Merged a | Unmerged a a

attemptMergeHeads ::
  (Eq lbl) =>
  BoolStruct lbl a ->
  BoolStruct lbl a ->
  MergeResult (BoolStruct lbl a)
attemptMergeHeads = curry \case
  (All xl xs, All ((== xl) -> True) ys) -> merge All xl xs ys
  (Any xl xs, Any ((== xl) -> True) ys) -> merge Any xl xs ys
  (x, y) -> Unmerged x y
  where
    merge anyAll xl xs ys = Merged $ anyAll xl $ xs <> ys

{-
  mergeMatch yields as output, a (finite) trace of the (deterministic) fixed
  point iteration of the following small-step operational semantics of a
  labelled transition system.
  This is essentially a simple process algebra with tau transitions but no
  composition combinators.
  Transfinite iteration terminates before ω, thereby guaranteeing the
  termination of mergeMatch, because configurations decrease in size with each
  transition.

  Configurations C are lists of bool structs, ie the type [BoolStruct lbl a]
  Actions A are bool structs, ie the type (BoolStruct lbl a)

  Judgment forms:
    tau transition: C =τ=> C'
    visible transition: C =A=> C'

  Transition rules:

  ----------------- [smallStep-singleton]
    [z] =z=> []

    attemptMergeHeads bs1 bs2 = Merged m
  ---------------------------------------- [smallStep-merged]
    bs1:bs2:zs =τ=> m:zs

    attemptMergeHeads bs1 bs2 = Unmerged x y
  ------------------------------------------- [smallStep-unmerged]
    bs1:bs2:zs =x=> y:zs
-}
mergeMatch :: (Eq lbl, Monoid lbl) => [BoolStruct lbl a] -> [BoolStruct lbl a]
mergeMatch =
  -- Iterate small step transitions to fixed point, obtaining a trace in the process.
  unfoldr smallStep
  -- Filter away tau transitions.
    >>> catMaybes
  where
    -- Stop iteration when configuration is empty.
    smallStep [] = Nothing
    -- smallStep-singleton
    smallStep [z] = Just (Just z, [])
    smallStep (bs1 : bs2 : zs) = case attemptMergeHeads bs1 bs2 of
      -- smallStep-merged
      Merged m -> Just (Nothing, m : zs)
      -- smallStep-unmerged
      Unmerged x y -> Just (Just x, y : zs)

-- mergeMatch [] = []
-- mergeMatch [x] = [x]
-- mergeMatch (bs1 : bs2 : zs) = case x of
--   Merged m -> mergeMatch (m:zs)
--   Unmerged x y -> x : mergeMatch (y:zs)
--   where
--     x = attemptMergeHeads bs1 bs2

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
  parseJSON = withObject "StdinSchema" \o -> do
    markingO <- o .: "marking"
    aotreeO  <- o .: "andOrTree"
    let marking = parseEither parseJSON markingO
        aotree  = parseEither parseJSON aotreeO

    pure $ StdinSchema
      (case marking of
         Left err -> trace ("AnyAll/BoolStruct ERROR parsing marking: " ++ err) $
                     Marking mempty
         Right m  -> Marking m)
      (case aotree of
         Left err -> Leaf ("ERROR: " <> T.pack err)
         Right m  -> m
      )

instance   (ToJSON lbl, ToJSON a) =>  ToJSON (BoolStruct lbl a)
-- TODO: Superfluous because covered by the above?
-- instance   ToJSON ItemJSON

instance   (FromJSON lbl, FromJSON a) =>  FromJSON (BoolStruct lbl a)

instance (Eq lbl, Eq a, Ord a, Ord lbl) => EqProp (BoolStruct lbl a) where
  (Leaf x) =-= (Leaf y) = property (x == y)
  (Not x) =-= (Not y) = property (x == y)
  (All xl xbs) =-= (All yl ybs) = property ((xl == yl) && (sort xbs == sort ybs))
  (Any xl xbs) =-= (Any yl ybs) = property ((xl == yl) && (sort xbs == sort ybs))
  _ =-= _ = property False

instance (Arbitrary a, Arbitrary lbl) => Arbitrary (BoolStruct lbl a) where
  arbitrary = boolStruct

boolStruct :: (Arbitrary a, Arbitrary lbl) => Gen (BoolStruct lbl a)
boolStruct = sized boolStruct'

boolStruct' :: (Arbitrary a, Arbitrary lbl) => Int -> Gen (BoolStruct lbl a)
boolStruct' 0 = Leaf <$> arbitrary
boolStruct' n =
  oneof [fmap Leaf arbitrary,
         liftA2 All arbitrary (vectorOf 2 subtree),
         liftA2 Any arbitrary (vectorOf 2 subtree),
         fmap Not subtree]
  where subtree = boolStruct' (n `div` 2)
