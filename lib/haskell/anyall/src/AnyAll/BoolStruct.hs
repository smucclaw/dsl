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
simplifyItem :: (Eq lbl, Monoid lbl) => BoolStruct lbl a -> BoolStruct lbl a
simplifyItem (Not (Not x)) = simplifyItem x
simplifyItem (All _ [x])   = simplifyItem x
simplifyItem (Any _ [x])   = simplifyItem x
simplifyItem (All l1 xs)   = All l1 $ concatMap (\case { (All l2 cs) | l1 == l2 -> cs; x -> [x] }) (siblingfyItem $ simplifyItem <$> xs)
simplifyItem (Any l1 xs)   = Any l1 $ concatMap (\case { (Any l2 cs) | l1 == l2 -> cs; x -> [x] }) (siblingfyItem $ simplifyItem <$> xs)
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
