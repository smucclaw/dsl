{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module AnyAll.BoolStructTree where
import Data.Tree
import AnyAll.Types
import qualified Data.Text as T

data Formula lbl a =
    FAtom a
  | FAll lbl
  | FAny lbl
  | FNot
  deriving (Eq, Show, Ord)

type BoolStructDT lbl a = Tree (Formula lbl a)

notDt :: BoolStructDT lbl a -> BoolStructDT lbl a
notDt at = Node FNot [at]

nnfDT :: BoolStructDT lbl a -> BoolStructDT lbl a
nnfDT (Node FNot [Node FNot [st]] ) = nnfDT st
nnfDT (Node FNot [Node (FAll l) fs] ) = Node (FAny l) (nnfDT . notDt <$> fs)
nnfDT (Node FNot [Node (FAny l) fs] ) = Node (FAll l) (nnfDT . notDt <$> fs)
nnfDT (Node (FAll l) fs) = Node (FAll l) (nnfDT <$> fs)
nnfDT (Node (FAny l) fs) = Node (FAny l) (nnfDT <$> fs)
nnfDT x = x

extractLeavesDT :: BoolStructDT lbl a -> [a]
extractLeavesDT (Node (FAtom x) _    ) = [x]
extractLeavesDT (Node FNot    x      ) = concatMap extractLeavesDT x
extractLeavesDT (Node (FAll l)    fs ) = concatMap extractLeavesDT fs
extractLeavesDT (Node (FAny l)    fs ) = concatMap extractLeavesDT fs

addJustDT ::  BoolStructDT lbl a -> BoolStructDT (Maybe lbl) a
addJustDT (Node (FAny l)    fs ) = Node (FAny (Just l)) (addJustDT <$> fs)
addJustDT (Node (FAll l)    fs ) = Node (FAll (Just l)) (addJustDT <$> fs)
addJustDT (Node (FAtom x)   _  ) = Node (FAtom x) []
addJustDT (Node FNot        fs ) = Node FNot (addJustDT <$> fs)

alwaysLabeledDT :: BoolStructDT (Maybe (Label T.Text)) a -> BoolStructDT (Label T.Text) a
alwaysLabeledDT (Node (FAny Nothing)    fs) = Node (FAny (Pre "any of:")) (alwaysLabeledDT <$> fs)
alwaysLabeledDT (Node (FAll Nothing)    fs) = Node (FAll (Pre "all of:")) (alwaysLabeledDT <$> fs)
alwaysLabeledDT (Node (FAny (Just lbl)) fs) = Node (FAny lbl) (alwaysLabeledDT <$> fs)
alwaysLabeledDT (Node (FAll (Just lbl)) fs) = Node (FAll lbl) (alwaysLabeledDT <$> fs)
alwaysLabeledDT (Node (FAtom x)         _ ) = Node (FAtom x) []
alwaysLabeledDT (Node FNot              fs) = Node FNot (alwaysLabeledDT <$> fs)

instance Monoid lbl => Semigroup (BoolStructDT lbl a) where
  (Node (FAll x) xs)  <> (Node (FAll y) ys) = Node (FAll (x<>y)) (xs <> ys)
  l                   <> (Node (FAll y) ys) = Node (FAll y) (l:ys)
  l@(Node (FAll _) _) <> r          = r <> l
  l                   <> r          = Node (FAll mempty) [l, r]

simplifyBoolStructDT :: (Eq lbl, Monoid lbl) => BoolStructDT lbl a -> BoolStructDT lbl a
simplifyBoolStructDT (Node FNot [Node FNot [xs]]) = simplifyBoolStructDT xs
simplifyBoolStructDT (Node (FAll _)  [xs])        = simplifyBoolStructDT xs
simplifyBoolStructDT (Node (FAny _)  [xs])        = simplifyBoolStructDT xs
simplifyBoolStructDT (Node (FAll l1) xs)          = Node (FAll l1) $ concatMap (\case { (Node (FAll l2) cs) | l1 == l2 -> cs; x -> [x] }) (siblingfyBoolStructDT $ simplifyBoolStructDT <$> xs)
simplifyBoolStructDT (Node (FAny l1) xs)          = Node (FAny l1) $ concatMap (\case { (Node (FAny l2) cs) | l1 == l2 -> cs; x -> [x] }) (siblingfyBoolStructDT $ simplifyBoolStructDT <$> xs)
simplifyBoolStructDT orig = orig

data MergeResult a = Merged a | Unmerged a a

attemptMergeHeads :: Eq lbl => BoolStructDT lbl a -> BoolStructDT lbl a -> MergeResult (BoolStructDT lbl a)
attemptMergeHeads  x@(Node (FAll xl) xs)  y@(Node (FAll yl) ys)
  | xl == yl = Merged $ Node (FAll xl) (xs++ys)
  | otherwise = Unmerged x y
attemptMergeHeads  x@(Node (FAny xl) xs)  y@(Node (FAny yl) ys)
  | xl == yl = Merged $ Node (FAny xl) (xs++ys)
  | otherwise = Unmerged x y
attemptMergeHeads  x  y = Unmerged x y

mergeMatch :: (Eq lbl, Monoid lbl) => [BoolStructDT lbl a] -> [BoolStructDT lbl a]
mergeMatch []  = []
mergeMatch [k] = [k]
mergeMatch (bs1 : bs2 : zs) = case x of
  (Merged m) -> mergeMatch (m:zs)
  (Unmerged x y) -> x : mergeMatch (y:zs)
  where
    x = attemptMergeHeads bs1 bs2

siblingfyBoolStructDT :: (Eq lbl, Monoid lbl) => [BoolStructDT lbl a] -> [BoolStructDT lbl a]
siblingfyBoolStructDT = mergeMatch