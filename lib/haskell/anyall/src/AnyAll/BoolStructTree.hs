{-# LANGUAGE OverloadedStrings #-}
module AnyAll.BoolStructTree where
import Data.Tree
import AnyAll.Types
import qualified Data.Text as T

data Formula lbl a =
    FAtom a
  | FAll lbl
  | FAny lbl
  | FNot
  deriving (Eq, Show)

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
