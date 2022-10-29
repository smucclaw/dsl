module AnyAll.BoolStructTree where
import Data.Tree

data Formula a =
    FAtom a
  | FAll
  | FAny
  | FNot
  deriving (Eq, Show)

type BoolStructDT lbl a = Tree (Maybe lbl, Formula a)

notDt :: BoolStructDT lbl a -> BoolStructDT lbl a
notDt at = Node (Nothing, FNot) [at]

nnfDT :: BoolStructDT lbl a -> BoolStructDT lbl a
nnfDT (Node (_,FNot) [Node (_, FNot) [st]] ) = nnfDT st
nnfDT (Node (_,FNot) [Node (l, FAll) fs] ) = Node (l, FAny) (nnfDT . notDt <$> fs)
nnfDT (Node (_,FNot) [Node (l, FAny) fs] ) = Node (l, FAll) (nnfDT . notDt <$> fs)
nnfDT (Node (l, FAll) fs) = Node (l, FAll) (nnfDT <$> fs)
nnfDT (Node (l, FAny) fs) = Node (l, FAny) (nnfDT <$> fs)
nnfDT x = x