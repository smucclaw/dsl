{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE RankNTypes, GADTs #-}

module LS.NLP.NL4Transformations where

import LS.NLP.NL4

flipPolarity :: forall a . Tree a -> Tree a
flipPolarity (GMkVPS temp GPOS vp) = (GMkVPS temp GNEG vp)
flipPolarity (GMkVPS temp GNEG vp) = (GMkVPS temp GPOS vp)
flipPolarity x = composOp flipPolarity x

