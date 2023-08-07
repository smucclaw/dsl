#!/bin/bash

set -euo pipefail

gf -make -f haskell --haskell=gadt --haskell=lexical --lexical=A,V,N,CN,PN,Prep,Conj,Subj,AP,VP,V2,VS,VV,V2V,Dig,Day,Month,YearComponent  NL4Eng.gf NL4May.gf NL4Chi.gf
cat NL4.hs |
    sed 's/module NL4 where/module LS.NLP.NL4 where/' | \
    sed 's/instance Show .*//' | \
    sed 's/-- below this line machine-generated/-- below this line machine-generated\ninstance (Gf (Tree a)) => Show (Tree a) where\n    show = showExpr [] . gf/' | \
    sed 's/LANGUAGE GADTs, FlexibleInstances, KindSignatures, RankNTypes, TypeSynonymInstances/LANGUAGE GADTs, UndecidableInstances/' | \
    sed 's/import Control.Monad.Identity/import Control.Monad.Identity (Identity ( Identity, runIdentity), MonadPlus (..), ap )/' | \
    sed 's/import Data.Monoid/import Data.Monoid ()/' | \
    sed 's/import PGF hiding (Tree)/import PGF (Expr, mkApp, mkCId, mkFloat, mkInt, mkStr, showCId, showExpr, unApp, unFloat, unInt, unStr )/'  > ../src/LS/NLP/NL4.hs

echo "moved NL4.hs into ../src/LS/NLP/"
# head -5 ../src/LS/NLP/NL4.hs
rm NL4.hs
