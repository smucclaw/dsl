#!/bin/bash

set -euo pipefail

gf -make -f haskell --haskell=gadt --haskell=lexical --lexical=CN,AP,VP,Month,V2,VS  NL4Eng.gf
cat NL4.hs |
    sed 's/module NL4 where/{-# OPTIONS_GHC -Wno-all #-}\nmodule LS.NLP.NL4 where/' | \
    sed 's/instance Show .*//' | \
    sed 's/-- below this line machine-generated/-- below this line machine-generated\ninstance (Gf (Tree a)) => Show (Tree a) where\n    show = showExpr [] . gf/' | \
    sed 's/LANGUAGE GADTs, FlexibleInstances, KindSignatures, RankNTypes, TypeSynonymInstances/LANGUAGE GADTs, FlexibleInstances, FlexibleContexts, UndecidableInstances, KindSignatures, RankNTypes/' > ../src/LS/NLP/NL4.hs

echo "moved NL4.hs into ../src/LS/NLP/"
# head -5 ../src/LS/NLP/NL4.hs
rm NL4.hs
