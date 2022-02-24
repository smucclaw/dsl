#!/bin/bash
gf -make -f haskell --haskell=gadt --haskell=lexical --lexical=N,V,A,N2,N3,V2,A2,VA,V2V,VV,V3,VS,V2A,V2S,V2Q,Adv,AdV,AdA,AdN,ACard,CAdv,Conj,Interj,PN,Prep,Pron,Quant,Det,Card,Text,Predet,Subj UDExtEng.gf
sed -i "" 's/module UDExt where/{-# OPTIONS_GHC -Wno-all #-}\nmodule LS.NLP.UDExt where/ ; s/instance Show .*//' UDExt.hs
# 
mv UDExt.hs ../src/LS/NLP/