#!/bin/bash
gf -make -f haskell --haskell=gadt --haskell=lexical --lexical=N,V,A,N2,N3,V2,A2,VA,V2V,VV,V3,VS,V2A,V2S,V2Q,Adv,AdV,AdA,AdN,ACard,CAdv,Conj,Interj,PN,Prep,Pron,Quant,Det,Card,Text,Predet,Subj UDExtEng.gf UDExtMay.gf
sed 's/module UDExt where/{-# OPTIONS_GHC -Wno-all #-}\nmodule LS.NLP.UDExt where/ ; s/instance Show .*//' UDExt.hs > /tmp/whatever.hs
mv /tmp/whatever.hs ../src/LS/NLP/UDExt.hs
rm UDExt.hs
gf -make UDAppEng.gf
