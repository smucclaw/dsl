abstract CustomSyntax =
    Numeral
  , Grammar [
          N, N2, CN, UseN, NP, Det, Pron, DetCN, MassNP, UsePron
        , V,  VV, V2, VS, V2S, VP, V2A
        , A, A2, AP, AdjCN, PositA
        , Comp, Adv, VP, UseComp, CompNP, CompAP, CompAdv -- is a public agency
        , Prep, PrepNP, AdvVP
        , AdA, AdAdv
        , ListAdv, BaseAdv, ConsAdv, ConjAdv
        , ListAP, BaseAP, ConsAP, ConjAP
        , ListNP, BaseNP, ConsNP, ConjNP
        , ListS, BaseS, ConsS, ConjS
        , S, QS, Conj
        ]
  , Structural [
        Prep, to_Prep, for_Prep, from_Prep, on_Prep, before_Prep, after_Prep, possess_Prep, they_Pron
        , VV, must_VV
        ]
  , Extend [
        VPS, MkVPS, ListVPS, BaseVPS, ConsVPS, ConjVPS
        , VPI, MkVPI --, [VPI], BaseVPI, ConsVPI, ConjVPI
        , VP, Tense, Ant, Temp, Pol, Conj -- for VPS
    --      , GenRP -- nice to have in the future?
        , ByVP
        , S, PredVPS
        , NP, GerundNP
        ]  ** {

-----------------------------------------------------------------------------
-- List instances for cats that don't have one in the RGL

  cat
    [Prep]{2} ;
    [QS]{2} ;

  fun
    ConjPrep : Conj -> [Prep] -> Prep ;
    ConjQS : Conj -> [QS] -> QS ;

    -- hack to use literal; taking advantage of the fact that literals' lincats are always SS
    -- and I know that in the functor, PrePost's lincat is a subtype of SS, so this works
    ConjPrePostVPS : (pr,pst : String) -> Conj -> [VPS] -> VPS ;
    ConjPrePostQS : (pr,pst : String) -> Conj -> [QS] -> QS ;
    ConjPrePostS : (pr,pst : String) -> Conj -> [S] -> S ;

-----------------------------------------------------------------------------
-- Misc shortcuts and extensions to RGL
  fun
    ComplVAS : V2 -> AP -> S -> VP ; -- become aware (that) a data breach may have occurred
    ComplV2S : V2 -> NP -> S -> VP ; -- notify PDPC that a data breach has occurred
                                     -- unable to pay (V2) liabilities (NP) (when_Subj they fall due) (S)

    ComplV2 : V2 -> NP -> VP ;
    ComplV2Swhen : V2S -> NP -> S -> VP ;
    ComplVSif,
    ComplVSthat : VS -> S -> VP ;
    MayHave : VP -> VPS ; -- getting "may have occurred" with pure RGL is a pain
    ReferenceNP : NP -> S ; -- it is NP — reference to a previous NP
--      ExpletiveVP : VP -> S ; -- it is raining — dummy subject it (TODO: restrict usage of this and above from HS)

    presAnt,   -- has occurred
    presSimul, -- occurs
    pastSimul  -- occurred
    : Temp ;

    POS : Pol ;
    NEG : Pol ;

    theSg : Det ;
    thePl : Det ;
    aSg : Det ;
    your : Det ;

    about_Prep : Prep ;
}
