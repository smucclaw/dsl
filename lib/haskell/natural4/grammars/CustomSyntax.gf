abstract CustomSyntax =
    Numeral
  , Grammar [
          N, N2, CN, UseN, NP, Det, DetCN, MassNP, RP
        , V,  VV, V2, VS, VP
        , A, A2, AP, AdjCN, PositA
        , Comp, Adv, VP, UseComp, CompNP, CompAP, CompAdv -- is a public agency
        , Prep, PrepNP, AdvVP
        , ListAdv, BaseAdv, ConsAdv, ConjAdv
        , ListAP, BaseAP, ConsAP, ConjAP
        , ListNP, BaseNP, ConsNP, ConjNP
        , ListS, BaseS, ConsS, ConjS
        , S, QS, Conj
        ]
  , Structural [
        Prep, to_Prep, by8means_Prep, for_Prep, from_Prep, on_Prep
        , VV, must_VV
        ]
  , Extend [
        VPS, MkVPS, ListVPS, BaseVPS, ConsVPS, ConjVPS
        , VPI, MkVPI --, [VPI], BaseVPI, ConsVPI, ConjVPI
        , VP, Tense, Ant, Temp, Pol, Conj -- for VPS
    --      , GenRP -- nice to have in the future?
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
    ComplV2 : V2 -> NP -> VP ;
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

-----------------------------------------------------------------------------
-- Lexicon

    about_Prep : Prep ;
    within_Prep : Prep ;
    vaguePrep : Prep ;
    month_N : N ;
    who_RP : RP ;
    may_VV : VV ;
    shant_VV : VV ;

}
