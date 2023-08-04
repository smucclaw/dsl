abstract CustomSyntax =
    Numeral
  , Grammar [
          N, N2, CN, PN, NP, UseN, ComplN2, UsePN, Num, NumSg, NumPl, Det, DetCN, MassNP
        , V,  VV, V2, VS, VP
        , A, A2, AP, AdjCN, PositA, ComplA2
        , Comp, Adv, VP, UseComp, CompNP, CompAP, CompAdv -- is a public agency
        , Prep, PrepNP, AdvVP
        , AdA, AdAdv, Card, CAdv, AdN, AdNum, AdnCAdv
        , Dig, Digits, NumDigits, IDig, IIDig, D_0, D_1, D_2, D_3, D_4, D_5, D_6, D_7, D_8, D_9
        , ListAdv, BaseAdv, ConsAdv, ConjAdv
        , ListAP, BaseAP, ConsAP, ConjAP
        , ListNP, BaseNP, ConsNP, ConjNP
        , ListS, BaseS, ConsS, ConjS
        , S, QS, Conj, Subj, SubjS
        , RS, RP, IdRP, RelCN
        ]
  , Structural [
        Prep, to_Prep, for_Prep, from_Prep, on_Prep, before_Prep, after_Prep, possess_Prep
        , VV, must_VV
        , AdN, CAdv, less_CAdv, more_CAdv, at_least_AdN, at_most_AdN
        , Subj, because_Subj
        ]
  , Extend [
        VPS, MkVPS, ListVPS, BaseVPS, ConsVPS, ConjVPS
        , VPI, MkVPI --, [VPI], BaseVPI, ConsVPI, ConjVPI
        , VP, Tense, Ant, Temp, Pol, Conj -- for VPS
    --      , GenRP -- nice to have in the future?
        , ByVP, N, CompoundN
        , S, PredVPS, RelVPS
        , NP, GerundNP, Num, GenModNP
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
    WhileDoing : VP -> Adv ; -- while travelling in a public transport
--      ExpletiveVP : VP -> S ; -- it is raining — dummy subject it (TODO: restrict usage of this and above from HS)
    CNwhereS : CN -> NP -> VPS -> CN ; -- premise where school activities take place

    presAnt,   -- has occurred
    presSimul, -- occurs
    pastSimul  -- occurred
    : Temp ;

    POS : Pol ;
    NEG : Pol ;

    theSg : Det ;
    thePl : Det ;
    aSg : Det ;
    aPl : Det ;
    your : Det ;

    about_Prep : Prep ;
}
