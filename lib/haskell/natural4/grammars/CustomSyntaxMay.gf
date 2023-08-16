concrete CustomSyntaxMay of CustomSyntax =
    NumeralMay
  , GrammarMay [
        N, N2, CN, PN, NP, UseN, ComplN2, UsePN, Num, NumSg, NumPl, Det, DetCN, MassNP
      , V, VV, V2, VS, VP, UseV
      , A, A2, AP, AdjCN, PositA, ComplA2
  --      , ProgrVP -- becoming aware
      , Comp, Adv, VP, UseComp, CompAP, CompNP, CompAdv -- is a public agency
      , Prep, PrepNP, AdvVP
      , AdA, AdAdv, Card, CAdv, AdN, AdNum, AdnCAdv
        , Dig, Digits, NumDigits, IDig, IIDig, D_0, D_1, D_2, D_3, D_4, D_5, D_6, D_7, D_8, D_9 -- only (within the organisation)
      , ListAdv, BaseAdv, ConsAdv, ConjAdv
      , ListAP, BaseAP, ConsAP, ConjAP
      , ListNP, BaseNP, ConsNP, ConjNP
      , ListS, BaseS, ConsS, ConjS
      , S, QS, Conj, Subj, SubjS
        , RS, RP, IdRP, RelCN
      ]
  , StructuralMay [
      Prep, for_Prep, from_Prep, on_Prep, after_Prep, possess_Prep
    , VV, must_VV
        , AdN, CAdv, less_CAdv, more_CAdv, at_least_AdN, at_most_AdN
        , Subj, because_Subj
    ]
  , ExtendMay [
        VPS, MkVPS, mkVPS, ListVPS, BaseVPS, ConsVPS, ConjVPS, baseVPS
      , VPI, MkVPI, mkVPI --, [VPI], BaseVPI, ConsVPI, ConjVPI
      , VP, Tense, Ant, Temp, Pol, Conj -- for VPS
      , ByVP, N, CompoundN
      , S, PredVPS, RelVPS
      , NP, GerundNP, Num, GenModNP -- by performing NDB qualification
      ]
  ** open
      SyntaxMay
    , ParadigmsMay
    , ExtendMay
    , SymbolicMay
    , LexiconMay
    , (R=ResMay)
    , (Co=Coordination)
    , Prelude
    in {

-----------------------------------------------------------------------------
-- List instances for cats that don't have one in the RGL

  lincat
    [Prep], [QS] = Co.ListX ;

  lin
   -- overrides RGL preps
    to_Prep = R.mkPrep "kepada" ;
    before_Prep = R.mkPrep "sebelum" ;

    BasePrep = Co.twoSS ;
    ConsPrep = Co.consrSS Co.comma ;
    ConjPrep c ps =
      let prep : SS = Co.conjunctDistrSS c ps ;
       in R.emptyPrep ** prep ;

    BaseQS = Co.twoSS ;
    ConsQS = Co.consrSS Co.comma ;
    ConjQS = Co.conjunctDistrSS ;
    -- hack to use String literals; their lincat is always SS
    -- : (pr,pst : String) -> Conj -> [VPS] -> VPS ;
    ConjPrePostVPS pr pst conj vpss =
      let vps : VPS = ConjVPS conj vpss ;
        in cc3 pr vps pst ;

    -- : (pr,pst : String) -> Conj -> [QS] -> QS ;
    ConjPrePostQS pr pst conj qss = cc2 (ConjPrePostS pr pst conj qss) (ss bindQM) ;

    ConjPrePostS pr pst conj ss =
      let s : S = ConjS conj ss
      in cc3 pr s pst ;

-----------------------------------------------------------------------------
-- Misc shortcuts and extensions to RGL

  lin
    -- : V2 -> AP -> S -> VP ; -- become aware (that) a data breach may have occurred
    ComplVAS become aware db_occurs =
      let become_aware : VP = mkVP <lin VA become : VA> <lin AP aware : AP> ;
        in become_aware ** {
            s = \\vf,p => become_aware.s ! vf ! p ++ "bahawa" ++ db_occurs.s
            } ;
    -- : V2 -> NP -> S -> VP ; -- notify PDPC that a data breach has occurred
    ComplV2S v2 np s = mkVP <lin V2S v2 : V2S> <lin NP np : NP> <lin S s : S> ; -- already in RGL, just a shortcut
    ComplV2 v2 np = mkVP <lin V2 v2 : V2> <lin NP np : NP>  ;
    ComplVSif vs s =
      let if_S : Str = (SyntaxMay.mkAdv if_Subj <s : S>).s ;
          vp : VP = R.useV <lin V vs : V> ;
       in lin VP (R.insertObj if_S vp) ;
    ComplVSthat vs s = mkVP <lin VS vs : VS> <lin S s : S> ;

    MayHave occur =
      let vps : ExtendMay.VPS = MkVPS presAnt POS occur ;
        in vps ** {s = "mungkin" ++ vps.s} ;
    -- : NP -> S ; -- it is NP — reference to a previous NP
    ReferenceNP np = mkS (mkCl it_NP <lin NP np : NP>) ;

    presSimul = mkTemp presentTense simultaneousAnt ;
    presAnt = mkTemp presentTense anteriorAnt ;
    pastSimul = mkTemp pastTense simultaneousAnt ;
    POS = positivePol ;
    NEG = negativePol ;

    theSg = theSg_Det ;
    thePl = thePl_Det ;
    aSg = aSg_Det ;
    your = mkDet youSg_Pron ;
    about_Prep = R.mkPrep "tentang" ;

oper
    emptyCN : CN = <mkCN cat_N : CN> ** {s = \\_ => []} ;
    emptyNP : NP = it_NP ** {s = \\_ => []} ;
    emptyPlNP : NP = they_NP ** {s = \\_ => []} ;

    whoRP : RP = which_RP ;
    uponPrep : Prep = R.mkPrep "semasa" ;
    by8timePrep : Prep = R.mkPrep "sebelum" ;
    withinPrep : Prep = R.mkPrep "dalam lingkungan" ;
    vaguePrep : Prep = R.mkPrep ""  ;

    monthN : N = ParadigmsMay.mkN "bulan" ;
    mayVV :  VV = must_VV ** {s = "mungkin"};
    shantVV :  VV = must_VV ** {
      s = "tidak mungkin" ; -- TODO check
      } ;

    strA2 : Str -> A2 = \str -> mkA2 str ;
    invarV : Str -> V = \v -> R.mkVerb v v v v ;

    bindQM : Str = BIND ++ "?" ;
}
