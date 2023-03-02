concrete CustomSyntaxChi of CustomSyntax =
    NumeralChi
  , GrammarChi [
        N, N2, CN, UseN, NP, Det, DetCN, MassNP, RP
      , V,  VV, V2, VS, VP
      , A, A2, AP, PositA
      , Comp, Adv, VP, UseComp, CompAP, CompAdv -- is a public agency
      , Prep, PrepNP, AdvVP
      , ListAdv, BaseAdv, ConsAdv, ConjAdv
      , ListAP, BaseAP, ConsAP, ConjAP
      , ListNP, BaseNP, ConsNP, ConjNP
      , ListS, BaseS, ConsS, ConjS
      , S, QS, Conj
      ]
  , StructuralChi [
      Prep, to_Prep, by8means_Prep, for_Prep, from_Prep, on_Prep
    , VV, must_VV
    ]
  , ExtendChi [
        VPS, MkVPS, mkVPS, ListVPS, BaseVPS, ConsVPS, ConjVPS, baseVPS
      , VPI, MkVPI, mkVPI --, [VPI], BaseVPI, ConsVPI, ConjVPI
      , VP, Tense, Ant, Temp, Pol, Conj -- for VPS
      , S, PredVPS
      , NP, GerundNP -- by performing NDB qualification
      ]
  ** open
      SyntaxChi
    , ParadigmsChi
    , ExtendChi
    , (R=ResChi)
    , (Co=Coordination)
    , Prelude
    in {

-----------------------------------------------------------------------------
-- List instances for cats that don't have one in the RGL

  lincat
    [Prep] = Co.ListX ** {advType : R.AdvType; hasDe : Bool};

  lin
    BasePrep p q = Co.twoSS {s = R.linPrep p} {s = R.linPrep q} ** q ;
    ConsPrep p ps = Co.consrSS Co.comma {s = R.linPrep p} ps ** ps ;
    ConjPrep co tcs =
      let conj : Co.ConjunctionDistr = co.s ! R.CSent ;
          tc : SS = Co.conjunctDistrSS conj tcs ;
       in lin Prep {prepPre = [] ; prepPost = tc.s} ** tcs ; ---- TODO: this ignores pre vs. post distinctions. Need a custom lincat that encodes combinations {Pre,Post}x{Pre,Post} in param.

    -- : (pr,pst : String) -> Conj -> [VPS] -> VPS ; -- hack to use String literals; their lincat is always SS
    ConjPrePostVPS pr pst conj vpss =
      let vps : VPS = ConjVPS conj vpss ;
        in cc3 pr vps pst ;

-----------------------------------------------------------------------------
-- Misc shortcuts and extensions to RGL

  lin
    -- : V2 -> AP -> S -> VP ; -- become aware (that) a data breach may have occurred
    ComplVAS become aware db_occurs =
      let become_aware : VP = mkVP <lin VA become : VA> <lin AP aware : AP> ;
        in become_aware ** {
            compl = become_aware.compl ++ R.linS db_occurs
            } ;
    -- : V2 -> NP -> S -> VP ; -- notify PDPC that a data breach has occurred
    ComplV2S v2 np s = mkVP <lin V2S v2 : V2S> <lin NP np : NP> <lin S s : S> ; -- already in RGL, just a shortcut
    ComplV2 v2 np = mkVP <lin V2 v2 : V2> <lin NP np : NP>  ;
    ComplVSif vs s =
      let if_S : SS = mkUtt (SyntaxChi.mkAdv if_Subj s) ;
          vp : VP = R.predV <lin V vs : V> [] ;
       in lin VP (R.insertObj if_S vp) ;
    ComplVSthat vs s = mkVP <lin VS vs : VS> <lin S s : S> ;

    AdjCN ap cn = {s = ap.s ! R.Attr ++ cn.s ; c = cn.c} ;
    CompNP np = R.insertObj np (R.predV local_copula []) ;

    MayHave occur =
      let vps : ExtendChi.VPS = MkVPS presAnt POS occur ;
        in vps ** {s = "可 能" ++ vps.s} ;
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

    who_RP = which_RP ;
    about_Prep = ParadigmsChi.mkPrep "about" ; -- TODO
    within_Prep = ParadigmsChi.mkPrep [] "内";
    vaguePrep = ParadigmsChi.mkPrep [] ;
    month_N = ParadigmsChi.mkN "month" ; -- TODO
    may_VV = must_VV ** {s = "可 能"};
    shant_VV = must_VV ** {
      s = "不 可 能" ;  -- TODO check
      } ;

  oper
    advVPS : ExtendChi.VPS -> CatChi.Adv -> ExtendChi.VPS = \vps,adv -> cc2 vps (mkUtt adv) ;
    invarV : Str -> V = \v -> lin V {
      s,sn = v ; pp = "了" ; ds = "着" ; dp = "在" ; ep = "过" ; neg = "不"
      } ;
    local_copula :  R.Verb = R.mkVerb "是" [] [] [] [] "不" ;
    hen_copula : R.Verb =
      {s = R.hen_s ; sn = [] ; pp = [] ; ds = [] ; dp = [] ; ep = [] ; neg = "不"} ; ---
    nocopula : R.Verb =
      {s = [] ; sn = [] ; pp = [] ; ds = [] ; dp = [] ; ep = [] ; neg = "不"} ; ---
    adjcopula : R.Verb =
      {s = "是" ; sn = [] ; pp = [] ; ds = [] ; dp = [] ; ep = [] ; neg = "不"} ; ---

    emptyNP : NP = it_NP ** {s = []} ;
}
