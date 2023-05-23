concrete CustomSyntaxChi of CustomSyntax =
    NumeralChi
  , GrammarChi [
        N, N2, CN, UseN, NP, Det, DetCN, MassNP
      , V, VV, V2, VS, VP, AdvVP
      , A, A2, AP, PositA
      , Comp, Adv, VP, UseComp, CompAP, CompAdv -- is a public agency
      , AdA, AdAdv -- only (within the organisation)
      , ListAdv, BaseAdv, ConsAdv, ConjAdv
      , ListAP, BaseAP, ConsAP, ConjAP
      , ListNP, BaseNP, ConsNP, ConjNP
      , ListS, BaseS, ConsS, ConjS
      , S, QS, Conj
      ]
  , StructuralChi [
      VV, must_VV
    ]
  , ExtendChi [
        VPS, MkVPS, mkVPS, ListVPS, BaseVPS, ConsVPS, ConjVPS, baseVPS
      , VPI, MkVPI, mkVPI --, [VPI], BaseVPI, ConsVPI, ConjVPI
      , VP, Tense, Ant, Temp, Pol, Conj -- for VPS
      , ByVP
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

-- override lincat for Prep
  lincat
    Prep = LinPrep ;

  param
    PrepType = Zai | NotZai ;
    Fields =
      PreField PrepType
    | PostField
    | PrePostField PrepType
    | Empty ;

  oper
    LinPrep : Type = SyntaxChi.Prep ** {fields : Fields} ;

    emptyPrep : LinPrep = ParadigmsChi.mkPrep [] [] ** {fields = Empty} ;

    LinListPrep : Type = {
      prepPre1 : Str ;
      prepPre2 : Str ;
      prepPost1 : Str ;
      prepPost2 : Str ;
      advType : R.AdvType ;
      hasDe : Bool ;
      fields : Fields
      } ;

    emptyListPrep : LinListPrep = {
      prepPre1,
      prepPre2,
      prepPost1,
      prepPost2 = [] ;
      advType = R.ATPlace False;
      hasDe = False ;
      fields = Empty
      } ;

    fillListPrep : LinPrep -> LinPrep -> LinListPrep = \p,q -> emptyListPrep ** {
        prepPre1 = p.prepPre ;
        prepPre2 = q.prepPre ;
        prepPost1 = p.prepPost ;
        prepPost2 = q.prepPost ;
        advType = q.advType ;  ---- TODO: to inherit from the later one?
        hasDe = q.hasDe ;
        fields = q.fields
      } ;

    mkPrepChi : Str -> Str -> LinPrep = \s,t -> ParadigmsChi.mkPrep s t ** {fields = getFields s t} ;

    getFields : Str -> Str -> Fields = \s,t -> case <s,t> of {
        <"", ""> => Empty ;
        <"" , _> => PostField ;
        <"在", ""> => PreField Zai ;
        <_ , ""> => PreField NotZai ;
        <"在", _> => PrePostField Zai ;
        <_+?, _+?> => PrePostField NotZai
      } ;

  lin
      to_Prep = mkPrepChi "往" [] ;
      for_Prep = mkPrepChi "为了" [] ;
      from_Prep = mkPrepChi "从" [] ** {advType =  R.ATPlace True} ;
      on_Prep = mkPrepChi "在" [] ;
      before_Prep = mkPrepChi "在" "之前" ;
      after_Prep = mkPrepChi "在" "之后" ;
      PrepNP prep np = SyntaxChi.mkAdv <prep : SyntaxChi.Prep> np ;
-----------------------------------------------------------------------------
-- List instances for cats that don't have one in the RGL

  lincat
    [Prep] = LinListPrep ;

  lin
    BasePrep p q = case <p.fields, q.fields> of {
      -- if one or both are empty
      <Empty, Empty> => emptyListPrep ;
      <Empty, _> => fillListPrep p q ;
      <_, Empty> => fillListPrep q p ; -- fillListPrep takes fields from latter so flip order

      -- if one or both are Zai
      <PreField Zai, PreField Zai> => fillListPrep p q ** {
        prepPre1 = [] -- don't duplicate Zai
      } ;
      <PreField _, PreField _> => fillListPrep p q ** {
        fields = PreField NotZai
      } ;
      <PrePostField Zai, PrePostField Zai> |
      <PrePostField Zai, PreField Zai> |
      <PrePostField Zai, PostField> |
      <PreField Zai, PrePostField Zai> |
      <PreField Zai, PostField> => fillListPrep p q ** {
        prepPre1 = [] ; --don't duplicate Zai
        fields = PrePostField Zai -- for sure it's now PrePost since at least one was PrePost
      } ;

      -- rest of the cases
      <PreField x, PostField> => fillListPrep p q ** {
        fields = PrePostField x
      } ;
      <PostField, PreField x> => fillListPrep p q ** {
        fields = PrePostField x
      } ;
      _ => fillListPrep p q ----- TODO
    } ;

    ConsPrep p ps = ps ** { -- Never used n>2 in our examples, so this is not general
      prepPre1 = p.prepPre ++ ps.prepPre1 ;
      prepPost1 = p.prepPost ++ ps.prepPost1
    } ;

    ConjPrep co tcs = let conj = co.s ! R.CSent in
    case tcs.fields of {
      PreField _ => {
        prepPre = conj.s1 ++ tcs.prepPre1 ++ conj.s2 ++ tcs.prepPre2 ; -- since we put the Zai in there in BasePrep
        prepPost = tcs.prepPost1 ++ tcs.prepPost2
      } ** tcs ;
      _ => {
        prepPre = tcs.prepPre2 ; -- since we put the Zai in there in BasePrep
        prepPost = conj.s1 ++ tcs.prepPost1 ++ conj.s2 ++ tcs.prepPost2
      } ** tcs
    } ;

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
      let if_S : SS = mkUtt (SyntaxChi.mkAdv (mkSubj "是否") s) ;
          vp : VP = R.predV <lin V vs : V> [] ;
       in lin VP (R.insertObj if_S vp) ;
    ComplVSthat vs s = mkVP <lin VS vs : VS> <lin S s : S> ;

    AdjCN ap cn = {s = ap.s ! R.Attr ++ cn.s ; c = cn.c} ;
    CompNP np = R.insertObj np (R.predV local_copula []) ;

    MayHave occur =
      let vps : ExtendChi.VPS = MkVPS presAnt POS occur ;
        in vps ** {s = "可 能 已 经" ++ vps.s} ; --
    -- : NP -> S ; -- it is NP — reference to a previous NP
    ReferenceNP np = mkS (mkCl it_NP <lin NP np : NP>) ;

    presSimul = mkTemp presentTense simultaneousAnt ;
    presAnt = mkTemp presentTense anteriorAnt ;
    pastSimul = mkTemp pastTense simultaneousAnt ;
    POS = positivePol ;
    NEG = negativePol ;

    theSg = theSg_Det ;
    thePl = thePl_Det ;
    aSg = theSg_Det ;
    your = mkDet youSg_Pron ;
    about_Prep = mkPrepChi "about" [] ; -- TODO

  oper
    whoRP : RP = which_RP ;
    withinPrep : LinPrep = mkPrepChi [] "内";
    uponPrep : Prep = mkPrep "什么时候" [] ; -- this is only used in a lin that expects a real Prep, confusingly enough
    by8timePrep : LinPrep = mkPrepChi "前" [] ;
    vaguePrep : LinPrep = mkPrepChi [] [] ;
    monthN : N = ParadigmsChi.mkN "month" ; -- TODO
    mayVV :  VV = must_VV ** {s = "可 能"};
    shantVV :  VV = must_VV ** {
      s = "不 可 能" ;  -- TODO check
      } ;

    bindQM : Str = BIND ++ "？" ;

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
