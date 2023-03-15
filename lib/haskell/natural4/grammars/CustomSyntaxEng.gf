concrete CustomSyntaxEng of CustomSyntax =
    NumeralEng
  , GrammarEng [
        N, N2, CN, UseN, NP, Det, DetCN, MassNP
      , V,  VV, V2, VS, VP
      , A, A2, AP, AdjCN, PositA
  --      , ProgrVP -- becoming aware
      , Comp, Adv, VP, UseComp, CompAP, CompNP, CompAdv -- is a public agency
      , Prep, PrepNP, AdvVP
      , ListAdv, BaseAdv, ConsAdv, ConjAdv
      , ListAP, BaseAP, ConsAP, ConjAP
      , ListNP, BaseNP, ConsNP, ConjNP
      , ListS, BaseS, ConsS, ConjS
      , S, QS, Conj
      ]
  , StructuralEng [
      Prep, to_Prep, for_Prep, from_Prep, on_Prep, before_Prep, after_Prep
    , VV, must_VV
    ]
  , ExtendEng [
        VPS, MkVPS, mkVPS, ListVPS, BaseVPS, ConsVPS, ConjVPS, baseVPS
      , VPI, MkVPI, mkVPI --, [VPI], BaseVPI, ConsVPI, ConjVPI
      , VP, Tense, Ant, Temp, Pol, Conj -- for VPS
      , S, PredVPS
      , NP, GerundNP -- by performing NDB qualification
      ]
  ** open
      SyntaxEng
    , ParadigmsEng
    , ExtendEng
    , SymbolicEng
    , (ExtraEng=ExtraEng)
    , (R=ResEng)
    , (Co=Coordination)
    , Prelude
    in {

-----------------------------------------------------------------------------
-- List instances for cats that don't have one in the RGL

  lincat
    [Prep] = Co.ListX ;
    [QS] = Co.ListTable R.QForm ;

  lin
    BasePrep = Co.twoSS ;
    ConsPrep = Co.consrSS Co.comma ;
    ConjPrep co ps = Co.conjunctDistrSS co ps ** {isPre = True} ; -- explicit args, because we manipulate the result afterwards

    BaseQS = Co.twoTable R.QForm ;
    ConsQS = Co.consrTable R.QForm Co.comma ;
    ConjQS = Co.conjunctDistrTable R.QForm ; -- no need to put args, just use eta reduction

    -- : (pr,pst : String) -> Conj -> [VPS] -> VPS ; -- hack to use String literals; their lincat is always SS
    ConjPrePostVPS pr pst conj vpss =
      let vps : VPS = ConjVPS conj vpss ;
        in vps ** {
            s  = \\o,a =>
                  let orig : {fin,inf : Str} = vps.s ! o ! a
                    in orig ** {fin = pr.s ++ orig.fin ; inf = orig.inf ++ pst.s}
          } ;

    -- : (pr,pst : String) -> Conj -> [QS] -> QS ;
    ConjPrePostQS pr pst conj qss =
      let qs : QS = ConjQS conj qss
      in qs ** {s = \\qf => pr.s ++ qs.s ! qf ++ pst.s ++ bindQM} ;
    ConjPrePostS  pr pst conj ss =
      let s : S = ConjS conj ss
      in s ** {s = pr.s ++ s.s ++ pst.s} ;

-----------------------------------------------------------------------------
-- Misc shortcuts and extensions to RGL

 lin
    -- : V2 -> AP -> S -> VP ; -- become aware (that) a data breach may have occurred
    ComplVAS become aware db_occurs =
      let become_aware : VP = mkVP <lin VA become : VA> <lin AP aware : AP> ;
          optThat : Str = "that" | "" ;
        in become_aware ** {
            ext = become_aware.ext ++ optThat ++ db_occurs.s
            } ;
    -- : V2 -> NP -> S -> VP ; -- notify PDPC that a data breach has occurred
    ComplV2S v2 np s = mkVP <lin V2S v2 : V2S> <lin NP np : NP> <lin S s : S> ; -- already in RGL, just a shortcut
    ComplV2 v2 np = mkVP <lin V2 v2 : V2> <lin NP np : NP>  ;
    ComplVSif vs s = R.insertObj (\\_ => "if" ++ s.s) (R.predV <lin V vs : V>) ;
    ComplVSthat vs s = mkVP <lin VS vs : VS> <lin S s : S> | ExtendEng.ComplBareVS vs s ;

    MayHave occur =
      let vps : ExtendEng.VPS = MkVPS presAnt POS occur ;
          have_occurred : {fin,inf : Str} = vps.s ! R.ODir False ! R.AgP3Pl R.Neutr ;
          may_have_occurred : {fin,inf : Str} = {fin = "may" ; inf = have_occurred.fin ++ have_occurred.inf} ;
        in vps ** {s = \\_,_ => may_have_occurred} ;
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

  oper

    whoRP : RP = ExtraEng.who_RP ;
    aboutPrep : Prep = ParadigmsEng.mkPrep "about" ;
    withinPrep : Prep = ParadigmsEng.mkPrep "within" ;
    vaguePrep : Prep = ParadigmsEng.noPrep ;
    uponPrep : Prep = ParadigmsEng.mkPrep "upon" ;
    by8timePrep : Prep = ParadigmsEng.mkPrep "by" ;

    monthN : N = ParadigmsEng.mkN "month" ;
    mayVV :  VV = ExtraEng.may_VV ; -- ** {s = \\_ => "may"};
    shantVV :  VV = ExtraEng.shall_VV ** { -- only used in NLG, not parsing
      s = \\_ => "shan't" ;        -- so negation here should be fine
      } ;

    every : CN -> NP = \cn -> mkNP <every_Det : Det> <cn : CN> ;
    strA2 : Str -> A2 = \str -> mkA2 (mkA str) noPrep ;
    invarV : Str -> V = \s -> mk5V s s s s s ;
    bindQM : Str = BIND ++ "?" ;

    postAdvS : S -> Adv -> S = \s,adv -> s ** mkS <lin Adv s : Adv> <lin S adv : S> ; -- hack that only works for Eng
    postAdvQS : QS -> Adv -> QS = \qs,adv -> qs ** {s = \\qf => qs.s ! qf ++ adv.s} ;
}
