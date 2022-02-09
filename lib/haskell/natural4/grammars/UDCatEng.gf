

concrete UDCatEng of UDCat = BareRGEng **
  open SyntaxEng, Prelude, (P=ParadigmsEng), (E=ExtraEng), ResEng, ExtendEng, (N=NounEng), SymbolicEng in {

  lincat
    UDS = LinUDS ;

    root = Root ;

    nsubjPass,
    nsubj,
    obj = NP ;
    iobj = NP ;
    ccomp = S ;
    obl,
    nmod,
    xcomp = Adv ;
    advmod = LinAdvmod ;
--   conj = {s : Number => Str} ;
    cc = Conj ;
    aclRelcl = RS ; -- which is a breach
    acl,
    advcl = Adv ; -- if it is a breach
    aux = LinAux ;
    det = Det ;
    vocative = NP ;
    expl = Pron ;
    amod = AP ;
    mark = Subj ;
    X = Symb ;

  linref
    UDS = linUDS ;


  param
    AuxType = Be | Have | Will | RealAux ;
  oper
    LinAdvmod : Type = {adv : Adv ; isNot : Bool} ;
    LinAux : Type = {s : Str ; vv : VV ; auxType : AuxType} ;
    mkAux = overload {
      mkAux : Str -> VV -> LinAux = \str,vv -> {
          s = str ; vv = vv ; auxType = RealAux } ;
      mkAux : Str -> AuxType -> LinAux = \str,at -> {
          s = str ; vv = must_VV ; auxType = at } -- dummy vv, not used
    } ;

  lin

    be_aux = mkAux "be" Be ;
    may_aux = mkAux "may" E.may_VV ;
    have_aux = mkAux "have" Have ;
    will_aux = mkAux "will" Will ;
    can_aux = mkAux "can" can_VV ;
    must_aux = mkAux "must" must_VV ;
    should_aux = mkAux "should" should_VV ;
    shall_aux = mkAux "shall" shall_VV ;
    be_cop,
    be_auxPass = ss "be" ;
    is_cop,
    is_auxPass = ss "is" ;
    not_advmod = {adv = lin Adv (ss "not") ; isNot = True} ;

    nsubj_,
    obj_,
    iobj_ = id NP ;
    aclRelclRS_ = id RS ;
    aclRelclUDS_ uds =
      let dummyRS : RS = mkRS (mkRCl (genericCl (mkVP (P.mkV "dummy")))) ;
       in dummyRS ** {s = \\_ => linUDS uds};
    cc_ = id Conj ;
    obl_ = id Adv ;
    advmod_ adv = {adv = adv ; isNot = False} ;
    oblPrep_ to = mkAdv to emptyNP ;
    oblRP_ for which = mkAdv for (rp2np which) ;

    rootV_ vp = mkRoot vp ;
    rootA_ ap = mkRoot ap ;
    rootN_ np = mkRoot np ;
    rootAdv_ adv = mkRoot (mkVP adv) ;
    rootDet_ det = mkRoot (N.DetNP det) ;
    rootQuant_ det = mkRoot (N.DetNP (N.DetQuant det N.NumSg)) ;
    nmod_ = PrepNP ;

    conjA_ ap = mkUtt ap ; -- : AP -> conj ;
    conjN_ np = mkUtt np ; -- : NP -> conj ;
    conjAdv_ a = mkUtt a ; -- : Adv -> conj ;
    ccomp_ uds = lin S {s = linUDS uds} ;
    xcompAdv_ adv = adv ;
    xcompA_ ap = lin Adv (mkUtt ap) ;
    xcompA_ccomp_ ap cc = xcompA_ (mkAP ap cc) ;
    aclUDS_,
    advclUDS_ = \uds -> lin Adv {s = linUDS uds} ;
    aclUDSpastpart_ uds = lin Adv {s = linUDS' PastPart uds} ;
    aclUDSgerund_ uds = lin Adv {s = "that" ++ linUDS uds} ; -- TODO: do we need actual gerund here? this is just to avoid "message obeys a certain format" when original is "… obeying …"

    expl_ = id Pron ;
    det_ = id Det ;
    vocative_ = id NP ;
    amod_ = id AP ;
    mark_ = id Subj ;

    -- passives
    nsubjPass_ = id NP ;

  param
    AclType = Finite | PastPart ;

  oper
    UDSPred : Type = {fin : VPS ; pp : AP} ; -- because UDS can become an acl, either finite, gerund or past participle

    LinUDS : Type = {subj : NP ; pred : UDSPred} ;

    mkUDS : NP -> VP -> LinUDS = \np,vp -> {
       subj = np ; pred = myVPS vp } ;

    linUDS : LinUDS -> Str = linUDS' Finite ;
    linUDS' : AclType -> LinUDS -> Str = \at,uds -> case at of {
      Finite => (PredVPS uds.subj uds.pred.fin).s ;
      PastPart => (cc2 (mkUtt uds.subj) (mkUtt uds.pred.pp)).s } ;

    myVPS = overload {
      myVPS : VP -> UDSPred = \vp -> {
        fin = MkVPS (mkTemp presentTense simultaneousAnt) positivePol vp ;
        pp = BareRGEng.PastPartAP vp
        } ;
      myVPS : Tense -> VP -> UDSPred = \tns,vp -> {
        fin = MkVPS (mkTemp tns simultaneousAnt) positivePol vp ;
        pp = BareRGEng.PastPartAP vp } ;
      myVPS : Ant -> VP -> UDSPred = \ant,vp -> {
        fin = MkVPS (mkTemp presentTense ant) positivePol vp ;
        pp = BareRGEng.PastPartAP vp }
    } ;
    --Aux : Type = {v : V ; isCop : Bool} ;
--   Root : Type = {vp : VP ; comp : Comp ; c2 : Str} ;

    Root : Type = {vp : VP ; c2 : Str ; adv : Adv} ;

    -- alternative Root: {a : A ; n : N ; v : V ; adv : Adv ; whichFieldIsLegit : LegitField}

    mkRoot = overload {
       mkRoot : AP -> Root = \ap -> emptyRoot ** {vp = mkVP ap ; adv = lin Adv (mkUtt ap)} ;
       mkRoot : NP -> Root = \np -> emptyRoot ** {vp = mkVP np ; adv = lin Adv (mkUtt np)} ;
       mkRoot : VP -> Root = \vp -> emptyRoot ** {vp = vp ; adv = lin Adv (mkUtt vp)} ; ---- ADV is bad
       mkRoot : VPSlash -> Root = \vp -> emptyRoot ** {vp = vp ; c2 = vp.c2 ; adv = lin Adv (mkUtt <vp : VP>)}
    } ;

    emptyRoot : Root = {
       vp = mkVP (P.mkN "dummy") ;
       adv = ss "dummy" ;
       c2 = []
    } ;

    emptyNP : NP = it_NP ** {s = \\_ => ""} ;

    should_VV : VV = lin VV {
      s = table {
        VVF VInf => ["be obliged to"] ;
        VVF VPres => "should" ;
        VVF VPPart => ["been obliged to"] ;
        VVF VPresPart => ["being obliged to"] ;
        VVF VPast => "should" ;
        VVPastNeg => "should not" ;
        VVPresNeg => "shouldn't"
        } ;
      p = [] ;
      typ = VVAux
    } ;

    shall_VV : VV = lin VV {
      s = table {
        VVF VInf => ["be obliged to"] ;
        VVF VPres => "shall" ;
        VVF VPPart => ["been obliged to"] ;
        VVF VPresPart => ["being obliged to"] ;
        VVF VPast => "shall" ;
        VVPastNeg => "shall not" ;
        VVPresNeg => "shan't"
        } ;
      p = [] ;
      typ = VVAux
    } ;

    rp2np : RP -> NP = \rp -> rp ** {
      s = \\c => rp.s ! RC Neutr c ;
      a = ragr2agr rp.a
      } ;

    ragr2agr : ResEng.RAgr -> ResEng.Agr = \ra -> case ra of {
      ResEng.RAg a => a ;
      ResEng.RNoAg => agrP3 Sg
    } ;

    -- copied this from ParseExtendEng, easier to duplicate code than to introduce new dependency?
    ParseExtendComplVV : VV -> Ant -> Pol -> VP -> VP ;
    ParseExtendComplVV v ant pol vp =
      insertObj (variants {\\agr => ant.s ++ pol.s ++
                                    infVP v.typ vp True  ant.a pol.p agr;
                           \\agr => ant.s ++ pol.s ++
                                    infVP v.typ vp False ant.a pol.p agr})
                (predVV v) ;
}