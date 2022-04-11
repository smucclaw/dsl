

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
    csubj = SC ;
    obl,
    nmod,
    xcomp = Adv ;
    advmod = LinAdvmod ;
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
    root = linRoot;


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

    -- Why do we have be_cop and is_cop?
    -- It is because ud2gf will look at both lemma and form, and we want it to
    -- find the cop regardless which one it looks for.
    -- We ignore all of the cops in linearisations always, because cop is always just is.
    -- However, we don't ignore aux, because aux has lot of interesting information.
    be_cop,
    be_auxPass = ss "be" ;
    is_cop,
    is_auxPass = ss "is" ;
    not_advmod = {adv = lin Adv (ss "not") ; isNot = True} ;
    '\'s_Gen' = ss ("'s"|"’s") ;

    csubj_ uds = mkSC (uds2s uds) ;
    csubjMark_ mark uds = -- assuming this is like "to assess the breach"
      lin SC {s = mark.s ++ linUDS' Infinite emptyNP uds} ;

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
    rootDAP_ dap = mkRoot (UseDAP dap) ;
    rootQuant_ det = mkRoot (N.DetNP (N.DetQuant det N.NumSg)) ;
    rootAdA_ ada = rootAdv_ (mkAdv ada (P.mkAdv "")) ;
    nmod_ = PrepNP ;

    ccomp_ uds = lin S {s = linUDS uds} ;
    xcompAdv_ adv = adv ;
    xcompA_ ap = lin Adv (mkUtt ap) ;
    xcompN_ np = lin Adv (mkUtt np) ;
    xcompToBeN_ to be np = lin Adv (cc3 to be (mkUtt np)) ;
    xcompA_ccomp_ ap cc = xcompA_ (mkAP ap cc) ;
    aclUDS_,
    advclUDS_ = \uds -> lin Adv {s = linUDS uds} ;
    aclUDSpastpart_ uds = lin Adv {s = linUDS' PastPart emptyNP uds} ;
    aclUDSgerund_ uds = lin Adv {s = linUDS' PresPart emptyNP uds} ;
    advclMarkUDS_ = \mark,uds -> lin Adv {
      s = case uds.hasSubj of {
            True => mark.s ++ linUDS uds ;
            False => mark.s ++ linUDS' Infinite emptyNP uds}
      } ;

    expl_ = id Pron ;
    det_ = id Det ;
    vocative_ = id NP ;
    amod_ = id AP ;
    mark_ = id Subj ;
    to_mark = ss "to" ;

    -- passives
    nsubjPass_ = id NP ;

  param
    AclType = Finite | PastPart | PresPart | Infinite ;

  oper
    UDSPred : Type = {  -- because UDS can become an acl, either finite, gerund or past participle
      fin : VPS ; inf : VPI ;
      pp, presp : AP ;
      np : NP ; isNP : Bool
      } ;

    defaultUDSPred : {np : NP ; isNP : Bool} = {
      np = emptyNP ;
      isNP = False
      } ;

    LinUDS : Type = {
      subj : NP ; hasSubj : Bool ;
      pred : UDSPred
      } ;

    mkUDS = overload {

      mkUDS : NP -> Root -> LinUDS = \np,rt -> {
        subj = np ; hasSubj = True ;
        pred = case rt.isNP of {
          True => myVPS rt.np ;
          False => myVPS rt.vp }
        } ;
      mkUDS : NP -> UDSPred -> LinUDS = \np,pr -> {
        subj = np ; hasSubj = True ;
        pred = pr
        }
    } ;

--    linUDS = overload {
      linUDS : LinUDS -> Str = \uds -> linUDS' Finite uds.subj uds ;
    --  linUDS : NP -> LinUDS -> Str = \np,uds -> linUDS' Finite np uds
  --  } ;

    uds2s : LinUDS -> S = \uds -> ExtendEng.PredVPS uds.subj uds.pred.fin ;

    linUDS' : AclType ->  NP -> LinUDS -> Str = \at,subj,uds -> case at of {
      Finite => (ExtendEng.PredVPS subj uds.pred.fin).s ;
      PresPart => (cc2 (mkUtt subj) (mkUtt uds.pred.presp)).s ;
      PastPart => (cc2 (mkUtt subj) (mkUtt uds.pred.pp)).s ;
      Infinite => (mkUtt subj).s ++ uds.pred.inf.s ! VVAux ! agrP3 Sg} ;

    myVPS = overload {
      myVPS : VP -> UDSPred = \vp -> defaultUDSPred ** {
        fin = MkVPS (mkTemp presentTense simultaneousAnt) positivePol vp ;
        pp = BareRGEng.PastPartAP vp ;
        presp = BareRGEng.PresPartAP vp ;
        inf = ExtendEng.MkVPI vp } ;
      myVPS : Tense -> VP -> UDSPred = \tns,vp -> defaultUDSPred ** {
        fin = MkVPS (mkTemp tns simultaneousAnt) positivePol vp ;
        pp = BareRGEng.PastPartAP vp ;
        presp = BareRGEng.PresPartAP vp ;
        inf = ExtendEng.MkVPI vp } ;
      myVPS : Ant -> VP -> UDSPred = \ant,vp -> defaultUDSPred ** {
        fin = MkVPS (mkTemp presentTense ant) positivePol vp ;
        pp = BareRGEng.PastPartAP vp ;
        presp = BareRGEng.PresPartAP vp ;
        inf = ExtendEng.MkVPI vp } ;
      myVPS : NP -> UDSPred = \np ->
        let vp:VP = mkVP np in {
        fin = MkVPS (mkTemp presentTense simultaneousAnt) positivePol vp ;
        pp = BareRGEng.PastPartAP vp ;
        presp = BareRGEng.PresPartAP vp ;
        inf = ExtendEng.MkVPI vp ;
        np = np ;
        isNP = True
      } ;
    } ;

    Root : Type = {
      np : NP ;
      isNP : Bool ;
      vp : VP ;
      c2 : Str
      } ;

    linRoot : Root -> Str = \rt -> (mkUtt rt.vp).s ;

    mkRoot = overload {
       mkRoot : AP -> Root = \ap -> emptyRoot ** {vp = mkVP ap ; adv = lin Adv (mkUtt ap)} ;
       mkRoot : NP -> Root = \np -> emptyRoot ** {vp = mkVP np ; adv = lin Adv (mkUtt np) ; np = np ; isNP = True } ;
       mkRoot : VP -> Root = \vp -> emptyRoot ** {vp = vp ; adv = lin Adv (mkUtt vp)} ; ---- ADV is bad
       mkRoot : VPSlash -> Root = \vp -> emptyRoot ** {vp = vp ; c2 = vp.c2 ; adv = lin Adv (mkUtt <vp : VP>)}
    } ;

    emptyRoot : Root = defaultUDSPred ** {
       vp = mkVP (P.mkN "dummy") ;
       c2 = []
    } ;


    -- Add an SC onto a Root, e.g.
    -- (ready : Root) (to_sleep : SC) -> ready to sleep
    scRoot : Root -> SC -> Root = \rt,sc -> advRoot rt <sc : Adv> ;

    -- Add an Adv onto a Root, e.g.
    -- (critical : Root) (always : Adv) -> always critical
    -- (warm : Root) (by_nature : Adv) -> warm by nature
    advRoot : Root -> Adv -> Root = \rt,adv -> rt ** {
      vp = mkVP rt.vp adv ;
      np = N.AdvNP <rt.np:NP> <adv:Adv> ;
    } ;

    -- Add a direct object onto a Root, e.g.
    -- (eat : Root) (food : NP) -> eat food
    dObjRoot : Root -> NP -> Root = \rt,np -> rt ** {
      vp = mkVP (slashV rt.vp) np ;
      np = ApposNP rt.np np
    } ;

    emptyNP : NP = it_NP ** {s = \\_ => ""} ;
    emptySubj : Subj = that_Subj ** {s = ""} ;

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