

concrete UDCatMay of UDCat = BareRGMay **
  open SyntaxMay, Prelude, (P=ParadigmsMay), ResMay, ExtendMay, (N=NounMay), SymbolicMay in {

  lincat
    UDS = LinUDS ;

    root = LinRoot ;

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
--    aclRelcl = RS ; -- which is a breach
    aclRelcl,
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
    -- TODO: get rid of this too? only RealAux handled in sentence pattern funs
    -- Be, Have, Will handled in rootV_
--    AuxType = Be | Have | Will | RealAux ;
    AuxType = RealAux | TenseAux ;
  oper
    LinAdvmod : Type = {adv : Adv ; isNot : Bool} ;
    LinAux : Type = {s : Str ; vv : VV ; auxType : AuxType} ;
    mkAux = overload {
      mkAux : Str -> VV -> LinAux = \str,vv -> {
          s = str ; vv = vv ; auxType = RealAux } ;
      mkAux : Str -> AuxType -> LinAux = \str,at -> {
          s = str ; vv = should_VV ; auxType = at } -- dummy vv, not used
    } ;

  lin
    do_aux = mkAux "do" TenseAux ;
    be_aux = mkAux "be" TenseAux ;
    may_aux = mkAux "may" TenseAux ;
    have_aux = mkAux "have" TenseAux ;
    will_aux = mkAux "will" TenseAux ;
    can_aux = mkAux "can" can_VV ;
    must_aux = mkAux "must" must_VV ;
    should_aux = mkAux "should" should_VV ;
    shall_aux = mkAux "shall" shall_VV ;

    -- Why do we have be_cop and is_cop?
    -- It is because ud2gf will look at both lemma and form, and we want it to
    -- find the cop regardless which one it looks for.
    -- We ignore all of the cops in linearisations always, because cop is always just is.
    -- However, we don't ignore aux, because aux has lot of interesting information.
    be_cop = ss "be" ;
    is_cop = ss "is" ;
    '\'s_Gen' = ss ("'s"|"’s") ;
    not_Neg = ss "not" ;

    csubj_ uds = lin SC (uds2s uds) ;
    csubjMarkInfinite_ mark uds = -- assuming this is like "to assess the breach"
      lin SC {s = mark.s ++ linUDS' Infinite emptyNP uds} ;
    csubjMarkFinite_ mark uds = -- assuming this is like "to assess the breach"
      lin SC {s = mark.s ++ linUDS uds} ;
    nsubj_,
    obj_,
    iobj_ = id NP ;
    {-
    aclRelclRS_ = id RS ;
    aclRelclUDS_ uds =
      let dummyRS : RS = mkRS (mkRCl (genericCl (mkVP (P.mkV "dummy")))) ;
       in dummyRS ** {s = \\_ => linUDS uds};
       -}
    aclRelclUDSRP_ rp uds = aclRelclUDS_ (uds ** {subj = rp2np rp}) ;
    aclRelclUDS_ uds = lin Adv {s = embedInCommas (linUDS uds)} ;
    cc_ = id Conj ;
    obl_ = id Adv ;
    advmod_ adv = {adv = adv ; isNot = False} ;
    oblPrep_ to = mkAdv to emptyNP ;
    oblRP_ for which = mkAdv for (rp2np which) ;

    rootV_ temp pol vp = mkRoot temp pol vp ;
    rootVaux_ temp pol aux vp =
      let ant : Ant = lin Ant {s = [] ; a = temp.a} ;
          may_have_occurred : VP = ParseExtendComplVV aux.vv ant pol vp ;
       in mkRoot presSimulTemp positivePol may_have_occurred ;

    rootA_ ap = mkRoot ap ;
    rootN_ np = mkRoot np ;
    rootAdv_ adv = mkRoot (mkVP adv) ;
    rootDet_ det = mkRoot (N.DetNP det) ;
    -- rootDAP_ dap = mkRoot (UseDAP dap) ;
    rootQuant_ det = mkRoot (N.DetNP (N.DetQuant det N.NumSg)) ;
    rootAdA_ ada = rootAdv_ (mkAdv ada (P.mkAdv "")) ;
    nmod_ = PrepNP ;

    ccomp_ uds = lin S {s = linUDS uds} ;
    ccompMarkUDS_ mark uds = cc2 mark (ccomp_ uds) ;
    xcompAdv_ adv = adv ;
    xcompA_ ap = lin Adv (mkUtt ap) ;
    xcompN_ np = lin Adv (mkUtt np) ;
    xcompToBeN_ to be np = lin Adv (cc3 to be (mkUtt np)) ;
    xcompA_ccomp_ ap cc = xcompA_ (AdvAP ap <cc : Adv>) ; -- need to treat cc as Adv, otherwise mkAP would put an extra "that"
    aclUDS_,
    advclUDS_ = \uds -> lin Adv {s = linUDS uds} ;
    aclUDSpastpart_ uds       = lin Adv {s =        linUDS' PastPart emptyNP uds        } ;
    aclUDSpastpartParens_ uds = lin Adv {s = "(" ++ linUDS' PastPart emptyNP uds ++ ")" } ;
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
      fin : VPS ; -- comes from the original VP + Temp + Pol

      -- Transformations based on the original VP — not using the given Temp + Pol
      inf : VPI ;
      pp,
      presp : AP ;

      -- I think this was here to get more natural conjunctions,
      -- e.g. "it is A and B", instead of "it is A and it is B"
      np : NP ; isNP : Bool ;
      } ;

    defaultRoot : {np : NP ; isNP : Bool} = {
      np = emptyNP ;
      isNP = False
      } ;

    defaultUDSPred : CatMay.VP -> UDSPred = \vp -> defaultRoot ** {
      fin = MkVPS (presSimulTemp) positivePol vp ;
      pp = BareRGMay.PastPartAP vp ;
      presp = BareRGMay.PresPartAP vp ;
      inf = ExtendMay.MkVPI vp
    } ;

    LinUDS : Type = {
      subj : NP ; hasSubj : Bool ;
      pred : UDSPred
      } ;

    mkUDS = overload {

      mkUDS : NP -> LinRoot -> LinUDS = \np,rt -> {
        subj = np ; hasSubj = True ;
        pred = case rt.isNP of {
          True => mkUDSPred rt.np ;
          False => mkUDSPred rt.temp rt.pol rt.vp }
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

    uds2s : LinUDS -> S = \uds -> ExtendMay.PredVPS uds.subj uds.pred.fin ;

    linUDS' : AclType ->  NP -> LinUDS -> Str = \at,subj,uds -> case at of {
      Finite => (ExtendMay.PredVPS subj uds.pred.fin).s ;
      PresPart => (cc2 (mkUtt subj) (mkUtt uds.pred.presp)).s ;
      PastPart => (cc2 (mkUtt subj) (mkUtt uds.pred.pp)).s ;
      Infinite => (mkUtt subj).s ++ uds.pred.inf.s} ;

    mkUDSPred = overload {
      mkUDSPred : LinRoot -> UDSPred = \rt -> defaultUDSPred rt.vp ** {
        fin = MkVPS rt.temp rt.pol rt.vp } ;
      mkUDSPred : CatMay.VP -> UDSPred = defaultUDSPred ;
      mkUDSPred : CatMay.Tense -> CatMay.VP -> UDSPred = \tns,vp -> defaultUDSPred vp ** {
        fin = MkVPS (mkTemp tns simultaneousAnt) positivePol vp } ;
      mkUDSPred : Ant -> CatMay.VP -> UDSPred = \ant,vp -> defaultUDSPred vp ** {
        fin = MkVPS (mkTemp presentTense ant) positivePol vp } ;
      mkUDSPred : Temp -> CatMay.Pol -> CatMay.VP -> UDSPred = \temp,pol,vp ->
        let neg : Str = case pol.p of {Pos => [] ; Neg => "tidak"}
         in defaultUDSPred vp ** {
              fin = MkVPS temp pol vp ;
              pp =
                let pp' : AP = BareRGMay.PastPartAP vp
                  in pp' ** {s = neg ++ pp'.s} ;
              presp =
                let pp' : AP = BareRGMay.PresPartAP vp
                  in pp' ** {s = neg ++ pp'.s } ;
              inf =
                let inf' : VPI = MkVPI vp
                  in inf' ** {s = neg ++ inf'.s }
            } ; -- TODO: VVInf becomes "not to <verb>", fix later
      mkUDSPred : NP -> UDSPred = \np ->
        let vp : CatMay.VP = mkVP np in {
        fin = MkVPS (presSimulTemp) positivePol vp ;
        pp = BareRGMay.PastPartAP vp ;
        presp = BareRGMay.PresPartAP vp ;
        inf = ExtendMay.MkVPI vp ;
        np = np ;
        isNP = True
      }
    } ;

    LinRoot : Type = {
      -- These together will become the VPS in LinUDS
      vp : VP ;
      temp : Temp ;
      pol : Pol ;

      np : NP ;
      isNP : Bool ;

      c2 : Str
      } ;

    linRoot : LinRoot -> Str = \rt -> (mkUtt rt.vp).s ;

    mkRoot = overload {
       mkRoot : AP -> LinRoot = \ap -> emptyRoot ** {vp = mkVP ap} ;
       mkRoot : NP -> LinRoot = \np -> emptyRoot ** {vp = mkVP np ; np = np ; isNP = True} ;
       mkRoot : CatMay.VP -> LinRoot = \vp -> emptyRoot ** {vp = vp} ;
       mkRoot : VPSlash -> LinRoot = \vp -> emptyRoot ** {vp = vp} ;
       mkRoot : Temp -> Pol -> CatMay.VP -> LinRoot = \t,p,vp ->
        emptyRoot ** {temp = t ; pol = p ; vp = vp}
    } ;

    emptyRoot : LinRoot = defaultRoot ** {
       temp = presSimulTemp ;
       pol = positivePol ;
       vp = mkVP (P.mkN "dummy") ;
       c2 = []
    } ;

    presSimulTemp : Temp = mkTemp presentTense simultaneousAnt ;

    -- Add an SC onto a LinRoot, e.g.
    -- (ready : LinRoot) (to_sleep : SC) -> ready to sleep
    scRoot : LinRoot -> SC -> LinRoot = \rt,sc -> advRoot rt <sc : Adv> ;

    -- Add an Adv onto a LinRoot, e.g.
    -- (critical : LinRoot) (always : Adv) -> always critical
    -- (warm : LinRoot) (by_nature : Adv) -> warm by nature
    advRoot : LinRoot -> Adv -> LinRoot = \rt,adv -> rt ** {
      vp = mkVP rt.vp adv ;
      np = mkNP <rt.np:NP> <adv:Adv> ;
    } ;

    -- Add a direct object onto a LinRoot, e.g.
    -- (eat : LinRoot) (food : NP) -> eat food
    dObjRoot : LinRoot -> NP -> LinRoot = \rt,np -> rt ** {
      np = ApposNP rt.np np ;
      vp = rt.vp ** {
             s2 = \\agr => rt.vp.s2 ! agr ++ (UttAccNP np).s
      } ;

      -- vp = mkVP (slashV rt.vp) np ;
      -- Doesn't work for gerunds: the main verb is in s2 field, and mkVP puts new object before
      -- so the result is "is personal data processing" for "is processing personal data"

    } ;

    UttAccNP : NP -> Utt = \np -> ss (np.s ! Bare ) ;

    emptySubj : Subj = that_Subj ** {s = ""} ;

    should_VV : VV = variants {} ;

    shall_VV : VV = variants {} ;


    rp2np : RP -> NP = \rp -> emptyNP ** {
      s = \\_ => rp.s ;
      } ;

       -- copied this from ParseExtendMay, easier to duplicate code than to introduce new dependency?
    ParseExtendComplVV : VV -> Ant -> Pol -> VP -> VP ;
    ParseExtendComplVV v ant pol vp = vp ;

}