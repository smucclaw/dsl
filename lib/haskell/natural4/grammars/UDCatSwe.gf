

concrete UDCatSwe of UDCat = BareRGSwe **
  open SyntaxSwe, Prelude, (P=ParadigmsSwe), (X=ExtraSwe), (RS=ResSwe), ExtendSwe, (N=NounSwe), SymbolicSwe, (CS=CommonScand) in {

  lincat
    UDS = LinUDS ;

    root = LinRoot ;

    nsubjPass,
    nsubj,
    obj,
    iobj = BareRGSwe.NPLite ;
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
    X = SymbolicSwe.Symb ;

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
          s = str ; vv = must_VV ; auxType = at } -- dummy vv, not used
    } ;

  lin
    do_aux = mkAux "do" TenseAux ;
    be_aux = mkAux "be" TenseAux ;
--    may_aux = mkAux "may" E.may_VV ;
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

    csubj_ uds = lin SC (mkUtt (uds2s uds)) ;
    csubjMarkInfinite_ mark uds = -- assuming this is like "to assess the breach"
      lin SC {s = mark.s ++ linUDS' Infinite emptyNP uds} ;
    csubjMarkFinite_ mark uds = -- assuming this is like "to assess the breach"
      lin SC {s = mark.s ++ linUDS uds} ;
    nsubj_,
    obj_,
    iobj_ = id NPLite ;
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

    rootV_ temp pol vp = mkRoot <lin Temp temp : CatSwe.Temp> <lin Pol pol : Pol> <lin VP vp : VP> ;
    rootVaux_ temp pol aux vp =
      let ant : Ant = lin Ant {s = [] ; a = temp.a} ;
          may_have_occurred : CatSwe.VP = mkVP aux.vv vp ; -- later: ParseExtendComplVV aux.vv ant pol vp ;
       in mkRoot presSimulTemp positivePol may_have_occurred ;

    rootA_ ap = mkRoot ap ;
    rootN_ np = mkRoot np ;
    rootAdv_ adv = mkRoot (mkVP adv) ;
 --   rootDet_ det = mkRoot (N.DetNP det) ;
--    rootDAP_ dap = mkRoot (UseDAP dap) ;
--    rootQuant_ det = mkRoot (N.DetNP (N.DetQuant det N.NumSg)) ;
    rootAdA_ ada = rootAdv_ (mkAdv ada (P.mkAdv "")) ;
    nmod_ prep np = PrepNP prep (nplite2np np);

    ccomp_ uds = uds2s uds ;
    -- ccompMarkUDS_ mark uds =
    --   let s : SS = ccomp_ uds ;
    --    in s ** {s = \\o => cc2 mark (s.s ! o)} ;
    xcompAdv_ adv = adv ;
    xcompA_ ap = lin Adv (mkUtt ap) ;
    xcompN_ np = lin Adv (mkUtt (nplite2np np)) ;
    xcompToBeN_ to be np = lin Adv (cc3 to be (mkUtt (nplite2np np))) ;
    xcompA_ccomp_ ap cc = xcompA_ (AdvAP ap <lin Adv (mkUtt cc) : Adv>) ; -- need to treat cc as Adv, otherwise mkAP would put an extra "that"
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
    -- vocative_ = id NP ;
    amod_ = id AP ;
    mark_ = id Subj ;
    to_mark = ss "att" ;

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
      np : NPLite ; isNP : Bool ;
      } ;

    defaultRoot : {np : NPLite ; isNP : Bool} = {
      np = emptyNP ;
      isNP = False
      } ;

    defaultUDSPred : CatSwe.VP -> UDSPred = \vp -> defaultRoot ** {
      fin = MkVPS (presSimulTemp) positivePol vp ;
      pp = BareRGSwe.PastPartAP vp ;
      presp = BareRGSwe.PresPartAP vp ;
      inf = ExtendSwe.MkVPI vp
    } ;

    LinUDS : Type = {
      subj : NPLite ; hasSubj : Bool ;
      pred : UDSPred
      } ;

    mkUDS = overload {

      mkUDS : NPLite -> LinRoot -> LinUDS = \np,rt -> {
        subj = np ; hasSubj = True ;
        pred = case rt.isNP of {
          True => mkUDSPred rt.np ;
          False => mkUDSPred rt.temp rt.pol rt.vp }
        } ;
      mkUDS : NPLite -> UDSPred -> LinUDS = \np,pr -> {
        subj = np ; hasSubj = True ;
        pred = pr
        }
    } ;

--    linUDS = overload {
      linUDS : LinUDS -> Str = \uds -> linUDS' Finite (nplite2np uds.subj) uds ;
    --  linUDS : NP -> LinUDS -> Str = \np,uds -> linUDS' Finite np uds
  --  } ;

    uds2s : LinUDS -> S = \uds -> ExtendSwe.PredVPS (nplite2np uds.subj) uds.pred.fin ;

    linUDS' : AclType -> NP -> LinUDS -> Str = \at,subj,uds -> case at of {
      Finite => (mkUtt (ExtendSwe.PredVPS subj uds.pred.fin)).s ;
      PresPart => (cc2 (mkUtt subj) (mkUtt uds.pred.presp)).s ;
      PastPart => (cc2 (mkUtt subj) (mkUtt uds.pred.pp)).s ;
      Infinite => (mkUtt subj).s ++ uds.pred.inf.s ! RS.VPIInf ! RS.agrP3 P.utrum CS.Sg} ;

    mkUDSPred = overload {
      mkUDSPred : LinRoot -> UDSPred = \rt -> defaultUDSPred rt.vp ** {
        fin = MkVPS rt.temp rt.pol rt.vp } ;
      mkUDSPred : CatSwe.VP -> UDSPred = defaultUDSPred ;
      mkUDSPred : CatSwe.Tense -> CatSwe.VP -> UDSPred = \tns,vp -> defaultUDSPred vp ** {
        fin = MkVPS (mkTemp tns simultaneousAnt) positivePol vp } ;
      mkUDSPred : Ant -> CatSwe.VP -> UDSPred = \ant,vp -> defaultUDSPred vp ** {
        fin = MkVPS (mkTemp presentTense ant) positivePol vp } ;
      mkUDSPred : CatSwe.Temp -> CatSwe.Pol -> CatSwe.VP -> UDSPred = \temp,pol,vp ->
        let neg : Str = case pol.p of {Pos => [] ; Neg => "inte"}
         in defaultUDSPred vp ** {
              fin = MkVPS temp pol vp ;
              pp =
                let pp' : AP = BareRGSwe.PastPartAP vp
                  in pp' ** {s = \\x => neg ++ pp'.s ! x} ;
              presp =
                let pp' : AP = BareRGSwe.PresPartAP vp
                  in pp' ** {s = \\x => neg ++ pp'.s ! x} ;
              inf =
                let inf' : VPI = MkVPI vp
                  in inf' ** {s = \\typ,agr => neg ++ inf'.s ! typ ! agr}
            } ; -- TODO: VVInf becomes "not to <verb>", fix later
      mkUDSPred : NPLite -> UDSPred = \np ->
        let vp : CatSwe.VP = mkVP (nplite2np np) in {
        fin = MkVPS (presSimulTemp) positivePol vp ;
        pp = BareRGSwe.PastPartAP vp ;
        presp = BareRGSwe.PresPartAP vp ;
        inf = ExtendSwe.MkVPI vp ;
        np = np ;
        isNP = True
      }
    } ;

    LinRoot : Type = {
      -- These together will become the VPS in LinUDS
      vp : CatSwe.VP ;
      temp : CatSwe.Temp ;
      pol : CatSwe.Pol ;

      np : NPLite ;
      isNP : Bool ;

      c2 : Str
      } ;

    linRoot : LinRoot -> Str = \rt -> (mkUtt rt.vp).s ;

    mkRoot = overload {
       mkRoot : AP -> LinRoot = \ap -> emptyRoot ** {vp = mkVP ap} ;
       mkRoot : NPLite -> LinRoot = \np -> emptyRoot ** {vp = mkVP (nplite2np np) ; np = np ; isNP = True} ;
       mkRoot : CatSwe.VP -> LinRoot = \vp -> emptyRoot ** {vp = vp} ;
       mkRoot : VPSlash -> LinRoot = \vp -> emptyRoot ** {vp = vp ; c2 = vp.c2.s} ;
       mkRoot : CatSwe.Temp -> CatSwe.Pol -> CatSwe.VP -> LinRoot = \t,p,vp ->
        emptyRoot ** {temp = t ; pol = p ; vp = vp}
    } ;

    emptyRoot : LinRoot = defaultRoot ** {
       temp = presSimulTemp ;
       pol = positivePol ;
       vp = mkVP (P.mkN "dummy") ;
       c2 = []
    } ;

    presSimulTemp : CatSwe.Temp = mkTemp presentTense simultaneousAnt ;

    aclRelclRS_ : RS -> Adv = \rs -> lin Adv {s = embedInCommas (rs.s ! RS.agrP3 P.utrum CS.Sg ! CS.RNom)} ;

    -- Add an SC onto a LinRoot, e.g.
    -- (ready : LinRoot) (to_sleep : SC) -> ready to sleep
    scRoot : LinRoot -> SC -> LinRoot = \rt,sc -> advRoot rt <sc : Adv> ;

    -- Add an Adv onto a LinRoot, e.g.
    -- (critical : LinRoot) (always : Adv) -> always critical
    -- (warm : LinRoot) (by_nature : Adv) -> warm by nature
    advRoot : LinRoot -> Adv -> LinRoot = \rt,adv -> rt ** {
      vp = mkVP rt.vp adv ;
      np = N.AdvNP <lin NP (nplite2np rt.np) : NP> <adv:Adv> ;
    } ;

    -- Add a direct object onto a LinRoot, e.g.
    -- (eat : LinRoot) (food : NP) -> eat food
    dObjRoot : LinRoot -> NPLite -> LinRoot = \rt,np -> rt ** {
      np = ApposNP (nplite2np rt.np) (nplite2np np) ;
      vp = rt.vp ** {
             s2 = \\agr => rt.vp.s2 ! agr ++ (UttAccNP np).s
      } ;

      -- vp = mkVP (slashV rt.vp) np ;
      -- Doesn't work for gerunds: the main verb is in s2 field, and mkVP puts new object before
      -- so the result is "is personal data processing" for "is processing personal data"

    } ;

    emptyNP : NP = it_NP ** {s = \\_ => ""} ;
    emptySubj : Subj = that_Subj ** {s = ""} ;

    should_VV, shall_VV = must_VV ;

    rp2np : RP -> NP = \rp -> lin NP ({
      s = \\_ => rp.s ! P.utrum ! CS.Sg ! CS.RNom ;
      a = ragr2agr rp.a ;
      isPron = False
      }) ;

    ragr2agr : CS.RAgr -> CS.Agr = \ra -> case ra of {
      CS.RAg g n p => {g = g ; n = n ; p = p} ;
      CS.RNoAg => {g = P.utrum ; n = CS.Sg ; p = CS.P3}
    } ;

    -- copied this from ParseExtendSwe, easier to duplicate code than to introduce new dependency?
    -- ParseExtendComplVV : VV -> Ant -> CatSwe.Pol -> CatSwe.VP -> CatSwe.VP ;
    -- ParseExtendComplVV v ant pol vp =

    --   CS.insertObjPost (\\a => vv.c2.s ++ ant.s ++ pol.s
    --                      ++ CS.infVPPlus vp a ant.a pol.p)
    --                    (RS.predV vv) ;


}