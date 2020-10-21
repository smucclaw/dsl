concrete ActionEng of Action = TermEng **
open
  Prelude,
  (R=ResEng),
  (E=ExtendEng),
  (C=ConjunctionEng),
  (Extra=ExtraEng),
  SyntaxEng,
  ParadigmsEng,
  (N=NounEng),
  (V=VerbEng),
  (A=AdjectiveEng) in {

  lincat

    Action = LinAction ; -- Negations affect more than standard RGL negation does
    Action_Dir = SlashDir ;
    Action_Indir = SlashIndir ;
    Action_Dir_Indir = SlashDirIndir ;
    [Action] = ListLinAction ;
    [Action_Dir] = ListSlashDir ;
    [Action_Indir] = ListSlashIndir ;
    [Action_Dir_Indir] = ListSlashDirIndir ;

    Temporal = Tense ;

  linref
    Action = linAction ;

  lin
    -- : Temporal
    TPresent = presentTense ;
    TPast    = pastTense ;
    TFuture  = futureTense ;

    -----------------
    -- Complements --
    -----------------
    -- These decrease the valency of the Action.
    -- : Action_Dir -> Term -> Action ;
    AComplDir = complDir ;
    -- : Action_Indir -> Term -> Action ;
    AComplIndir = complIndir ;
    -- : Action_Dir_Indir -> Term -> Action_Indir ; -- sell stock (at fixed valuation)
    ASlashDir = slashDir ;
    -- : Action_Dir_Indir -> Term -> Action_Dir ;   -- sell (stock) at fixed valuation
    ASlashIndir = slashIndir ;


    ---------------------------------
    -- Valency changing operations --
    ---------------------------------

    -- Decrease valency: use the intransitive version of an action
    -- : Action_Dir -> Action ;   -- refund _ -> "issue a refund" ;
    ANoComplDir a = a ** {
      s = \\tmp,vc => case vc of {
            Passive => a.s ! tmp ! Passive ; -- passive already decreases valency
            Active => a.intrans ! tmp } ;
      } ;

    -- : Action_Indir -> Action ; -- same as above, but for indirect object
    -- ANoComplIndir a = a ** {s = a.intrans} ; -- is this useful?

    -------------
    -- Gerunds --
    -------------

    -- : Action -> ActionGerund ;        -- not selling X
    NegGerund action = action.gerund ! Neg ;

    -- : Action -> ActionGerund ;        -- selling X
    PosGerund action = action.gerund ! Pos ;

    ---------------
    -- Negations --
    ---------------

    -- : Action_Dir -> [Term] -> Action ; -- sells neither X, Y nor Z
    AComplNoneDir v2 obj =
      let none_of : NP = mkNP neither7nor_DConj obj ;
       in complDir v2 none_of ;

    --  AComplNoneIndir : Action_Indir -> [Term] -> Action ; -- sells (X) neither to A nor to B

    ---------------
    -- Relatives --
    ---------------
  lin
    -- : Term -> Term -> Temporal -> Action_Indir -> Term ;
    RelIndir iobj subj tense vpslash =
      let vp : LinAction = complIndir (vpslash ** {indir=emptyPrep}) emptyTerm ;
          rs : RS = relAction tense subj vpslash.indir vp ;
       in mkNP iobj rs ;

    -- : Term -> Term -> Temporal -> Action_Dir -> Term ;
    RelDir dobj subj tense vpslash =
      let vp : LinAction = complDir (vpslash ** {dir=emptyPrep}) emptyTerm ;
          rs : RS = relAction tense subj vpslash.dir vp ;
       in mkNP dobj rs ;

  oper
    relAction : Tense -> Term -> PrepPol -> LinAction -> RS = \tns,subj,prep,action ->
      let dummyRS : RS = mkRS (mkRCl (mkCl (mkN "dummy"))) ; -- to get all fields in RS and not touch RGL internals. TODO: eventually add this construction to Extend.
          pr : PrepPlus = prep ! Pos ; -- TODO check if negation works properly
          s : S = cl (tns2tmp tns.t) subj action ;
       in dummyRS ** {s = \\agr => pr.s ++ "which" ++ s.s} ;

    ------------------
    -- Conjunctions --
    ------------------
  lin
    BaseAction a1 a2 = {
      s = \\tmp,vc => E.BaseVPS (a1.s ! tmp ! vc) (a2.s ! tmp ! vc) ; -- doesnt sell X and doesnt issue Y
      gerund = \\p => mkListAdv (a1.gerund ! p) (a2.gerund ! p) ; -- not selling X and not issuing Y
      passSubject = mkListNP (np a1.passSubject) (np a2.passSubject) ;
      } ;
    ConsAction a as = as ** {
      s = \\tmp,vc => E.ConsVPS (a.s ! tmp ! vc) (as.s ! tmp ! vc) ;
      gerund = \\p => mkListAdv (a.gerund ! p) (as.gerund ! p) ;
      passSubject = mkListNP a.passSubject as.passSubject ;
      } ;
    ConjAction co as = {
      s = \\tmp,vc =>
        E.ConjVPS co (as.s ! tmp ! vc) ;
      gerund = \\p =>
        SyntaxEng.mkAdv co (as.gerund ! p) ;
      passSubject = mkNP co as.passSubject
      } ;

    BaseAction_Dir a1 a2 =
      let a1' : LinAction = complDir a1 emptyTerm ;
          a2' : LinAction = complDir a2 emptyTerm ;
      in BaseAction a1' a2' ** {
           intrans = \\tmp => E.BaseVPS (a1.intrans ! tmp) (a2.intrans ! tmp) ;
           dir = a1.dir ; -- : PrepPol
           indir = \\p => emptyAdv ; -- the existing indir has been incorporated in a1 and a2
         } ;
    ConsAction_Dir a as =
      let a' : LinAction = complDir a emptyTerm ;
      in ConsAction a' <as:ListLinAction> ** {
           intrans = \\tmp => E.ConsVPS (a.intrans ! tmp) (as.intrans ! tmp) ;
           dir = as.dir ; -- : PrepPol
           indir = \\p => emptyAdv
         } ;
    ConjSlashDir co as = ConjAction co as ** {
      intrans = \\tmp => E.ConjVPS co (as.intrans ! tmp) ;
      dir = as.dir ;
      indir = as.indir
      } ;

    BaseAction_Indir a1 a2 =
      let a1' : LinAction = complIndir a1 emptyTerm ;
          a2' : LinAction = complIndir a2 emptyTerm ;
      in BaseAction a1' a2' ** {
           intrans = \\tmp => E.BaseVPS (a1.intrans ! tmp) (a2.intrans ! tmp) ;
           indir = a1.indir ; -- : PrepPol
           dir = \\p => emptyAdv ; -- the existing dir has been incorporated in a1 and a2
         } ;
    ConsAction_Indir a as =
      let a' : LinAction = complIndir a emptyTerm ;
      in ConsAction a' <as:ListLinAction> ** {
           intrans = \\tmp => E.ConsVPS (a.intrans ! tmp) (as.intrans ! tmp) ;
           indir = as.indir ; -- : PrepPol
           dir = \\p => emptyAdv
         } ;

    ConjSlashIndir co as = ConjAction co as ** {
      intrans = \\tmp => E.ConjVPS co (as.intrans ! tmp) ;
      dir = as.dir ;
      indir = as.indir
      } ;

    BaseAction_Dir_Indir a1 a2 = BaseAction a1 a2 ** {
      intrans = \\tmp => E.BaseVPS (a1.intrans ! tmp) (a2.intrans ! tmp) ;
      dir = a2.dir ;
      indir = a2.indir
      } ;
    ConsAction_Dir_Indir a as = ConsAction a as ** {
      intrans = \\tmp => E.ConsVPS (a.intrans ! tmp) (as.intrans ! tmp) ;
      dir = as.dir ;
      indir = as.indir
      } ;
    ConjSlashDirIndir co as = ConjAction co as ** {
      intrans = \\tmp => E.ConjVPS co (as.intrans ! tmp) ;
      dir = as.dir ;
      indir = as.indir
      } ;

  param
    -- Merging tense and modality
    -- Polarity comes from Term
    TenseModPol = PMay | PMust | PShant | PPres Polarity | PFut Polarity ;
    Voice = Active | Passive ;

  oper
    -- Special VP construction.
    TnsPolAction : Type = TenseModPol => E.VPS;

    LinAction : Type = {
      s : TenseModPol => Voice => E.VPS ;
      gerund : Polarity => Adv ;
      passSubject : LinTerm ; -- If the sentence becomes passive, object becomes subject.
      } ;

    ListLinAction : Type = {
      s : TenseModPol => Voice => E.ListVPS ;
      gerund : Polarity => [Adv] ;
      passSubject : ListNP ;
      } ;

    linAction : LinAction -> Str = \l ->
      (mkUtt (cl (PPres Pos) emptyTerm l)).s ;

    mkVPS : Voice -> TenseModPol -> V2 -> E.VPS = \voice,tm,v2 ->
      let vp : VP = case voice of {
            Active => mkVP v2 emptyNP ;
            Passive => passiveVP v2 } ;
       in mkVPS' tm vp ;

    mkVPS' : TenseModPol -> VP -> E.VPS = \tm,vp ->
      let vp_t_p : VP*Tense*Pol = case tm of {
            PMay => <mkVP Extra.may_VV vp
                    ,presentTense
                    ,positivePol> ;
            PMust => <mkVP must_VV vp
                     ,presentTense
                     ,positivePol> ;
            PShant => <mkVP Extra.shall_VV vp
                      ,presentTense
                      ,negativePol> ;

            PPres Pos => <vp, presentTense, positivePol> ;
            PPres Neg => <vp, presentTense, negativePol> ;
            PFut Pos => <vp, futureTense, positivePol> ;
            PFut Neg => <vp, futureTense, negativePol>
            } ;
          vp' = vp_t_p.p1 ;
          tense : Tense = vp_t_p.p2 ;
          pol : Pol = vp_t_p.p3 ;
       in E.MkVPS (mkTemp tense simultaneousAnt) pol vp' ;

    emptyTerm : LinTerm = emptyNP ;

    ----------------------
    -- Slash categories --
    ----------------------

    mkGerSIntrans : V2 -> LinAction ** {intrans : TnsPolAction} = \v2 ->
      let linAction : LinAction = mkGerS v2  -- default: intransitive == s
       in linAction ** {intrans = \\tmp => linAction.s ! tmp ! Active} ;

    mkGerS : V2 -> LinAction = \v2 -> {
      s = \\tmp,vc => mkVPS vc tmp v2 ;
      gerund =
        let posAdv : Adv = E.GerundAdv (mkVP <v2:V2> emptyNP) ;
            negAdv : Adv = posAdv ** {s = "not" ++ posAdv.s}
        in table {
          Pos => posAdv ;
          Neg => negAdv } ;
      passSubject = emptyTerm ;
      } ;

    -- Action_Dir
    SlashDir : Type = LinAction ** {
      intrans : TnsPolAction ;
      indir : Polarity => Adv ; -- at fixed valuation / whether at fv nor without fv
      dir : PrepPol
      } ;
    mkDir = overload {
      mkDir : V2 -> SlashDir = \v2 -> mkGerSIntrans v2 ** {
        dir = prepPol v2.c2 ;
        indir = \\_ => emptyAdv ;
        } ;
      mkDir : V2 -> VP -> SlashDir = \v2,intransvp -> mkGerSIntrans v2 ** {
        dir = prepPol v2.c2 ;
        indir = \\_ => emptyAdv ;
        intrans = \\tmp => mkVPS' tmp intransvp }
      } ;
    slashDir : SlashDirIndir -> LinTerm -> SlashIndir = \vps,do -> vps ** {
      dir = applyPrepPol vps.dir do ;
      passSubject = do ; -- in case the Action becomes a passive sentence
      } ;
    complDir : SlashDir -> LinTerm -> LinAction = \action,do -> action ** {
      s = \\tmp,vc =>
        let vps : E.VPS = action.s ! tmp ! vc ;
            dirObj : Adv = applyPrepPol action.dir do ! tmp2pol tmp ;
            indirObj : Adv = action.indir ! tmp2pol tmp ;
         in case vc of {
              Passive => complS vps emptyAdv indirObj ;
              Active => complS vps dirObj indirObj } ;
      passSubject = do ;

      -- Gerunds won't be used in structures that need passive.
      gerund = \\p => complGer (action.gerund ! p)
                               (action.indir ! p)
                               (applyPrepPol action.dir do ! p)
      } ;

    -- Action_Indir
    SlashIndir : Type = LinAction ** {
      intrans : TnsPolAction ;
      dir : Polarity => Adv ; -- (Acme will/wont sell) some/any stock
      indir : PrepPol
      } ;
    mkIndir : V2 -> SlashIndir = \v2 -> mkGerSIntrans v2 ** {
      dir = \\_ => emptyAdv ;
      indir = prepPol v2.c2 ;
      } ;
    slashIndir : SlashDirIndir -> LinTerm -> SlashDir = \vps,io -> vps ** {
      indir = applyPrepPol vps.indir io
      } ;
    complIndir : SlashIndir -> LinTerm -> LinAction = \action,io -> action ** {
      s = \\tmp,vc =>
        let vps : E.VPS = action.s ! tmp ! vc ;
            dirObj : Adv = action.dir ! tmp2pol tmp ;
            indirObj : Adv = applyPrepPol action.indir io ! tmp2pol tmp ;
         in case vc of {
              Passive => complS vps emptyAdv indirObj ;
              Active => complS vps dirObj indirObj } ;
      gerund = \\p => complGer (action.gerund ! p)
                               (action.dir ! p)
                               (applyPrepPol action.indir io ! p)
      } ;


    -- _Dir_Indir
    SlashDirIndir : Type = LinAction ** {
      intrans : TnsPolAction ;
      dir,
      indir : PrepPol ;
      } ;
    mkDirIndir = overload {
      mkDirIndir : V3 -> SlashDirIndir = \v3 -> mkGerSIntrans v3 ** {
        dir = prepPol v3.c2 ;
        indir = prepPol v3.c3
        } ;
      mkDirIndir : V3 -> PrepPol -> SlashDirIndir = \v3,indir -> mkGerSIntrans v3 ** {
        indir = indir ;
        dir = prepPol v3.c2
        }
      } ;
    -- PrepPol is more powerful than Prep: prepared for multilayer negations
    PrepPol : Type = Polarity => PrepPlus ;
    PrepPlus : Type = {  -- Positive version  / Negative version
      s : Str ;      -- at (fixed valuation) / whether at (fixed valuation)
      post : Str ;   -- ∅                    / or without
      redupl : Bool  -- False                / True       (fixed valuation)
      } ;

    prepPol = overload {
      prepPol : Str -> PrepPol = \p -> \\pol => {
        s = p ;
        post = [] ;
        redupl = False
        } ;
      prepPol : (p,n : PrepPlus) -> PrepPol = \pos,neg -> table {
        Pos  => pos ;
        Neg  => neg
        }
      } ;

    prepPlus : (s,post : Str) -> (redupl : Bool) -> PrepPlus = \s,post,r -> {
      s = s ;
      post = post ;
      redupl = r
      } ;

    emptyPrep : PrepPol = prepPol "" ;

    applyPrepPol : PrepPol -> LinTerm -> (Polarity=>Adv) = \pp,term -> \\pol =>
      let np : NP = term ; -- ! cpol2pol pol ;
          npacc : Str = np.s ! R.NPAcc ;
          prep : PrepPlus = pp ! pol
      in lin Adv {
        s = prep.s ++ npacc ++ prep.post ++ case prep.redupl of {
                                                True => npacc ;
                                                False => [] }
      } ;

    -- helpers for complDir and complIndir
    complS : E.VPS -> Adv -> Adv -> E.VPS = \vps,dir,indir -> lin VPS {
      s = \\a => vps.s ! a ++ dir.s ++ indir.s
      } ;
    complGer : (a,b,c : Adv) -> Adv = \ger,indir,dir -> lin Adv {
      s = ger.s ++ dir.s ++ indir.s
      } ;

    -------------------
    -- List versions --
    -------------------
    ListSlashDir : Type = ListLinAction ** {
      intrans : TenseModPol => E.ListVPS ;
      indir : Polarity => Adv ; -- at fixed valuation / whether at fv nor without fv
      dir : PrepPol ;
      } ;

    ListSlashIndir : Type = ListLinAction ** {
      intrans : TenseModPol => E.ListVPS ;
      dir : Polarity => Adv ; -- (Acme will/wont sell) some/any stock
      indir : PrepPol ;
      } ;

    ListSlashDirIndir : Type = ListLinAction ** {
      intrans : TenseModPol => E.ListVPS ;
      dir,
      indir : PrepPol ;
      } ;

    ---------------------
    -- Generic helpers --
    ---------------------
    cl : TenseModPol -> LinTerm -> LinAction -> S = \tmp,subj,pred ->
      E.PredVPS (np subj) (pred.s ! tmp ! Active) ;

    tmp2pol : TenseModPol -> Polarity = \p -> case p of {
      PShant => Neg ;
      PPres p => p ;
      PFut p  => p ;
      _ => Pos
      } ;

    tns2tmp : R.Tense -> TenseModPol = \tns -> case tns of {
      R.Fut => PFut Pos ;
      _ => PPres Pos
      } ;

}
