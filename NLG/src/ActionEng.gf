concrete ActionEng of Action = TermEng **
open
  Prelude,
  Coordination,
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

    -- : Action_Dir -> Action_Dir_Indir
    Dat action = action ** {
      indir = postCompoundPrep action.indir datPrep ;
      } ;

    -- : Action_Dir -> Action_Dir_Indir
    -- like previous but flip dir and indir
    -- DatDir action = action ** {
    --   dir = postCompoundPrep action.indir datPrep ;
    --   indir = action.dir ;
    --   } ;

    -- Decrease valency: use the intransitive version of an action
    -- TODO: rethink this, weird interference with passive
    -- : Action_Dir -> Action ;   -- refund _ -> "issue a refund" ;
    ANoComplDir a = a ** {s = a.intrans} ;

    -- : Action_Indir -> Action ; -- same as above, but for indirect object
    -- ANoComplIndir a = a ** {s = a.intrans} ; -- is this useful?

    -------------
    -- Gerunds --
    -------------

    -- : Action -> ActionGerund ;        -- not selling X
    NegGerund action = action.inf.gerund ! Neg ;

    -- : Action -> ActionGerund ;        -- selling X
    PosGerund action = action.inf.gerund ! Pos ;

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
      let dummyRS : RS = mkRS tns (mkRCl (mkCl emptyNP)) ; -- to get all fields in RS and not touch RGL internals. TODO: eventually add this construction to Extend.
          pr : PrepPlus = prep ! Pos ; -- TODO check if negation works properly
          s : S = cl (tns2vf tns.t) subj action ;
       in dummyRS ** {s = \\agr => pr.s ++ "which" ++ s.s ++ tns.s} ;

    ------------------
    -- Conjunctions --
    ------------------
  lin
    BaseAction a1 a2 = {
      s = \\vf => E.BaseVPS (a1.s ! vf) (a2.s ! vf) ; -- [sells X and issues Y]
      inf = {  -- shan't/may/must [transfer Z , sell X and issue Y]
        gerund = \\p => mkListAdv (a1.inf.gerund ! p) (a2.inf.gerund ! p) ; -- (not) selling X and (not) issuing Y
        passSubj = mkListNP (np a1.inf.passSubj) (np a2.inf.passSubj) ;
        infComp = twoTable2 Voice Polarity a1.inf.infComp a2.inf.infComp }
      } ;

    ConsAction a as = as ** {
      s = \\vf => E.ConsVPS (a.s ! vf) (as.s ! vf) ;
      inf = {
        gerund = \\p => mkListAdv (a.inf.gerund ! p) (as.inf.gerund ! p) ;
        passSubj = mkListNP (np a.inf.passSubj) as.inf.passSubj ;
        infComp = consrTable2 Voice Polarity "," a.inf.infComp as.inf.infComp }
      } ;

    ConjAction co as = {
      s = \\vf => E.ConjVPS <co:Conj> (as.s ! vf) ;
      inf = {
        gerund = \\p => SyntaxEng.mkAdv co (as.inf.gerund ! p) ;
        passSubj = mkNP co as.inf.passSubj ;
        infComp = conjunctDistrTable2 Voice Polarity co as.inf.infComp }
      } ;

    OtherwiseAction_Dir a = a ** OtherwiseAction a ;

    OtherwiseAction a = a ** {
      s = \\vf => let vps : E.VPS = a.s ! vf in vps ** {s = \\agr => "otherwise" ++ vps.s ! agr} ;
      inf = a.inf ** {
        infComp = {s = \\vc,p => "otherwise" ++ a.inf.infComp.s ! vc ! p} ;
        gerund = \\p => prefixSS "otherwise" (a.inf.gerund ! p) ;
        }
      } ;

  lin

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
      intrans = \\vf => E.ConjVPS co (as.intrans ! vf) ;
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
           intrans = \\vf => E.ConsVPS (a.intrans ! vf) (as.intrans ! vf) ;
           indir = as.indir ; -- : PrepPol
           dir = \\p => emptyAdv
         } ;

    ConjSlashIndir co as = ConjAction co as ** {
      intrans = \\vf => E.ConjVPS co (as.intrans ! vf) ;
      dir = as.dir ;
      indir = as.indir
      } ;

    BaseAction_Dir_Indir a1 a2 = BaseAction a1 a2 ** {
      intrans = \\vf => E.BaseVPS (a1.intrans ! vf) (a2.intrans ! vf) ;
      dir = a2.dir ;
      indir = a2.indir
      } ;
    ConsAction_Dir_Indir a as = ConsAction a as ** {
      intrans = \\vf => E.ConsVPS (a.intrans ! vf) (as.intrans ! vf) ;
      dir = as.dir ;
      indir = as.indir
      } ;
    ConjSlashDirIndir co as = ConjAction co as ** {
      intrans = \\vf => E.ConjVPS co (as.intrans ! vf) ;
      dir = as.dir ;
      indir = as.indir
      } ;

  param
    -- Merging tense and modality
    -- Polarity comes from Term
    TenseModPol =
        PMay Polarity
      | PMust
      | PShant
      | PPres Polarity
      | PPast Polarity
      | PFut Polarity ;

    -- The only forms in which the actual verb inflects in our grammar:
    -- raise, raises, raised
    FinVF = Present | Past ;

    Anteriority = Sim | Ant ;
    Voice = VAct Anteriority | VPass ;

  oper

    -- Special VP construction.
    -- Normal VPs don't allow lists, that's why we use Extend.VPS.
    -- Seriously, if the RGL allowed VP coordination, this grammar would be much less hassle.
    LinAction : Type = {
      s : FinForms ;
      inf : InfForms ;
      } ;

    ListLinAction : Type = {
      s : ListFinForms ;
      inf : ListInfForms
      } ;

    linAction : LinAction -> Str = \l ->
      (mkUtt (cl Present emptyTerm l)).s ;

    -- Finite verb forms
    FinForms : Type = FinVF => E.VPS ;
    ListFinForms : Type = FinVF => E.ListVPS ;

    -- Non-finite verb forms: Passive constructions and anything with auxiliary
    -- Active:  may (not)/must/will/won't/… [cook the potato]
    -- Passive: may (not)/must/will/won't be [cooked]

    InfComp : Type = {s : Voice => Polarity => Str} ;
    ListInfComp : Type = ListTable2 Voice Polarity ;

    PolAdv : Type = Polarity => Adv ;
    emptyPolAdv : PolAdv = \\_ => emptyAdv ;

    InfForms : Type = {
      passSubj : LinTerm ;     -- cabbage
      infComp : InfComp ;      -- cooked (at a fixed valuation / whether at a fv or without fv)
      gerund : PolAdv -- cooking the cabbage
      } ;
    ListInfForms : Type = {
      passSubj : ListNP ;      -- cabbage and refund
      infComp : ListInfComp ;  -- cooked and issued at a fixed valuation
      gerund : Polarity => [Adv]
      } ;

    predVPS : Voice -> TenseModPol -> InfForms -> E.VPS = \voice,tmp,p ->
      let s : Str = p.infComp.s ! voice ! tmp2pol tmp ;
          compV : V = mkV s s s s s ; -- to prevent pattern matching on runtime strings
          vp : VP = case voice of {
                      VPass => mkVP (ParadigmsEng.mkAdv s) ;
                      VAct _ => mkVP compV } ;
       in mkVPS tmp vp ;


    finVPS : FinVF -> VP -> E.VPS = \finvf,vp ->
      let tense : Tense = case finvf of {
            Present => presentTense ;
            Past => pastTense }
       in E.MkVPS (mkTemp tense simultaneousAnt) positivePol vp ;


    mkVPS : TenseModPol -> VP -> E.VPS = \tm,vp ->
      let vp_t_p : VP*Tense*Pol = case tm of {
            PMay Pos => <mkVP Extra.may_VV vp
                       ,presentTense
                       ,positivePol> ;
            PMay Neg => <mkVP Extra.may_VV vp
                       ,presentTense
                       ,negativePol> ;
            PMust => <mkVP must_VV vp
                     ,presentTense
                     ,positivePol> ;
            PShant => <mkVP Extra.shall_VV vp
                      ,presentTense
                      ,negativePol> ;

            PPres Pos => <vp, presentTense, positivePol> ;
            PPres Neg => <vp, presentTense, negativePol> ;
            PPast Pos => <vp, pastTense, positivePol> ;
            PPast Neg => <vp, pastTense, negativePol> ;
            PFut Pos => <vp, futureTense, positivePol> ;
            PFut Neg => <vp, futureTense, negativePol>
            } ;
          vp' = vp_t_p.p1 ;
          tense : Tense = vp_t_p.p2 ;
          pol : Pol = vp_t_p.p3 ;
       in E.MkVPS (mkTemp tense simultaneousAnt) pol vp' ;

    emptyTerm : LinTerm = emptyNP ;

    {-- Force the VPS to look like infinitive.
    -- Massive hack, only works for English. For next language, rethink the lincats.
    infVPS : E.VPS -> E.VPS = \vps -> vps ** {
      s = \\_ => let emptyPlNP : NP = they_NP ** {s = \\_ => []}
                  in (E.PredVPS emptyPlNP vps).s ;
      } ;
    infListVPS : E.ListVPS -> E.ListVPS = \listvps -> listvps ** {
      s1 = let vps : E.VPS = lin VPS {s = listvps.s1}
            in (infVPS vps).s ;

      s2 = let vps : E.VPS = lin VPS {s = listvps.s2}
            in (infVPS vps).s ;
      } ; --}

    vps2str : E.VPS -> Str = \vps -> (E.PredVPS emptyNP vps).s ;

    ----------------------
    -- Slash categories --
    ----------------------

    mkGerSIntrans : V2 -> LinAction ** {intrans : FinForms} = \v2 ->
      let linAction : LinAction = mkGerS v2  -- default: intransitive == s
       in linAction ** {intrans = linAction.s} ;

    mkGerS : V2 -> LinAction = \v2 -> let vp : VP = mkVP v2 emptyNP in {
      s = \\vf => finVPS vf vp ;
      inf = {
        passSubj = emptyTerm ;
        infComp = { -- NB. polarity only applies to the potential indirect object
          s = \\vc,p => case vc of {
                          VAct Sim => v2.s ! R.VInf ;
                          VAct Ant => v2.s ! R.VPPart ; -- can't extract this form out of VP anymore
                          VPass => v2.s ! R.VPast }
          } ;
        gerund =
          let posAdv : Adv = E.GerundAdv vp ;
              negAdv : Adv = posAdv ** {s = "not" ++ posAdv.s}
          in table {
            Pos => posAdv ;
            Neg => negAdv }
        }
      } ;

    -- Action_Dir
    SlashDir : Type = LinAction ** {
      intrans : FinForms ;
      indir : PolAdv ; -- at fixed valuation / whether at fv nor without fv
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
        intrans = \\vf => finVPS vf intransvp }
      } ;
    slashDir : SlashDirIndir -> LinTerm -> SlashIndir = \action,do ->
      let doAdv : PolAdv = applyPrepPol action.dir do in action ** {
      dir = doAdv ;
      inf = action.inf ** {
        passSubj = np do ; -- in case the Action becomes a passive sentence
        infComp = doComp action doAdv }
        } ;
    complDir : SlashDir -> LinTerm -> LinAction = \action,do ->
      let doAdv : Polarity=>Adv = applyPrepPol action.dir do in action ** {
        s = \\vf => complS (action.s ! vf) action.indir doAdv ;
        inf = action.inf ** {
          passSubj = np do ;
          infComp = doComp action doAdv ;
          gerund = complGer action.inf.gerund action.indir doAdv
          }
      } ;


    -- Action_Indir
    SlashIndir : Type = LinAction ** {
      intrans : FinForms ;
      dir : PolAdv ; -- (Acme will/wont sell) some/any stock
      indir : PrepPol
      } ;
    mkIndir : V2 -> SlashIndir = \v2 -> mkGerSIntrans v2 ** {
      dir = \\_ => emptyAdv ;
      indir = prepPol v2.c2 ;
      } ;
    slashIndir : SlashDirIndir -> LinTerm -> SlashDir = \action,io ->
      let ioAdv : PolAdv = applyPrepPol action.indir io in action ** {
        indir = ioAdv ;
        intrans = \\fv => complS (action.intrans ! fv) emptyPolAdv ioAdv ;
        inf = action.inf ** {infComp = ioComp action ioAdv}
      } ;
    complIndir : SlashIndir -> LinTerm -> LinAction = \action,io ->
      let ioAdv : Polarity=>Adv = applyPrepPol action.indir io in action ** {
        s = \\vf => complS (action.s ! vf) action.dir ioAdv ;
        inf = action.inf ** {
          infComp = ioComp action ioAdv ;
          gerund = complGer action.inf.gerund action.dir ioAdv
          }
      } ;

    ioComp : {inf : InfForms} -> (Polarity=>Adv) -> InfComp = \a,ioAdv -> {
      s = \\vc,p => a.inf.infComp.s ! vc ! p ++ (ioAdv ! p).s
      } ;

    doComp : {inf : InfForms} -> (Polarity=>Adv) -> InfComp = \a,doAdv -> {
      s = \\vc,p => case vc of {
        VAct _ => a.inf.infComp.s ! vc ! p ++ (doAdv ! p).s ;
        VPass  => a.inf.infComp.s ! vc ! p -- don't add direct object to passive
        }
      } ;

    -- _Dir_Indir
    SlashDirIndir : Type = LinAction ** {
      intrans : FinForms ;
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
    datPrep : PrepPol = prepPol "to" ;

    -- preCompoundPrep : Polarity=>Adv -> PrepPol -> PrepPol = \polAdv,prep -> \\pol =>
    --   let prep : PrepPlus = prep ! pol ;
    --       pref : Str = (polAdv ! pol).s
    --    in prep ** {s = pref ++ prep.s} ;

    postCompoundPrep : (Polarity=>Adv) -> PrepPol -> PrepPol = \polAdv,prep -> \\pol =>
      let prep : PrepPlus = prep ! pol ;
          postf : Str = (polAdv ! pol).s
       in prep ** {post = prep.post ++ postf} ;


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
    complS : E.VPS -> PolAdv -> PolAdv -> E.VPS = \vps,a,b ->
      let aAdv : Adv = a ! Pos ;
          bAdv : Adv = b ! Pos ;
       in vps ** {s = \\agr => vps.s ! agr ++ aAdv.s ++ bAdv.s} ;

    complGer : (a,b,c : PolAdv) -> PolAdv = \g,a,b -> \\p =>
      let ger : Adv = g ! p ;
          aAdv : Adv = a ! p ;
          bAdv : Adv = b ! p ;
       in ger ** {s = ger.s ++ aAdv.s ++ bAdv.s} ;

    -------------------
    -- List versions --
    -------------------
    ListSlashDir : Type = ListLinAction ** {
      intrans : ListFinForms  ;
      indir : PolAdv ; -- at fixed valuation / whether at fv nor without fv
      dir : PrepPol ;
      } ;

    ListSlashIndir : Type = ListLinAction ** {
      intrans : ListFinForms  ;
      dir : PolAdv ; -- (Acme will/wont sell) some/any stock
      indir : PrepPol ;
      } ;

    ListSlashDirIndir : Type = ListLinAction ** {
      intrans : ListFinForms  ;
      dir,
      indir : PrepPol ;
      } ;

    ---------------------
    -- Generic helpers --
    ---------------------
    cl : FinVF -> LinTerm -> LinAction -> S = \vf,subj,pred ->
      E.PredVPS (np subj) (pred.s ! vf) ;

    tmp2pol : TenseModPol -> Polarity = \p -> case p of {
      PShant => Neg ;
      PMay p => p ;
      PPres p => p ;
      PFut p  => p ;
      PPast p => p ;
      PMust => Pos
      } ;

    fv2pol : FinVF -> Polarity = \_ -> Pos ;


    tns2vf : R.Tense -> FinVF = \tns -> case tns of {
      --R.Fut => PFut Pos ;
      R.Past => Past ;
      _ => Present
      } ;

}
