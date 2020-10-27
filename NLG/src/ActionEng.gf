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
    -- : Action_Dir -> Action ;   -- refund _ -> "issue a refund" ;
    ANoComplDir a = a ** {
      s = a.intrans ;
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
      let dummyRS : RS = mkRS tns (mkRCl (mkCl emptyNP)) ; -- to get all fields in RS and not touch RGL internals. TODO: eventually add this construction to Extend.
          pr : PrepPlus = prep ! Pos ; -- TODO check if negation works properly
          s : S = cl (tns2tmp tns.t) subj action ;
       in dummyRS ** {s = \\agr => pr.s ++ "which" ++ s.s ++ tns.s} ;

    ------------------
    -- Conjunctions --
    ------------------
  lin
    BaseAction a1 a2 = {
      s = \\tmp,vc =>
        let infvps : E.VPS =
              case vc of {
                Passive => infVPS (a2.s ! PPast Pos ! Active) ;
                Active => case tmp of {
                  PPast Pos|PPres Pos => a2.s ! tmp ! vc ; -- [sells X and issues Y]
                  _ => infVPS (a2.s ! PPres Pos ! Active) -- shan't/may/must [sell X and issue Y]
                  }
              } ;
         in E.BaseVPS (a1.s ! tmp ! vc) infvps ;
      gerund = \\p => mkListAdv (a1.gerund ! p) (a2.gerund ! p) ; -- not selling X and not issuing Y
      passSubject = mkListNP (np a1.passSubject) (np a2.passSubject) ;
      } ;
    ConsAction a as = as ** {
      s = \\tmp,vc =>
        let infvps : E.ListVPS =
              case vc of {
                Passive => infListVPS (as.s ! PPast Pos ! Active) ;
                Active => case tmp of {
                  PPast Pos|PPres Pos => as.s ! tmp ! vc ; -- [sells X and issues Y]
                  _ => infListVPS (as.s ! PPres Pos ! Active) -- shan't/may/must [sell X and issue Y]
                  }
              } ;
         in E.ConsVPS (a.s ! tmp ! vc) infvps ; -- shan't/may/must [transfer Z , sell X and issue Y]
      gerund = \\p => mkListAdv (a.gerund ! p) (as.gerund ! p) ;
      passSubject = mkListNP (np a.passSubject) as.passSubject ;
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
    TenseModPol =
        PMay Polarity
      | PMust
      | PShant
      | PPres Polarity
      | PPast Polarity
      | PFut Polarity ;
    Voice = VAct | VPass ;

  oper

    -- Special VP construction.
    -- Normal VPs don't allow lists, that's why we use Extend.VPS
    LinAction : Type = {
      s : TenseModPol => E.VPS ;
      gerund : Polarity => Adv ;
      passive : Passive ;
      } ;

    ListLinAction : Type = {
      s : TenseModPol => E.ListVPS ;
      gerund : Polarity => [Adv] ;
      passive : ListPassive ;
      } ;

    linAction : LinAction -> Str = \l ->
      (mkUtt (cl (PPres Pos) emptyTerm l)).s ;

    -- Passive constructions
    Passive : Type = {
      passSubj : LinTerm ;          -- cabbage
      passComp : {s : Polarity => Str} ;  -- cooked at a fixed valuation / whether at a fv or without fv
      } ;
    ListPassive : Type = {
      passSubj : ListNP ;           -- cabbage and refund
      passComp : ListTable Polarity -- cooked and issued at a fixed valuation
      } ;

    passVPS : TenseModPol -> Passive -> E.VPS = \tmp,p ->
      let compAdv : Adv = lin Adv {s = p.passComp.s ! tmp2pol tmp} ;
       in mkVPS tmp (mkVP compAdv) ;

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

    -- Force the VPS to look like infinitive.
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
      } ;

    vps2str : E.VPS -> Str = \vps -> (E.PredVPS emptyNP vps).s ;

    ----------------------
    -- Slash categories --
    ----------------------
    TnsPolAction : Type = TenseModPol => E.VPS;

    mkGerSIntrans : V2 -> LinAction ** {intrans : TnsPolAction} = \v2 ->
      let linAction : LinAction = mkGerS v2  -- default: intransitive == s
       in linAction ** {intrans = linAction.s} ;

    mkGerS : V2 -> LinAction = \v2 -> let vp : VP = mkVP v2 emptyNP in {
      s = \\tmp => mkVPS tmp vp ;
      passive = {
        passSubj = emptyTerm ;
        passComp = {s = \\p => vps2str (mkVPS (PPast Pos) vp)} ; -- NB. polarity only applies to the potential indirect object!
        } ;
      gerund =
        let posAdv : Adv = E.GerundAdv (mkVP <v2:V2> emptyNP) ;
            negAdv : Adv = posAdv ** {s = "not" ++ posAdv.s}
        in table {
          Pos => posAdv ;
          Neg => negAdv } ;
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
        intrans = \\tmp => mkVPS tmp intransvp }
      } ;
    slashDir : SlashDirIndir -> LinTerm -> SlashIndir = \action,do -> action ** {
      dir = applyPrepPol action.dir do ;
      passive = action.passive ** {
        passSubj = np do -- in case the Action becomes a passive sentence
        }
      } ;
    complDir : SlashDir -> LinTerm -> LinAction = \action,do ->
      let doAdv : Polarity=>Adv = applyPrepPol action.dir do in action ** {
        s = \\tmp => complS (action.s ! tmp)
                            (action.indir ! tmp2pol tmp)
                            (doAdv ! tmp2pol tmp) ;

        passive = action.passive ** {
          passSubj = do
          } ;

        gerund = \\p => complGer (action.gerund ! p)
                                 (action.indir ! p)
                                 (doAdv ! p)
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
    slashIndir : SlashDirIndir -> LinTerm -> SlashDir = \action,io ->
      let ioAdv : Polarity => Adv = applyPrepPol action.indir io in action ** {
        indir = ioAdv ;
        intrans = \\tmp => complS (action.intrans ! tmp) emptyAdv (ioAdv ! tmp2pol tmp) ;
        passive = action.passive ** {passComp = ioComp action.passive ioAdv}
      } ;

    complIndir : SlashIndir -> LinTerm -> LinAction = \action,io ->
      let ioAdv : Polarity=>Adv = applyPrepPol action.indir io in action ** {
        s = \\tmp => complS (action.s ! tmp)
                            (action.dir ! tmp2pol tmp)
                            (ioAdv ! tmp2pol tmp) ;

        passive = action.passive ** {passComp = ioComp action.passive ioAdv} ;

        gerund = \\p => complGer (action.gerund ! p)
                                 (action.dir ! p)
                                 (ioAdv ! p)
      } ;

    ioComp : Passive -> (Polarity=>Adv) -> {s : Polarity => Str} = \pass,ioAdv -> {
      s = \\p => pass.passComp.s ! p ++ (ioAdv ! p).s
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
      E.PredVPS (np subj) (pred.s ! tmp) ;

    tmp2pol : TenseModPol -> Polarity = \p -> case p of {
      PShant => Neg ;
      PMay p => p ;
      PPres p => p ;
      PFut p  => p ;
      PPast p => p ;
      PMust => Pos
      } ;

    tns2tmp : R.Tense -> TenseModPol = \tns -> case tns of {
      R.Fut => PFut Pos ;
      R.Past => PPast Pos ;
      _ => PPres Pos
      } ;

}
