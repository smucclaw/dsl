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
    [Action_Dir] = ListLinAction ** {
      intrans : ListFinForms  ;
      indir : PolAdv ; -- at fixed valuation / whether at fv nor without fv
      dir : PrepPol ;
      } ;
    [Action_Indir] = ListLinAction ** {
      intrans : ListFinForms  ;
      indir : PrepPol ;
      dir : PolAdv ; -- (Acme will/wont sell) some/any stock
      } ;
    [Action_Dir_Indir] = ListLinAction ** {
      intrans : ListFinForms  ;
      dir,indir : PrepPol ;
      } ;

    Temporal = Tense ;

  linref
    Action = \l -> (uttAction l).s ;
  oper
    uttAction : LinAction -> Utt = \l -> mkUtt (E.PredVPS emptyNP (l.s ! Present)) ;

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
    -- like previous but flip dir and indir. Probably not useful.
    -- DatDir action = action ** {
    --   dir = postCompoundPrep action.indir datPrep ;
    --   indir = action.dir ;
    --   } ;

    -- Decrease valency: use the intransitive version of an action
    {- TODO: rethink this, weird interference with passive and infinite forms
    -- : Action_Dir -> Action ;   -- refund _ -> "issue a refund" ;
     ANoComplDir a = a ** {s = a.intrans} ;

    -- : Action_Indir -> Action ; -- same as above, but for indirect object
    ANoComplIndir a = a ** {s = a.intrans} ; -- is this useful? -}

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
          vps : E.VPS = action2VPS (VAct Sim) (tns2tmp tns.t) action ;
          s : S = E.PredVPS subj vps ;
       in dummyRS ** {s = \\agr => pr.s ++ "which" ++ s.s ++ tns.s} ;


    ---------------
    -- Otherwise --
    ---------------
  lin
    OtherwiseAction_Dir a = a ** OtherwiseAction a ;

    OtherwiseAction a = a ** {
      s = \\vf => let vps : E.VPS = a.s ! vf in vps ** {s = \\agr => "otherwise" ++ vps.s ! agr} ;
      pass = a.pass ** {
        comp = \\p => prefixSS "otherwise" (a.pass.comp ! p)
        } ;
      inf = a.inf ** {
        comp = \\p => let vpi : E.VPI = a.inf.comp ! p in vpi ** {s = \\vv,agr => "otherwise" ++ vpi.s ! vv ! agr} ;
        gerund = \\p => prefixSS "otherwise" (a.inf.gerund ! p) ;
        }
      } ;

    ------------------
    -- Conjunctions --
    ------------------
  lin
    BaseAction a1 a2 = {
      s = mkListFinForms a1.s a2.s ; -- [sells X and issues Y]
      pass = {
        subj = mkListNP (np a1.pass.subj) (np a2.pass.subj) ;
        comp = mkListPolAdv a1.pass.comp a2.pass.comp } ;
      inf = {  -- shan't/may/must [transfer Z , sell X and issue Y]
        gerund = mkListPolAdv a1.inf.gerund a2.inf.gerund ; -- (not) selling X and (not) issuing Y
        comp = mkListInfComp a1.inf.comp a2.inf.comp }
      } ;

    ConsAction a as = as ** {
      s = mkListFinForms a.s as.s ;
      pass = {
        subj = mkListNP (np a.pass.subj) as.pass.subj ;
        comp = mkListPolAdv a.pass.comp as.pass.comp } ;
      inf = {
        gerund = mkListPolAdv a.inf.gerund as.inf.gerund ;
        comp = mkListInfComp a.inf.comp as.inf.comp }
      } ;

    ConjAction co as = {
      s = mkFinForms co as.s ;
      pass = {
        subj = mkNP co as.pass.subj ;
        comp = mkPolAdv co as.pass.comp } ;
      inf = {
        gerund = mkPolAdv co as.inf.gerund ;
        comp = mkInfComp co as.inf.comp }
      } ;

    -- /Dir
    BaseAction_Dir a1 a2 =
      let a1' : LinAction = complDir a1 emptyTerm ;
          a2' : LinAction = complDir a2 emptyTerm ;
      in BaseAction a1' a2' ** {
           intrans = mkListFinForms a1.intrans a2.intrans ;
           dir = a1.dir ; -- : PrepPol
           indir = emptyPolAdv ; -- the existing indir has been incorporated in a1' and a2'
         } ;
    ConsAction_Dir a as =
      let a' : LinAction = complDir a emptyTerm ;
      in ConsAction a' <as:ListLinAction> ** {
           intrans = mkListFinForms a.intrans as.intrans ;
           dir = as.dir ; -- : PrepPol
           indir = emptyPolAdv
         } ;
    ConjSlashDir co as =
      let a : LinAction = ConjAction co as in a ** {
      intrans = mkFinForms co as.intrans ;
      pass = a.pass ** {subj = emptyTerm} ;
      dir = as.dir ;
      indir = as.indir
      } ;

    -- /Indir
    BaseAction_Indir a1 a2 =
      let a1' : LinAction = complIndir a1 emptyTerm ;
          a2' : LinAction = complIndir a2 emptyTerm ;
      in BaseAction a1' a2' ** {
           intrans = mkListFinForms a1.intrans a2.intrans ;
           indir = a1.indir ; -- : PrepPol
           dir = emptyPolAdv ; -- the existing dir has been incorporated in a1' and a2'
         } ;
    ConsAction_Indir a as =
      let a' : LinAction = complIndir a emptyTerm ;
      in ConsAction a' <as:ListLinAction> ** {
           intrans = mkListFinForms a.intrans as.intrans ;
           indir = as.indir ; -- : PrepPol
           dir = emptyPolAdv
         } ;

    ConjSlashIndir co as = let a : LinAction = ConjAction co as in a ** {
      intrans = mkFinForms co as.intrans ;
      dir = as.dir ;
      indir = as.indir
      -- /Indir has already pass.subj, don't replace it with emptyTerm
      } ;

    -- /Dir/Indir
    BaseAction_Dir_Indir a1 a2 = BaseAction a1 a2 ** {
      intrans = mkListFinForms a1.intrans a2.intrans ;
      dir = a2.dir ;
      indir = a2.indir
      } ;
    ConsAction_Dir_Indir a as = ConsAction a as ** {
      intrans = mkListFinForms a.intrans as.intrans ;
      dir = as.dir ;
      indir = as.indir
      } ;
    ConjSlashDirIndir co as = let a : LinAction = ConjAction co as in a ** {
      intrans = mkFinForms co as.intrans ;
      dir = as.dir ;
      indir = as.indir ;
      pass = a.pass ** {subj = emptyTerm} ; -- /Dir didn't have pass.subj, so make sure it stays empty
      } ;


    --------------------------
    -- Internal definitions --
    --------------------------

  param
    -- Merging tense and modality
    TenseModPol =
        PMay Polarity    -- Polarity comes from TermEng
      | PMust
      | PShant
      | PPres Polarity
      | PPast Polarity
      | PFut Polarity ;

    -- The only forms in which the actual verb inflects in our grammar
    FinVF = Present | Past ; -- raise/raises, raised

    Anteriority = Sim | Ant ;
    Voice = VAct Anteriority | VPass ;

  oper

    -- RGL VP doesn't allow coordination, that's why we use Extend.VPS and Extend.VPI
    -- We need to store non-finite forms (in VPI) due to adverbs:
    --   "may not otherwise issue stock", not *"otherwise may not issue stock".
    LinAction : Type = {
      s : FinForms ;
      pass : PassForms ;
      inf : InfForms ;
      } ;

    ListLinAction : Type = {
      s : ListFinForms ;
      pass : ListPassForms ;
      inf : ListInfForms
      } ;

    mkVPS : Tense -> Pol -> VP -> E.VPS = \tense,pol,vp ->
      E.MkVPS (mkTemp tense simultaneousAnt) pol vp ;


    -- Finite verb forms
    FinForms : Type = FinVF => E.VPS ;
    ListFinForms : Type = FinVF => E.ListVPS ;

    mkFinForms : Conj -> ListFinForms -> FinForms = \co,vfs ->
      \\vf => E.ConjVPS co (vfs ! vf) ;
    mkListFinForms = overload {
      mkListFinForms : (a, b : FinForms) -> ListFinForms = \a,b ->
        \\p => E.BaseVPS (a ! p) (b ! p) ;
      mkListFinForms : FinForms -> ListFinForms -> ListFinForms = \a,as ->
        \\p => E.ConsVPS (a ! p) (as ! p)
      } ;

    -- We store complements in the following type: adverb depending on polarity
    -- Pos => at a fixed valuation ; Neg => whether at a fv or without fv
    PolAdv : Type = Polarity => Adv ;
    ListPolAdv : Type = Polarity => [Adv] ;

    emptyPolAdv : PolAdv = \\_ => emptyAdv ;

    mkPolAdv : Conj -> ListPolAdv -> PolAdv = \co,as ->
      \\p => SyntaxEng.mkAdv co (as ! p) ;
    mkListPolAdv = overload {
      mkListPolAdv : (a,b : PolAdv) -> ListPolAdv = \a,b ->
        \\p => mkListAdv (a ! p) (b ! p) ;
      mkListPolAdv : PolAdv -> ListPolAdv -> ListPolAdv = \a,as ->
        \\p => mkListAdv (a ! p) (as ! p)
      } ;

    -- We store the non-finite form of the verb in InfComp. Anything that will have an auxiliary:
    -- (may/must/will/won't/…) cook the potato
    InfComp : Type = Polarity => E.VPI ;
    ListInfComp : Type = Polarity => E.ListVPI ;

    mkInfComp : Conj -> ListInfComp -> InfComp = \co,as ->
      \\p => E.ConjVPI co (as ! p) ;
    mkListInfComp = overload {
      mkListInfComp : (a, b : InfComp) -> ListInfComp = \a,b ->
        \\p => E.BaseVPI (a ! p) (b ! p) ;
      mkListInfComp : InfComp -> ListInfComp -> ListInfComp = \a,as ->
        \\p => E.ConsVPI (a ! p) (as ! p)
      } ;

    -- The InfForms also include a gerund, used in constructions like "with purpose of raising capital".
    InfForms : Type = {
      comp : InfComp ; -- (to) cook the cabbage (at a fixed valuation / whether at a fv or without fv)
      gerund : PolAdv  -- (not) cooking the cabbage (at fv / …)
      } ;
    ListInfForms : Type = {
      comp : ListInfComp ;
      gerund : ListPolAdv ;
      } ;

    -- Passives are handled separately. Two differences from InfForms:
    -- 1) The form is past participle, i.e. (may/must/will/won't be) cooked
    -- 2) Direct object stored as a NP in the subj field, to be used later as a subject.
    PassForms : Type = {
      subj : NP ;       -- cabbage
      comp : PolAdv ;   -- cooked
      } ;
    ListPassForms : Type = {
      subj : ListNP ;      -- cabbage and refund
      comp : ListPolAdv ;  -- cooked and issued at a fixed valuation
      } ;

    -- Given voice, tense, modality and polarity, return a final VPS
    action2VPS : Voice -> TenseModPol -> LinAction -> E.VPS = \voice,tmp,a ->
      let p : Polarity = tmp2pol tmp ;
       in case voice of {
          VPass  => complVPS tmp (mkVP (a.pass.comp ! p)) ; -- VP out of Adverb
          VAct _ =>
            case tmp of {
              PPres Pos => a.s ! Present ;
              PPast Pos => a.s ! Past ;
             _ => complVPI tmp (a.inf.comp ! p) }
          } ;

    complVPI : TenseModPol -> E.VPI -> E.VPS = \tmp,vpi ->
      let vtp : VV*Tense*Pol = tmp2vv_tns_pol tmp ;
          vv : VV = vtp.p1 ;
          tense : Tense = vtp.p2 ;
          pol : Pol =  vtp.p3 ;
       in mkVPS tense pol (E.ComplVPIVV vv vpi) ;

    complVPS : TenseModPol -> VP -> E.VPS = \tmp,vp ->
      let vtp : VV*Tense*Pol = tmp2vv_tns_pol tmp ;
          tense : Tense = vtp.p2 ;
          pol : Pol =  vtp.p3 ;
          vp' : VP = case tmp of { -- Only use VV for May, Must and Shan't.
            PMay _|PMust|PShant    -- Other auxiliaries come from tense and pol.
              => mkVP vtp.p1 vp ;
            _ => vp }
       in mkVPS tense pol vp' ;


    ----------------------
    -- Slash categories --
    ----------------------

    mkGerSIntrans : V2 -> LinAction ** {intrans : FinForms} = \v2 ->
      let linAction : LinAction = mkGerS v2  -- default: intransitive == s
       in linAction ** {intrans = linAction.s} ;

    mkGerS : V2 -> LinAction = \v2 -> let vp : VP = mkVP v2 emptyNP in {
      s = \\vf => finVPS vf vp ;
      pass = {
        subj = emptyTerm ;
        comp = \\p => ParadigmsEng.mkAdv (v2.s ! R.VPPart) ;
        } ;
      inf = {
        comp = \\p => E.MkVPI vp ; -- No indirect object yet so we ignore polarity.
        gerund =
          let posAdv : Adv = E.GerundAdv vp ;
              negAdv : Adv = prefixSS "not" posAdv ;
          in table {
            Pos => posAdv ;
            Neg => negAdv }
        }
      } ;

    finVPS : FinVF -> VP -> E.VPS = \vf -> mkVPS (vf2tns vf) positivePol ;

    -- Action_Dir
    SlashDir : Type = LinAction ** {
      intrans : FinForms ;
      indir : PolAdv ; -- at fixed valuation / whether at fv nor without fv
      dir : PrepPol
      } ;
    mkDir = overload {
      mkDir : V2 -> SlashDir = \v2 -> mkGerSIntrans v2 ** {
        dir = prepPol v2.c2 ;
        indir = emptyPolAdv ;
        } ;
      mkDir : V2 -> VP -> SlashDir = \v2,intransvp -> mkGerSIntrans v2 ** {
        dir = prepPol v2.c2 ;
        indir = emptyPolAdv ;
        intrans = \\vf => finVPS vf intransvp }
      } ;
    slashDir : SlashDirIndir -> LinTerm -> SlashIndir = \action,do -> action ** {
      dir = applyPrepPol action.dir do ;
      pass = action.pass ** {
        subj = np do ; -- in case the Action becomes a passive sentence
        }
      } ;
    complDir : SlashDir -> LinTerm -> LinAction = \action,do ->
      let doAdv : PolAdv = applyPrepPol action.dir do in action ** {
        s = complS action.s action.indir doAdv ;
        pass = action.pass ** {
          subj = np do ;
          comp = complAdv action.pass.comp action.indir emptyPolAdv ;
          } ;
        inf = action.inf ** {
          comp = complI action.inf.comp action.indir doAdv ;
          gerund = complAdv action.inf.gerund action.indir doAdv
          }
      } ;

    -- Action_Indir
    SlashIndir : Type = LinAction ** {
      intrans : FinForms ;
      dir : PolAdv ; -- (Acme will/wont sell) some/any stock
      indir : PrepPol
      } ;
    mkIndir : V2 -> SlashIndir = \v2 -> mkGerSIntrans v2 ** {
      dir = emptyPolAdv ;
      indir = prepPol v2.c2 ;
      } ;
    slashIndir : SlashDirIndir -> LinTerm -> SlashDir = \action,io ->
      let ioAdv : PolAdv = applyPrepPol action.indir io in action ** {
        indir = ioAdv ;
        intrans = complS action.intrans ioAdv emptyPolAdv ;
        inf = action.inf ** {
          comp = complI action.inf.comp ioAdv emptyPolAdv }
      } ;
    complIndir : SlashIndir -> LinTerm -> LinAction = \action,io ->
      let ioAdv : PolAdv = applyPrepPol action.indir io in action ** {
        s = complS action.s action.dir ioAdv ;
        pass = action.pass ** {
          comp = complAdv action.pass.comp emptyPolAdv ioAdv
          } ;
        inf = action.inf ** {
          comp = complI action.inf.comp action.dir ioAdv ;
          gerund = complAdv action.inf.gerund action.dir ioAdv
          }
      } ;

    -- /Dir/Indir
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

    -- helpers for complDir and complIndir
    complI : InfComp -> (a,b : PolAdv) -> InfComp = \ic,a,b -> \\p =>
      let vpi : E.VPI = ic ! p ;
          aAdv : Adv = a ! p ;
          bAdv : Adv = b ! p ;
       in vpi ** {s = \\vv,agr => vpi.s ! vv ! agr ++ aAdv.s ++ bAdv.s} ;

    complS : FinForms -> (a,b : PolAdv) -> FinForms = \ff,a,b -> \\vf =>
      let vps : E.VPS = ff ! vf ;
          aAdv : Adv = a ! Pos ;
          bAdv : Adv = b ! Pos ;
       in vps ** {s = \\agr => vps.s ! agr ++ aAdv.s ++ bAdv.s} ;

    complAdv : PolAdv -> (a,b : PolAdv) -> PolAdv = \g,a,b -> \\p =>
      let ger : Adv = g ! p ;
          aAdv : Adv = a ! p ;
          bAdv : Adv = b ! p ;
       in ger ** {s = ger.s ++ aAdv.s ++ bAdv.s} ;

    -------------------
    -- Prepositions  --
    -------------------

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

    -- preCompoundPrep : PolAdv -> PrepPol -> PrepPol = \polAdv,prep -> \\pol =>
    --   let prep : PrepPlus = prep ! pol ;
    --       pref : Str = (polAdv ! pol).s
    --    in prep ** {s = pref ++ prep.s} ;

    postCompoundPrep : PolAdv -> PrepPol -> PrepPol = \polAdv,prep -> \\pol =>
      let prep : PrepPlus = prep ! pol ;
          postf : Str = (polAdv ! pol).s
       in prep ** {post = prep.post ++ postf} ;


    applyPrepPol : PrepPol -> LinTerm -> PolAdv = \pp,term -> \\pol =>
      let np : NP = term ;
          npacc : Str = np.s ! R.NPAcc ;
          prep : PrepPlus = pp ! pol
      in lin Adv {
        s = prep.s ++ npacc ++ prep.post ++ case prep.redupl of {
                                                True => npacc ;
                                                False => [] }
      } ;

    ---------------------
    -- Generic helpers --
    ---------------------

    vf2tns : FinVF -> Tense = \vf -> case vf of {
      Present => presentTense ;
      Past    => pastTense }  ;

    tmp2vv_tns_pol : TenseModPol -> VV*Tense*Pol = \tmp -> case tmp of {
      PMay Pos  => <Extra.may_VV,   presentTense, positivePol> ;
      PMay Neg  => <Extra.may_VV,   presentTense, negativePol> ;
      PMust     => <must_VV,        presentTense, positivePol> ;
      PShant    => <Extra.shall_VV, presentTense, negativePol> ;
      PPres Neg => <Extra.do_VV,    presentTense, negativePol> ;
      PPast Neg => <Extra.do_VV,    pastTense  ,  negativePol> ;
      PFut Pos  => <emptyVV,        futureTense,  positivePol> ;
      PFut Neg  => <emptyVV,        futureTense,  negativePol> ;
      PPres Pos => <emptyVV,        presentTense, positivePol> ; -- VV not used
      PPast Pos => <emptyVV,        pastTense,    positivePol>   -- VV not used
      } where {
          emptyVV : VV = must_VV ** {s = \\_ => []} ;
      } ;

    tmp2pol : TenseModPol -> Polarity = \tmp -> case tmp of {
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

    emptyTerm : LinTerm = emptyNP ;
}
