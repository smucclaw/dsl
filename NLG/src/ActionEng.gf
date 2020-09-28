concrete ActionEng of Action = TermEng **
open
  Prelude,
  (R=ResEng),
  (E=ExtendEng),
  (C=ConjunctionEng),
  SyntaxEng,
  ParadigmsEng,
  NounEng,
  VerbEng,
  AdjectiveEng in {

  lincat

    Action = LinAction ; -- Negations affect more than standard RGL negation does
    Action_Dir = SlashDir ;
    Action_Indir = SlashIndir ;
    Action_Dir_Indir = SlashDirIndir ;
    [Action] = ListLinAction ;
    [Action_Dir] = ListSlashDir ;
    [Action_Indir] = ListSlashIndir ;
    [Action_Dir_Indir] = ListSlashDirIndir ;

  linref
    Action = linAction ;


  lin
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


{-    -- Opposite to complements: make the Action open for more arguments.
    -- : Action -> Action_Indir
    PursuantTo a = a ** {
      indir = pursuant_to_Prep ;
      dir = \\_ => emptyAdv
      } ;
  oper
    pursuant_to_Prep : PrepPol = prepPol "pursuant to" ; -}

  lin

    ---------------
    -- Negations --
    ---------------

    -- : Action -> Action ;        -- doesnt sell X / doesnt sell X and Y
    ANeg action = action ** {
      s = \\t,p => case p of {
        --R.CNeg _ => action.s ! t ! R.CPos ; -- double negation = positive
        _ => action.s ! t ! negativePol.p
        } ;
      gerund = table {
        --R.Neg => action.gerund ! R.Pos ; -- double negation = positive
        _ => action.gerund ! R.Neg
        }
      } ;
    -- : Action_Dir -> [Term] -> Action ; -- sells neither X, Y nor Z
    AComplNoneDir v2 obj =
      let none_of : NP = mkNP neither7nor_DConj obj ;
       in complDir v2 none_of ;

    --  AComplNoneIndir : Action_Indir -> [Term] -> Action ; -- sells (X) neither to A nor to B

    ------------------
    -- Conjunctions --
    ------------------

    BaseAction a1 a2 = {
      s = \\t,p => E.BaseVPS (a1.s ! t ! p) (a2.s ! t ! p) ; -- doesnt sell X and doesnt issue Y
      gerund = \\p => mkListAdv (a1.gerund ! p) (a2.gerund ! p) ; -- not selling X and not issuing Y
      actor = mkNP the_Det (mkN "actor") ; -- TODO
      } ;
    ConsAction a as = as ** {
      s = \\t,p => E.ConsVPS (a.s ! t ! p) (as.s ! t ! p) ;
      gerund = \\p => mkListAdv (a.gerund ! p) (as.gerund ! p)
      } ;
    ConjAction co as = {
      s = \\t,p =>
        E.ConjVPS co (as.s ! t ! p) ;
      gerund = \\p =>
        SyntaxEng.mkAdv co (as.gerund ! p) ;
      actor = as.actor
      } ;

    BaseAction_Dir a1 a2 =
      let a1' : LinAction = complDir a1 emptyTerm ;
          a2' : LinAction = complDir a2 emptyTerm ;
      in BaseAction a1' a2' ** {
           dir = a1.dir ; -- : PrepPol
           indir = \\p => emptyAdv ; -- the existing indir has been incorporated in a1 and a2
         } ;
    ConsAction_Dir a as =
      let a' : LinAction = complDir a emptyTerm ;
      in ConsAction a' <as:ListLinAction> ** {
           dir = as.dir ; -- : PrepPol
           indir = \\p => emptyAdv
         } ;
    ConjSlashDir co as = ConjAction co as ** {
      dir = as.dir ;
      indir = as.indir
      } ;

    BaseAction_Indir a1 a2 =
      let a1' : LinAction = complIndir a1 emptyTerm ;
          a2' : LinAction = complIndir a2 emptyTerm ;
      in BaseAction a1' a2' ** {
           indir = a1.indir ; -- : PrepPol
           dir = \\p => emptyAdv ; -- the existing dir has been incorporated in a1 and a2
         } ;
    ConsAction_Indir a as =
      let a' : LinAction = complIndir a emptyTerm ;
      in ConsAction a' <as:ListLinAction> ** {
           indir = as.indir ; -- : PrepPol
           dir = \\p => emptyAdv
         } ;

    ConjSlashIndir co as = ConjAction co as ** {
      dir = as.dir ;
      indir = as.indir
      } ;

    BaseAction_Dir_Indir a1 a2 = BaseAction a1 a2 ** {
      dir = a2.dir ;
      indir = a2.indir
      } ;
    ConsAction_Dir_Indir a as = ConsAction a as ** {
      dir = as.dir ;
      indir = as.indir
      } ;
    ConjSlashDirIndir co as = ConjAction co as ** {
      dir = as.dir ;
      indir = as.indir
      } ;

  oper
    -- CPolarity so that it contains "do not" and "don't", can choose later
    LinAction : Type = {
      s : R.Tense => R.CPolarity => E.VPS ;
      gerund : R.Polarity => Adv ;
      actor : NP ; -- sell -> seller
      } ;

    ListLinAction : Type = {
      s : R.Tense => R.CPolarity => E.ListVPS ;
      gerund : R.Polarity => [Adv] ;
      actor : NP ;
      } ;

    linAction : LinAction -> Str = \l ->
      (mkUtt (cl presentTense positivePol emptyTerm l)).s ;

    mkVPS : R.Tense -> R.CPolarity -> VP -> E.VPS = \t,p ->
      let tense : Tense = lin Tense {s=[] ; t=t} ;
          pol : Pol = lin Pol {s=[] ; p=p} ;
       in E.MkVPS (mkTemp tense simultaneousAnt) pol ;

    emptyTerm : LinTerm = emptyNP ;


    ----------------------
    -- Slash categories --
    ----------------------

    mkGerS : V2 -> LinAction = \v2 -> {
      s = \\t,p => mkVPS t p (mkVP <v2:V2> emptyNP) ;
      gerund =
        let posAdv : Adv = E.GerundAdv (mkVP <v2:V2> emptyNP) ;
            negAdv : Adv = posAdv ** {s = "not" ++ posAdv.s}
        in table {
          R.Pos => posAdv ;
          R.Neg => negAdv } ;
      actor = mkNP (mkN "TODO: we should get this from wordnet") ;
      } ;

    -- _Dir
    SlashDir : Type = LinAction ** {
      indir : R.CPolarity => Adv ; -- at fixed valuation / whether at fv nor without fv
      dir : PrepPol
      } ;
    mkDir : V2 -> SlashDir = \v2 -> mkGerS v2 ** {
      dir = prepPol v2.c2 ;
      indir = \\_ => emptyAdv ;
      } ;
    slashDir : SlashDirIndir -> LinTerm -> SlashIndir = \vps,do -> vps ** {
      dir = applyPrepPol vps.dir do
      } ;
    complDir : SlashDir -> LinTerm -> LinAction = \vps,do -> vps ** {
      s = \\t,p => complS (vps.s ! t ! p)
                          (vps.indir ! p)
                          (applyPrepPol vps.dir do ! p) ;
      gerund = \\p => complGer (vps.gerund ! p)
                            (vps.indir ! pol2cpol p)
                            (applyPrepPol vps.dir do ! pol2cpol p)
      } ;

    -- _Indir
    SlashIndir : Type = LinAction ** {
      dir : R.CPolarity => Adv ; -- (Acme will/wont sell) some/any stock
      indir : PrepPol ;
      } ;
    mkIndir : V2 -> SlashIndir = \v2 -> mkGerS v2 ** {
      dir = \\_ => emptyAdv ;
      indir = prepPol v2.c2 ;
      } ;
    slashIndir : SlashDirIndir -> LinTerm -> SlashDir = \vps,io -> vps ** {
      indir = applyPrepPol vps.indir io
      } ;
    complIndir : SlashIndir -> LinTerm -> LinAction = \vps,io -> vps ** {
      s = \\t,p => complS (vps.s ! t ! p)
                          (vps.dir ! p)
                          (applyPrepPol vps.indir io ! p) ;
      gerund = \\p => complGer (vps.gerund ! p)
                            (vps.dir ! pol2cpol p)
                            (applyPrepPol vps.indir io ! pol2cpol p)
      } ;


    -- _Dir_Indir
    SlashDirIndir : Type = LinAction ** {
      dir,
      indir : PrepPol ;
      } ;
    mkDirIndir = overload {
      mkDirIndir : V3 -> SlashDirIndir = \v3 -> mkGerS v3 ** {
        dir = prepPol v3.c2 ;
        indir = prepPol v3.c3
        } ;
      mkDirIndir : V3 -> PrepPol -> SlashDirIndir = \v3,indir -> mkGerS v3 ** {
        indir = indir ;
        dir = prepPol v3.c2
        }
      } ;
    -- PrepPol is more powerful than Prep: prepared for multilayer negations
    PrepPol : Type = R.CPolarity => PrepPlus ;
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
        R.CPos   => pos ;
        R.CNeg _ => neg
        }
      } ;

    prepPlus : (s,post : Str) -> (redupl : Bool) -> PrepPlus = \s,post,r -> {
      s = s ;
      post = post ;
      redupl = r
      } ;

    applyPrepPol : PrepPol -> LinTerm -> (R.CPolarity=>Adv) = \pp,term -> \\pol =>
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
      indir : R.CPolarity => Adv ; -- at fixed valuation / whether at fv nor without fv
      dir : PrepPol ;
      } ;

    ListSlashIndir : Type = ListLinAction ** {
      dir : R.CPolarity => Adv ; -- (Acme will/wont sell) some/any stock
      indir : PrepPol ;
      } ;

    ListSlashDirIndir : Type = ListLinAction ** {
      dir,
      indir : PrepPol ;
      } ;

    ---------------------
    -- Generic helpers --
    ---------------------
    cl : Tense -> Pol -> LinTerm -> LinAction -> S = \t,p,subj,pred ->
      let s : S = E.PredVPS (np subj) (pred.s ! t.t ! p.p)
       in s ** {s = s.s ++ t.s ++ p.s} ;
    -- This is silly, but I need to do it this way, because instead of VP, which is variable in
    -- tense and polarity, Im storing /fully formed VPS/s in a table with R.Tense and R.CPolarity as LHS.
    -- (Why do I store VPS instead of VP? To be able to coordinate them.)
    -- When an abstract syntax value like TPresent or PPositive is used to choose the correct VPS,
    -- I need to use the s fields of those values, so that every argument contributes to the linearization.
    -- See https://inariksit.github.io/gf/2018/08/28/gf-gotchas.html#metavariables-or-those-question-marks-that-appear-when-parsing

    gerund : LinAction -> R.Polarity=>NP = \pred -> \\pol =>
      let s : Str = (pred.gerund ! pol).s in mkNP (mkN s s s s) ;

    cpol2pol : R.CPolarity -> R.Polarity = \p -> case p of {
      R.CPos => R.Pos ;
      R.CNeg _ => R.Neg
      } ;

    pol2cpol : R.Polarity -> R.CPolarity = \p -> case p of {
      R.Pos => R.CPos ;
      R.Neg => R.CNeg True
      } ;
}
