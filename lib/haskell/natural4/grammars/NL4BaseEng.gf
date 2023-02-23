concrete NL4BaseEng of NL4Base =
    NumeralEng
  , GrammarEng [
        N, N2, CN, UseN, NP, Det, DetCN, MassNP
      , V,  VV, V2, VS, VP
      , A, A2, AP, AdjCN, PositA
  --      , ProgrVP -- becoming aware
      , Comp, Adv, VP, UseComp, CompAP, CompNP, CompCN, CompAdv -- is a public agency
      , Prep, PrepNP, AdvVP
      , ListAdv, BaseAdv, ConsAdv, ConjAdv
      , ListAP, BaseAP, ConsAP, ConjAP
      , ListNP, BaseNP, ConsNP, ConjNP
      , ListS, BaseS, ConsS, ConjS
      ]
  , StructuralEng [
      Prep, to_Prep, by8means_Prep, for_Prep, from_Prep, on_Prep
    , VV, must_VV
    ]
  , ExtendEng [
        VPS, MkVPS, mkVPS, ListVPS, BaseVPS, ConsVPS, ConjVPS
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
    , IrregEng
    , Coordination
    , Prelude
    in {
  lincat
    Rule = S ;

    Text = Utt ;

    Cond = LinCond ; -- RPConstraint
                      -- [ MTT "the data breach occurs" ] ( RPTC TOn )
                      -- [ MTT "1 Feb 2022" ]
    [Cond] = LinListCond ;
    Action = ExtendEng.VPI ;
    Who = ExtendEng.VPS ;
    [Who] = ExtendEng.ListVPS ;
    Subj = NP ;
    Deontic = VV ;
    Upon = VP ; -- hack: thanks to linref, parse in gerund, and linearise finite forms in qUPON question
                -- would be smaller to use VPI or VPS, and doable in English (thanks to questions taking inf form), but dangerous for other langs

  linref
    Who = linWho ;
    Cond = \c -> c.s.s ;
    Upon = linUpon ;
  oper
    LinCond : Type = {s : S ; qs : QS} ; -- {subj : NP ; pred : ExtendEng.VPS} ;
    LinListCond : Type = {s : SyntaxEng.ListS ; qs : ListQS} ;
    ListQS : Type = {s1,s2 : R.QForm => Str} ;
    linWho : ExtendEng.VPS -> Str = \vps ->
      let vpss = vps.s ! R.ODir False ! R.agrP3 R.Sg
        in vpss.fin ++ vpss.inf ;
    linUpon : VP -> Str = \vp -> (GerundAdv vp).s ;

  lin
-- Application layer
    -- : Subj -> Deontic -> Action -> Rule ;
    Regulative subj deontic action = mkS (mkCl subj (ComplVPIVV deontic action)) ;
    qWHO subj who = cc2 (mkUtt (ExtendEng.SQuestVPS subj who)) (ss "?") ;
    sWHO subj who = mkUtt (ExtendEng.PredVPS subj who) ;
    qCOND cond = cc2 (mkUtt cond.qs) (ss "?") ;
    sCOND cond = mkUtt cond.s ;
    qUPON subj upon = qWHO subj (MkVPS presAnt positivePol upon) ;
    sUPON subj upon = sWHO subj (MkVPS presAnt positivePol upon) ;

    EVERY cn = every <lin CN cn : CN> ;
    PARTY cn = mkNP <lin CN cn : CN> ;
    AN cn = mkNP <lin Det a_Det : Det> <lin CN cn : CN> ;
    THE cn = mkNP <lin Det the_Det : Det> <lin CN cn : CN> ;
    WHO t p who = MkVPS t p who ;
    ACTION act = MkVPI act ;

    MUST = must_VV ;
    MAY = ExtraEng.may_VV ;
    SHANT = ExtraEng.shall_VV ** { -- only used in NLG, not parsing
      s = \\_ => "shan't" ;        -- so negation here should be fine
      } ;
    AND = and_Conj ;
    OR = or_Conj ;
    BaseWho = ExtendEng.BaseVPS ;
    ConsWho = ExtendEng.ConsVPS ;
    ConjWho = ExtendEng.ConjVPS ;
    --  : PrePost -> Conj -> [Who] -> Who ;
    ConjPreWho pr conj cs = ConjPrePostWho pr {s,qs=[]} conj cs ;
    --  : (_,_ : PrePost) -> Conj -> [Who] -> Who ;
    ConjPrePostWho pr pst conj cs =
      let who : Who = ConjWho conj cs ;
        in who ** {
            s  = \\o,a =>
                  let orig : {fin,inf : Str} = who.s ! o ! a
                    in orig ** {fin = pr.s ++ orig.fin ; inf = orig.inf ++ pst.s}
          } ;
    -- : Subj -> Who -> Subj ;
    SubjWho subj who = mkNP subj (RelVPS ExtraEng.who_RP who) ;

    You = you_NP | mkNP (mkN "You" "You" "Your" "Your") ;

    UPON vp = lin VP vp ;

    WHEN np t p vp =
      let vps : VPS = MkVPS t p <vp : VP>
       in {s = PredVPS np vps ; qs = SQuestVPS np vps} ;

    BaseCond c d = {s = BaseS c.s d.s ; qs = twoTable R.QForm c.qs d.qs} ;
    ConsCond c d = {s = ConsS c.s d.s ; qs = consrTable R.QForm comma c.qs d.qs} ;
    ConjCond conj cs = {s = ConjS conj cs.s ; qs = lin QS (conjunctDistrTable R.QForm conj cs.qs)} ;

-- Time expressions
  lincat
    Temporal = Adv ;
    TimeUnit = CN ;
    Date = NP ;
    TComparison = Prep ;
    [TComparison] = ListX ;

  lin
    BaseTComparison = twoSS ;
    ConsTComparison = consrSS comma ;
    ConjTComparison co tcs = conjunctDistrSS co tcs ** {isPre = True} ;

    TemporalConstraint cond on date =
      let onDate : Adv = SyntaxEng.mkAdv on date ;
       in {s = postAdvS cond.s onDate ; qs = postAdvQS cond.qs onDate} ;

    BEFORE = mkPrep "before" ;
    AFTER = mkPrep "after" ;
    BY = by8means_Prep ;
    ON = on_Prep ;
    VAGUE = noPrep ;

  oper
    postAdvS : S -> Adv -> S = \s,adv -> s ** mkS <lin Adv s : Adv> <lin S adv : S> ; -- hack that only works for Eng
    postAdvQS : QS -> Adv -> QS = \qs,adv -> qs ** {s = \\qf => qs.s ! qf ++ adv.s} ;
  lin
    MkDate a b c = symb (cc3 a b c) ;

    --  : Int -> TimeUnit -> Temporal ; -- TODO: fix "1 days" by using Dig from RGL
    WITHIN int time =
      let sym : Symb = mkSymb int.s ; -- mkSymb : Str -> Symb ;
          card : Card = symb sym ;    -- symb : Symb -> Card ;
          det : Det = mkDet card ;
      in SyntaxEng.mkAdv (mkPrep "within") (mkNP det time) ;

-- General BoolStruct stuff, just first sketch — should be handled more structurally in HS
  lincat
    PrePost,  -- "Loss or Damage caused by", "an animal caused water to escape from"
--      IncompleteConstraint,
    Constraint = LinConstraint ;
--      [IncompleteConstraint],
    [Constraint] = LinListConstraint ;
  oper
    LinConstraint : Type = {s, qs : Str} ; -- TODO later proper RGL structures and parsing
    LinListConstraint : Type = {s, qs : ListX} ;
    npStr : NP -> Str = \np -> (UttNP np).s ;
  lin

    RPleafNP np = {s = npStr np ; qs = npStr np ++ "?"} ;
    RPleafS np vps = {
      s = (PredVPS np vps).s ;
      qs = (SQuestVPS np vps).s ! R.QDir ++ "?"
    } ;

    ConjCond conj cs = {s = ConjS conj cs.s ; qs = lin QS (conjunctDistrTable R.QForm conj cs.qs)} ;
    --  : PrePost -> Conj -> [Cond] -> Cond ;
    ConjPreCond pr conj cs = ConjPrePostCond pr {s,qs=[]} conj cs ;
    --  : (_,_ : PrePost) -> Conj -> [Cond] -> Cond ;
    ConjPrePostCond pr pst conj cs =
      let cond : Cond = ConjCond conj cs ;
        in cond ** {
            s  = lin S {s = pr.s ++ cond.s.s ++ pst.s} ;
            qs = lin QS {s = \\qf => pr.s ++ cond.qs.s ! qf ++ pst.s ++ "?"} ;
          } ;

    BaseConstraint c d = {s = twoStr c.s d.s ; qs = twoStr c.qs d.qs} ;
    ConsConstraint c d = {s = consrStr comma c.s d.s ; qs = consrStr comma c.qs d.qs} ;
    ConjConstraint conj cs = {s = conjunctDistrX conj cs.s ; qs = conjunctDistrX conj cs.qs} ;

    --  : PrePost -> Conj -> [Constraint] -> Constraint ;
    ConjPreConstraint pr conj cs = ConjPrePostConstraint pr {s,qs=[]} conj cs ;

    --  : (_,_ : PrePost) -> Conj -> [Constraint] -> Constraint ;
    ConjPrePostConstraint pr pst conj cs =
      let constr : Constraint = ConjConstraint conj cs ;
        in constr ** {
            s  = pr.s ++ constr.s ++ pst.s ;
            qs =  pr.s ++ constr.s ++ pst.s ++ "?" ; -- if Constraint has undergone ConjConstraint, it will have ? after every item, we don't want that
          } ;

    qPREPOST,
    qCONSTR = \c -> lin Utt (ss c.qs) ;

-----------------------------------------------------------------------------
-- Instead of crashing, every category should have a dummy constructor where to put a string

    recoverUnparsedPrePost string = {
      s = "·" ++ string.s ; -- if PrePost isn't parsed, use the original string
      qs = "Did the following happen:" ++ string.s -- make a question in an awkward way
      } ;

    -- : String -> String -> Constraint ;
    recoverRPis damage toContents = {
      s = "·" ++ damage.s ++ "is" ++ toContents.s ; -- if constraint isn't parsed, use the original string
      qs = "Is" ++ damage.s ++ toContents.s ++ "?"
      } ;
    recoverUnparsedConstraint string = recoverUnparsedPrePost string ;

    recoverUnparsedWho string = MkVPS presSimul POS (mkVP (invarV string.s)) ;

    recoverUnparsedCond string = {
      s = lin S string ;
      qs = lin QS {s = \\_ => string.s}
      } ;

    recoverUnparsedUpon string = mkVP (invarV string.s) ;

    recoverUnparsedSubj string = symb string ;

    recoverUnparsedAction string = MkVPI (mkVP (invarV string.s)) ;

  oper
    invarV : Str -> V = \s -> mk5V s s s s s ;
-----------------------------------------------------------------------------
-- RGL layer, later to be automatically generated in different modules

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
    ComplVSthat vs s = mkVP <lin VS vs : VS> <lin S s : S> ;

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

    about_Prep = mkPrep "about" ;
    may_VV = ExtraEng.may_VV ; -- ** {s = \\_ => "may"};

    oper
      every : CN -> NP = \cn -> mkNP <every_Det : Det> <cn : CN> ;
      strA2 : Str -> A2 = \str -> mkA2 (mkA str) noPrep ;

}

