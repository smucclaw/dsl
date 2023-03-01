concrete NL4BaseChi of NL4Base =
    NumeralChi
  , GrammarChi [
        N, N2, CN, UseN, NP, Det, DetCN, MassNP
      , V,  VV, V2, VS, VP
      , A, A2, AP, PositA
      , Comp, Adv, VP, UseComp, CompAP, CompAdv -- is a public agency
      , Prep, PrepNP, AdvVP
      , ListAdv, BaseAdv, ConsAdv, ConjAdv
      , ListAP, BaseAP, ConsAP, ConjAP
      , ListNP, BaseNP, ConsNP, ConjNP
      ]
  , StructuralChi [
      Prep, to_Prep, by8means_Prep, for_Prep, from_Prep, on_Prep
    , VV, must_VV
    ]
  , ExtendChi [
        VPS, MkVPS, mkVPS, ListVPS, BaseVPS, ConsVPS, ConjVPS
      , VPI, MkVPI, mkVPI --, [VPI], BaseVPI, ConsVPI, ConjVPI
      , VP, Tense, Ant, Temp, Pol, Conj -- for VPS
      , S, PredVPS
      , NP, GerundNP -- by performing NDB qualification
      ]
  ** open
      SyntaxChi
    , (P=ParadigmsChi)
    , ExtendChi
    , SymbolicChi
    , (R=ResChi)
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
    Action = ExtendChi.VPI ;
    Who = ExtendChi.VPS ;
    [Who] = ExtendChi.ListVPS ;
    Subj = NP ;
    Deontic = VV ;
    Upon = VP ; -- hack: thanks to linref, parse in gerund, and linearise finite forms in qUPON question
                -- would be smaller to use VPI or VPS, and doable in English (thanks to questions taking inf form), but dangerous for other langs

  linref
    Cond = linCond ;
  oper
    LinCond : Type = {subj : NP ; pred : ExtendChi.VPS} ;
    linCond : LinCond -> Str = \c -> ResChi.linS (ExtendChi.PredVPS c.subj c.pred) ;

    LinListCond : Type = {subj : NP ; preds : ExtendChi.ListVPS} ; -- TODO see if this works

  lin
-- Application layer
    -- : Subj -> Deontic -> Action -> Rule ;
    Regulative subj deontic action = mkS (mkCl subj (ComplVPIVV deontic action)) ;
    qWHO subj who = cc2 (mkUtt (ExtendChi.SQuestVPS subj who)) (ss "?") ;
    sWHO subj who = mkUtt (ExtendChi.PredVPS subj who) ;
    qCOND cond = cc2 (mkUtt (ExtendChi.SQuestVPS cond.subj cond.pred)) (ss "?") ;
    sCOND cond = mkUtt (ExtendChi.PredVPS cond.subj cond.pred) ;
    qUPON subj upon = qWHO subj (MkVPS presAnt positivePol upon) ;
    sUPON subj upon = sWHO subj (MkVPS presAnt positivePol upon) ;

    EVERY cn = every <lin CN cn : CN> ;
    PARTY cn = mkNP <lin CN cn : CN> ;
    AN cn = mkNP <lin Det a_Det : Det> <lin CN cn : CN> ;
    THE cn = mkNP <lin Det this_Det : Det> <lin CN cn : CN> ;
    WHO t p who = MkVPS t p who ;
    ACTION act = MkVPI act ;

    MUST = must_VV ;
    MAY = must_VV ;  ----
    SHANT = must_VV ; ----
    AND = and_Conj ;
    OR = or_Conj ;
    BaseWho = ExtendChi.BaseVPS ;
    ConsWho = ExtendChi.ConsVPS ;
    ConjWho = ExtendChi.ConjVPS ;

    --  : PrePost -> Conj -> [Who] -> Who ;
    ConjPreWho pr conj cs = ConjPrePostWho pr {s,qs=[]} conj cs ;

    --  : (_,_ : PrePost) -> Conj -> [Who] -> Who ;
    ConjPrePostWho pr pst conj cs =
      let who : Who = ConjWho conj cs ;
        in cc3 pr who pst ;

    -- : Subj -> Who -> Subj ;
    SubjWho subj who = mkNP subj (RelVPS IdRP who) ;

    You = you_NP ;

    UPON vp = vp ;

    WHEN np t p vp =
      let vps : VPS = MkVPS t p vp
       in {subj = np ; pred = vps} ;

    BaseCond c d = {subj = c.subj ; preds = ExtendChi.BaseVPS c.pred (mergeSubjectVPS d)} ;
    ConsCond c cs = {subj = c.subj ; preds = ExtendChi.ConsVPS c.pred (mergeSubjectListVPS cs)} ;
    ConjCond conj cs = {subj = cs.subj ; pred = ExtendChi.ConjVPS conj cs.preds} ;

  oper
    -- NB. doesn't work if we change lincat of VPS in ExtendChi
    mergeSubjectVPS : LinCond -> ExtendChi.VPS = \cond -> lin VPS {s = linCond cond} ;

   -- {subj : NP ; preds : ExtendChi.ListVPS}
    mergeSubjectListVPS : LinListCond -> ExtendChi.ListVPS = \conds ->
      let trueVPSNoSubj : VPS = lin VPS {s = conds.preds.s1} ; -- s1 has latest VPS that has no subject, older ones have had subject merged in
          newS1 : Str = R.linS (ExtendChi.PredVPS conds.subj trueVPSNoSubj) ;
          newPreds : ListVPS = conds.preds ** {s1 = newS1} ;
       in newPreds ;

-- Time expressions
  lincat
    Temporal = Adv ;
    TimeUnit = CN ;
    Date = NP ;
    TComparison = Prep ;
    [TComparison] = ListX ** {advType : ResChi.AdvType; hasDe : Prelude.Bool};

  lin
    BaseTComparison p q = twoSS {s = R.linPrep p} {s = R.linPrep q} ** q ;
    ConsTComparison p ps = consrSS comma {s = R.linPrep p} ps ** ps ;
    ConjTComparison co tcs =
      let conj : ConjunctionDistr = co.s ! R.CSent ;
          tc : SS = conjunctDistrSS conj tcs ;
       in lin Prep {prepPre = [] ; prepPost = tc.s} ** tcs ;

    TemporalConstraint cond on date =
      let onDate : Adv = SyntaxChi.mkAdv on date ;
       in cond ** {pred = advVPS cond.pred onDate} ;

    BEFORE = P.mkPrep "前" ;
    AFTER = P.mkPrep "在" "之 后" ;
    BY = by8means_Prep ;
    ON = P.mkPrep "在" ;
    VAGUE = P.mkPrep [] ;

  oper
    advVPS : ExtendChi.VPS -> CatChi.Adv -> ExtendChi.VPS = \vps,adv -> cc2 vps (mkUtt adv) ;
  lin
    MkDate a b c = symb (cc3 c b a) ;

    --  : Int -> TimeUnit -> Temporal ; -- TODO: fix "1 days" by using Dig from RGL
    WITHIN int time =
      let sym : Symb = mkSymb int.s ; -- mkSymb : Str -> Symb ;
          card : Card = symb sym ;    -- symb : Symb -> Card ;
          det : Det = mkDet card ;
      in SyntaxChi.mkAdv (P.mkPrep [] "内") (mkNP det time) ;

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
      s = R.linS (PredVPS np vps) ;
      qs = (SQuestVPS np vps).s ! True ++ "?"
    } ;

    --  : PrePost -> Conj -> [Cond] -> Cond ;
    ConjPreCond pr conj cs = ConjPrePostCond pr {s,qs=[]} conj cs ;
    --  : (_,_ : PrePost) -> Conj -> [Cond] -> Cond ;
    ConjPrePostCond pr pst conj cs =
      let cond : Cond = ConjCond conj cs ;
          predet : Predet = lin Predet pr ; -- NB. using this instead of mkPredet pattern matches its argument,
          newSubj : NP = mkNP predet cond.subj ;
          newPred : VPS = cond.pred ** {s = cond.pred.s ++ pst.s} ;
       in cond ** {subj = newSubj ; pred = newPred} ;

    BaseConstraint c d = {s = twoStr c.s d.s ; qs = twoStr c.qs d.qs} ;
    ConsConstraint c d = {s = consrStr comma c.s d.s ; qs = consrStr comma c.qs d.qs} ;
    ConjConstraint co cs = let conj : ConjunctionDistr = co.s ! R.CSent in {s = conjunctDistrX conj cs.s ; qs = conjunctDistrX conj cs.qs} ;

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
  lin
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
      subj = emptyNP ;
      pred = MkVPS presSimul POS (mkVP (invarV string.s)) ;
      } ;

    recoverUnparsedUpon string = mkVP (invarV string.s) ;

    recoverUnparsedSubj string = symb string ;

    recoverUnparsedAction string = MkVPI (mkVP (invarV string.s)) ;

  oper
    invarV : Str -> V = \v -> lin V {
      s,sn = v ; pp = "了" ; ds = "着" ; dp = "在" ; ep = "过" ; neg = "不"
      } ;
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
    ComplVSif vs s =
      let if_S : SS = mkUtt (SyntaxChi.mkAdv if_Subj s) ;
          vp : VP = R.predV <lin V vs : V> [] ;
       in lin VP (R.insertObj if_S vp) ;
    ComplVSthat vs s = mkVP <lin VS vs : VS> <lin S s : S> ;

    AdjCN ap cn = {s = ap.s ! R.Attr ++ cn.s ; c = cn.c} ;
    CompNP np = R.insertObj np (R.predV local_copula []) ;

    MayHave occur =
      let vps : ExtendChi.VPS = MkVPS presAnt POS occur ;
        in vps ** {s = "可 能" ++ vps.s} ;
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

    about_Prep = P.mkPrep "about" ;
    may_VV = must_VV ; -- ** {s = \\_ => "may"};

    oper
      every : CN -> NP = \cn -> mkNP <every_Det : Det> <cn : CN> ;
      strA2 : Str -> A2 = \str -> P.mkA2 str ;
      local_copula :  R.Verb = R.mkVerb "是" [] [] [] [] "不" ;
      hen_copula : R.Verb =
        {s = R.hen_s ; sn = [] ; pp = [] ; ds = [] ; dp = [] ; ep = [] ; neg = "不"} ; ---
      nocopula : R.Verb =
        {s = [] ; sn = [] ; pp = [] ; ds = [] ; dp = [] ; ep = [] ; neg = "不"} ; ---
      adjcopula : R.Verb =
        {s = "是" ; sn = [] ; pp = [] ; ds = [] ; dp = [] ; ep = [] ; neg = "不"} ; ---

      emptyNP : NP = it_NP ** {s = []} ;
}

