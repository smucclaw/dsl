incomplete concrete NL4BaseFunctor of NL4Base = CustomSyntax ** open
      Syntax
    , Extend
    , Symbolic
    , Lexicon
    , CustomSyntax
    , Coordination
    , Prelude
    in {


  lincat
    Text = Syntax.Utt ;

    Cond = LinCond ;       -- defined in this module
    [Cond] = LinListCond ; -- defined in this module
    Action = Extend.VPI ;
    Who = Extend.VPS ;
    [Who] = Extend.ListVPS ;
    Subj = Syntax.NP ;
    Deontic = Syntax.VV ;
    Upon = Syntax.VP ;

-- In general, all non-prefixed mkXxx funs come from RGL API (Syntax.mkXxx)

  oper
    LinCond : Type = {s : Syntax.S ; qs : Syntax.QS} ;
    LinListCond : Type = {
      s : Syntax.ListS ;  -- from RGL API
      qs : CustomSyntax.ListQS -- from our own custom module
      } ;

  linref
    Upon = linUpon ; -- defined in this module
    Who = linWho ;   -- defined in NL4Base<Lang> per lang

  oper
    -- hack: thanks to linref, parse in gerund, and linearise finite forms in qUPON question
    linUpon : VP -> Str = \vp -> (Extend.GerundAdv vp).s ;

  lin
    -- for fancy NLG
    -- : Subj -> Deontic -> Action -> Rule ;
    Regulative subj deontic action = mkUtt (mkS (mkCl subj (Extend.ComplVPIVV deontic action))) ;

    -- for web forms
    qWHO subj who = lin Utt (cc2 (mkUtt (Extend.SQuestVPS subj who)) (ss "?")) ;
    sWHO subj who = mkUtt (Extend.PredVPS subj who) ;
    qCOND cond = lin Utt (cc2 (mkUtt cond.qs) (ss "?")) ;
    sCOND cond = mkUtt cond.s ;
    advUPON upon = mkUtt (Syntax.mkAdv CustomSyntax.upon_Prep (Extend.GerundNP upon)) ;
    qUPON subj upon = qWHO subj (Extend.MkVPS presAnt positivePol upon) ;
    sUPON subj upon = sWHO subj (Extend.MkVPS presAnt positivePol upon) ;

    -- general Regulative stuff
    EVERY cn = mkNP <lin Det every_Det : Det> <lin CN cn : CN> ;
    PARTY cn = mkNP <lin CN cn : CN> ;
    AN cn = mkNP <lin Det a_Det : Det> <lin CN cn : CN> ;
    THE cn = mkNP <lin Det the_Det : Det> <lin CN cn : CN> ;
    WHO t p who = Extend.MkVPS t p who ;
    ACTION act = Extend.MkVPI act ;

    MUST = must_VV ; -- from RGL API
    MAY = CustomSyntax.may_VV ; -- oper in CustomSyntax
    SHANT = CustomSyntax.shant_VV ;  -- oper in CustomSyntax
    AND = and_Conj ;
    OR = or_Conj ;

    BaseWho = Extend.BaseVPS ;
    ConsWho = Extend.ConsVPS ;
    ConjWho = Extend.ConjVPS ;

    --  : PrePost -> Conj -> [Who] -> Who ;
    ConjPreWho pr conj cs = ConjPrePostWho pr {s,qs=[]} conj cs ;
    --  : (_,_ : PrePost) -> Conj -> [Who] -> Who ;
    ConjPrePostWho = CustomSyntax.ConjPrePostVPS ; -- fun/lin in CustomSyntax

    -- : Subj -> Who -> Subj ;
    SubjWho subj who = mkNP subj (Extend.RelVPS CustomSyntax.who_RP who) ; -- who_RP is oper in CustomSyntax

    You = you_NP ;

    UPON vp = lin VP vp ;

    WHEN np t p vp =
      let vps : VPS = Extend.MkVPS t p <lin VP vp : VP> in {
        s = Extend.PredVPS np vps ;
        qs = Extend.SQuestVPS np vps
        } ;

    -- BaseS, ConsS and ConjS are available in this grammar via CustomSyntax,
    -- but originally they come from the RGL abstract syntax.
    -- We could as well write Syntax.mkListS c.s d.s etc.
    -- In contrast, {Base,Cons,Conj}QS are only defined in CustomSyntax.
    BaseCond c d = {
      s = BaseS c.s d.s ;
      qs = CustomSyntax.BaseQS c.qs d.qs} ;
    ConsCond c d = {
      s = ConsS c.s d.s ;
      qs = CustomSyntax.ConsQS c.qs d.qs
      } ;
    ConjCond conj cs = {
      s = ConjS conj cs.s ;
      qs = CustomSyntax.ConjQS conj cs.qs
      } ;

    --  : PrePost -> Conj -> [Cond] -> Cond ;
    ConjPreCond pr conj cs = ConjPrePostCond pr {s,qs=[]} conj cs ;
    ConjPrePostCond pr pst conj cs = {
      s  = CustomSyntax.ConjPrePostS pr pst conj cs.s ;
      qs = CustomSyntax.ConjPrePostQS pr pst conj cs.qs ;
    } ;
-----------------------------------------------------------------------------
-- General BoolStruct stuff, just first sketch — should be handled more structurally in HS
  lincat
    PrePost,  -- "Loss or Damage caused by", "an animal caused water to escape from"
    Constraint = LinConstraint ;
    [Constraint] = LinListConstraint ;

  oper
    LinConstraint : Type = {s, qs : Str} ; -- TODO later proper RGL structures and parsing
    LinListConstraint : Type = {s, qs : Coordination.ListX} ;
    npStr : NP -> Str = \np -> (UttNP np).s ;
    sStr : S -> Str = \s -> (UttS s).s ;
    qsStr : QS -> Str = \qs -> (UttQS qs).s ;
  lin

    RPleafNP np = {s = npStr np ; qs = npStr np ++ "?"} ;
    RPleafS np vps = {
      s = sStr (Extend.PredVPS np vps) ;
      qs = qsStr (Extend.SQuestVPS np vps) ++ "?"
    } ;

    BaseConstraint c d = {
      s = Coordination.twoStr c.s d.s ;
      qs = Coordination.twoStr c.qs d.qs
      } ;
    ConsConstraint c d = {
      s = Coordination.consrStr Coordination.comma c.s d.s ;
      qs = Coordination.consrStr Coordination.comma c.qs d.qs
      } ;
    ConjConstraint conj cs = {
      s = Coordination.conjunctDistrX conj cs.s ;
      qs = Coordination.conjunctDistrX conj cs.qs
      } ;

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
-- Time expressions
  lincat
    Temporal = Syntax.Adv ;
    TimeUnit = Syntax.CN ;

  lin
    -- : TComparison -> Digits -> TimeUnit -> Temporal ;
    TemporalConstraint tcomp digits time =
      let card : Card = mkCard <lin Digits digits : Digits> ;
          det : Det = mkDet card ;
      in Syntax.mkAdv tcomp (mkNP det time) ;

    Day_Unit = mkCN Lexicon.day_N ;
    Month_Unit = mkCN CustomSyntax.month_N ;
    Year_Unit = mkCN Lexicon.year_N ;

  lincat
    Date = Syntax.NP ;
    TComparison = Syntax.Prep ;
    [TComparison] = CustomSyntax.ListPrep ;

  lin
    BaseTComparison = CustomSyntax.BasePrep ;
    ConsTComparison = CustomSyntax.ConsPrep ;
    ConjTComparison = CustomSyntax.ConjPrep ;

    BEFORE = CustomSyntax.within_Prep | before_Prep ;
    AFTER = after_Prep ;
    BY = by8means_Prep ;
    ON = on_Prep ;
    VAGUE = CustomSyntax.vaguePrep ;

  oper
    cc4 : (_,_,_,_ : SS) -> SS = \s1, s2, s3, s4 -> {s = glue s1.s (glue s2.s (glue s3.s s4.s))} ;

  lincat
    Day, Month, Year, YearComponent = SS ;
  lin
    MkDate day month year = symb (cc3 day month year);

    Day1 = ss "1" ; Day2 = ss "2" ; Day3 = ss "3" ; Day4 = ss "4" ;
    Day5 = ss "5" ; Day6 = ss "6" ; Day7 = ss "7" ; Day8 = ss "8" ;
    Day9 = ss "9" ; Day10 = ss "10" ; Day11 = ss "11" ; Day12 = ss "12" ;
    Day13 = ss "13" ; Day14 = ss "14" ; Day15 = ss "15" ; Day16 = ss "16" ;
    Day17 = ss "17" ; Day18 = ss "18" ; Day19 = ss "19" ; Day20 = ss "20" ;
    Day21 = ss "21" ; Day22 = ss "22" ; Day23 = ss "23" ; Day24 = ss "24" ;
    Day25 = ss "25" ; Day26 = ss "26" ; Day27 = ss "27" ; Day28 = ss "28" ;
    Day29 = ss "29" ; Day30 = ss "30" ; Day31 = ss "31" ;

    Y0 = ss "0" ; Y1 = ss "1" ; Y2 = ss "2" ; Y3 = ss "3" ; Y4 = ss "4" ;
    Y5 = ss "5" ; Y6 = ss "6" ; Y7 = ss "7" ; Y8 = ss "8" ; Y9 = ss "9" ;

    MkYear = cc4 ;

}

