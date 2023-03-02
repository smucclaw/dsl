concrete NL4BaseChi of NL4Base =
     CustomSyntaxChi
  ** NL4BaseFunctor - [
      Cond,ListCond,BaseCond, ConsCond, ConjCond, ConjPreCond, ConjPrePostCond
    , WHEN, qCOND, sCOND
    , ConjConstraint -- Conj has different lincat
    , Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9
    , Day1,Day2,Day3,Day4,Day5,Day6,Day7,Day8,Day9,Day10,Day11,Day12,Day13,Day14,Day15,Day16,Day17,Day18,Day19,Day20,Day21,Day22,Day23,Day24,Day25,Day26,Day27,Day28,Day29,Day30,Day31
    ]
  with
      (Syntax=SyntaxChi)
    , (Extend=ExtendChi)
    , (Symbolic=SymbolicChi)
    , (Lexicon=LexiconChi)
    , (CustomSyntax=CustomSyntaxChi)
   ** open Coordination, Prelude, ParadigmsChi, (R=ResChi) in {

-----------------------------------------------------------------------------
-- These are not implemented in the functor, different for all 3

  lin TemporalConstraint cond on date =
      let onDate : Adv = SyntaxChi.mkAdv on date ;
       in cond ** {pred = advVPS cond.pred onDate} ;

  oper
    linWho : ExtendChi.VPS -> Str = \vps -> vps.s ;

-----------------------------------------------------------------------------
-- Different lincat for Cond, override all funs that produce and consume Cond

  oper
    LinCondChi : Type = {subj : NP ; pred : ExtendChi.VPS} ;
    linCond : LinCondChi -> Str = \c -> ResChi.linS (ExtendChi.PredVPS c.subj c.pred) ;

    LinListCondChi : Type = {subj : NP ; preds : ExtendChi.ListVPS} ; -- TODO see if this works

  lincat
    Cond = LinCondChi ;
    [Cond] = LinListCondChi ;

  lin
    WHEN np t p vp =
      let vps : VPS = MkVPS t p vp
       in {subj = np ; pred = vps} ;

    qCOND cond = cc2 (mkUtt (ExtendChi.SQuestVPS cond.subj cond.pred)) (ss "?") ;
    sCOND cond = mkUtt (ExtendChi.PredVPS cond.subj cond.pred) ;

  -- custom opers for [Cond]
  oper
    -- NB. doesn't work if we change lincat of VPS in ExtendChi
    mergeSubjectVPS : LinCondChi -> ExtendChi.VPS = \cond -> lin VPS {s = linCond cond} ;

   -- {subj : NP ; preds : ExtendChi.ListVPS}
    mergeSubjectListVPS : LinListCondChi -> ExtendChi.ListVPS = \conds ->
      let trueVPSNoSubj : VPS = lin VPS {s = conds.preds.s1} ; -- s1 has latest VPS that has no subject, older ones have had subject merged in
          newS1 : Str = R.linS (ExtendChi.PredVPS conds.subj trueVPSNoSubj) ;
          newPreds : ListVPS = conds.preds ** {s1 = newS1} ;
       in newPreds ;

  lin
    BaseCond c d = {subj = c.subj ; preds = ExtendChi.BaseVPS c.pred (mergeSubjectVPS d)} ;
    ConsCond c cs = {subj = c.subj ; preds = ExtendChi.ConsVPS c.pred (mergeSubjectListVPS cs)} ;
    ConjCond conj cs = {subj = cs.subj ; pred = ExtendChi.ConjVPS conj cs.preds} ;

    --  : PrePost -> Conj -> [Cond] -> Cond ;
    ConjPreCond pr conj cs = ConjPrePostCond pr {s,qs=[]} conj cs ;

    --  : (_,_ : PrePost) -> Conj -> [Cond] -> Cond ;
    ConjPrePostCond pr pst conj cs =
      let cond : Cond = ConjCond conj cs ;
          predet : Predet = lin Predet pr ; -- NB. using this instead of mkPredet pattern matches its argument,
          newSubj : NP = mkNP predet cond.subj ;
          newPred : VPS = cond.pred ** {s = cond.pred.s ++ pst.s} ;
       in cond ** {subj = newSubj ; pred = newPred} ;

-----------------------------------------------------------------------------
-- Different lincat for Conj in the RGL, can't use Coordination as is

  lin
    ConjConstraint co cs =
      let conj : ConjunctionDistr = co.s ! R.CSent in {
        s = conjunctDistrX conj cs.s ;
        qs = conjunctDistrX conj cs.qs
      } ;

-----------------------------------------------------------------------------
-- Time units
lin
    Jan = ss "一 月" ; Feb = ss "二 月" ; Mar = ss "三 月" ; Apr = ss "四 月" ;
    May = ss "五 月" ; Jun = ss "六 月" ; Jul = ss "七 月" ; Aug = ss "八 月" ;
    Sep = ss "九 月" ; Oct = ss "十 月" ; Nov = ss "十 一 月" ; Dec = ss "十 二 月" ;

    Day1 = ss "一 号" ; Day2 = ss "二 号" ; Day3 = ss "三 号" ; Day4 = ss "四 号" ;
    Day5 = ss "五 号" ; Day6 = ss "六 号" ; Day7 = ss "七 号" ; Day8 = ss "八 号" ;
    Day9 = ss "九 号" ; Day10 = ss "十 号" ; Day11 = ss "十 一 号" ; Day12 = ss "十 二 号" ;
    Day13 = ss "十 三 号" ; Day14 = ss "十 四 号" ; Day15 = ss "十 五 号" ; Day16 = ss "十 六 号" ;
    Day17 = ss "十 七 号" ; Day18 = ss "十 八 号" ; Day19 = ss "十 九 号" ; Day20 = ss "二 十 号" ;
    Day21 = ss "二 十 一 号" ; Day22 = ss "二 十 二 号" ; Day23 = ss "二 十 三 号" ; Day24 = ss "二 十 四 号" ;
    Day25 = ss "二 十 五 号" ; Day26 = ss "二 十 六 号" ; Day27 = ss "二 十 七 号" ; Day28 = ss "二 十 八 号" ;
    Day29 = ss "二 十 九 号" ; Day30 = ss "三 十 号" ; Day31 = ss "三 十 一 号" ;

    Y0 = ss "零" ; Y1 = ss "一" ; Y2 = ss "二" ; Y3 = ss "三" ; Y4 = ss "四" ;
    Y5 = ss "五" ; Y6 = ss "六" ; Y7 = ss "七" ; Y8 = ss "八" ; Y9 = ss "九" ;

-----------------------------------------------------------------------------
-- Instead of crashing, every category should have a dummy constructor where to put a string
  lin
    recoverUnparsedPrePost string = {
      s = "·" ++ string.s ; -- if PrePost isn't parsed, use the original string
      qs = "Adakah perkara berikut berlaku:" ++ string.s -- make a question in an awkward way
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
}

