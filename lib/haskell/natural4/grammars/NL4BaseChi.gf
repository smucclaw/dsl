concrete NL4BaseChi of NL4Base =
     CustomSyntaxChi
  ** NL4BaseFunctor - [
      Cond,ListCond,BaseCond, ConsCond, ConjCond, ConjPreCond, ConjPrePostCond
    , WHEN, qCOND, sCOND
    , qUPON, sUPON -- change tense
    , ConjConstraint -- Conj has different lincat
    , MkDate -- different order
    , recoverUnparsedAdv -- different lincat
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

  lin RPConstraint cond on date =
      let onDate : CatChi.Adv = SyntaxChi.mkAdv <on : SyntaxChi.Prep> date ;
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

    qCOND cond = cc2 (mkUtt (ExtendChi.SQuestVPS cond.subj cond.pred)) (ss bindQM) ;
    sCOND cond = mkUtt (ExtendChi.PredVPS cond.subj cond.pred) ;

    qUPON subj upon = qWHO subj (Extend.MkVPS presSimul positivePol upon) ;
    sUPON subj upon = sWHO subj (Extend.MkVPS presSimul positivePol upon) ;


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
    MkDate day month year = symb (cc3 (cc2 year (ss "年")) month (cc2 day (ss "日"))) ;

    Jan = ss "1 月" ; Feb = ss "2 月" ; Mar = ss "3 月" ; Apr = ss "4 月" ;
    May = ss "5 月" ; Jun = ss "6 月" ; Jul = ss "7 月" ; Aug = ss "8 月" ;
    Sep = ss "9 月" ; Oct = ss "10 月" ; Nov = ss "11 月" ; Dec = ss "12 月" ;

    -- Jan = ss "一 月" ; Feb = ss "二 月" ; Mar = ss "三 月" ; Apr = ss "四 月" ;
    -- May = ss "五 月" ; Jun = ss "六 月" ; Jul = ss "七 月" ; Aug = ss "八 月" ;
    -- Sep = ss "九 月" ; Oct = ss "十 月" ; Nov = ss "十 一 月" ; Dec = ss "十 二 月" ;

-----------------------------------------------------------------------------
-- Instead of crashing, every category should have a dummy constructor where to put a string
  lin
    recoverUnparsedPrePost string = {
      s = string.s ; -- if PrePost isn't parsed, use the original string
      qs = string.s -- make a question in an awkward way
      } ;

    -- : String -> String -> Constraint ;
    recoverRPis damage toContents = {
      s = damage.s ++ "是" ++ toContents.s ; -- if constraint isn't parsed, use the original string
      qs = damage.s ++ "是" ++ toContents.s ++ "吗" ++ bindQM
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

    recoverUnparsedTimeUnit string = mkCN <LexiconChi.day_N ** {s = string.s} : N> ;

    recoverUnparsedAdv string = LexiconChi.today_Adv ** {s = string.s} ;
}

