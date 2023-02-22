concrete StandardLexiconChi of StandardLexicon = NL4BaseChi **
  open
    SyntaxChi
  , ParadigmsChi
  , (R=ResChi)
  , Prelude
  in {

  -- Collection of "basic" words that we expect to appear across multiple documents in legal domain
  -- Very ad hoc at the moment, should consult a proper legal corpus, analyse valencies and complement types etc.
  -- This module should be the really high quality stuff
  lin
    organisation = mkCN (mkN ("organisation"|"Organisation")) ;
    agency = mkCN (mkN ("agency"|"Agency")) ;
    loss = mkCN (mkN "loss") ;

    demand = mkV2 "demand" ;
    perform = mkV2 "perform" ;
    become = mkV2 "become" ;
    assess = mkVS (mkV "assess") ;

    apply = mkVP (mkV "apply") ;
    occur = mkVP (mkV "occur") ;
    respond = mkVP (mkV "respond") ;

    covered = mkAP (mkA ("covered"|"Covered")) ;
    ensuing np = mkAP (strA2 "ensuing") <lin NP np : NP>  ;
    caused_by np = mkAP (mkA2 (mkA "caused") by8agent_Prep) <lin NP np : NP> ;

    NP_caused_by_PrePost np = {
      s = npStr np ++ "caused by" ;
      qs = "Is the" ++ npStr np ++ "caused by"
      } ;

    NP_caused_NP_to_VP_Prep_PrePost np water escape from =
      let cl : Cl = mkCl <np : NP> (mkVP cause_V2V <water : NP> <escape : VP>) ;
          cls : ClSlash = mkClSlash cl <from : Prep> ;
          qcl : QCl = hackQCl cls ;
          ss : SSlash = mkSSlash (mkTemp pastTense simultaneousAnt) positivePol cls ;
          qs : QS = mkQS pastTense qcl ;
      in {s = ss.s ++ R.linPrep ss.c2 ; qs = (mkUtt qs).s} ;

  oper
    cause_V2V : V2V = mkV2V (mkV "引起") ;

-- hack: just to get "does NP cause water to escape from", not "whom does NP cause water to escape from"
    hackQCl : ClSlash -> QCl = \clSlash -> mkQCl emptyIP clSlash ;

    emptyIP : IP = whatSg_IP ** {s = []} ;

-----------------------------------------------------------------------------
-- Time units, currencies, …
lin
    Jan = ss "一月" ;
    Feb = ss "二月" ;
    Mar = ss "三月" ;
    Apr = ss "四月" ;
    May = ss "五月" ;
    Jun = ss "六月" ;
    Jul = ss "七月" ;
    Aug = ss "八月" ;
    Sep = ss "九月" ;
    Oct = ss "十月" ;
    Nov = ss "十一月" ;
    Dec = ss "十二月" ;

    Day_Unit = mkCN (mkN "day") ;
    Month_Unit = mkCN (mkN "month") ;
    Year_Unit = mkCN (mkN "year") ;

}