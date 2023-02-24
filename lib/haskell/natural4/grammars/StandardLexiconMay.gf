concrete StandardLexiconMay of StandardLexicon = NL4BaseMay **
  open
    SyntaxMay
  , ParadigmsMay
  , (R=ResMay)
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
      in {s = ss.s ; qs = (mkUtt qs).s} ;

  oper
    cause_V2V : V2V = mkV2V "cause" ;

-- hack: just to get "does NP cause water to escape from", not "whom does NP cause water to escape from"
    hackQCl : ClSlash -> QCl = \clSlash -> mkQCl emptyIP clSlash ;

    emptyIP : IP = whatSg_IP ** {s = \\_ => []} ;

-----------------------------------------------------------------------------
-- Time units, currencies, …
lin
    Jan = ss "Jan" ;
    Feb = ss "Feb" ;
    Mar = ss "Mar" ;
    Apr = ss "Apr" ;
    May = ss "May" ;
    Jun = ss "Jun" ;
    Jul = ss "Jul" ;
    Aug = ss "Aug" ;
    Sep = ss "Sep" ;
    Oct = ss "Oct" ;
    Nov = ss "Nov" ;
    Dec = ss "Dec" ;

    Day_Unit = mkCN (mkN "day") ;
    Month_Unit = mkCN (mkN "month") ;
    Year_Unit = mkCN (mkN "year") ;

}