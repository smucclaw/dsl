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
    organisation = mkCN (mkN ("organisasi"|"Organisasi")) ;
    agency = mkCN (mkN ("agensi"|"Agensi")) ;
    loss = mkCN (mkN "kerugian") ;

    demand = mkV2 "tuntutan" ;
    perform = mkV2 "melaksanakan" ;
    become = mkV2 "menjadi" ;
    assess = mkVS (mkV "menilai") ;

    apply = mkVP (mkV "memohon") ;
    occur = mkVP (mkV "berlaku") ;
    respond = mkVP (mkV "membalas") ;

    covered = mkAP (mkA ("dilindungi"|"Dilindungi")) ;
    ensuing np = mkAP (strA2 "seterusnya") <lin NP np : NP>  ;
    caused_by np = mkAP (mkA2 (mkA "disebabkan") by8agent_Prep) <lin NP np : NP> ;

    NP_caused_by_PrePost np = {
      s = npStr np ++ "disebabkan oleh" ;
      qs = "Adakah" ++ npStr np ++ "disebabkan oleh"
      } ;

    NP_caused_NP_to_VP_Prep_PrePost np water escape from =
      let cl : Cl = mkCl <np : NP> (mkVP cause_V2V <water : NP> <escape : VP>) ;
          cls : ClSlash = mkClSlash cl <from : Prep> ;
          qcl : QCl = hackQCl cls ;
          ss : SSlash = mkSSlash (mkTemp pastTense simultaneousAnt) positivePol cls ;
          qs : QS = mkQS pastTense qcl ;
      in {s = ss.s ; qs = (mkUtt qs).s} ;

  oper
    cause_V2V : V2V = mkV2V "sebab" ;

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
    May = ss "Mai" ;
    Jun = ss "Jun" ;
    Jul = ss "Jul" ;
    Aug = ss "Ogos" ;
    Sep = ss "Sep" ;
    Oct = ss "Oct" ;
    Nov = ss "Nov" ;
    Dec = ss "Dis" ;

    Day_Unit = mkCN (mkN "hari") ;
    Month_Unit = mkCN (mkN "bulan") ;
    Year_Unit = mkCN (mkN "tahun") ;

}