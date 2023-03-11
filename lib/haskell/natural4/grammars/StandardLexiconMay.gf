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
    become = mkV2 "jadi" ;
    become_aware = mkVS (mkV "sedari" R.Meng) ;
    assess = mkVS (mkV "nilai") ;
    -- apply = mkVP (mkV "kaitan") ;
    apply = mkVP (ComplVSthat (mkVS (mkV "kaitan")) that_Subj) ;
    occur = mkVP (mkV "laku") ;
    respond = mkVP (mkV "membalas") ;

    covered = mkAP (mkA ("yang dilindungi")) ;
    ensuing np = mkAP (strA2 "berlaku akibat") <lin NP np : NP> ;
    caused_by np = mkAP (strA2  "disebabkan") <lin NP np : NP> ;

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

}