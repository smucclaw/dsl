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

  lin Day1 = ss "1" ;
      Day2 = ss "2" ;
      Day3 = ss "3" ;
      Day4 = ss "4" ;
      Day5 = ss "5" ;
      Day6 = ss "6" ;
      Day7 = ss "7" ;
      Day8 = ss "8" ;
      Day9 = ss "9" ;
      Day10 = ss "10" ;
      Day11 = ss "11" ;
      Day12 = ss "12" ;
      Day13 = ss "13" ;
      Day14 = ss "14" ;
      Day15 = ss "15" ;
      Day16 = ss "16" ;
      Day17 = ss "17" ;
      Day18 = ss "18" ;
      Day19 = ss "19" ;
      Day20 = ss "20" ;
      Day21 = ss "21" ;
      Day22 = ss "22" ;
      Day23 = ss "23" ;
      Day24 = ss "24" ;
      Day25 = ss "25" ;
      Day26 = ss "26" ;
      Day27 = ss "27" ;
      Day28 = ss "28" ;
      Day29 = ss "29" ;
      Day30 = ss "30" ;
      Day31 = ss "31" ;

  lin 
    Y0 = ss "0" ;
    Y1 = ss "1" ;
    Y2 = ss "2" ;
    Y3 = ss "3" ;
    Y4 = ss "4" ;
    Y5 = ss "5" ;
    Y6 = ss "6" ;
    Y7 = ss "7" ;
    Y8 = ss "8" ;
    Y9 = ss "9" ;
    Y10 = ss "10" ;

    lin MkYear = cc4 ; 

    Day_Unit = mkCN (mkN "hari") ;
    Month_Unit = mkCN (mkN "bulan") ;
    Year_Unit = mkCN (mkN "tahun") ;

}