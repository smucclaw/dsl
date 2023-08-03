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

  -- Words from closed classes, like prepositions etc.
  lin
    within_Prep = withinPrep ;
    only_AdA = lin AdA only_Predet ;

  -- Open classes, like nouns, verbs etc.
  lin
    organisation = mkCN (mkN ("organisation"|"Organisation")) ;
    agency = mkCN (mkN ("机 构")) ;
    loss = mkCN (mkN "亏 损") ;

    demand = mkV2 "需 求" ;
    perform = mkV2 "履 行" ;
    become = mkV2 "成 为" ;
    become_aware = mkVS "意识到" ;
    assess = mkVS (mkV "评估") ;

    apply = mkVP (mkV "申 请") ;
    occur = mkVP (mkV "发生") ;
    respond = mkVP (mkV "回 应") ;

    covered = mkAP (mkA ("保 户")) ;
    ensuing np = mkAP (mkA2 "接 着") <lin NP np : NP>  ;
    caused_by np = mkAP (mkA2 (mkA "的 原 因") by8agent_Prep) <lin NP np : NP> ;

    NP_caused_by_PrePost np = {
      s = npStr np ++ "的 原 因" ; -- np ++ "caused by"
      qs = "那 个" ++ npStr np ++ "的 原 因" -- "Is the" ++ np ++ "caused by"
      } ;

    NP_caused_NP_to_VP_Prep_PrePost np water escape from =
      let cl : Cl = mkCl <np : NP> (mkVP cause_V2V <water : NP> <escape : VP>) ;
          cls : ClSlash = mkClSlash cl <from : CatChi.Prep> ;
          qcl : QCl = hackQCl cls ;
          ss : SSlash = mkSSlash (mkTemp pastTense simultaneousAnt) positivePol cls ;
          qs : QS = mkQS pastTense qcl ;
      in {s = ss.s ++ R.linPrep ss.c2 ; qs = (mkUtt qs).s} ;

  oper
    cause_V2V : V2V = mkV2V (mkV "导 致") ;

-- hack: just to get "does NP cause water to escape from", not "whom does NP cause water to escape from"
    hackQCl : ClSlash -> QCl = \clSlash -> mkQCl emptyIP clSlash ;

    emptyIP : IP = whatSg_IP ** {s = []} ;

}