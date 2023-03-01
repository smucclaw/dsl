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
    agency = mkCN (mkN ("机 构")) ;
    loss = mkCN (mkN "亏 损") ;

    demand = mkV2 "需 求" ;
    perform = mkV2 "履 行" ;
    become = mkV2 "成 为" ;
    become_aware = mkVS "察 觉" ;
    -- assess = mkVS (mkV "评估") ;

    apply = mkVP (mkV "申 请") ;
    occur = mkVP (mkV "发 案") ;
    respond = mkVP (mkV "回 应") ;

    covered = mkAP (mkA ("保 户")) ;
    ensuing np = mkAP (strA2 "接 着") <lin NP np : NP>  ;
    caused_by np = mkAP (mkA2 (mkA "的 原 因") by8agent_Prep) <lin NP np : NP> ;

    NP_caused_by_PrePost np = {
      s = npStr np ++ "的 原 因" ; -- np ++ "caused by"
      qs = "那 个" ++ npStr np ++ "的 原 因" -- "Is the" ++ np ++ "caused by"
      } ;

    NP_caused_NP_to_VP_Prep_PrePost np water escape from =
      let cl : Cl = mkCl <np : NP> (mkVP cause_V2V <water : NP> <escape : VP>) ;
          cls : ClSlash = mkClSlash cl <from : Prep> ;
          qcl : QCl = hackQCl cls ;
          ss : SSlash = mkSSlash (mkTemp pastTense simultaneousAnt) positivePol cls ;
          qs : QS = mkQS pastTense qcl ;
      in {s = ss.s ++ R.linPrep ss.c2 ; qs = (mkUtt qs).s} ;

  oper
    cause_V2V : V2V = mkV2V (mkV "导 致") ;

-- hack: just to get "does NP cause water to escape from", not "whom does NP cause water to escape from"
    hackQCl : ClSlash -> QCl = \clSlash -> mkQCl emptyIP clSlash ;

    emptyIP : IP = whatSg_IP ** {s = []} ;

-----------------------------------------------------------------------------
-- Time units, currencies, …
lin
    Jan = ss "一 月" ;
    Feb = ss "二 月" ;
    Mar = ss "三 月" ;
    Apr = ss "四 月" ;
    May = ss "五 月" ;
    Jun = ss "六 月" ;
    Jul = ss "七 月" ;
    Aug = ss "八 月" ;
    Sep = ss "九 月" ;
    Oct = ss "十 月" ;
    Nov = ss "十 一 月" ;
    Dec = ss "十 二 月" ;

    Day1 = ss "一 号" ;
    Day2 = ss "二 号" ;
    Day3 = ss "三 号" ;
    Day4 = ss "四 号" ;
    Day5 = ss "五 号" ;
    Day6 = ss "六 号" ;
    Day7 = ss "七 号" ;
    Day8 = ss "八 号" ;
    Day9 = ss "九 号" ;
    Day10 = ss "十 号" ;
    Day11 = ss "十 一 号" ;
    Day12 = ss "十 二 号" ;
    Day13 = ss "十 三 号" ;
    Day14 = ss "十 四 号" ;
    Day15 = ss "十 五 号" ;
    Day16 = ss "十 六 号" ;
    Day17 = ss "十 七 号" ;
    Day18 = ss "十 八 号" ;
    Day19 = ss "十 九 号" ;
    Day20 = ss "二 十 号" ;
    Day21 = ss "二 十 一 号" ;
    Day22 = ss "二 十 二 号" ;
    Day23 = ss "二 十 三 号" ;
    Day24 = ss "二 十 四 号" ;
    Day25 = ss "二 十 五 号" ;
    Day26 = ss "二 十 六 号" ;
    Day27 = ss "二 十 七 号" ;
    Day28 = ss "二 十 八 号" ;
    Day29 = ss "二 十 九 号" ;
    Day30 = ss "三 十 号" ;
    Day31 = ss "三 十 一 号" ;

    Y0 = ss "零" ;
    Y1 = ss "一" ;
    Y2 = ss "二" ;
    Y3 = ss "三" ;
    Y4 = ss "四" ;
    Y5 = ss "五" ;
    Y6 = ss "六" ;
    Y7 = ss "七" ;
    Y8 = ss "八" ;
    Y9 = ss "九" ;
    Y10 = ss "十" ;

    lin MkYear a b c d = cc2 (cc4 a b c d) (ss "年") ; 

    Day_Unit = mkCN (mkN "day") ;
    Month_Unit = mkCN (mkN "month") ;
    Year_Unit = mkCN (mkN "year") ;

}