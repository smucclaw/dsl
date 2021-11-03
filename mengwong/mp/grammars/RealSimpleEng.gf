
concrete RealSimpleEng of RealSimple = BareRGEng, JustWordsWordNetEng ** open SyntaxEng, (P=ParadigmsEng), Prelude in {
    lincat

      UDS = {s : Str} ;

    lin
      -- : NP -> VP -> UDS ;
      subjPred np vp = mkS (mkCl np vp) ;

      -- : NP -> VP -> NP ;
      addWho person walks = mkNP person (mkRS (mkRCl who_RP walks)) ;

      -- : Prep -> NP -> NP
      -- [ AdvNP (DetNP aPl_Det) (PrepNP after_Prep (MassNP (UseN lunch_N)))]
      --tempAft after lunch = mkNP (mkNP aPl_Det) (mkPrep after_Prep (mkNP (UseN lunch_N)))

      --  : VP -> UDS -> UDS ;
      addHaving sing king_may_pay =
        let sung_AP : AP = BareRGEng.PastPartAP sing ;
            sung_Utt : Utt = mkUtt sung_AP ;       -- lincat {s : Str}
            having_Adv : Adv = P.mkAdv "having" ;  -- lincat {s : Str}
            having_sung : SS = cc2 having_Adv sung_Utt ;
        in cc3 having_sung (ss ",") king_may_pay ;


}