
concrete RealSimpleEng of RealSimple = BareRGEng, JustWordsWordNetEng ** open SyntaxEng, (P=ParadigmsEng), ExtendEng, Prelude in {

    lin
      -- : NP -> VP -> S ;
      subjPred np vp = mkS (mkCl np vp) ;

      -- : NP -> VP -> NP ;
      addWho person walks = mkNP person (mkRS (mkRCl who_RP walks)) ;

      -- : Prep -> NP -> NP
      -- [ AdvNP (DetNP aPl_Det) (PrepNP after_Prep (MassNP (UseN lunch_N)))]
      -- tempAft after lunch = mkNP (mkNP aPl_Det) (mkPrep after_Prep (mkNP (UseN lunch_N)))

      --  : VP -> S -> S ;
      addHaving sing king_may_pay =
        let sung_AP : AP = BareRGEng.PastPartAP sing ;
            sung_Utt : Utt = mkUtt sung_AP ;       -- lincat {s : Str}
            having_Adv : Adv = P.mkAdv "having" ;  -- lincat {s : Str}
            having_sung : SS = cc2 having_Adv sung_Utt ;
        in mkS having_sung king_may_pay ;

    -- : VP -> Adv
    addUpon enter_the_club =
        let entering_the_club_NP : NP = GerundNP enter_the_club; -- make VP "enter the club" into NP first
        in mkAdv upon_Prep entering_the_club_NP;

    -- S -> S -> S ;
    addCond see_a_cat pat_it = mkS if_then_Conj see_a_cat pat_it ; 
}