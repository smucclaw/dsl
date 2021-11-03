
concrete RealSimpleEng of RealSimple = BareRGEng, JustWordsWordNetEng ** open SyntaxEng in {
    lin
      -- : NP -> VP -> UDS ;
      subjPred np vp = mkS (mkCl np vp) ;

      -- : NP -> VP -> NP ;
      addWho person walks = mkNP person (mkRS (mkRCl who_RP walks)) ;

      -- : Prep -> NP -> NP
      -- [ AdvNP (DetNP aPl_Det) (PrepNP after_Prep (MassNP (UseN lunch_N)))]
      tempAft after lunch = mkNP (mkNP aPl_Det) (mkPrep after_Prep (mkNP (UseN lunch_N)))
}