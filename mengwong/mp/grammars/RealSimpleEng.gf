
concrete RealSimpleEng of RealSimple = BareRGEng, JustWordsWordNetEng ** open SyntaxEng in {
    lin
      -- : NP -> VP -> UDS ;
      subjPred np vp = mkS (mkCl np vp) ;

      -- : NP -> VP -> NP ;
      addWho person walks = mkNP person (mkRS (mkRCl who_RP walks)) ;
}