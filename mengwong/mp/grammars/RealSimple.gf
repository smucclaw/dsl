
abstract RealSimple = BareRG, JustWordsWordNet ** {
    flags startcat = UDS ;
    cat
      UDS ;

    fun
      subjPred : NP -> VP -> UDS ;
      addWho : NP -> VP -> NP ;
      tempAft: Prep -> NP -> NP;

      addHaving : VP -> UDS -> UDS ;

}