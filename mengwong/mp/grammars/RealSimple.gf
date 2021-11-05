
abstract RealSimple = BareRG, JustWordsWordNet ** {
    flags startcat = UDS ;
    cat
      UDS ;

    fun
      -- ( every person ) (must sing)
      subjPred : NP -> VP -> UDS ;
      -- ( every person ) who (walks)
      addWho : NP -> VP -> NP ;
      -- having (sung a song) (the king must pay)
      addHaving : VP -> UDS -> UDS ;
      -- upon (seeing a cat)
      addUpon : VP -> Adv ;
      -- (if) (i see a cat) (i must pat it)
      addCond : UDS -> UDS -> UDS ;

}