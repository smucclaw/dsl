
abstract RealSimple = BareRG, JustWordsWordNet ** {
    flags startcat = S ;

    fun
      -- ( every person ) (must sing)
      subjPred : NP -> VP -> S ;
      -- ( every person ) who (walks)
      addWho : NP -> VP -> NP ;
      -- having (sung a song) (the king must pay)
      addHaving : VP -> S -> S ;
      -- upon (seeing a cat)
      addUpon : VP -> Adv ;
      -- (if) (i see a cat) (i must pat it)
      addCond : S -> S -> S ;

}