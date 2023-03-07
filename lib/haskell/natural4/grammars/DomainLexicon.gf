abstract DomainLexicon = NL4Base ** {

-- Generated from whatever document we are processing currently

    fun
    -- must sing
      person : CN ;
      walk, eat, drink, sing : VP ; -- VP = intransitive verb

    -- PDPA
      explanation
       , inaction
       , notification
       , PDPC
       , data_breach
       : CN ;
      public, notifiable : AP ;
      NDB_Qualification : NP ;

  -- rodents and vermin
      Loss_or_Damage : NP ;
      Contents : NP ;
      rodents : NP ;
      insects : NP ;
      vermin : NP ;
      birds : NP ;
      animal : NP ;
      household_appliance : NP ;
      swimming_pool : NP ;
      plumbing_heating_or_AC : NP ;
      any_other_exclusion : NP ;
      water : NP ;
      escape : VP ;


}