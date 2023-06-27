abstract DomainLexicon = NL4Base ** {

-- Generated from whatever document we are processing currently

    fun
    -- must sing
      person : CN ;
      beverage : CN ;
      consume : V2 ;
      alcoholic, non_alcoholic : AP ;
      walk, eat, drink, sing : VP ; -- VP = intransitive verb
      in_part, in_whole : Adv ;

    -- PDPA
      explanation
       , inaction
       , notification
       , PDPC
       , data_breach
       , public_agency
       , access
       , use
       , disclosure
       , copying
       , modification
       , disposal
       , personal_data
       : CN ;
      public, notifiable, any_unauthorised, aware : AP ;
      unable : VS ;
      fall_due : VPS ;
      NDB_Qualification
      , liabilities
      : NP ;


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