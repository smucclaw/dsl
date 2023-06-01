concrete DomainLexiconEng of DomainLexicon = NL4BaseEng **
  open
    SyntaxEng
  , ParadigmsEng
  , IrregEng
  in {

-- Generated from whatever document we are processing currently
-- TODO: is that feasible? how many different modules we need? any other ways to fail gracefully?

  lin
  -- must sing
      person = mkCN (mkN ("person"|"Person")) ;
      beverage = mkCN (mkN "beverage") ;
      alcoholic = mkAP (mkA ("alcoholic" | "an alcoholic")) ; -- just to make it parse
      non_alcoholic = mkAP (mkA "non-alcoholic") ;

      consume = mkV2 "consume" ;
      walk = mkVP (mkV "walk") ;
      eat = mkVP IrregEng.eat_V ;
      drink = mkVP IrregEng.drink_V ;
      sing = mkVP IrregEng.sing_V ;

  -- pdpa
      explanation = mkCN (mkN "explanation") ;
      inaction = mkCN (mkN "inaction") ;
      notification = mkCN (mkN ("notification"|"Notification")) ;
      PDPC = mkCN (mkN "PDPC") ;
      data_breach = mkCN (mkN ("data breach"|"Data Breach")) ;
      public = mkAP (mkA ("public"|"Public")) ;
      public_agency = mkCN (mkN "public agency") ;
      notifiable = mkAP (mkA ("notifiable"|"Notifiable")) ;
      aware = mkAP (mkA "aware") ;
      NDB_Qualification = mkNP (mkN "NDB Qualification") ;


  -- rodents and vermin
      Loss_or_Damage = mkNP (mkN "Loss or Damage") ;
      Contents = mkNP aPl_Det (mkN ("content"|"Content")) ;
      rodents = mkNP aPl_Det (mkN "rodent") ;
      insects = mkNP aPl_Det (mkN "insect") ;
      vermin = mkNP (mkN "vermin") ;
      birds = mkNP aPl_Det (mkN "bird") ;
      water = mkNP (mkN "water") ;

      animal = mkNP aSg_Det (mkN "animal") ;
      household_appliance = mkNP aSg_Det (mkN "household appliance") ;
      swimming_pool = mkNP aSg_Det (mkN "swimming pool") ;
      plumbing_heating_or_AC = mkNP aSg_Det (mkN "plumbing, heating, or air conditioning system") ;

      any_other_exclusion = mkNP (mkN "any other exclusion") ;
      escape = mkVP (mkV "escape") ;


}