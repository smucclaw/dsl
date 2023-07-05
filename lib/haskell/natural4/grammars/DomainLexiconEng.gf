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
      non_alcoholic = mkAP (mkA ("non-alcoholic"|"a non-alcoholic")) ;

      consume = mkV2 "consume" ;
      walk = mkVP (mkV "walk") ;
      eat = mkVP IrregEng.eat_V ;
      drink = mkVP IrregEng.drink_V ;
      sing = mkVP IrregEng.sing_V ;
      in_part = ParadigmsEng.mkAdv "in part" ;
      in_whole = ParadigmsEng.mkAdv "in whole" ;

  -- pdpa
      explanation = mkCN (mkN "explanation") ;
      inaction = mkCN (mkN "inaction") ;
      notification = mkCN (mkN ("notification"|"Notification")) ;
      PDPC = mkCN (mkN "PDPC") ;
      data_breach = mkCN (mkN ("data breach"|"Data Breach")) ;
      access = mkCN (mkN "access") ;
      use = mkCN (mkN "use") ;
      disclosure = mkCN (mkN "disclosure") ;
      copying = mkCN (mkN "copying") ;
      modification = mkCN (mkN "modification") ;
      disposal = mkCN (mkN "disposal") ;
      personal_data = mkCN (mkN "personal data") ;
      any_unauthorised = mkAP (mkA "any unauthorised") ;
      public = mkAP (mkA ("public"|"Public")) ;
      public_agency = mkCN (mkN "public agency") ;
      notifiable = mkAP (mkA ("notifiable"|"Notifiable")) ;
      aware = mkAP (mkA "aware") ;
      NDB_Qualification = mkNP (mkN "NDB Qualification") ;
      liabilities = mkNP (mkN "liabilities") ;
      fall_due = MkVPS presSimul POS (mkVP (partV (mkV "fall") "due")) ;
      unable = mkVS (mkV "unable to pay") ;
      unable_to_VP_VP vp = mkVP unable_VV vp ;
      pay_V2S = mkV2S IrregEng.pay_V noPrep ;

oper
  unable_VV : VV = mkVV (mkV "unable" "unable" "was unable" "has been unable" "being unable") ;

lin
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