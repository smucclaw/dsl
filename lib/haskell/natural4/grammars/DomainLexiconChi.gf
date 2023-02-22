concrete DomainLexiconChi of DomainLexicon = NL4BaseChi **
  open
    SyntaxChi
  , ParadigmsChi
  , LexiconChi
  in {

  lin
  -- must sing
    person = mkCN person_N ;
    walk = mkVP walk_V ;
    eat = mkVP <eat_V2 : V> ;
    drink = mkVP <drink_V2 : V> ;
    sing = mkVP sing_V ;

  -- pdpa
    explanation = mkCN (mkN "explanation") ;
    inaction = mkCN (mkN "inaction") ;
    notification = mkCN (mkN ("notification"|"Notification")) ;
    PDPC = mkCN (mkN "PDPC") ;
    data_breach = mkCN (mkN ("data breach"|"Data Breach")) ;
    public = mkAP (mkA ("public"|"Public")) ;
    notifiable = mkAP (mkA ("notifiable"|"Notifiable")) ;
    aware = mkAP (mkA "aware") ;
    NDB_Qualification = mkNP (mkN "NDB Qualification") ;


  -- rodents and vermin
    Loss_or_Damage = mkNP (mkN "Loss or Damage") ;
    Contents = mkNP aPl_Det (mkN ("content"|"Content")) ;
    rodents = mkNP aPl_Det (mkN "rodent") ;
    insects = mkNP aPl_Det (mkN "insect") ;
    vermin = mkNP (mkN "vermin") ;
    birds = mkNP aPl_Det bird_N ;
    water = mkNP water_N ;

    animal = mkNP aSg_Det animal_N ;
    household_appliance = mkNP aSg_Det appliance_N ;
    swimming_pool = mkNP aSg_Det pool_N ;
    plumbing_heating_or_AC = mkNP or_Conj (mkNP plumbing_N) (mkNP hvac_N) ;

    any_other_exclusion = mkNP (mkN "any other exclusion") ;
    escape = mkVP leak_V ;

  oper
  -- Google translate or Wikipedia, TODO check these
    plumbing_N : N = mkN "管路系统" ;
    hvac_N : N = mkN "暖通空調" ;
    leak_V : V = mkV "漏" ;
    pool_N : N = mkN "游泳池" ;
    appliance_N : N = mkN "家用电器" ;

}