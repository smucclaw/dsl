concrete DomainLexiconMay of DomainLexicon = NL4BaseMay **
  open
    SyntaxMay
  , ParadigmsMay
  , LexiconMay
  in {

  lin
  -- must sing
    person = mkCN (mkN "seseorang") ;
    walk = mkVP (mkV "jalan") ;
    eat = mkVP <eat_V2 : V> ;
    drink = mkVP <drink_V2 : V> ;
    sing = mkVP (mkV "nyanyi") ;

  -- pdpa
    explanation = mkCN (mkN "penjelasan") ;
    inaction = mkCN (mkN "tidak bertindak") ;
    notification = mkCN (mkN ("pemberitahuan"|"Pemberitahuan")) ;
    PDPC = mkCN (mkN "PDPC") ;
    data_breach = mkCN (mkN ("pelanggaran data"|"Pelanggaran Data")) ;
    public = mkAP (mkA ("awam"|"Awam")) ;
    notifiable = mkAP (mkA ("boleh dimaklumkan"|"Boleh Dimaklumkan")) ;
    aware = mkAP (mkA "sedar") ;
    NDB_Qualification = mkNP (mkN "Kelayakan NDB") ;


  -- rodents and vermin
    Loss_or_Damage = mkNP (mkN "Kerugian atau Kerosakan") ;
    Contents = mkNP aPl_Det (mkN ("kandungan"|"Kandungan")) ;
    rodents = mkNP aPl_Det (mkN "rodensia") ;
    insects = mkNP aPl_Det (mkN "serangga") ;
    vermin = mkNP (mkN "binatang perosak") ;
    birds = mkNP aPl_Det (mkN "burung") ;
    water = mkNP (mkN "air") ;

    animal = mkNP aSg_Det (mkN "binatang") ;
    household_appliance = mkNP aSg_Det (appliance_N) ;
    swimming_pool = mkNP aSg_Det pool_N ;
    plumbing_heating_or_AC = mkNP or_Conj (mkNP plumbing_N) (mkNP hvac_N) ;

    any_other_exclusion = mkNP (mkN "sebarang pengecualian lain") ;
    escape = mkVP leak_V ;

  oper
  -- Google translate or Wikipedia, TODO check these
    plumbing_N : N = mkN "sistem paip" ;
    hvac_N : N = mkN "HVAC" ;
    leak_V : V = mkV "kebocoran" ;
    pool_N : N = mkN "kolam renang" ;
    appliance_N : N = mkN "perkakas rumah" ;
}