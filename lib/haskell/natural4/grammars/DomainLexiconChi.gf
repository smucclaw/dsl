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
    explanation = mkCN (mkN "说法") ;
    inaction = mkCN (mkN "不作为") ; --indirectly obtained from zaobao.com, specifically the term "democratic inaction"
    notification = mkCN (mkN ("通知")) ;
    PDPC = mkCN (mkN "个人资料保护委员会") ; --obtained from zaobao.com
    data_breach = mkCN (mkN ("资料外泄事件")) ; --obtained from zaobao.com
    public = mkAP (mkA ("公共")) ;
    notifiable = mkAP (mkA ("应通报的")) ;
    -- aware = mkAP (mkA "发现") ;
    NDB_Qualification = mkNP (mkN "NDB Qualification") ;


  -- rodents and vermin
    Loss_or_Damage = mkNP (mkN "损失或损害") ;
    Contents = mkNP aPl_Det (mkN ("内容")) ;
    rodents = mkNP aPl_Det (mkN "啮齿") ;
    insects = mkNP aPl_Det (mkN "昆虫") ;
    vermin = mkNP (mkN "蠹虫") ;
    birds = mkNP aPl_Det bird_N ;
    water = mkNP water_N ;

    animal = mkNP aSg_Det animal_N ;
    household_appliance = mkNP aSg_Det appliance_N ;
    swimming_pool = mkNP aSg_Det pool_N ;
    plumbing_heating_or_AC = mkNP or_Conj (mkNP plumbing_N) (mkNP hvac_N) ;

    any_other_exclusion = mkNP (mkN "任何其他豁除") ;
    escape = mkVP leak_V ;

  oper
  -- Google translate or Wikipedia, TODO check these
    plumbing_N : N = mkN "管路系统" ;
    hvac_N : N = mkN "暖通空调" ;
    leak_V : V = mkV "漏" ;
    pool_N : N = mkN "游泳池" ;
    appliance_N : N = mkN "家用电器" ;

}