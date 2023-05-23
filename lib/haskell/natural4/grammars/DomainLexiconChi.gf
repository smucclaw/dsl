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
    eat = mkVP <lin V eat_V2 : V> ;
    drink = mkVP <lin V drink_V2 : V> ;
    sing = mkVP sing_V ;

  -- pdpa
    explanation = mkCN (mkN "说 法") ;
    inaction = mkCN (mkN "不 作 为") ; --indirectly obtained from zaobao.com, specifically the term "democratic inaction"
    notification = mkCN (mkN ("通 知")) ;
    PDPC = mkCN (mkN "个 人 资 料 保 护 委 员 会") ; --obtained from zaobao.com
    data_breach = mkCN (mkN ("数据泄露")) ;
    public = mkAP (mkA ("公 共")) ;
    public_agency = mkCN (mkN "公众的代理行") ; -- TODO check, just copy and paste from unreliable source
    notifiable = mkAP (mkA ("应 通 报 的")) ;
    -- aware = mkAP (mkA "发现") ;
    NDB_Qualification = mkNP (mkN "NDB 验证") ;


  -- rodents and vermin
    Loss_or_Damage = mkNP (mkN "损 失 或 损 害") ;
    Contents = mkNP aPl_Det (mkN ("内 件")) ;
    rodents = mkNP aPl_Det (mkN "啮 齿") ;
    insects = mkNP aPl_Det (mkN "昆 虫") ;
    vermin = mkNP (mkN "蠹 虫") ;
    birds = mkNP aPl_Det bird_N ;
    water = mkNP water_N ;

    animal = mkNP aSg_Det animal_N ;
    household_appliance = mkNP aSg_Det appliance_N ;
    swimming_pool = mkNP aSg_Det pool_N ;
    plumbing_heating_or_AC = mkNP or_Conj (mkNP plumbing_N) (mkNP hvac_N) ;

    any_other_exclusion = mkNP (mkN "任 何 其 他 排 除") ;
    escape = mkVP leak_V ;

  oper
  -- Google translate or Wikipedia, TODO check these
    plumbing_N : N = mkN "管 路 系 统" ;
    hvac_N : N = mkN "暖 通 空 调" ;
    leak_V : V = mkV "漏" ;
    pool_N : N = mkN "游 泳 池" ;
    appliance_N : N = mkN "家 用 电 器" ;

}