

concrete UDExtEng of UDExt = UDAppEng ** open
  Prelude,
  SyntaxEng, (P=ParadigmsEng), ExtendEng,
  (Se=SentenceEng),
  (Ex=ExtraEng) in {

  lin
    -- : UDS -> UDS -> UDFragment ;
    Upon upon action =
      let upon_Adv : Adv = SyntaxEng.mkAdv upon_Prep (AdjAsNP upon.pred.presp) ;
          action_S : S = lin S action ; --{s = linUDS action} ;
       in Se.ExtAdvS upon_Adv action_S ;

    -- : NP -> UDS -> UDFragment ;
    subjAction subj uds = PredVPS subj uds.pred.fin ;

    -- : UDS -> NP -> NP ; -- EVERY king WHO is a singer
    Who is_singer king =
      let who_is_singer_Adv : Adv = lin Adv (PredVPS who_NP is_singer.pred.fin) ;
       in ExtAdvNP king who_is_singer_Adv ;

    Every np = mkNP (lin Predet {s = "every"}) np ;

    May = applyDeontic ExtraEng.may_VV ;
    Must = applyDeontic must_VV ;
    Shant = applyDeontic shant_VV ;

    -- : AP -> Conj -> [CN] -> NP -> CN ; -- unauthorised access or copying of personal data
    CN_AP_Conj_CNs_of_NP ap conj cns np =
      let conjCN : CN = ConjCN conj cns ;
       in mkCN ap (mkCN conjCN (mkAdv possess_Prep np)) ;

  oper
    applyDeontic : VV -> LinUDS -> LinUDS = \may,king_sing ->
      let may_sing : VP = ComplVPIVV may king_sing.pred.inf ;
          king_may_sing : LinUDS = king_sing ** {pred = myVPS may_sing} ;
       in king_may_sing ;
     --in {s = linUDS king_may_sing} ;

    shant_VV : VV = Ex.shall_VV ** { -- only used in (very limited) NLG, not parsing
      s = \\_ => "shan't" ;                -- so negation here should be fine
    } ;

    who_NP : NP = mkNP (P.mkPN "who") ;

  lin
-- Aarne
    -- : Numeral -> UDS -> UDFragment ;
    Adv_no_later_than_Num_calendar_days_after_the_day_UDS num uds =
      SyntaxEng.mkAdv
        (P.mkPrep "no later than")
        (mkNP (mkNP num (P.mkN "calendar day"))
              (lin Adv {s = "after the day" ++ linUDS uds})) ;



}
