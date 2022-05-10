

concrete UDExtEng of UDExt = UDAppEng ** open
  Prelude,
  SyntaxEng, (P=ParadigmsEng), ExtendEng,
  (Se=SentenceEng),
  (Ex=ExtraEng) in {


  lincat
    UDFragment = S ;
    [UDFragment] = [S];

  lin
    -- : UDS -> UDFragment -> UDFragment ;
    Upon upon action =
      let upon_Adv : Adv = SyntaxEng.mkAdv upon_Prep (AdjAsNP upon.pred.presp) ;
       in Se.ExtAdvS upon_Adv action ;

    Cond cond action =
      let cond_Adv : Adv = SyntaxEng.mkAdv SyntaxEng.if_Subj (udsToS cond) ;
       in Se.AdvS action cond_Adv;

    Temporal temp action = Se.AdvS action temp;

    Given given action =
      let given_Subj : Subj = lin Subj (ss "given that") ;
          given_Adv : Adv = SyntaxEng.mkAdv given_Subj (udsToS given);
       in Se.AdvS action given_Adv ;

    -- : NP -> UDS -> UDFragment ;
    subjAction subj uds = PredVPS subj uds.pred.fin ;

    -- : UDS -> NP -> NP ; -- EVERY king WHO is a singer
    Who is_singer king =
      let who_is_singer_Adv : Adv = lin Adv (PredVPS who_NP is_singer.pred.fin) ;
       in ExtAdvNP king who_is_singer_Adv ;

    Every np = mkNP (lin Predet {s = "every"}) np ;
    TokAll np = mkNP (lin Predet {s = "all"}) np ;
    Party np = np ;

    DMay = applyDeontic ExtraEng.may_VV ;
    DMust = applyDeontic must_VV ;
    DShant = applyDeontic shant_VV ;

    -- TODO: types ???
    Means breach data_is_lost =
      let mean_VS   : VS = P.mkVS mean_V ;
          meansThat : VP = mkVP mean_VS (udsToS data_is_lost) ;
          mean_V2   : V2 = P.mkV2 mean_V ;
          meansThing : VP = mkVP mean_V2 data_is_lost.pred.np ;
          chosenVP : VP = case data_is_lost.pred.isNP of {
            True => meansThing ;
            False => meansThat
          } ;
       in mkS (mkCl breach chosenVP) ;

    -- : NP -> UDS -> UDFragment ; -- TODO: types?
    RPis,
    RPeq = \sky,blue -> PredVPS sky blue.pred.fin ;
{-  RPlt,  -- TODO: later. maybe need different types?
    RPlte,
    RPgt,
    RPgte,
    RPelem,
    RPnotElem -}

    -- : UDFragment -> UDS -> UDFragment ; -- breach is severe WHEN data is lost
    HornClause2 breach_is_severe data_is_lost =
      let when_data_lost_Adv = mkAdv SyntaxEng.when_Subj (udsToS data_is_lost)
       in hornlike breach_is_severe when_data_lost_Adv ;

    CondStandalone uds = ss (linUDS uds) ;
    TemporalStandalone uds = ss (linUDS uds) ;
    GivenStandalone uds = ss (linUDS uds) ;
    UponStandalone uds = ss (linUDS uds) ;

    -- : UDS -> UDS -> UDFragment -> UDFragment

    CondUpon cond upon king =
      let cond_Adv : Adv = SyntaxEng.mkAdv if_Subj (udsToS cond) ;
       in Se.ExtAdvS cond_Adv (Upon upon king) ;

    CondTemporal cond temporal king =
      let cond_Adv : Adv = SyntaxEng.mkAdv if_Subj (udsToS cond) ;
       in Se.ExtAdvS cond_Adv (Se.AdvS king temporal) ;


    CondGiven cond given king =
      Se.AdvS (conditionsHold king) (makeList cond given);

    -- : AP -> Conj -> [CN] -> NP -> CN ; -- unauthorised access or copying of personal data
    CN_AP_Conj_CNs_of_NP ap conj cns np =
      let conjCN : CN = ConjCN conj cns ;
       in mkCN ap (mkCN conjCN (mkAdv possess_Prep np)) ;


  oper
    applyDeontic : VV -> LinUDS -> LinUDS = \may,king_sing ->
      let may_sing : VP = ComplVPIVV may king_sing.pred.inf ;
          king_may_sing : LinUDS = king_sing ** {pred = mkUDSPred may_sing} ;
       in king_may_sing ;
     --in {s = linUDS king_may_sing} ;

    shant_VV : VV = Ex.shall_VV ** { -- only used in (very limited) NLG, not parsing
      s = \\_ => "shan't" ;                -- so negation here should be fine
    } ;

    who_NP : NP = mkNP (P.mkPN "who") ;

    udsToS : LinUDS -> S = \given ->
      PredVPS given.subj given.pred.fin ;

    conditionsHold : UDFragment -> UDFragment = \king -> king ** -- extends the type
      cc2 king (ss "if the following conditions hold") ; --cc2 : SS -> SS -> SS ; ss : Str -> SS
 --  lin S {s = king.s ++ "if the following conditions hold"}
  -- {s = king.s ++ "if the following conditions hold" ; lock_S = <>}

    addBullet : S -> S = \s -> s ** {s = "\\*" ++ s.s} ;

    makeList : UDS -> UDS -> S = \uds1,uds2 ->
      mkS emptyConj (mkListS (addBullet (udsToS uds1)) (addBullet (udsToS uds2))) ;

    emptyConj : Conj = and_Conj ** {s1,s2 = ""} ;

    -- hack to make the order "S , Adv"
    -- in English RG, lincat of S and Adv is both {s : Str} so we can do this
    -- Unsafe, don't copy for other languages
    hornlike : S -> Adv -> S = \consequence,condition ->
      Se.ExtAdvS (lin Adv consequence) (lin S condition) ;
  lin
-- Aarne
    -- : Numeral -> UDS -> UDFragment ;
    Adv_no_later_than_Num_calendar_days_after_the_day_UDS num uds = lin S(
      SyntaxEng.mkAdv
        (P.mkPrep "no later than")
        (mkNP (mkNP num (P.mkN "calendar day"))
              (lin Adv {s = "after the day" ++ linUDS uds}))) ;



}
