

concrete UDExtEng of UDExt = UDAppEng,
  ExtendEng [
    S, ExistS, ExistsNP, ExistCN, ExistNPQS, ExistIPQS
    ,ApposNP, AdjAsNP, GerundCN, GerundAdv
    ,ICompAP, IAdvAdv, PredIAdvVP
    ,PredVPS, ConsVPS
  ],
  IdiomEng [
    GenericCl, ImpersCl
  ],
  SentenceEng [PredSCVP, EmbedS, EmbedQS]

 ** open
  Prelude,
  SyntaxEng, (P=ParadigmsEng), ExtendEng,
  (R=ResEng), MorphoEng,
  (Se=SentenceEng),
  (Ex=ExtraEng) in {

--------------------------------------------------------------------------------------------
-- This set of functions used to live in BareRG, but they weren't actually used for parsing
-- They look more like extensions to the RGL, so add them here.
  lin
    -- : Cl -> ClSlash ; -- make a full Cl into ClSlash
    SlashCl cl = cl ** {c2=[]} ;

    -- : Adv -> Adv -> Adv ;
    AdvAdv a1 a2 = {s = a1.s ++ a2.s} ;

    --  : ACard -> Det ;
    ACard2Det acard = SyntaxEng.every_Det **
      {s = acard.s ! R.Nom ;
       -- sp : Gender => Bool => NPCase => Str
       sp = \\_g,_b,npc => acard.s ! R.npcase2case npc ;
       n = acard.n ;
       hasNum = False} ;

    -- : NP -> SC -> NP ;     -- to get "a data breach occurred" to become a NP
    SentNP np sc = mkNP np (lin Adv sc) ;

    -- : VP -> NP -> VP ; -- "eat enthusiastically pizza"--the first argument is already VP. TODO improve NLG.hs so we can remove this
    ComplVP vp np = ComplSlash (slashV vp) np ;
    -- ComplA a prep np = mkAP (P.mkA2 a prep) np ;

    -- : VP -> S -> VP ; -- [assess]:VP [if it is a NDB]:S
    ComplSVP vp s = AdvVP vp <s : Adv> ;

    -- : VP -> Prep -> VP ; -- like VPSlashPrep but on VPs. Probably this is also better to handle by other means and should be removed later.
    PrepVP vp prep = vp ** {p = vp.p ++ prep.s} ;

    -- : A -> Prep -> A2 ;
    MkA2 a p = P.mkA2 a p ;

    -- : N -> Prep -> Prep -> N3;
    MkN3 n p q = P.mkN3 n p q;

    -- : S -> Adv -> S ; -- [we go [where it's warm]:Adv ]:S
    PostAdvS s adv = cc2 s adv ;

    You = you_NP ;
    Someone = somebody_NP ;

    -- : VV  -> Ant -> Pol -> VP -> VP ;
    ComplVV = ParseExtendComplVV ;

   -- : aux -> Temp -> Pol -> VP -> VPS ;
    ComplAux aux t p vp =
      let ant = lin Ant {s = [] ; a = t.a} ;
      in MkVPS presSimulTemp positivePol (ParseExtendComplVV aux.vv ant p vp) ;

    -- All of their lincat is already Adv
    -- : advcl/acl/xcomp -> Adv ;
    advcl2Adv,
    xcomp2Adv,
    advmod2Adv,
    csubj2Adv = \adv -> lin Adv adv ;

-- Aarne's additions

    -- : NP -> VP -> RS ;
    RS_that_NP_VP np vp =
    let cl : Cl = mkCl np vp ;
      in mkRS (mkRCl that_RP (SlashCl cl)) ;

    apply_concurrently_VP = mkVP (mkVP apply_V) concurrently_Adv ;
    does_not_apply_to_V = P.mkV "do not apply to" "does not apply to" "did not apply to" "has not applied to" "is not applying to" ;
    on_or_after_Prep = P.mkPrep "on or after" ;
    prior_to_the_occurrence_of_Prep = P.mkPrep "prior to the occurrence of" ;
    that_other_Det = mkDeterminer P.singular "that other" ;

    -- : CN -> NP -> CN ;
    CN_CN_relating_to_NP cn np = mkCN cn (mkAdv relating_to_Prep np) ;

    -- : NP -> VP -> CN ;
    CN_obligation_of_NP_to_VP np vp = mkCN (mkCN (P.mkN2 obligation_N) np) vp ;

    -- : CN -> RS -> NP ;
    NP_all_the_CN_RS cn rs = mkNP all_Predet (mkNP thePl_Det (mkCN cn rs)) ;
    NP_the_loss_of_any_CN_RS cn rs =
      mkNP BareRGEng.theSg_Det (
        mkCN (P.mkN2 loss_N)
          (mkNP anySg_Det (mkCN cn rs))
        ) ;

    -- : CN -> NP -> NP ;
    NP_the_unauthorised_N2_of_NP cn np =
      let n2_of_np : CN = mkCN (P.mkN2 cn possess_Prep) np ;
       in mkNP theSg_Det (mkCN unauthorized_A n2_of_np) ;



    -- : [CN] -> NP -> NP ;
    NP_the_unauthorised_ConjN2_of_NP n2s np = NP_the_unauthorised_N2_of_NP (ConjCN and_Conj n2s) np ;

  {-  Adv_Adv__but_in_any_case_Adv : Adv -> Adv -> Adv ;
    Adv_at_the_time_NP_notifies_NP : NP -> NP -> Adv ;

    RS_to_whom_NP_VP : NP -> VP -> RS ;
    VP_assesses__Adv__that_S : Adv -> S -> VP ;
    VP_may__SeqAdv__VP : [Adv] -> VP -> VP ;
    VP_must__SeqAdv__VP : [Adv] -> VP -> VP ;
    VP_notify_NP_of_NP : NP -> NP -> VP ;
  -}
  oper
    relating_to_Prep : Prep = P.mkPrep "relating to" ;
    concurrently_Adv : SyntaxEng.Adv = P.mkAdv "concurrently" ;

--------------------------------------------------------------------------------------------
-- This set of functions is for the more high-level NLG stuff
-- They mimic more the structure of the Natural L4 abstract syntax

  lincat
    UDFragment = S ;
    [UDFragment] = [S];

  lin

    -- : UDS -> UDFragment ;
    UDS2Fragment = uds2s ;

    -- : UDS -> UDFragment -> UDFragment ;
    Upon upon action =
      let upon_Adv : SyntaxEng.Adv = SyntaxEng.mkAdv upon_Prep (AdjAsNP upon.pred.presp) ;
       in Se.ExtAdvS upon_Adv action ;

    Cond cond action =
      let cond_Adv : SyntaxEng.Adv = SyntaxEng.mkAdv SyntaxEng.if_Subj (udsToS cond) ;
       in Se.AdvS action cond_Adv;

    Temporal temp action = Se.AdvS action temp;

    Given given action =
      let given_Subj : Subj = lin Subj (ss "given that") ;
          given_Adv : SyntaxEng.Adv = SyntaxEng.mkAdv given_Subj (udsToS given);
       in Se.AdvS action given_Adv ;

    -- : NP -> UDS -> UDFragment ;
    subjAction subj uds = PredVPS subj uds.pred.fin ;

    -- : UDS -> NP -> NP ; -- EVERY king WHO is a singer
    Who is_singer king =
      let who_is_singer_Adv : SyntaxEng.Adv = lin Adv (PredVPS who_NP is_singer.pred.fin) ;
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

    -- : UDFragment -> S -> UDFragment ; -- breach is severe WHEN data is lost
    HornClause2 breach_is_severe data_is_lost =
      let when_data_lost_Adv : SyntaxEng.Adv = mkAdv SyntaxEng.when_Subj data_is_lost
       in hornlike breach_is_severe when_data_lost_Adv ;

    CondStandalone uds = ss (linUDS uds) ;
    TemporalStandalone uds = ss (linUDS uds) ;
    GivenStandalone uds = ss (linUDS uds) ;
    UponStandalone uds = ss (linUDS uds) ;

    -- : UDS -> UDS -> UDFragment -> UDFragment

    CondUpon cond upon king =
      let cond_Adv : SyntaxEng.Adv = SyntaxEng.mkAdv if_Subj (udsToS cond) ;
       in Se.ExtAdvS cond_Adv (Upon upon king) ;

    CondTemporal cond temporal king =
      let cond_Adv : SyntaxEng.Adv = SyntaxEng.mkAdv if_Subj (udsToS cond) ;
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
    hornlike : S -> SyntaxEng.Adv -> S = \consequence,condition ->
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
