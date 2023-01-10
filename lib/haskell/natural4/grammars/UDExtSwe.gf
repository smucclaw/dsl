

concrete UDExtSwe of UDExt = UDAppSwe,
  ExtendSwe [
    S, ExistS, ExistsNP, ExistCN, ExistNPQS, ExistIPQS
    ,ApposNP, AdjAsNP, GerundCN, GerundAdv
    ,ICompAP, IAdvAdv, PredIAdvVP
    ,PredVPS, ConsVPS
  ],
  IdiomSwe [
    GenericCl, ImpersCl
  ],
  SentenceSwe [PredSCVP, EmbedS, EmbedQS]

 ** open
  Prelude,
  SyntaxSwe, (P=ParadigmsSwe), ExtendSwe,
  (R=ResSwe), (CS=CommonScand),
  (Se=SentenceSwe),
  (Ex=ExtraSwe) in {

--------------------------------------------------------------------------------------------
-- This set of functions used to live in BareRG, but they weren't actually used for parsing
-- They look more like extensions to the RGL, so add them here.
  lin
    -- : Cl -> ClSlash ; -- make a full Cl into ClSlash
--    SlashCl cl = cl ** {c2=[]} ;

    -- : Adv -> Adv -> Adv ;
    AdvAdv a1 a2 = {s = a1.s ++ a2.s} ;

    --  : ACard -> Det ;
    ACard2Det acard = SyntaxSwe.every_Det **
      {s,sp = \\_,_ => acard.s } ;

    -- : NP -> SC -> NP ;     -- to get "a data breach occurred" to become a NP
    SentNP np sc = np2nplite (mkNP (nplite2np np) (lin Adv sc)) ;

    -- : VP -> NP -> VP ; -- "eat enthusiastically pizza"--the first argument is already VP. TODO improve NLG.hs so we can remove this
    ComplVP vp np = ComplSlash (slashV vp) (nplite2np np) ;
    -- ComplA a prep np = mkAP (P.mkA2 a prep) np ;

    -- : VP -> S -> VP ; -- [assess]:VP [if it is a NDB]:S
    ComplSVP vp s = AdvVP vp <lin Adv {s = s.s ! CS.Main} : Adv> ;

    -- : VP -> Prep -> VP ; -- like VPSlashPrep but on VPs. Probably this is also better to handle by other means and should be removed later.
    PrepVP vp prep = vp ** {p = vp.p ++ prep.s} ;

{- -- TODO: these give illegal pattern matching error
    -- : A -> Prep -> A2 ;
    MkA2 a p = P.mkA2 a p ;

    -- : N -> Prep -> Prep -> N3;
    MkN3 n p q = P.mkN3 n p q;
-}
    -- : S -> Adv -> S ; -- [we go [where it's warm]:Adv ]:S
    PostAdvS s adv = s ** {s = \\o => s.s ! o ++ adv.s} ;

    You = you_NP ;
    Someone = somebody_NP ;

    -- : VV  -> Ant -> Pol -> VP -> VP ;
    ComplVV vv a p vp = VerbSwe.ComplVV vv vp ; -- TODO fix ParseExtend.ComplVV

   -- : aux -> Temp -> Pol -> VP -> VPS ;
    ComplAux aux t p vp =
      let ant = lin Ant {s = [] ; a = t.a} ;
      in MkVPS (mkTemp t ant) p (VerbSwe.ComplVV aux.vv vp) ;

    -- All of their lincat is already Adv
    -- : advcl/acl/xcomp -> Adv ;
    advcl2Adv,
    xcomp2Adv,
    advmod2Adv,
    csubj2Adv = \adv -> lin Adv adv ;



    -- : CN -> NP -> CN ;
    CN_CN_relating_to_NP cn np = mkCN cn (mkAdv relating_to_Prep np) ;

    -- : NP -> VP -> CN ;
    CN_obligation_of_NP_to_VP np vp = mkCN (mkCN (P.mkN2 obligation_N) np) vp ;



  oper
    relating_to_Prep : Prep = P.mkPrep "relating to" ;
    concurrently_Adv : SyntaxSwe.Adv = P.mkAdv "concurrently" ;

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
      let upon_Adv : SyntaxSwe.Adv = SyntaxSwe.mkAdv upon_Prep (AdjAsNP upon.pred.presp) ;
       in Se.ExtAdvS upon_Adv action ;

    Cond cond action =
      let cond_Adv : SyntaxSwe.Adv = SyntaxSwe.mkAdv SyntaxSwe.if_Subj (udsToS cond) ;
       in Se.AdvS cond_Adv action;

    Temporal temp action = Se.AdvS temp action;

    Given given action =
      let given_Subj : Subj = lin Subj (ss "given that") ;
          given_Adv : SyntaxSwe.Adv = SyntaxSwe.mkAdv given_Subj (udsToS given);
       in Se.AdvS given_Adv action ;

    -- : NP -> UDS -> UDFragment ;
    subjAction subj uds = PredVPS (nplite2np subj) uds.pred.fin ;

    -- : UDS -> NP -> NP ; -- EVERY king WHO is a singer
    Who is_singer king =
      let who_is_singer_S : S = PredVPS who_NP is_singer.pred.fin ;
          who_is_singer_Adv : SyntaxSwe.Adv = lin Adv {s = who_is_singer_S.s ! CS.Main} ;
       in np2nplite (ExtAdvNP (nplite2np king) who_is_singer_Adv) ;

oper varje_Predet : Predet = lin Predet {s = \\_,_ => "varje" ; p = [] ; a = CS.PNoAg} ;
lin
    Every np = np2nplite (mkNP varje_Predet (nplite2np np)) ;
    TokAll np = np2nplite (mkNP all_Predet (nplite2np np)) ;
    Party np = np ;

    DMay = applyDeontic may_VV ;
    DMust = applyDeontic must_VV ;
    DShant = applyDeontic shant_VV ;

    -- TODO: types ???
    Means breach data_is_lost =
      let mean_VS   : VS = P.mkVS mean_V ;
          meansThat : VP = mkVP mean_VS (udsToS data_is_lost) ;
          mean_V2   : V2 = P.mkV2 mean_V ;
          meansThing : VP = mkVP mean_V2 (nplite2np data_is_lost.pred.np) ;
          chosenVP : VP = case data_is_lost.pred.isNP of {
            True => meansThing ;
            False => meansThat
          } ;
       in mkS (mkCl (nplite2np breach) chosenVP) ;

    -- : NP -> UDS -> UDFragment ; -- TODO: types?
    RPis,
    RPeq = \sky,blue -> PredVPS (nplite2np sky) blue.pred.fin ;


    -- : UDFragment -> S -> UDFragment ; -- breach is severe WHEN data is lost
    HornClause2 breach_is_severe data_is_lost =
      let when_data_lost_Adv : SyntaxSwe.Adv = mkAdv SyntaxSwe.when_Subj data_is_lost
       in hornlike breach_is_severe when_data_lost_Adv ;

    CondStandalone uds = lin S {s = \\o => linUDS uds} ;
    TemporalStandalone uds = lin S {s = \\o => linUDS uds} ;
    GivenStandalone uds = lin S {s = \\o => linUDS uds} ;
    UponStandalone uds = lin S {s = \\o => linUDS uds} ;

    -- : UDS -> UDS -> UDFragment -> UDFragment

    CondUpon cond upon king =
      let cond_Adv : SyntaxSwe.Adv = SyntaxSwe.mkAdv if_Subj (udsToS cond) ;
       in Se.ExtAdvS cond_Adv (Upon upon king) ;

    CondTemporal cond temporal king =
      let cond_Adv : SyntaxSwe.Adv = SyntaxSwe.mkAdv if_Subj (udsToS cond) ;
       in Se.ExtAdvS cond_Adv (Se.AdvS temporal king) ;

    CondGiven cond given king =
      Se.AdvS (conditionsHold king) (makeList cond given);

    -- : AP -> Conj -> [CN] -> NP -> CN ; -- unauthorised access or copying of personal data
    CN_AP_Conj_CNs_of_NP ap conj cns np =
      let conjCN : CN = ConjCN conj cns ;
       in mkCN ap (mkCN conjCN (mkAdv possess_Prep (nplite2np np))) ;


  oper
    applyDeontic : VV -> LinUDS -> LinUDS = \may,king_sing ->
      let may_sing : VP = ComplVPIVV may king_sing.pred.inf ;
          king_may_sing : LinUDS = king_sing ** {pred = mkUDSPred may_sing} ;
       in king_may_sing ;
     --in {s = linUDS king_may_sing} ;

    shant_VV : VV = must_VV ** { -- only used in (very limited) NLG, not parsing
      s = \\_ => "skall inte" ;                -- so negation here should be fine
    } ;

    may_VV : VV = must_VV ** { -- only used in (very limited) NLG, not parsing
      s = \\_ => "må" ;                -- so negation here should be fine
    } ;
    who_NP : CatSwe.NP = mkNP (P.mkPN "vem") ;

    udsToS : LinUDS -> S = \given ->
      PredVPS (nplite2np given.subj) given.pred.fin ;

    conditionsHold : S -> Adv = \s ->
      lin Adv {s = s.s ! CS.Main ++ "om följande villkor stämmer"} ;

    addBullet : S -> S = \s -> s ** {s = \\o => "\\*" ++ s.s ! o} ;

    makeList : UDS -> UDS -> S = \uds1,uds2 ->
      mkS emptyConj (mkListS (addBullet (udsToS uds1)) (addBullet (udsToS uds2))) ;

    emptyConj : Conj = and_Conj ** {s1,s2 = ""} ;

    hornlike : S -> SyntaxSwe.Adv -> S = \consequence,condition -> consequence ** {
      s = \\o => consequence.s ! CS.Inv ++ "," ++ condition.s
      } ;
--  lin
-- Aarne
    -- : Numeral -> UDS -> UDFragment ;
    -- Adv_no_later_than_Num_calendar_days_after_the_day_UDS num uds = lin S(
    --   SyntaxSwe.mkAdv
    --     (P.mkPrep "no later than")
    --     (mkNP (mkNP num (P.mkN "calendar day"))
    --           (lin Adv {s = "after the day" ++ linUDS uds}))) ;



}
