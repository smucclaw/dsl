concrete BareRGEng of BareRG =

  ExtendEng [
    Temp, Pol, NP, Tense,
    S, ExistS, ExistNP,
    AP, VP, PresPartAP,
    Num, CN, NP, GenModNP, GenNP, GenRP,
    N, CompoundN
  ],

  SentenceEng [
    S,QS,RS,Cl,RCl,QCl,NP,Temp,Tense,Ant,Pol,VP,Imp,Adv,
    ImpVP ,      -- VP -> Imp ;                 -- walk / do not walk
    AdvS ,
    ExtAdvS
    ,UseCl, UseRCl
    ,EmbedVP,EmbedS -- used in UDExt
  ],

  VerbEng [
   VP,AdV,Adv,AP,Comp,NP,V,Tense
  ,UseV  ,     -- V   -> VP ;             -- sleep
  UseComp,
  CompAP,
  CompAdv,
  CompNP
  ,AdvVP    -- VP -> Adv -> VP ;       -- sleep here
  ,AdVVP
  ],

  IdiomEng [ProgrVP],

  NounEng - [
    CountNP,
    PartNP,
    ApposCN
    ,UseN2, Use2N3
    ,PPartNP
--    ,IndefArt, DefArt
   ],


  AdjectiveEng [
    AP,AdA,A,Ord,
    PositA    , -- A  -> AP ;              -- warm
    UseComparA,
    AdAP,
    AdvAP,
    ComplA2,
    AdjOrd
  ],

  AdverbEng [
    A,Prep,NP,Adv,Subj,S,
    PrepNP    , -- Prep -> NP -> Adv ;     -- in the house
    SubjS,
    PositAdvAdj,   -- : A -> Adv  --- not sure if this should be used
    ComparAdvAdj,  -- : CAdv -> A -> NP -> Adv ; -- more warmly than John
    ComparAdvAdjS, -- : CAdv -> A -> S  -> Adv ; -- more warmly than he runs
    AdnCAdv        -- : CAdv -> AdN ;
  ],

  StructuralEng [Prep, possess_Prep, by8agent_Prep],
  SymbolEng [
      Symb
    , SymbPN
    ],
  ConjunctionEng,
  RelativeEng,
  QuestionEng,
  NumeralEng,
  TenseX - [CAdv,Pol, PPos, PNeg] ** open
    Prelude,
    (G=GrammarEng),
    MorphoEng,
    ExtraEng,
    SyntaxEng,
    (P=ParadigmsEng),
    (E=ExtendEng),
    (Co=Coordination),
    (R=ResEng),
    JustWordsWordNetEng
    in {

  lin
    PPos = G.PPos ;
    PNeg = G.PNeg ;

    theSg_Det = DetQuant DefArt NumSg ;
    thePl_Det = DetQuant DefArt NumPl ;
    aSg_Det = DetQuant IndefArt NumSg ;
    aPl_Det = DetQuant IndefArt NumPl ;

    someSg_Det = mkDeterminerSpec P.singular "some" (variants {"someone"; "somebody"}) "something" False ;
    somePl_Det = mkDeterminerSpec P.plural "some" (variants {"someone"; "somebody"}) "something" False ;

    in_accordance_with_Prep = P.mkPrep "in accordance with" ;

    at_least_AdN = ss "at least" ;
    anySg_Det = mkDeterminerSpec P.singular "any" (variants {"anyone"; "anybody"}) "anything" False ;
    anyPl_Det = mkDeterminerSpec P.plural "any" (variants {"anyone"; "anybody"}) "anything" False ;

    everyone_Pron = mkPron "everyone" "everyone" "everyone's" "everyone's" P.singular R.P3 P.human ;
    who_RP = ExtraEng.who_RP ;
    that_RP = lin RP {
      s = table {
        R.RC _ (R.NCase R.Gen) | R.RC _ R.NPNomPoss => "whose" ;
        _     => "that"
        } ;
      a = R.RNoAg
      } ;

    -- : NP -> SC -> NP ;     -- to get "a data breach occurred" to become a NP
    SentNP np sc = AdvNP np <sc : Adv> ;

    ComplV v np = ComplSlash (slashV (UseV v)) np ;
    -- ComplA a prep np = mkAP (P.mkA2 a prep) np ;

    -- : V -> VP ;       -- is affected (needs auxPass to trigger)
    PassV v = PassV2 (P.mkV2 v) ;

    -- : V -> NP -> VP ; -- is affected by the breach (needs auxPass to trigger)
    PassVAgent v ag = AdvVP (PassV v) (PrepNP by8agent_Prep ag) ;

    -- : VP -> AP ;      -- stored in electronic formats, no auxPass (Extend.PastPartAP takes a VPSlash)
    PastPartAP vp = E.PastPartAP (slashV vp) ;

    -- : VP -> NP -> AP ; -- affected by breach, no auxPass (Extend.PastPartAP takes a VPSlash)
    PastPartAgentAP vp = E.PastPartAgentAP (slashV vp) ;

    CompoundCN = CompoundN ;

    --  : Card -> Det ; -- one/two/… or more
    XorMore card =
    let baseDet : Det = mkDet card
     in baseDet ** {
      s = baseDet.s ++ "or more" ;
      n = ResEng.Pl ;
    } ;

    --  : ACard -> Det ;
    ACard2Det acard = every_Det **
      {s = acard.s ! R.Nom ;
       -- sp : Gender => Bool => NPCase => Str
       sp = \\_g,_b,npc => acard.s ! R.npcase2case npc ;
       n = acard.n ;
       hasNum = False} ;
  lincat
    [Prep] = Co.ListX ;

  lin
    BasePrep = Co.twoSS ;
    ConsPrep = Co.consrSS Co.comma ;
    ConjPrep co pps = Co.conjunctDistrSS co pps ** {isPre = True} ;
    PredVPS np vp = mkS (mkCl np vp) ;

  oper
    slashV : VP -> VPSlash = \vp -> vp ** {
      c2 = [] ;
      gapInMiddle = True ;
      missingAdv = False
      } ;

-- Aarne's additions
  lin

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
      mkNP theSg_Det (
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
    RS_that_NP_VP : NP -> VP -> RS ;
    RS_to_whom_NP_VP : NP -> VP -> RS ;
    VP_assesses__Adv__that_S : Adv -> S -> VP ;
    VP_may__SeqAdv__VP : [Adv] -> VP -> VP ;
    VP_must__SeqAdv__VP : [Adv] -> VP -> VP ;
    VP_notify_NP_of_NP : NP -> NP -> VP ;
  --}
    oper
    relating_to_Prep : Prep = P.mkPrep "relating to" ;
    concurrently_Adv : Adv = P.mkAdv "concurrently" ;
}
