concrete BareRGEng of BareRG =

  ExtendEng [
    Temp, Pol, NP, Tense,
    S, ExistS, ExistNP,
    AP, VP, PresPartAP,
    Num, CN, NP, GenModNP, GenNP, GenRP,
    N, CompoundN,
    ApposNP, AdjAsNP, GerundNP, GerundCN
  ],

  SentenceEng,

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

  IdiomEng [ProgrVP, GenericCl], -- Not used for parsing

  NounEng - [
    CountNP,
    PartNP
    ,UseN2, Use2N3
    ,PPartNP
    ,DefArt,IndefArt,DetQuant -- Changing the linearisations
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

    -- Overriding DefArt and IndefArt, so they don't have
    -- the strings "it", "they", "one" etc. in their sp fields
    DefArt = NounEng.DefArt ** {
      sp = \\_,_,_,_ => "the"
    } ;
    IndefArt = NounEng.IndefArt ** {
      sp = \\_,_,_,_ => "a"
    } ;

    DetQuant quant num = NounEng.DetQuant quant num ** {
      sp = \\g,hasAdj,c => case <num.hasCard,num.n> of {
                             <False,_> => quant.sp ! g ! hasAdj ! num.n ! c ++ num.s  ! quant.isDef ! R.Nom ;
                             _         => quant.s  !     True   ! num.n     ++ num.sp ! quant.isDef ! R.npcase2case c
                           }
      } ;

    -- Shorthands for the common determiners
    theSg_Det = BareRGEng.DetQuant BareRGEng.DefArt NumSg ;
    thePl_Det = BareRGEng.DetQuant BareRGEng.DefArt NumPl ;
    aSg_Det = BareRGEng.DetQuant BareRGEng.IndefArt NumSg ;
    aPl_Det = BareRGEng.DetQuant BareRGEng.IndefArt NumPl ;

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
    ComplVP vp np = ComplSlash (slashV vp) np ;
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

    more_than_Quant = DefArt ** {
      s = \\_,_ => "more than" ;
      sp = \\_,_,_,_ => "more than"
      } ;

    --  : ACard -> Det ;
    ACard2Det acard = every_Det **
      {s = acard.s ! R.Nom ;
       -- sp : Gender => Bool => NPCase => Str
       sp = \\_g,_b,npc => acard.s ! R.npcase2case npc ;
       n = acard.n ;
       hasNum = False} ;

    PrepRP prep rp = rp ** {
      s = \\c => prep.s ++ rp.s ! c
      } ;

    one_NP = DetNP (BareRGEng.DetQuant BareRGEng.IndefArt (NumCard (NumNumeral (num (pot2as3 (pot1as2 (pot0as1 pot01))))))) ;

    AdvAdv a1 a2 = {s = a1.s ++ a2.s} ;

  lincat
    [Prep] = Co.ListX ;

    -- This relies on the fact that auxiliaries are a separate category in UD
    -- So I can assume that all VPs we need to deal with in root are non-auxiliary verbs
    -- Other C that become VP by (UseComp (CompC c)) already have list instance in RGL
    [VP] = {
        -- The common parts
        p    : Str ;   -- verb particle
        ad   : R.Agr => Str ; -- sentence adverb (can be Xself, hence Agr)
        s2   : R.Agr => Str ; -- complement
        ext  : Str ;        -- extreposed field such as S, QS, VP
        prp1,prp  : Str ;   -- present participle
        ptp1,ptp  : Str ;   -- past participle
        inf1,inf  : Str ;   -- the infinitive form
        isSimple : Bool ;   -- regulates the place of participle used as adjective

        -- The variable parts, depending on whether the main verb of the VP is auxiliary or not
        isAux : Bool ;
        auxForms : {
          past,
          contr,
          pres : R.Polarity => R.Agr => Str ;
          } ;
        nonAuxForms1,nonAuxForms : NonAuxForms ;
      } ;
  oper
    NonAuxForms : Type = {  -- nonExist when isAux=True
      pres : R.Agr => Str ; -- sing/sings ; can be streamlined into two forms if needed
      past : Str ; --# notpresent
      } ;


  lin
    BasePrep = Co.twoSS ;
    ConsPrep = Co.consrSS Co.comma ;
    ConjPrep co pps = Co.conjunctDistrSS co pps ** {isPre = True} ;

    BaseVP vp1 vp2 = vp2 ** {
      prp1 = linVPOnlyForListVP vp1.prp vp1 ;
      ptp1 = linVPOnlyForListVP vp1.ptp vp1 ;
      inf1 = linVPOnlyForListVP vp1.inf vp1 ;
      nonAuxForms1 = {
        pres = \\agr => linVPOnlyForListVP (vp1.nonAuxForms.pres ! agr) vp1 ;
        past = linVPOnlyForListVP vp1.nonAuxForms.past vp1 } ;
      } ;

    ConsVP vp vps = vps ** {
      prp1 = vp.prp ++ bindComma ++ vps.prp1 ;
      ptp1 = vp.ptp ++ bindComma ++ vps.ptp1 ;
      inf1 = vp.inf ++ bindComma ++ vps.inf1 ;
      nonAuxForms = vps.nonAuxForms ** {
        pres1 = \\agr => vp.nonAuxForms.pres ! agr ++ bindComma ++ vps.pres1 ! agr ;
        past1 = vp.nonAuxForms.past ++ bindComma ++ vps.past1 } ;
      } ;

    ConjVP conj vps = vps ** {
      prp = applyConj conj vps.prp1 vps.prp ;
      ptp = applyConj conj vps.ptp1 vps.ptp ;
      inf = applyConj conj vps.inf1 vps.inf ;
      nonAuxForms = applyConjNAF conj vps.nonAuxForms1 vps.nonAuxForms ;
    } ;

    PredVPS np vp = mkS (mkCl np vp) ;
    SlashCl cl = cl ** {c2=[]} ;
    PrepVP vp prep = vp ** {p = vp.p ++ prep.s} ;

  oper
    -- This is a hack, if you need to make a VP into Str in other places, use ResEng.infVP
    linVPOnlyForListVP : Str -> VP -> Str = \form,vp ->
      let a : R.Agr = R.agrP3 R.Sg
       in vp.ad ! a ++ form ++ vp.p ++ vp.s2 ! a ++ vp.ext ;

    applyConj : Conj -> (s1,s2 : Str)-> Str = \or,s1,s2 ->
      or.s1 ++ s1 ++ or.s2 ++ s2 ;

    applyConjNAF : Conj -> (_,_ : NonAuxForms) -> NonAuxForms = \or,naf1,naf2 -> {
      pres = \\agr => or.s1 ++ naf1.pres ! agr ++ or.s2 ++ naf2.pres ! agr ;
      past =  or.s1 ++ naf1.past ++ or.s2 ++ naf2.past ;
    } ;


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

   MkA2 a p = P.mkA2 a p ;
   MkN3 n p q = P.mkN3 n p q;
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

    --  : NP -> VP -> RS ;
    RS_that_NP_VP np vp =
    let cl : Cl = mkCl np vp ;
      in mkRS (mkRCl that_RP (SlashCl cl)) ;

    -- : [CN] -> NP -> NP ;
    NP_the_unauthorised_ConjN2_of_NP n2s np = NP_the_unauthorised_N2_of_NP (ConjCN and_Conj n2s) np ;

  {-  Adv_Adv__but_in_any_case_Adv : Adv -> Adv -> Adv ;
    Adv_at_the_time_NP_notifies_NP : NP -> NP -> Adv ;

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
