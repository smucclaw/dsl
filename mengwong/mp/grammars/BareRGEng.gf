concrete BareRGEng of BareRG =

  ExtendEng [
    Temp, Pol, NP, Tense,
    S, ExistS, ExistNP,
    AP, VP, PresPartAP,
    N, CompoundN
  ],

  SentenceEng [
    S,QS,RS,Cl,RCl,QCl,NP,Temp,Tense,Ant,Pol,VP,Imp,Adv,
    ImpVP ,      -- VP -> Imp ;                 -- walk / do not walk
    AdvS ,
    ExtAdvS
    ,UseCl, UseRCl
  ],

  VerbEng [
  VP,AdV,Adv,AP,Comp,NP,V,Tense,
  UseV      , -- V   -> VP ;             -- sleep
  --    UseComp,
  -- CompAP,
  -- CompAdv,
  -- CompNP,
    AdvVP    , -- VP -> Adv -> VP ;       -- sleep here
    AdVVP
  ],

  NounEng - [
    CountNP,
    PartNP,
    ApposCN
    ,UseN2, Use2N3
   ],


  AdjectiveEng [
    AP,AdA,A,Ord,
    PositA    , -- A  -> AP ;              -- warm
    UseComparA,
    AdAP,
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

  ConjunctionEng,
  RelativeEng,
  QuestionEng,
  NumeralEng,
  TenseX - [CAdv,Pol, PPos, PNeg] ** open Prelude, (G=GrammarEng), MorphoEng, ExtraEng, (P=ParadigmsEng), (E=ExtendEng), ResEng in {

  lin
    PPos = G.PPos ;
    PNeg = G.PNeg ;

    theSg_Det = DetQuant DefArt NumSg ;
    thePl_Det = DetQuant DefArt NumPl ;
    aSg_Det = DetQuant IndefArt NumSg ;
    aPl_Det = DetQuant IndefArt NumPl ;

    everyone_Pron = mkPron "everyone" "everyone" "everyone's" "everyone's" P.singular P3 P.human ;
    who_RP = ExtraEng.who_RP ;
    that_RP = lin RP {
      s = table {
        RC _ (NCase Gen) | RC _ NPNomPoss => "whose" ;
        _     => "that"
        } ;
      a = RNoAg
      } ;

    ComplV v np = ComplSlash (slashV (UseV v)) np ;

    -- : V -> VP ;       -- affected
    PassV v = PassV2 (P.mkV2 v) ;

    -- : V -> NP -> VP ; -- affected by the breach
    PassVAgent v ag = AdvVP (PassV v) (PrepNP by8agent_Prep ag) ;

    -- : VP -> AP ;      -- stored in electronic formats (Extend.PastPartAP takes a VPSlash)
    PastPartAP vp = E.PastPartAP (slashV vp) ;

  oper
    slashV : VP -> VPSlash = \vp -> vp ** {
      c2 = [] ;
      gapInMiddle = True ;
      missingAdv = False
      } ;

  -- Application-specific additions to RGL
  lin
    may_Deontic = ComplVV ExtraEng.may_VV ;
    must_Deontic = ComplVV G.must_VV ;
    shall_Deontic  = ComplVV ExtraEng.shall_VV ;
--    shant_Deontic,
    should_Deontic = ComplVV (lin VV {
      s = table {
        VVF VInf => ["be obliged to"] ;
        VVF VPres => "should" ;
        VVF VPPart => ["been obliged to"] ;
        VVF VPresPart => ["being obliged to"] ;
        VVF VPast => "shall" ;
        VVPastNeg => "shall not" ;
        VVPresNeg => "shouldn't"
        } ;
      p = [] ;
      typ = VVAux
    }) ;

   PDPA_N = P.mkN "PDPA" ;
}