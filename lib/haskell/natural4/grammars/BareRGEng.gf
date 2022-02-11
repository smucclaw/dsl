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
  ],

  VerbEng [
   VP,AdV,Adv,AP,Comp,NP,V,Tense
  ,UseV       -- V   -> VP ;             -- sleep
  --    UseComp,
  -- CompAP,
  -- CompAdv,
  -- CompNP,
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
    ResEng
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

  lincat
    [Prep] = Co.ListX ;

  lin
    BasePrep = Co.twoSS ;
    ConsPrep = Co.consrSS Co.comma ;
    ConjPrep co pps = Co.conjunctDistrSS co pps ** {isPre = True} ;

  oper
    slashV : VP -> VPSlash = \vp -> vp ** {
      c2 = [] ;
      gapInMiddle = True ;
      missingAdv = False
      } ;


}
