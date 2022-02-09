-- A smaller set of RGL functions and cats,
-- which will form the base of the UD application grammar

abstract BareRG =

  Extend [
    Temp, Pol, NP, Tense,
    S, ExistS, ExistNP,
    AP, VP, PresPartAP,
    Num, CN, NP, GenModNP, GenNP, GenRP,
    N, CompoundN -- : N -> N -> N    -- control system
  ],

  Sentence [
    S,QS,RS,Cl,RCl,QCl,NP,Temp,Tense,Pol,VP,Imp,Adv,
    ImpVP ,      -- VP -> Imp ;                 -- walk / do not walk
    AdvS ,
    ExtAdvS
    ,UseCl, UseRCl
  ],

  Verb [
     VP,AdV,Adv,AP,Comp,NP,V,Tense
    ,UseV       -- V   -> VP ;             -- sleep
    --    UseComp,
   --  CompAP,
   --  CompAdv,
   --  CompNP,
   ,UseAdv     -- Adv -> VP ;             -- be in the house ---s
   ,AdvVP      -- VP -> Adv -> VP ;       -- sleep here
   ,AdVVP
  ],

  Idiom [ProgrVP],

   Noun - [
      CountNP,
      PartNP,
      ApposCN
      ,UseN2, Use2N3
      ,PPartNP
--      ,IndefArt, DefArt
   ],

  Adjective [
    AP,AdA,A,Ord,
    PositA    , -- A  -> AP ;              -- warm
    UseComparA,
    AdAP,
    AdjOrd
  ],

  Adverb [
    A,
    Prep,NP,Adv,Subj,S,
    PrepNP    , -- Prep -> NP -> Adv ;     -- in the house
    SubjS,
    PositAdvAdj,   -- : A -> Adv  --- not sure if this should be used
    ComparAdvAdj,  -- : CAdv -> A -> NP -> Adv ; -- more warmly than John
    ComparAdvAdjS, -- : CAdv -> A -> S  -> Adv ; -- more warmly than he runs
    AdnCAdv        -- : CAdv -> AdN ;
  ],

  Structural [Prep, possess_Prep, by8agent_Prep],
  Symbol [
      Symb
    , SymbPN
    -- , MkSymb
    ],
  Conjunction,
  Relative,
  Question,
  Numeral,
  Tense ** {

  flags startcat = S ; -- TODO is this necessary?

  -- hacks and workarounds
  fun
    theSg_Det,
    thePl_Det,
    aSg_Det,
    aPl_Det : Det ;

    at_least_AdN : AdN ;
    more_than_Quant : Quant ;
    anySg_Det, anyPl_Det : Det ;
    someSg_Det, somePl_Det : Det ;

    in_accordance_with_Prep : Prep ;

    everyone_Pron : Pron ;
    who_RP, that_RP : RP ;

    XorMore : Card -> Det ; -- one/two/… or more

    CompoundCN : CN -> N -> N ; -- [[unlimited] area ] buildings

  -- passives
    PassV : V -> VP ;             -- affected
    PassVAgent : V -> NP -> VP ; -- affected by the breach
    PastPartAP : VP -> AP ;       -- stored in electronic formats (Extend.PastPartAP takes a VPSlash)

  -- TODO revisit
    -- PredetPrep : Predet -> Prep -> Prep ;               -- more than
    -- PredetPrepCard : Predet -> Prep -> Card -> Prep ;   -- more than 500/enough

    ComplV : V -> NP -> VP ; -- JustWordsWordNet has no V2 etc

  cat
    [Prep]{2} ;

  fun
    ConjPrep : Conj -> [Prep] -> Prep ;

  }
