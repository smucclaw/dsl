-- A smaller set of RGL functions and cats,
-- which will form the base of the UD application grammar

abstract BareRG =

  Extend [
    Temp, Tense, Pol, NP, Tense, Quant, RP, Conj,
    AP, VP, PresPartAP,
    Num, CN, NP, GenModNP, GenNP, GenRP,
    N, CompoundN, -- : N -> N -> N    -- control system
    GerundNP -- used only in an auxfun to recover nmod misparsed as acl — disabled otherwise!
    ,VPS, MkVPS, Conj, ConjVPS, ListVPS
  ],

  Sentence [
    NP, VP, Cl, SC, S, Subj, Temp, Tense, Pol, QCl, QS, RCl, RS,
    PredVP, UseCl, UseRCl, UseQCl
    , SSubjS , EmbedVP
    ],

  Verb [
     VP,AdV,Adv,AP,Comp,NP,V,Tense
    ,UseV ,      -- V   -> VP ;             -- sleep
    UseComp,
    CompAP,
    CompAdv,
    CompNP
   ,UseAdv     -- Adv -> VP ;             -- be in the house ---s
   ,AdvVP      -- VP -> Adv -> VP ;       -- sleep here
   ,AdVVP
  ],

  Idiom [VP, ProgrVP],

   Noun - [
      CountNP,
      PartNP
      ,UseN2, Use2N3
      ,PPartNP
   ],

  Adjective [
    AP, AdA, A, Ord, Adv, A2, NP,
    PositA    , -- A  -> AP ;              -- warm
    UseComparA,
    AdAP,
    ComplA2,
    AdvAP,
    AdjOrd
  ],

  Adverb [
    A, Prep, NP, Adv, Subj, S, AdN, AdnCAdv, CAdv,
    PrepNP    , -- Prep -> NP -> Adv ;     -- in the house
    SubjS,
    PositAdvAdj,   -- : A -> Adv  --- not sure if this should be used
    ComparAdvAdj,  -- : CAdv -> A -> NP -> Adv ; -- more warmly than John
    ComparAdvAdjS, -- : CAdv -> A -> S  -> Adv ; -- more warmly than he runs
    AdnCAdv        -- : CAdv -> AdN ;
  ],

  Structural [
    Prep, possess_Prep, by8agent_Prep,
    always_Adv
  ],
  Symbol [
      PN
    , Symb
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
    PrepRP : Prep -> RP -> RP ; -- for which

    one_NP : NP ;

    XorMore : Card -> Det ; -- one/two/… or more

    CompoundCN : CN -> N -> N ; -- [[unlimited] area] buildings

  -- passives
    PassV : V -> VP ;             -- is:auxPass affected
    PassVAgent : V -> NP -> VP ;  -- is:auxPass affected by the breach
    PastPartAP : VP -> AP ;       -- stored in electronic formats (Extend.PastPartAP takes a VPSlash)
    PastPartAgentAP : VP -> NP -> AP ; -- affected by the breach
  -- TODO revisit
    -- PredetPrep : Predet -> Prep -> Prep ;               -- more than
    -- PredetPrepCard : Predet -> Prep -> Card -> Prep ;   -- more than 500/enough

    ParentheticalAP : AP -> AP ;
    -- JustWordsWordNet has no V2 etc
    ComplV : V -> NP -> VP ; -- eat pizza

-- fallback for unknown vocabulary
    StrPN : String -> PN ;
    StrN : String -> N ;
    StrA : String -> A ;
    StrAdv : String -> Adv ;
    StrAP : String -> AP ;
    StrCard : String -> Card ;
    StrNum : String -> Num ;
    StrV : String -> V ;
    StrSymb : String -> Symb ; -- e.g. URLs
  -- 	SymbNP : Symb -> NP ; -- so that words tagged as X can be used in other funs easier

  -- for e.g. abbreviations/defined terms that are erroneously parsed as PNs; "the NDB", "a DI"
    DefPN : PN -> NP ;
    IndefPN : PN -> NP ;

  cat
    [Prep]{2} ;
    [VP]{2} ;
    [VPS]{2} ;

  fun
    ConjPrep : Conj -> [Prep] -> Prep ;
    ConjVP : Conj -> [VP] -> VP ;
    -- ConjVPS : Conj -> [VPS] -> VPS ;
    SentNP : NP -> SC -> NP ; -- like SentCN but for NP instead
  }
