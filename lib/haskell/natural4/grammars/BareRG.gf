-- A smaller set of RGL functions and cats,
-- which will form the base of the UD application grammar

abstract BareRG =

  Extend [
    Temp, Pol, NP, Tense,
    S, ExistS, ExistNP,
    AP, VP, PresPartAP,
    Num, CN, NP, GenModNP, GenNP, GenRP,
    N, CompoundN -- : N -> N -> N    -- control system
    -- these only for UDExt; not in labels file
    ,ApposNP, AdjAsNP, GerundNP, GerundCN
  ],

  Sentence, -- The whole  module is included, but only some of the funs have labels, i.e. are used for parsing

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

  Idiom [ProgrVP, GenericCl], -- Not used for parsing

   Noun - [
      CountNP,
      PartNP
      ,UseN2, Use2N3
      ,PPartNP
   ],

  Adjective [
    AP,AdA,A,Ord,
    PositA    , -- A  -> AP ;              -- warm
    UseComparA,
    AdAP,
    ComplA2,
    AdvAP,
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
    PrepRP : Prep -> RP -> RP ; -- for which

    one_NP : NP ;

    XorMore : Card -> Det ; -- one/two/… or more

    CompoundCN : CN -> N -> N ; -- [[unlimited] area ] buildings

    SentNP : NP -> SC -> NP ; -- like SentCN but for NP instead

  -- passives
    PassV : V -> VP ;             -- is:auxPass affected
    PassVAgent : V -> NP -> VP ;  -- is:auxPass affected by the breach
    PastPartAP : VP -> AP ;       -- stored in electronic formats (Extend.PastPartAP takes a VPSlash)
    PastPartAgentAP : VP -> NP -> AP ; -- affected by the breach
  -- TODO revisit
    -- PredetPrep : Predet -> Prep -> Prep ;               -- more than
    -- PredetPrepCard : Predet -> Prep -> Card -> Prep ;   -- more than 500/enough

    -- JustWordsWordNet has no V2 etc
    ComplV : V -> NP -> VP ; -- eat pizza
    ComplVP : VP -> NP -> VP ; -- "eat enthusiastically pizza"--the first argument is already VP. TODO improve NLG.hs so we can remove this
    PrepVP : VP -> Prep -> VP ; -- like VPSlashPrep but on VPs. Probably this is also better to handle by other means and should be removed later.
    -- ComplA : A -> NP -> AP ; -- applicable to X  (TODO: where to put prep?)
    -- ComplN : N -> NP -> CN ; -- mother of X  (TODO: where to put prep?)

    MkA2 : A -> Prep -> A2 ;
    MkN3 : N -> Prep -> Prep -> N3;
    ACard2Det : ACard -> Det ;

    PredVPS : NP -> VP -> S ;
    SlashCl : Cl -> ClSlash ; -- make a full Cl into ClSlash

    AdvAdv : Adv -> Adv -> Adv ;

  cat
    [Prep]{2} ;
    [VP]{2} ;

  fun
    ConjPrep : Conj -> [Prep] -> Prep ;
    ConjVP : Conj -> [VP] -> VP ;

-- Aarne's additions
fun
  -- apply_concurrently_VP : VP ;
  -- does_not_apply_to_V : V ;
  -- on_or_after_Prep : Prep ;
  -- prior_to_the_occurrence_of_Prep : Prep ;
  -- that_other_Det : Det ;

  -- CN_CN_relating_to_NP : CN -> NP -> CN ;
  -- CN_obligation_of_NP_to_VP : NP -> VP -> CN ;
  -- NP_all_the_CN_RS : CN -> RS -> NP ;
  -- NP_the_loss_of_any_CN_RS : CN -> RS -> NP ;
  -- NP_the_unauthorised_N2_of_NP : CN -> NP -> NP ;
  -- NP_the_unauthorised_ConjN2_of_NP : [CN] -> NP -> NP ;
  -- Adv_Adv__but_in_any_case_Adv : Adv -> Adv -> Adv ;
  -- Adv_at_the_time_NP_notifies_NP : NP -> NP -> Adv ;
  -- RS_to_whom_NP_VP : NP -> VP -> RS ;
  -- VP_assesses__Adv__that_S : Adv -> S -> VP ;
  -- VP_may__SeqAdv__VP : [Adv] -> VP -> VP ;
  -- VP_must__SeqAdv__VP : [Adv] -> VP -> VP ;
  -- VP_notify_NP_of_NP : NP -> NP -> VP ;

  RS_that_NP_VP : NP -> VP -> RS ;


  }
