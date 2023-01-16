concrete BareRGMay of BareRG =

  ExtendMay [
    Temp, Tense, Pol, NP, Tense, Quant, RP, Conj,
    AP, VP, PresPartAP,
    Num, CN, NP, GenModNP, GenNP, GenRP,
    N, CompoundN,
    GerundNP -- used only in an auxfun to recover nmod misparsed as acl — disabled otherwise!
    ,VPS,MkVPS,
    ConjVPS, ListVPS
  ],

  SentenceMay [
    NP, VP, Cl, SC, S, Subj, Temp, Tense, Pol, QCl, QS, RCl, RS,
    PredVP, UseCl, UseRCl, UseQCl
    , SSubjS , EmbedVP
    ],


  VerbMay [
   VP,AdV,Adv,AP,Comp,NP,V,Tense
  ,UseV  ,     -- V   -> VP ;             -- sleep
  UseComp,
  CompAP,
  CompAdv,
  CompNP
  ,UseAdv
  ,AdvVP    -- VP -> Adv -> VP ;       -- sleep here
  ,AdVVP
  ],

  IdiomMay [VP, ProgrVP], -- ProgrVP only used via auxfun, disabled in labels

  NounMay - [
    CountNP,
    PartNP
    ,UseN2, Use2N3
    ,PPartNP
   ],

  AdjectiveMay [
    AP, AdA, A, Ord, Adv, A2, NP,
    PositA    , -- A  -> AP ;              -- warm
    UseComparA,
    AdAP,
    AdvAP,
    ComplA2,
    AdjOrd
  ],

  AdverbMay [
    A, Prep, NP, Adv, Subj, S, AdN, AdnCAdv, CAdv,
    PrepNP    , -- Prep -> NP -> Adv ;     -- in the house
    SubjS,
    PositAdvAdj,   -- : A -> Adv  --- not sure if this should be used
    ComparAdvAdj,  -- : CAdv -> A -> NP -> Adv ; -- more warmly than John
    ComparAdvAdjS, -- : CAdv -> A -> S  -> Adv ; -- more warmly than he runs
    AdnCAdv        -- : CAdv -> AdN ;
  ],

  StructuralMay [
    Prep, possess_Prep, by8agent_Prep,
    always_Adv
  ],
  SymbolMay [
      PN
    , Symb
    , SymbPN
    ],
  ConjunctionMay,
  RelativeMay,
  QuestionMay,
  NumeralMay,
  TenseX - [IAdv] ** open
    Prelude,
    (G=GrammarMay),
    SyntaxMay,
    (P=ParadigmsMay),
    (E=ExtendMay),
    (Co=Coordination),
    (R=ResMay),
    SymbolicMay,
    (SyE=SymbolMay),
    (N=NounMay)
    in {

  lin

    -- Shorthands for the common determiners
    theSg_Det = BareRGMay.DetQuant BareRGMay.DefArt NumSg ;
    thePl_Det = BareRGMay.DetQuant BareRGMay.DefArt NumPl ;
    aSg_Det = BareRGMay.DetQuant BareRGMay.IndefArt NumSg ;
    aPl_Det = BareRGMay.DetQuant BareRGMay.IndefArt NumPl ;

    someSg_Det = variants {} ;
    somePl_Det = variants {} ;

    in_accordance_with_Prep = R.mkPrep "in accordance with" ;

    at_least_AdN = ss "at least" ;
    anySg_Det = variants {} ;
    anyPl_Det = variants {} ;

    everyone_Pron = variants {} ;
    who_RP = IdRP ;
    that_RP = IdRP ;

    ComplV v np = ComplSlash (slashV (UseV v)) np ;

    -- : V -> VP ;       -- is affected (needs auxPass to trigger)
   -- PassV v = PassV2 (P.mkV2 v (R.mkPrep "")) ;

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
      n = R.NoNum R.Pl ;
    } ;

    more_than_Quant = BareRGMay.DefArt ** {
      s = "more than" ;
      sp = \\_ => "more than"
      } ;

    PrepRP prep rp = rp ** {
      s = prep.s ++ rp.s
      } ;

    one_NP = DetNP (BareRGMay.DetQuant BareRGMay.IndefArt (NumCard (NumNumeral (num (pot2as3 (pot1as2 (pot0as1 pot01))))))) ;

-- fallback for unknown vocabulary
    StrPN str = {s = \\_ => str.s} ;
    StrN str = {s = \\_  => str.s} ;
    StrA str = <P.mkA "dummy" : A> ** {s = str.s};
    StrAP str = <mkAP (P.mkA "dummy") : AP> ** {s = str.s};
    StrCard str = SymbNum (mkSymb str.s) ;
    StrNum str = N.NumPl ** {s = str.s} ;
    StrSymb = SyE.MkSymb ; -- String -> Symb
    SymbNP x = symb x ;

    -- DefPN pn = mkNP pn ;
    -- IndefPN pn = mkNP pn ;

  lincat
    [Prep] = Co.ListX ;

  lin
    BasePrep = Co.twoSS ;
    ConsPrep = Co.consrSS Co.comma ;
    ConjPrep co pps = Co.conjunctDistrSS co pps ** {
      obj = \\_ => "" ; -- dengan+nya -- needed in relative clauses to refer to the object
      prepType = R.EmptyPrep ; -- TODO rename, the name is confusing
    } ;

  oper
    applyConj : Conj -> (s1,s2 : Str)-> Str = \or,s1,s2 ->
      or.s1 ++ s1 ++ or.s2 ++ s2 ;

    slashV : VP -> VPSlash = \vp -> lin VPSlash (vp ** {
      c2 = R.mkPrep "" ;
      adjCompl = "" ;
      }) ;

}
