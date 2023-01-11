concrete BareRGSwe of BareRG =

  ExtendSwe [
    Temp, Tense, Pol, Tense, Quant, RP, Conj,
    AP, VP, PresPartAP,
    Num, CN, GenModNP, GenNP, GenRP,
    N, CompoundN,
    GerundNP -- used only in an auxfun to recover nmod misparsed as acl — disabled otherwise!
    ,VPS,MkVPS, Conj, ListVPS, ConjVPS, BaseVPS
    ,baseVPS,mkVPS -- opers
  ],

  SentenceSwe [
      VP, Cl, SC, S, Subj, Temp, Tense, Pol, QCl, QS, RCl, RS -- cats
    , ctr -- oper
    , PredVP, UseCl, UseRCl, UseQCl
    , SSubjS , EmbedVP
    ],


  VerbSwe [
   VP,AdV,Adv,AP,Comp,V,Tense
  ,UseV  ,     -- V   -> VP ;             -- sleep
  UseComp,
  CompAP,
  CompAdv,
  CompNP
  ,UseAdv
  ,AdvVP    -- VP -> Adv -> VP ;       -- sleep here
  ,AdVVP
  ],

  IdiomSwe [VP, ProgrVP], -- ProgrVP only used via auxfun, disabled in labels

  NounSwe [
     DefArt
   , DetQuant
   , IndefArt
   , NumPl, NumSg, NumCard
   , UseN, AdjCN, AdvCN, RelCN
   , MassNP, UsePron, DetCN
  -- NounSwe - [
  --     CountNP
  --   , UseN2, Use2N3
  --   , PartNP
  --   ,PPartNP
  ],

  AdjectiveSwe [
    AP, AdA, A, Ord, Adv, A2,
    PositA    , -- A  -> AP ;              -- warm
    UseComparA,
    AdAP,
    AdvAP,
    ComplA2,
    AdjOrd
  ],

  AdverbSwe [
    A, Prep, Adv, Subj, S, AdN, AdnCAdv, CAdv,
    PrepNP    , -- Prep -> NP -> Adv ;     -- in the house
    SubjS,
    PositAdvAdj,   -- : A -> Adv  --- not sure if this should be used
    ComparAdvAdj,  -- : CAdv -> A -> NP -> Adv ; -- more warmly than John
    ComparAdvAdjS, -- : CAdv -> A -> S  -> Adv ; -- more warmly than he runs
    AdnCAdv        -- : CAdv -> AdN ;
  ],

  StructuralSwe [
    Prep, possess_Prep,
    always_Adv
  ],
  SymbolSwe [
      Symb, PN, addGenitiveS
    , SymbPN
    ],
  ConjunctionSwe - [NP],
  RelativeSwe - [NP],
  QuestionSwe - [NP],
  NumeralSwe - [NP],
  TenseX - [Tense, Temp] ** open
    Prelude,
    (G=GrammarSwe),
    MorphoSwe,
    (X=ExtraSwe),
    SyntaxSwe,
    (P=ParadigmsSwe),
    (E=ExtendSwe),
    (M=MakeStructuralSwe),
    (Co=Coordination),
    (R=ResSwe),
    SymbolicSwe,
    (CS=CommonScand),
    (SyE=SymbolSwe),
    (N=NounSwe)
    in {

  lincat NP = NPLite ;

  oper NPLite : Type = {s : CS.NPForm => Str ; a : Agr} ;

  oper AgrLite : PType = {g : Gender ; n : Number} ;

  lin
    -- Shorthands for the common determiners
    theSg_Det = DetQuant DefArt NumSg ;
    thePl_Det = DetQuant DefArt NumPl ;
    aSg_Det = DetQuant IndefArt NumSg ;
    aPl_Det = DetQuant IndefArt NumPl ;

    someSg_Det = SyntaxSwe.someSg_Det ;
    somePl_Det = SyntaxSwe.somePl_Det ;

    by8agent_Prep = ss "genom" ; -- actually this is by means

    in_accordance_with_Prep = P.mkPrep "in accordance with" ;

    at_least_AdN = ss "at least" ;
    anySg_Det = SyntaxSwe.someSg_Det ;
    anyPl_Det = SyntaxSwe.somePl_Det ;

    everyone_Pron = lin Pron everybody_NP ;
    who_RP = IdRP ;
    that_RP = IdRP ;

    ComplV v np = ComplSlash (slashV (UseV v)) (nplite2np np) ;

    -- : V -> VP ;       -- is affected (needs auxPass to trigger)
    PassV v = PassV2 (P.mkV2 v) ;

    -- : V -> NP -> VP ; -- is affected by the breach (needs auxPass to trigger)
    PassVAgent v ag = AdvVP (PassV v) (PrepNP by8agent_Prep (nplite2np ag)) ;

    -- : VP -> AP ;      -- stored in electronic formats, no auxPass (Extend.PastPartAP takes a VPSlash)
    PastPartAP vp = E.PastPartAP (slashV vp) ;

    -- : VP -> NP -> AP ; -- affected by breach, no auxPass (Extend.PastPartAP takes a VPSlash)
    PastPartAgentAP vp np = E.PastPartAgentAP (slashV vp) (nplite2np np) ;

    -- : AP -> AP ;
    ParentheticalAP ap =  ap ** {
      -- s = table {agr => "(" ++ ap.s!agr ++ ")" }
      -- short for table with one case
      s = \\agr => "(" ++ ap.s!agr ++ ")"
    };
--    AP = {s : Agr => Str ; isPre : Bool} ;

    CompoundCN cn n = CompoundN (cn2n cn) n;

    oper
      cn2n : CN -> N = \cn -> cn ** {
        s = \\num,spec,cas => cn.s ! num ! DDef spec ! cas ;
        co = "foo"
      } ;

     nplite2np : NPLite -> NP = \nplite -> nplite ** {isPron = False} ;
    lin

    --  : Card -> Det ; -- one/two/… or more
    XorMore card =
    let baseDet : Det = mkDet <lin Card card : Card>
     in baseDet ** {
      s = \\b,g => baseDet.s ! b ! g ++ "eller mer" ;
      n = Pl ;
    } ;

    more_than_Quant = DefArt ** {
      s,sp = \\_,_,_,_ => "mer än" ;
      } ;


    PrepRP prep rp = rp ** {
      s = \\g,n,c => prep.s ++ rp.s ! g ! n ! c
      } ;

    one_NP = DetNP (DetQuant IndefArt (NumCard (NumNumeral (num (pot2as3 (pot1as2 (pot0as1 pot01))))))) ;

-- fallback for unknown vocabulary
    StrPN str = {s = \\_ => str.s ; g = P.utrum} ;
    StrN str = {s = \\_,_,_  => str.s ; co = str.s ; g = P.utrum} ;
    StrA str = <P.mkA "dummy" : A> ** {s = \\_ => str.s ; isMost=True};
    StrAP str = <mkAP (P.mkA "dummy") : AP> ** {s = \\_ => str.s};
    StrCard str = symb (mkSymb str.s) ;
    StrNum str = N.NumPl ** {s = \\_ => str.s} ;
    StrSymb = SyE.MkSymb ; -- String -> Symb
    StrV str = <P.mkV "dummy" : V>** {s = \\_ => str.s} ;
    SymbNP x = symb x ;

    DefPN pn = (N.UsePN pn) ;
    IndefPN pn = (N.UsePN pn) ;

  lincat
    [Prep] = Co.ListX ;

    -- This relies on the fact that auxiliaries are a separate category in UD
    -- So I can assume that all VPs we need to deal with in root are non-auxiliary verbs
    -- Other C that become VP by (UseComp (CompC c)) already have list instance in RGL
    -- [VP] = {
    --     -- The common parts
    --     p    : Str ;   -- verb particle
    --     ad   : R.Agr => Str ; -- sentence adverb (can be Xself, hence Agr)
    --     s2   : R.Agr => Str ; -- complement
    --     ext  : Str ;        -- extreposed field such as S, QS, VP
    --     prp1,prp  : Str ;   -- present participle
    --     ptp1,ptp  : Str ;   -- past participle
    --     inf1,inf  : Str ;   -- the infinitive form
    --     isSimple : Bool ;   -- regulates the place of participle used as adjective

    --     -- The variable parts, depending on whether the main verb of the VP is auxiliary or not
    --     isAux : Bool ;
    --     auxForms : {
    --       past,
    --       contr,
    --       pres : R.Polarity => R.Agr => Str ;
    --       } ;
    --     nonAuxForms1,nonAuxForms : NonAuxForms ;
    --   } ;
  -- oper
  --   NonAuxForms : Type = {  -- nonExist when isAux=True
  --     pres : R.Agr => Str ; -- sing/sings ; can be streamlined into two forms if needed
  --     past : Str ; --# notpresent
  --     } ;


  lin
    BasePrep = Co.twoSS ;
    ConsPrep = Co.consrSS Co.comma ;
    ConjPrep co pps = Co.conjunctDistrSS co pps ** {isPre = True} ;

    -- BaseVP vp1 vp2 = vp2 ** {
    --   prp1 = linVPOnlyForListVP vp1.prp vp1 ;
    --   ptp1 = linVPOnlyForListVP vp1.ptp vp1 ;
    --   inf1 = linVPOnlyForListVP vp1.inf vp1 ;
    --   nonAuxForms1 = {
    --     pres = \\agr => linVPOnlyForListVP (vp1.nonAuxForms.pres ! agr) vp1 ;
    --     past = linVPOnlyForListVP vp1.nonAuxForms.past vp1 } ;
    --   } ;

    -- ConsVP vp vps = vps ** {
    --   prp1 = vp.prp ++ bindComma ++ vps.prp1 ;
    --   ptp1 = vp.ptp ++ bindComma ++ vps.ptp1 ;
    --   inf1 = vp.inf ++ bindComma ++ vps.inf1 ;
    --   nonAuxForms1 = {
    --     pres = \\agr => linVPOnlyForListVP (vp.nonAuxForms.pres ! agr) vp ++ bindComma ++ vps.nonAuxForms1.pres ! agr ;
    --     past = linVPOnlyForListVP vp.nonAuxForms.past vp ++ bindComma ++ vps.nonAuxForms1.past } ;
    --   } ;

    -- ConjVP conj vps = vps ** {
    --   prp = applyConj conj vps.prp1 vps.prp ;
    --   ptp = applyConj conj vps.ptp1 vps.ptp ;
    --   inf = applyConj conj vps.inf1 vps.inf ;
    --   nonAuxForms = applyConjNAF conj vps.nonAuxForms1 vps.nonAuxForms ;
    -- } ;

  oper
  --   -- This is a hack, if you need to make a VP into Str in other places, use ResSwe.infVP
  --   linVPOnlyForListVP : Str -> VP -> Str = \form,vp ->
  --     let a : R.Agr = R.agrP3 R.Sg
  --      in vp.ad ! a ++ form ++ vp.p ++ vp.s2 ! a ++ vp.ext ;

  --   applyConj : Conj -> (s1,s2 : Str)-> Str = \or,s1,s2 ->
  --     or.s1 ++ s1 ++ or.s2 ++ s2 ;

  --   applyConjNAF : Conj -> (_,_ : NonAuxForms) -> NonAuxForms = \or,naf1,naf2 -> {
  --     pres = \\agr => or.s1 ++ naf1.pres ! agr ++ or.s2 ++ naf2.pres ! agr ;
  --     past =  or.s1 ++ naf1.past ++ or.s2 ++ naf2.past ;
  --   } ;

    slashV : VP -> VPSlash = \vp -> lin VPSlash (vp ** {
      n3 = \\_ => [] ;  -- object-control complement
      c2 = {s = [] ; hasPrep = False}
      }) ;

}
