concrete BareRGEng of BareRG =

  ExtendEng [
    Temp, Tense, Pol, NP, Tense, Quant, RP, Conj,
    AP, VP, PresPartAP,
    Num, CN, NP, GenModNP, GenNP, GenRP,
    N, CompoundN,
    GerundNP -- used only in an auxfun to recover nmod misparsed as acl — disabled otherwise!
    ,VPS,MkVPS, Conj, ListVPS, ConjVPS, BaseVPS
    ,baseVPS,mkVPS -- opers
  ],

  SentenceEng [
      NP, VP, Cl, SC, S, Subj, Temp, Tense, Pol, QCl, QS, RCl, RS -- cats
    , ctr -- oper
    , PredVP, UseCl, UseRCl, UseQCl
    , SSubjS , EmbedVP
    ],


  VerbEng [
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

  IdiomEng [VP, ProgrVP], -- ProgrVP only used via auxfun, disabled in labels

  NounEng - [
    CountNP,
    PartNP
    ,UseN2, Use2N3
    ,PPartNP
    ,DefArt,IndefArt,DetQuant -- Changing the linearisations
   ],

  AdjectiveEng [
    AP, AdA, A, Ord, Adv, A2, NP,
    PositA    , -- A  -> AP ;              -- warm
    UseComparA,
    AdAP,
    AdvAP,
    ComplA2,
    AdjOrd
  ],

  AdverbEng [
    A, Prep, NP, Adv, Subj, S, AdN, AdnCAdv, CAdv,
    PrepNP    , -- Prep -> NP -> Adv ;     -- in the house
    SubjS,
    PositAdvAdj,   -- : A -> Adv  --- not sure if this should be used
    ComparAdvAdj,  -- : CAdv -> A -> NP -> Adv ; -- more warmly than John
    ComparAdvAdjS, -- : CAdv -> A -> S  -> Adv ; -- more warmly than he runs
    AdnCAdv        -- : CAdv -> AdN ;
  ],

  StructuralEng [
    Prep, possess_Prep, by8agent_Prep,
    always_Adv
  ],
  SymbolEng [
      Symb, PN, addGenitiveS
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
    SymbolicEng,
    (SyE=SymbolEng),
    (N=NounEng)
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

    ComplV v np = ComplSlash (slashV (UseV v)) np ;

    -- : V -> VP ;       -- is affected (needs auxPass to trigger)
    PassV v = PassV2 (P.mkV2 v) ;

    -- : V -> NP -> VP ; -- is affected by the breach (needs auxPass to trigger)
    PassVAgent v ag = AdvVP (PassV v) (PrepNP by8agent_Prep ag) ;

    -- : VP -> AP ;      -- stored in electronic formats, no auxPass (Extend.PastPartAP takes a VPSlash)
    PastPartAP vp = E.PastPartAP (slashV vp) ;

    -- : VP -> NP -> AP ; -- affected by breach, no auxPass (Extend.PastPartAP takes a VPSlash)
    PastPartAgentAP vp = E.PastPartAgentAP (slashV vp) ;

    -- : AP -> AP ;
    ParentheticalAP ap =  ap ** {
      -- s = table {agr => "(" ++ ap.s!agr ++ ")" }
      -- short for table with one case
      s = \\agr => "(" ++ ap.s!agr ++ ")"
    };
--    AP = {s : Agr => Str ; isPre : Bool} ;

    CompoundCN = CompoundN ;

    --  : Card -> Det ; -- one/two/… or more
    XorMore card =
    let baseDet : Det = mkDet card
     in baseDet ** {
      s = baseDet.s ++ "or more" ;
      n = ResEng.Pl ;
    } ;

    more_than_Quant = BareRGEng.DefArt ** {
      s = \\_,_ => "more than" ;
      sp = \\_,_,_,_ => "more than"
      } ;


    PrepRP prep rp = rp ** {
      s = \\c => prep.s ++ rp.s ! c
      } ;

    one_NP = DetNP (BareRGEng.DetQuant BareRGEng.IndefArt (NumCard (NumNumeral (num (pot2as3 (pot1as2 (pot0as1 pot01))))))) ;

-- fallback for unknown vocabulary
    StrPN str = {s = \\_ => str.s ; g = P.human} ;
    StrN str = {s = \\_,_  => str.s ; g = P.human} ;
    StrA str = <P.mkA "dummy" : A> ** {s = \\_ => str.s ; isMost=True};
    StrAP str = <mkAP (P.mkA "dummy") : AP> ** {s = \\_ => str.s};
    StrCard str = symb (mkSymb str.s) ;
    StrNum str = N.NumPl ** {s,sp = \\_,_ => str.s} ;
    StrSymb = SyE.MkSymb ; -- String -> Symb
    StrV str = <P.mkV "dummy" : V>** {s = \\_ => str.s} ;
    SymbNP x = symb x ;

    DefPN pn = N.PredetNP (lin Predet {s= "the"}) (N.UsePN pn) ;
    IndefPN pn = N.PredetNP (lin Predet {s= "a"}) (N.UsePN pn) ;

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
      nonAuxForms1 = {
        pres = \\agr => linVPOnlyForListVP (vp.nonAuxForms.pres ! agr) vp ++ bindComma ++ vps.nonAuxForms1.pres ! agr ;
        past = linVPOnlyForListVP vp.nonAuxForms.past vp ++ bindComma ++ vps.nonAuxForms1.past } ;
      } ;

    ConjVP conj vps = vps ** {
      prp = applyConj conj vps.prp1 vps.prp ;
      ptp = applyConj conj vps.ptp1 vps.ptp ;
      inf = applyConj conj vps.inf1 vps.inf ;
      nonAuxForms = applyConjNAF conj vps.nonAuxForms1 vps.nonAuxForms ;
    } ;

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

    slashV : VP -> VPSlash = \vp -> lin VPSlash (vp ** {
      c2 = [] ;
      gapInMiddle = True ;
      missingAdv = False
      }) ;

}
