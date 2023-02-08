concrete NL4Eng of NL4 = 
    NumeralEng
  , GrammarEng [
        N, N2, CN, UseN, NP, Det, DetCN
      , V, V2, VS, VP
      , A, A2, AP, AdjCN, PositA
      , Cl, ImpersCl -- it is a NDB
--      , ProgrVP -- becoming aware
      , Comp, Adv, VP, UseComp, CompAP, CompNP, CompCN, CompAdv -- is a public agency
      , Prep, PrepNP, AdvVP
      ]
  , StructuralEng [
       Prep, to_Prep, by8means_Prep, for_Prep
     ]
  , ExtendEng [
        VPS, MkVPS --, [VPS], BaseVPS, ConsVPS, ConjVPS
      , VPI, MkVPI --, [VPI], BaseVPI, ConsVPI, ConjVPI
      , VP, Tense, Ant, Temp, Pol, Conj -- for VPS
      , S, PredVPS
      , GerundNP -- by performing NDB qualification
      ]
  ** open 
      SyntaxEng
    , ParadigmsEng
    , ExtendEng
    , (ExtraEng=ExtraEng)
    , (R=ResEng)
    , IrregEng 
    , Coordination
    , Prelude
    in {
    lincat
      Rule = S ;
      Question = QS ;
      Cond = LinCond ; -- RPConstraint
                        -- [ MTT "the data breach occurs" ] ( RPTC TOn )
                        -- [ MTT "1 Feb 2022" ] 
      [Cond] = LinListCond ;
      Action = ExtendEng.VPI ;
      Who = ExtendEng.VPS ;
      [Who] = ExtendEng.ListVPS ;
      Subj = NP ;
      Deontic = VV ;

    linref
      Who = linWho ;
      Cond = \c -> c.s.s ;
    oper
      LinCond : Type = {s : S ; qs : QS} ; -- {subj : NP ; pred : ExtendEng.VPS} ;
      LinListCond : Type = {s : [S] ; qs : ListQS} ;
      ListQS : Type = {s1,s2 : R.QForm => Str} ;
      linWho : ExtendEng.VPS -> Str = \vps -> 
        let vpss = vps.s ! R.ODir False ! R.agrP3 R.Sg
         in vpss.fin ++ vpss.inf ;
    lin 
-- Application layer
      -- : Subj -> Deontic -> Action -> Rule ;
      Regulative subj deontic action = mkS (mkCl subj (ComplVPIVV deontic action)) ;
      qWHO subj who = ExtendEng.SQuestVPS subj who ;
      qCOND c = c.qs ;

      EVERY cn = every <cn : CN> ;
      PARTY cn = mkNP cn ;
      AN cn = mkNP <a_Det : Det> <cn : CN> ;
      THE cn = mkNP <the_Det : Det> <cn : CN> ;
      WHO who = lin VPS who ; 
      ACTION act = lin VPI act ;

      MUST = must_VV ;
      MAY = ExtraEng.may_VV ;
      SHANT = ExtraEng.shall_VV ** { -- only used in NLG, not parsing
        s = \\_ => "shan't" ;        -- so negation here should be fine
        } ;
      AND = and_Conj ;
      OR = or_Conj ;
      BaseWho = ExtendEng.BaseVPS ;
      ConsWho = ExtendEng.ConsVPS ;
      ConjWho = ExtendEng.ConjVPS ;

      -- : Subj -> Who -> Subj ;
      SubjWho subj who = mkNP subj (RelVPS ExtraEng.who_RP who) ;

      You = you_NP | mkNP (mkN "You" "You" "Your" "Your") ;

      WHEN np vps = {s = PredVPS np vps ; qs = SQuestVPS np vps} ;

      BaseCond c d = {s = BaseS c.s d.s ; qs = twoTable R.QForm c.qs d.qs} ;
      ConsCond c d = {s = ConsS c.s d.s ; qs = consrTable R.QForm comma c.qs d.qs} ;
      ConjCond conj cs = {s = ConjS conj cs.s ; qs = conjunctDistrTable R.QForm conj cs.qs} ;

-- Time expressions
    lincat 
      Temporal = Adv ;
      TimeUnit = NP ;
      Date = Adv ;

    lin
      -- : Cond -> Date -> Cond ; -- ON 1 Feb 2022 -- TODO: switch from parsing the string ON to handling the RelationalPredicate structurally, this is just quick and dirty 
      ON cond date = 
        let onDate : Adv = lin Adv {s = "ON" ++ date.s} ;
        in {s = postAdvS cond.s onDate ; qs = postAdvQS cond.qs onDate} ;
    oper
      postAdvS : S -> Adv -> S = \s,adv -> s ** mkS <s : Adv> <adv : S> ; -- hack that only works for Eng
      postAdvQS : QS -> Adv -> QS = \qs,adv -> qs ** {s = \\qf => qs.s ! qf ++ adv.s} ;
    lin
      MkDate a b c = lin Adv (cc3 a b c) ;
      Jan = ss "Jan" ;
      Feb = ss "Feb" ;
      Mar = ss "Mar" ;
      Apr = ss "Apr" ;
      May = ss "May" ;
      Jun = ss "Jun" ;
      Jul = ss "Jul" ;
      Aug = ss "Aug" ;
      Sep = ss "Sep" ;
      Oct = ss "Oct" ;
      Nov = ss "Nov" ;
      Dec = ss "Dec" ;

      -- WITHIN : Int -> TimeUnit -> Temporal ;
      -- Day, Month, Year : TimeUnit ;


-----------------------------------------------------------------------------
-- RGL layer, later to be automatically generated in different modules

  -- must sing
      person = mkCN (mkN ("person"|"Person")) ;
      walk = mkVP (mkV "walk") ; 
      eat = mkVP IrregEng.eat_V ;
      drink = mkVP IrregEng.drink_V ; 
      sing = mkVP IrregEng.sing_V ;

  -- pdpa
      organisation = mkCN (mkN ("organisation"|"Organisation")) ;
      agency = mkCN (mkN ("agency"|"Agency")) ;
      explanation = mkCN (mkN "explanation") ;
      inaction = mkCN (mkN "inaction") ;
      notification = mkCN (mkN ("notification"|"Notification")) ;
      PDPC = mkCN (mkN "PDPC") ;
      data_breach = mkCN (mkN ("data breach"|"Data Breach")) ;
      public = mkAP (mkA ("public"|"Public")) ;
      notifiable = mkAP (mkA ("notifiable"|"Notifiable")) ;
      NDB_Qualification = mkNP (mkN "NDB Qualification") ;

      -- PDPA use case
      demand = mkV2 "demand" ;
      perform = mkV2 "perform" ;
      assess = mkVS (mkV "assess") ; 
      occur = mkVP (mkV "occur") ;
      respond = mkVP (mkV "respond") ;
      ComplV2 v2 np = mkVP v2 np ;
      ComplVSif vs s = R.insertObj (\\_ => "if" ++ s.s) (R.predV vs) ;
      ComplVSthat vs s = mkVP vs s ;

      -- : NP -> S ; -- it is NP — reference to a previous NP
      ReferenceNP np = mkS (mkCl it_NP np) ;

      presentIndicative = mkTemp presentTense simultaneousAnt ; 
      POS = positivePol ;
      NEG = negativePol ;

      theSg = theSg_Det ;
      thePl = thePl_Det ;
      aSg = aSg_Det ;
      your = mkDet youSg_Pron ;

      about_Prep = mkPrep "about" ;
      oper 
        every : CN -> NP = \cn -> mkNP <every_Det : Det> <cn : CN> ;
}