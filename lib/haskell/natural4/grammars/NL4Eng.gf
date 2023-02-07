concrete NL4Eng of NL4 = 
    NumeralEng
  , GrammarEng [
        N, N2, CN, NP
      , V, V2, VP
      , A, A2, AP 
      ]
  , ExtendEng [
        VPS, MkVPS --, [VPS], BaseVPS, ConsVPS, ConjVPS
      , VPI, MkVPI --, [VPI], BaseVPI, ConsVPI, ConjVPI
      , VP, Tense, Ant, Temp, Pol, Conj -- for VPS
      ]
  ** open SyntaxEng, ParadigmsEng, ExtendEng, (ExtraEng=ExtraEng), (R=ResEng), IrregEng in {
    lincat
      Rule = S ;
      Action = ExtendEng.VPI ;
      Who = ExtendEng.VPS ;
      [Who] = ExtendEng.ListVPS ;
      Subj = CN ;
      Deontic = VV ;

    linref
      Who = ExtendEng.linVPS (R.agrP3 R.Sg) ;
    lin 
-- Application layer
      -- : Subj -> Deontic -> Action -> Rule ;
      Regulative subj deontic action = mkS (mkCl (every subj) (ComplVPIVV deontic action)) ;
      -- EVERY cn = mkNP <every_Det : Det> <cn : CN> ;
      -- PARTY cn = mkNP <a_Det : Det> <cn : CN> ;
      EVERY, PARTY = \cn -> lin CN cn ;
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
      SubjWho subj who = mkCN subj (RelVPS ExtraEng.who_RP who) ;

-- RGL layer
      person = mkCN (mkN ("person"|"Person")) ;
      walk = mkVP (mkV "walk") ; 
      eat = mkVP IrregEng.eat_V ;
      drink = mkVP IrregEng.drink_V ; 
      sing = mkVP IrregEng.sing_V ;

      presentIndicative = mkTemp presentTense simultaneousAnt ; 
      POS = positivePol ;
      NEG = negativePol ;

      oper 
        every : CN -> NP = \cn -> mkNP <every_Det : Det> <cn : CN> ;
}