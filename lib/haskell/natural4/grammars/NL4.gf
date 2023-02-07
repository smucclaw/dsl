abstract NL4 = 
    Numeral
  , Grammar [
        N, N2, CN, NP
      , V, V2, VP
      , A, A2, AP 
      ]
  , Extend [
        VPS, MkVPS --, [VPS], BaseVPS, ConsVPS, ConjVPS
      , VPI, MkVPI --, [VPI], BaseVPI, ConsVPI, ConjVPI
      , VP, Tense, Ant, Temp, Pol, Conj -- for VPS
      ]
  ** {
    flags startcat = Rule ;
    cat
      Rule ;
      Action ;
      Who ;
      [Who]{2} ;
      Subj ;
      Deontic ;
    fun 
-- Application layer
      Regulative : Subj -> Deontic -> Action -> Rule ;
      EVERY,
      PARTY : CN -> Subj ; -- EVERY Person
      WHO : VPS -> Who ;    -- WHO walks
      ACTION : VPI -> Action ;

      MUST, MAY, SHANT : Deontic ;
      AND, OR : Conj ;

      ConjWho : Conj -> [Who] -> Who ;
      SubjWho : Subj -> Who -> Subj ;



-- RGL layer

      person : CN ;
      walk, eat, drink, sing : VP ;

      presentIndicative : Temp ; 
      POS : Pol ;
      NEG : Pol ;

    -- [Subject Constraint]
    -- [Attribute Constraint]
    -- [Conditional Constraint]
    -- [Upon Trigger]
    --     Deontic Action Temporal | Deontic Temporal Action
}