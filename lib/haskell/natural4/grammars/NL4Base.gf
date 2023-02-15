abstract NL4Base = 
    Numeral
  , Grammar [
        N, N2, CN, UseN, NP, Det, DetCN, MassNP
      , V,  VV, V2, VS, VP
      , A, A2, AP, AdjCN, PositA
      , Comp, Adv, VP, UseComp, CompAP, CompNP, CompCN, CompAdv -- is a public agency
      , Prep, PrepNP, AdvVP
      , ListAdv, BaseAdv, ConsAdv, ConjAdv 
      , ListAP, BaseAP, ConsAP, ConjAP
      , ListNP, BaseNP, ConsNP, ConjNP
      ]
  , Structural [
        Prep, to_Prep, by8means_Prep, for_Prep, from_Prep, on_Prep
      , VV, must_VV  
      ]
  , Extend [
        VPS, MkVPS, ListVPS, BaseVPS, ConsVPS, ConjVPS
      , VPI, MkVPI --, [VPI], BaseVPI, ConsVPI, ConjVPI
      , VP, Tense, Ant, Temp, Pol, Conj -- for VPS
--      , GenRP -- nice to have in the future?
      , S, PredVPS
      , NP, GerundNP
      ]
  ** {
    flags startcat = Rule ;
    cat
      Rule ;
      Question ;

      -- Any structure that is using BoolStruct needs to prepare for PrePost
      PrePost ; -- "Loss or Damage caused by", "an animal caused water to escape from"

      -- Regulative
      Cond ;
      [Cond]{2} ;
      Action ;
      Who ;
      [Who]{2} ;
      Subj ;
      Deontic ;
      Upon ;
    fun 
-- Application layer
      Regulative : Subj -> Deontic -> Action -> Rule ;
      qWHO : Subj -> Who -> Question ;
      qUPON : Subj -> Upon -> Question ; -- TODO rethink types when adding more langs 
                                         -- TODO2 do we allow upon to take full sentence or just VP*?
      qCOND : Cond -> Question ;

      EVERY,
      PARTY,
      AN, THE : CN -> Subj ; -- EVERY Person
      WHO : VPS -> Who ;    -- WHO walks
      ACTION : VPI -> Action ;

      MUST, MAY, SHANT : Deontic ;
      AND, OR : Conj ;

      SubjWho : Subj -> Who -> Subj ;
      ConjWho : Conj -> [Who] -> Who ;
      ConjPreWho : PrePost -> Conj -> [Who] -> Who ; -- TODO need to find examples in the wild
      ConjPrePostWho : (_,_ : PrePost) -> Conj -> [Who] -> Who ;

      You : Subj ;

      UPON : VP -> Upon ; -- upon becoming
      
      WHEN : NP -> VPS -> Cond ;
      ConjCond : Conj -> [Cond] -> Cond ;
      ConjPreCond : PrePost -> Conj -> [Cond] -> Cond ; -- TODO need to find examples in the wild
      ConjPrePostCond : (_,_ : PrePost) -> Conj -> [Cond] -> Cond ;

      recoverUnparsedPre : String -> PrePost ; -- Workaround if PrePost not parsed (since they are not full constituents)

-- Time expressions
    cat
      Temporal ;
      TimeUnit ; -- day, month, year …
      Date ;
      Month ;
      TComparison ;
      [TComparison]{2} ;

    fun
      TemporalConstraint : 
        Cond -> TComparison -- ON , AFTER, …
             -> Date        -- 1 Feb 2022
             -> Cond ;
      BEFORE, AFTER, BY, ON, VAGUE : TComparison ;
      ConjTComparison : Conj -> [TComparison] -> TComparison ;
      MkDate : Int -> Month -> Int -> Date ;
 
      WITHIN : Int -> TimeUnit -> Temporal ;
      -- NB. time units and months in StandardLexicon

-- General BoolStruct stuff, just first sketch — should be handled more structurally in HS
    cat
      Constraint ;
      [Constraint]{2} ;
    --   IncompleteConstraint ;
    --   [IncompleteConstraint]{2} ;
    fun
      recoverRPis : String -> String -> Constraint ;

      RPleafS : NP -> VPS -> Constraint ;
      RPleafNP : NP -> Constraint ; -- to pair with PrePost to get a full sentence ???
      ConjConstraint : Conj -> [Constraint] -> Constraint ;
      ConjPreConstraint : PrePost -> Conj -> [Constraint] -> Constraint ;
      ConjPrePostConstraint : PrePost -> PrePost -> Conj -> [Constraint] -> Constraint ;

    -- convert into questions – lincats have fields for question and statement
    -- TODO how about using Question type? or is that only for Regulative rules?
      qPREPOST : PrePost -> PrePost ;
      qCONSTR : Constraint -> Constraint ;




-----------------------------------------------------------------------------
-- Shortcuts and extensions to RGL

      ComplVAS : V2 -> AP -> S -> VP ; -- become aware (that) a data breach may have occurred 
      ComplV2S : V2 -> NP -> S -> VP ; -- notify PDPC that a data breach has occurred
      ComplV2 : V2 -> NP -> VP ;
      ComplVSif,
      ComplVSthat : VS -> S -> VP ;
      MayHave : VP -> VPS ; -- getting "may have occurred" with pure RGL is a pain

      ReferenceNP : NP -> S ; -- it is NP — reference to a previous NP
--      ExpletiveVP : VP -> S ; -- it is raining — dummy subject it (TODO: restrict usage of this and above from HS)

      presAnt,   -- has occurred
      presSimul, -- occurs
      pastSimul  -- occurred
        : Temp ; 
      
      POS : Pol ;
      NEG : Pol ;

      theSg : Det ;
      thePl : Det ;
      aSg : Det ;
      your : Det ;

      about_Prep : Prep ;
      may_VV : VV ;
}