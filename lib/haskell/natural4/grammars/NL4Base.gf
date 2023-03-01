abstract NL4Base =
    Numeral
  , Grammar [
        N, N2, CN, UseN, NP, Det, DetCN, MassNP
      , V,  VV, V2, VS, VP
      , A, A2, AP, AdjCN, PositA
      , Comp, Adv, VP, UseComp, CompNP, CompAP, CompAdv -- is a public agency
      , Prep, PrepNP, AdvVP
      , ListAdv, BaseAdv, ConsAdv, ConjAdv
      , ListAP, BaseAP, ConsAP, ConjAP
      , ListNP, BaseNP, ConsNP, ConjNP
      , ListS, BaseS, ConsS, ConjS
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
      -- for fancy NLG
      Rule ;

      -- for web forms
      Text ;

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
      -- for fancy NLG
      Regulative : Subj -> Deontic -> Action -> Rule ;

      -- for web forms
      qWHO,
      sWHO : Subj -> Who -> Text ;
      qUPON,  -- TODO rethink types when adding more langs
              -- TODO2 do we allow upon to take full sentence or just VP*?
      sUPON : Subj -> Upon -> Text ;
      qCOND,
      sCOND : Cond -> Text ;

      EVERY,
      PARTY,
      AN, THE : CN -> Subj ; -- EVERY Person
      WHO : Temp -> Pol -> VP -> Who ;    -- WHO walks
      ACTION : VP -> Action ;

      MUST, MAY, SHANT : Deontic ;
      AND, OR : Conj ;

      SubjWho : Subj -> Who -> Subj ;
      ConjWho : Conj -> [Who] -> Who ;
      ConjPreWho : PrePost -> Conj -> [Who] -> Who ; -- TODO need to find examples in the wild
      ConjPrePostWho : (_,_ : PrePost) -> Conj -> [Who] -> Who ;

      You : Subj ;

      UPON : VP -> Upon ; -- upon becoming

      WHEN : NP -> Temp -> Pol -> VP -> Cond ;
      ConjCond : Conj -> [Cond] -> Cond ;
      ConjPreCond : PrePost -> Conj -> [Cond] -> Cond ; -- TODO need to find examples in the wild
      ConjPrePostCond : (_,_ : PrePost) -> Conj -> [Cond] -> Cond ;

-- Time expressions
    cat
      Temporal ;
      TimeUnit ; -- day, month, year …
      Date ;
      Day ;
      Month ;
      Year ;
      YearComponent ;
      TComparison ;
      [TComparison]{2} ;

    fun
      TemporalConstraint :
        Cond -> TComparison -- ON , AFTER, …
             -> Date        -- 1 Feb 2022
             -> Cond ;
      BEFORE, AFTER, BY, ON, VAGUE : TComparison ;
      ConjTComparison : Conj -> [TComparison] -> TComparison ;
      MkDate : Day -> Month -> Year -> Date ;

      WITHIN : Int -> TimeUnit -> Temporal ;
      -- NB. time units, months and years in StandardLexicon

-- General BoolStruct stuff, just first sketch — should be handled more structurally in HS
    cat
      Constraint ;
      [Constraint]{2} ;
    --   IncompleteConstraint ;
    --   [IncompleteConstraint]{2} ;
    fun

      RPleafS : NP -> VPS -> Constraint ;
      RPleafNP : NP -> Constraint ; -- to pair with PrePost to get a full sentence ???
      ConjConstraint : Conj -> [Constraint] -> Constraint ;
      ConjPreConstraint : PrePost -> Conj -> [Constraint] -> Constraint ;
      ConjPrePostConstraint : PrePost -> PrePost -> Conj -> [Constraint] -> Constraint ;

      qPREPOST : PrePost -> Text ;
      qCONSTR : Constraint -> Text ;

-----------------------------------------------------------------------------
-- Instead of crashing, every category should have a dummy constructor where to put a string

    fun
      recoverUnparsedPrePost : String -> PrePost ; -- Workaround if PrePost not parsed (since they are not full constituents)
      recoverUnparsedConstraint : String -> Constraint ;
      recoverUnparsedWho : String -> Who ;
      recoverUnparsedCond : String -> Cond ;
      recoverUnparsedUpon : String -> Upon ;
      recoverUnparsedSubj : String -> Subj ;
      recoverUnparsedAction : String -> Action ;

      recoverRPis : String -> String -> Constraint ;


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