abstract NL4 = 
    Numeral
  , Grammar [
        N, N2, CN, UseN, NP, Det, DetCN, MassNP
      , V,  VV, V2, VS, VP
      , A, A2, AP, AdjCN, PositA
--      , ProgrVP -- becoming aware
      , Comp, Adv, VP, UseComp, CompAP, CompNP, CompCN, CompAdv -- is a public agency
      , Prep, PrepNP, AdvVP
      ]
  , Structural [
       Prep, to_Prep, by8means_Prep, for_Prep
     , VV, must_VV  
     ]
  , Extend [
        VPS, MkVPS --, [VPS], BaseVPS, ConsVPS, ConjVPS
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

      ConjWho : Conj -> [Who] -> Who ;
      SubjWho : Subj -> Who -> Subj ;

      You : Subj ;

      UPON : VP -> Upon ; -- upon becoming
      
      WHEN : NP -> VPS -> Cond ;
      ConjCond : Conj -> [Cond] -> Cond ;


-- Time expressions
    cat
      Temporal ;
      TimeUnit ; -- day, month, year …
      Date ;
      Month ;

    fun
      ON : Cond -> Date -> Cond ; -- ON 1 Feb 2022
      MkDate : Int -> Month -> Int -> Date ;
 
      WITHIN : Int -> TimeUnit -> Temporal ;

      Day_Unit, Month_Unit, Year_Unit : TimeUnit ;
      Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec : Month ;

-- General BoolStruct stuff, just first sketch — should be handled more structurally in HS
    cat
      Pre ; -- "Loss or Damage caused by", "an animal caused water to escape from"
      Constraint ; -- TODO don't parse in GF but create GF constructors that correspond to 
    fun
      NP_caused_by_Pre : NP -> Pre ;
      NP_caused_water_to_escape_from_Pre : NP -> Pre ; -- TODO generalise later
      qPRE : Pre -> Pre ; -- hack

      RPisAdv,   -- damage IS to contents
      RPisnotAdv : NP -> Adv -> Constraint ; 
      RPisAP,    -- damage IS caused by birds
      RPisnotAP : NP -> AP -> Constraint ; -- damage IS not covered
      RPleafNP : NP -> Constraint ; -- to pair with Pre to get a full sentence ???
      RPleafS : NP -> VPS -> Constraint ;
      qCONSTR : Constraint -> Constraint ; -- also hack TODO make this not suck so much

-----------------------------------------------------------------------------
-- Lexicon, later to be automatically generated in different modules

    fun
    -- must sing
      person
       , organisation
       , agency
       , explanation
       , inaction
       , notification
       , PDPC
       , data_breach
       : CN ;
      public, notifiable, aware : AP ;
      NDB_Qualification : NP ; 
      walk, eat, drink, sing : VP ; -- VP = intransitive verb

    -- PDPA
      demand,
      perform,
      become : V2 ;
      assess : VS ; 
      occur,
      respond : VP ; -- in corpus, takes oblique complements 
                     -- TODO: create verb subcats from lexicon based on in which context they appear
                     -- "respond" :| []  -> respond : VP 
                     -- "demand" :| [ "an explanation for your inaction" ] -> demand : V2, NP complement
                     -- "assess" :| [ "if it is a Notifiable Data Breach" ] -> assess : VS, S complement
                     -- TODO: is it overkill to have keywords in language? assess,IF,it is a NDB

  -- rodents and vermin
      Loss_or_Damage : NP ;
      Contents : NP ;
      rodents : NP ;
      insects : NP ;
      vermin : NP ;
      birds : NP ;
      animal : NP ;
      household_appliance : NP ;
      swimming_pool : NP ;
      plumbing_heating_or_AC : NP ;
      any_other_exclusion : NP ;

      loss : CN ;
      covered : AP ;
      ensuing,
      caused_by : NP -> AP ;

      apply : VP ;
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

      presAnt,  -- has occurred
      presSimul  -- occurs
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