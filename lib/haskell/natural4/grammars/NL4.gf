abstract NL4 = 
    Numeral
  , Grammar [
        N, N2, CN, UseN, NP, Det, DetCN
      , V, V2, VS, VP
      , A, A2, AP, AdjCN, PositA
      , Cl, ImpersCl -- it is a NDB
--      , ProgrVP -- becoming aware
      , Comp, Adv, VP, UseComp, CompAP, CompNP, CompCN, CompAdv -- is a public agency
      , Prep, PrepNP, AdvVP
      ]
  , Structural [
       Prep, to_Prep, by8means_Prep, for_Prep
     ]
  , Extend [
        VPS, MkVPS --, [VPS], BaseVPS, ConsVPS, ConjVPS
      , VPI, MkVPI --, [VPI], BaseVPI, ConsVPI, ConjVPI
      , VP, Tense, Ant, Temp, Pol, Conj -- for VPS
--      , GenRP -- nice to have in the future?
      , S, PredVPS
      , GerundNP
      ]
  ** {
    flags startcat = Rule ;
    cat
      Rule ;
      Question ;
      Cond ;
      [Cond]{2} ;
      Action ;
      Who ;
      [Who]{2} ;
      Subj ;
      Deontic ;

    fun 
-- Application layer
      Regulative : Subj -> Deontic -> Action -> Rule ;
      qWHO : Subj -> Who -> Question ;
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

-- RGL layer

      person
       , organisation
       , agency
       , explanation
       , inaction
       , notification
       , PDPC
       , data_breach
       : CN ;
      public, notifiable : AP ;
      NDB_Qualification : NP ; 
      walk, eat, drink, sing : VP ; -- VP = intransitive verb

      -- PDPA use case
      demand,
      perform : V2 ;
      assess : VS ; 
      occur,
      respond : VP ; -- in corpus, takes oblique complements 
                     -- TODO: create verb subcats from lexicon based on in which context they appear
                     -- "respond" :| []  -> respond : VP 
                     -- "demand" :| [ "an explanation for your inaction" ] -> demand : V2, NP complement
                     -- "assess" :| [ "if it is a Notifiable Data Breach" ] -> assess : VS, S complement
                     -- TODO: is it overkill to have keywords in language? assess,IF,it is a NDB
      ComplV2 : V2 -> NP -> VP ;
      ComplVSif,
      ComplVSthat : VS -> S -> VP ;
      ReferenceNP : NP -> S ; -- it is NP — reference to a previous NP
--      ExpletiveVP : VP -> S ; -- it is raining — dummy subject it (TODO: restrict usage of this and above from HS)

      presentIndicative : Temp ; 
      POS : Pol ;
      NEG : Pol ;

      theSg : Det ;
      thePl : Det ;
      aSg : Det ;
      your : Det ;

      about_Prep : Prep ;
}