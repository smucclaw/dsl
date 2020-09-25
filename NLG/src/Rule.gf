abstract Rule = Action ** {

  flags startcat = Rule ;
  cat
    Rule ;
    Rulebody ;

    GivenLimb ;
    WithLimb ;
    WhenLimb ;
    WhereLimb ;
    DeadlineLimb ;
    HenceLimb ;
    DeonticLimb ;

    Party ;
    PartyAlias ;

    -- Action comes from Action
    ActionAlias ; -- VP: "sell a potato" + NP: "the sale"

    PredType ;

  fun

    -- Name and # of the rule
    MkRule : Int -> String -> Rulebody -> Rule ;

    --
    Modal : GivenLimb -> PartyAlias -> DeonticLimb -> WhenLimb -> DeadlineLimb -> HenceLimb -> WhereLimb -> Rulebody ;

    -----
    Is, Isa, Has : PredType ;


    -- Limbs
    NoWith : WithLimb ;
    NoWhen : WhenLimb ;
    NoWhere : WhereLimb ;
    NoHence : HenceLimb ;
    NoGiven : GivenLimb ;
    NoDeadline : DeadlineLimb ;

}
