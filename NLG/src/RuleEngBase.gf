incomplete concrete RuleEngBase of Rule = ActionEng ** open
  Prelude,
  Syntax,
  Extend in {
  lincat
    Rule = Text ;
    Rulebody = Utt ;

    WithLimb,
      WhenLimb,
      WhereLimb,
      GivenLimb,
      HenceLimb,
      DeadlineLimb = {adv : Adv ; isEmpty : Bool} ;

    DeonticLimb = actionAlias ** {modal : Modal} ;

    Party = PN ;
    PartyAlias = {party : PN ; alias : NP} ; -- Meng Wong, "the Farmer"

    ActionAlias = actionAlias ;

      --    PredType ;

  oper
    actionAlias : Type = {action : LinAction ; alias : NP} ; -- VP: "sell a potato" + NP: "the sale"

  param
    PModal = PMay | PMust | PShant ;

  lin

    -- Name and # of the rule
    -- : Int -> String -> Rulebody -> Rule ;
    MkRule int name body = {
      s = int.s ++ name.s ++ "\\" ++ body.s
      } ;

    -- : GivenLimb -> PartyAlias -> DeonticLimb -> WhenLimb -> DeadlineLimb -> HenceLimb -> WhereLimb -> Rulebody ;
    -- Modal pa de wh dl he wh = TODO

    NoWith,
    NoWhen,
    NoWhere,
    NoHence,
    NoGiven,
    NoDeadline = {adv = emptyAdv ; isEmpty = True} ;


--    Is, Isa, Has : PredType ;


}
