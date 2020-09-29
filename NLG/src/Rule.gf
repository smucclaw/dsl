abstract Rule = Action ** {

  flags startcat = Move ;
  cat
    Move ; -- Can be a rule, or a definition, any independent sentence.

    Deontic ;

    Party ;
    PartyAlias ;

    -- Action comes from Action
    ActionAlias ; -- VP: "sell a potato" + NP: "the sale"

    Temporal ;

  fun
    TPresent  : Temporal ;
    TPast     : Temporal ;
    TFuture   : Temporal ;

    MAction : Temporal ->
      Term -> ActionAlias -> Move ; -- the seller must issue the refund within 3 days.

    MDefTerm : Kind -> Term -> Move ;

    May,
      Must,
      Shant : ActionAlias -> Deontic ;


    -- Aliases
    AAlias : Term -> Action -> ActionAlias ;
    PAlias : Term -> Party -> PartyAlias ;

    -- Definitions

  cat
    WhereLimb ; Variable ;
  fun
    ParenDef, -- Cabbage (a vegetable with species Brassica oleracea)
    DefParen  -- A vegetable with species Brassica oleracea ("Cabbage")
      : Variable -> WhereLimb -> Term ;

}
