abstract Term = {
  cat
    Kind ; -- Not quantified yet
    Term ; -- Subjects, objects, adjuncts
    [Term]{2} ;
    Conjunction ;
    Determiner ;
  fun
    And, Or : Conjunction ;
    ConjTerm : Conjunction -> [Term] -> Term ;

    -- Determiners
    ASg,                                     -- a post-money valuation
    APl,                                     -- creditors
    TheSg,                                   -- the company
    ThePl,                                   -- the companies
    All,                                     -- all dissolution events
    Any : Determiner ;                       -- any liquidation event

    TDet : Determiner -> Kind -> Term ;

}
