abstract Term = {
  cat
    Kind ; -- Not quantified yet
    [Kind]{2} ;
    Term ; -- Subjects, objects, adjuncts
    [Term]{2} ;
    Property ;
    [Property]{2} ;
    Conjunction ;
    Determiner ;
  fun
    And, Or : Conjunction ;
    ConjKind
      : Conjunction -> [Kind] -> Kind ; -- [winding up or dissolution] of the Company
    ConjTerm
      : Conjunction -> [Term] -> Term ;
    ConjProperty                              -- pre-money or post-money
      : Conjunction -> [Property] -> Property ;

    -- Determiners
    ASg,            -- a post-money valuation
    APl,            -- creditors
    TheSg,          -- the company
    ThePl,          -- the companies
    All,            -- all dissolution events
    Any,            -- any liquidation event
    AnyOther,       -- any other liquidation, dissolution or winding up
    Certain,        -- certain additional defined terms
    This, These,    -- These securities
    That, Those
      : Determiner ;


    TDet : Determiner -> Kind -> Term ;

    -- Kinds and Properties
    PNeg : Property -> Property ;             -- not fixed / involuntary
    KProperty : Property -> Kind -> Kind ;    -- voluntary termination

    -- Kinds with complements
    ComplKind : Kind -> Term -> Kind ;    -- liquidation of the company

}
