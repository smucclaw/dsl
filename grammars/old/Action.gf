abstract Action = Term ** {
  cat
    Action ;           -- A complete action: "raise capital"
    Action_Dir ;       -- Action that is missing a direct object
    Action_Indir ;     -- Action that is missing an indirect object
    Action_Dir_Indir ; -- Action that is missing both

    -- Lists of actions
    [Action]{2} ;             -- sells stock to Acme and raises capital
    [Action_Dir]{2} ;         -- sells today and issues (stock) at fixed valuation
    [Action_Indir]{2} ;       -- sells widgets and issues stock (at fixed valuation)
    [Action_Dir_Indir]{2} ;   -- sells and issues (stock) (at fixed valuation)

    Temporal ;
    ActionGerund ; -- as an argument to Action -> Property or Action -> Term.
                   -- has lost its finite inflection, only gerund left.
  fun
    TPresent  : Temporal ;
    TPast     : Temporal ;
    TFuture   : Temporal ;

    -- Complements
    AComplDir   : Action_Dir -> Term -> Action ;
    AComplIndir : Action_Indir -> Term -> Action ;
    ASlashDir   : Action_Dir_Indir -> Term -> Action_Indir ; -- sell stock (at fixed valuation)
    ASlashIndir : Action_Dir_Indir -> Term -> Action_Dir ;   -- sell (stock) at fixed valuation

    -- Valency changes
    -- TODO: see if we can get rid of SlashDirIndir conjunction ???
    Dat           : Action_Dir -> Action_Dir_Indir ; -- indirect object in dative: sell [stock] [to the Investor]
    ANoComplDir   : Action_Dir -> Action ;   -- refund _ -> "issue a refund" ; return _ -> "return the purchase"
    ANoComplIndir : Action_Indir -> Action ; -- same but for indirect object

    -- Negation regarding the complements
    AComplNoneDir   : Action_Dir -> [Term] -> Action ; -- sells neither X, Y nor Z
    AComplNoneIndir : Action_Indir -> [Term] -> Action ; -- sells (X) neither to B nor to B

    -- Relatives
    RelIndir
      : Term ->       -- the contract
      Term ->         -- the Company
      Temporal ->     -- (will)
      Action_Indir -> -- sell(s) stock (under)
      Term ; -- the contract, under which the company sells stock

    RelDir
      : Term ->       -- the contract
      Term ->         -- the Company
      Temporal ->     -- (will)
      Action_Dir ->   -- sign(s)
      Term ; -- the contract, which the company signs/will sign

    -- Gerund
    PosGerund,
      NegGerund : Action -> ActionGerund ;

    -- Conjunctions
    ConjAction : Conjunction -> [Action] -> Action ;
    ConjSlashDir : Conjunction -> [Action_Dir] -> Action_Dir ;
    ConjSlashIndir : Conjunction -> [Action_Indir] -> Action_Indir ;
    ConjSlashDirIndir : Conjunction -> [Action_Dir_Indir] -> Action_Dir_Indir ;

    -- "X may not be sold, transferred or otherwise pledged or hypothecated"
    OtherwiseAction : Action -> Action ;
    OtherwiseAction_Dir : Action_Dir -> Action_Dir ;
}
