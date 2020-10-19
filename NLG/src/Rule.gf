abstract Rule = Action ** {

  flags startcat = Sentence ; -- Phrase ;
  cat
--    Phrase ;

    Sentence; -- Can be a rule, or a definition, any independent sentence.

    Deontic ;

    Party ;
    PartyAlias ;

    -- Action comes from Action
    ActionAlias ; -- VP: "sell a potato" + NP: "the sale"

  fun

    IfThen : Sentence -> Sentence -> Sentence ;

    MAction,
      MActionAlias :
      PartyAlias -> Deontic -> Sentence ; -- the seller must issue the refund within 3 days.

    MDefTerm : Kind -> Term -> Sentence ;

    May,
      Must,
      Shant : ActionAlias -> Deontic ;

    PosPres,  -- For now these are also deontics.
      NegPres,
      PosFut,
      NegFut : ActionAlias -> Deontic ;


    -- Aliases
    AAlias : Term -> Action -> ActionAlias ;
    PAlias : Term -> Party -> PartyAlias ;

    -- Parties
    Nobody,
      Everybody : Party ;

    MkParty : String -> Party ;

    -- Arithmetic operations
  cat
    Relation ; -- TODO: check https://github.com/GrammaticalFramework/gf-contrib/tree/master/mgl/abstract for inspiration
  fun
    Eq,
      Gt,
      Lt,
      GtEq,
      LtEq,
      NEq : Relation ;

    NumberOf : Term -> Term ; -- the # of shares

    NumberOfRel : -- the # of …
      Term ->     -- shares
      Relation -> -- equal to/less than
      Term ->     -- the purchase amount
      Term ;

    Div,
      Mul,
      Add,
      Sub
      : Term -> Term -> Term ; -- the Purchase Amount divided by the Conversion Price

    -- Definitions
  cat
    WhereLimb ; Variable ;
  fun
    ParenDef, -- Cabbage (a vegetable with species Brassica oleracea)
    DefParen  -- A vegetable with species Brassica oleracea ("Cabbage")
      : Variable -> WhereLimb -> Term ;

    -- Individual verbs

    Refund : Action_Dir ;
}
