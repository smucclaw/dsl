abstract Rule = Action ** {

  flags startcat = Sentence ; -- Phrase ;
  cat
--    Phrase ;

    Sentence; -- Can be a rule, or a definition, any independent sentence.

    Deontic ;

    Party ;

    -- Action comes from Action.gf
    ActionAlias ; -- VP: "sell a potato" + NP: "the sale"

  fun
    -- Incantations

    CrossRefDefs : Int -> Sentence ; -- "See Section {#Definitions{%n}} for certain additional defined terms."
    SubjectToTermsBelow : Sentence -> Sentence ;

    -- Sentences
    IfThen : Sentence -> Sentence -> Sentence ;

    MAction : Party -> Deontic -> Sentence ; -- the seller must issue the refund within 3 days.

    MPass : Deontic -> Sentence ; -- the refund must be issued within 3 days.

    MDefTermIs,
      MDefTermMeans : Kind -> Term -> Sentence ;

    MDefProp : Kind -> Property -> Sentence ;

    -- Deontics
    May, MayNot,
      Must,
      Shant : ActionAlias -> Deontic ;

    PosPres,  -- For now these are also deontics.
      NegPres,
      PosPast,
      NegPast,
      PosFut,
      NegFut : ActionAlias -> Deontic ;


    -- Aliases
    AAlias : Action -> ActionAlias ; -- default alias is just the word "action"
    AAliasNamed : Term -> Action -> ActionAlias ;

    -- Parties
    Nobody,
      Everybody : Party ;

    StrParty : String -> Party ;
    MkParty : Term -> Party ;

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

    -- Individual words

    Refund : Action_Dir ;
    DefinedTerm : Kind ;
    Additional : Property ;
}
