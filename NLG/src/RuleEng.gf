-- concrete RuleEng of RuleEng = RuleEngBase  with
--   (Syntax=SyntaxEng),
--   (Extend=ExtendEng) ** {} ;
concrete RuleEng of Rule = ActionEng ** open
  Prelude,
  SyntaxEng,
  SymbolicEng,
  ExtendEng,
  (P=ParadigmsEng),
  (WN=WordNetEng) in {
  lincat
    Sentence = Utt ;

    Party = NP ;

    ActionAlias = LinActionAlias ;
    Deontic = LinDeontic ;

  oper
    LinDeontic : Type = {
      action : Voice => VPS ;
      alias : NP ;
      passSubject : LinTerm ;
      } ;

    LinActionAlias : Type = {
      s : TenseModPol => Voice => VPS ; -- VP: "sell a potato"
      passSubject : LinTerm ;
      alias : NP                        -- NP: "the sale"
      } ;

  lin
    -- Incantations
    -- : Int -> Sentence ; -- "See Section {#Definitions{%n}} for certain additional defined terms."
    CrossRefDefs int =
      let sectionN : NP = mkNP (mkCN WN.section_1_N (symb int)) ;
          certDefTerms : NP = mkNP certainPl_Det (merge (KProperty additional_Prop DefinedTerm)) ;
          seeTerms : VP = mkVP (mkVP WN.see_1_V2 sectionN) (adv for_Prep certDefTerms) ;
       in mkUtt (mkImp seeTerms) ;

    -- : Sentence -> Sentence ;
    SubjectToTermsBelow sent = sent ** {s = sent.s ++ ", subject to terms below"} ;

    -- Sentences

    -- : Sentence -> Sentence -> Sentence ;
    IfThen if then = mkUtt (mkAdv if_then_Conj <if : Adv> <then : Adv>) ;


    -- : PartyAlias -> Deontic -> Sentence ; -- the seller must issue the refund
    MAction party deontic = mkUtt (PredVPS party (deontic.action ! Active)) ;

    -- : Deontic -> Sentence ;
    MPass deontic = mkUtt (PredVPS deontic.passSubject (deontic.action ! Passive)) ;
    -- : Kind -> Term -> Sentence ;
    MDefTermIs kind term = mkUtt (mkCl (defTerm kind) (np term)) ;

    --  : Kind -> Term -> Sentence ;
    MDefTermMeans kind term = mkUtt (mkCl (defTerm kind) WN.mean_3_V2 (np term)) ;

    -- : Kind -> Property -> Sentence ;
    MDefProp kind prop = mkUtt (mkCl (defTerm kind) (prop ! Pos)) ;

    -- : ActionAlias -> Deontic ;
    May a = a ** {
      action = a.s ! PMay Pos} ;
    MayNot a = a ** {
      action = a.s ! PMay Neg} ;
    Must a = a ** {
      action = a.s ! PMust} ;
    Shant a = a ** {
      action = a.s ! PShant} ;

    -- TODO: is this a good place for these?
    PosPres a = a ** {
      action = a.s ! PPres Pos} ;
    NegPres a = a ** {
      action = a.s ! PPres Neg} ;
    PosPast a = a ** {
      action = a.s ! PPast Pos} ;
    NegPast a = a ** {
      action = a.s ! PPast Neg} ;
    PosFut a = a ** {
      action = a.s ! PFut Pos } ;
    NegFut a = a ** {
      action = a.s ! PFut Neg } ;


    -- Aliases
    -- : Action -> ActionAlias ;
    AAlias action = action ** {alias = mkNP theSg_Det WN.action_1_N} ;

    -- : Term -> Action -> ActionAlias ;
    -- AAliasNamed alias action = action ** {alias = alias} ;

    -- Parties
    Everybody = everybody_NP ;
    Nobody = nobody_NP ;
    MkParty p = p ;
    StrParty = symb ;

    -- Arithmetic operations
  lincat
    Relation = Prep ;
  lin
    -- : Relation ;
    Eq = mkRel "equal to" ;
    Gt = mkRel "greater than" ;
    Lt = mkRel "less than" ;
    GtEq = mkRel "greater than or equal to" ;
    LtEq = mkRel "less than or equal to" ;
    NEq = mkRel "not equal to" ;

    -- : Term -> Term ; -- the # of shares
    NumberOf term = mkNP theSg_Det (mkCN (P.mkN2 WN.number_1_N) (np term)) ;

    -- : Term -> Relation -> Term -> Term -- the # of shares equal to the purchase amount
    NumberOfRel shares equal amount =
      let numShares : Term = NumberOf shares ;
          equalToAmount : Adv = SyntaxEng.mkAdv equal (np amount) ;
       in mkNP numShares equalToAmount ;

    -- : Term -> Term -> Term ; -- the Purchase Amount divided by the Conversion Price
    Div = binOp "divided by" ;
    Mul = binOp "multiplied by" ;
    Add = binOp "added to" ; -- ?
    Sub = binOp "subtracted from" ;
  oper
    binOp : Str -> LinTerm -> LinTerm -> LinTerm = \divby,a,b ->
      mkNP (np a) (SyntaxEng.mkAdv (P.mkPrep divby) (np b)) ;

    mkRel = P.mkPrep ;

    -- Definitions

  -- lincat
  --   WhereLimb ; Variable ;
  -- lin
  --   ParenDef, -- Cabbage (a vegetable with species Brassica oleracea)
  --   DefParen  -- A vegetable with species Brassica oleracea ("Cabbage")
  --     : Variable -> WhereLimb -> Term ;

  -- Individual words
  lin
    Refund =
      mkDir WN.refund_V2 (mkVP WN.issue_1_V2 (mkNP aSg_Det WN.refund_1_N)) ;
    DefinedTerm = kind "defined term" ;
    Additional = additional_Prop ;
  oper

    additional_Prop : LinProp = prop "additional" ;
}
