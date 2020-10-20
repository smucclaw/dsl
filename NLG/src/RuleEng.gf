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
    PartyAlias = {party, alias : NP} ; -- Meng Wong, "the Farmer"

    ActionAlias = LinActionAlias ;
    Deontic = LinDeontic ;
  param
    TnsPol = Present Polarity | Future Polarity ;
  oper
    LinDeontic : Type = {
      action : VPS ; -- TODO: can we safely throw away gerund and actor?
      alias : NP ;
      } ;

    LinActionAlias : Type = {
      action : LinAction ; -- VP: "sell a potato"
      alias : NP           -- NP: "the sale"
      } ;

  lin

    -- : PartyAlias -> Deontic -> Sentence ; -- the seller must issue the refund
    MAction party deontic = mkUtt (PredVPS party.party deontic.action) ;

    -- : PartyAlias -> Deontic -> Sentence ; -- the seller must issue the refund
    MActionAlias party deontic = mkUtt (PredVPS party.alias deontic.action) ;

    --  : Kind -> Term -> Sentence ;
    MDefTermIs kind term = mkUtt (mkCl (defTerm kind) (np term)) ;

    --  : Kind -> Term -> Sentence ;
    MDefTermMeans kind term = mkUtt (mkCl (defTerm kind) WN.mean_3_V2 (np term)) ;

    -- : ActionAlias -> Deontic ;
    May a = a ** {
      action = a.action.s ! PMay} ;
    Must a = a ** {
      action = a.action.s ! PMust} ;
    Shant a = a ** {
      action = a.action.s ! PShant } ;
    -- TODO: is this a good place for these?
    PosPres a = a ** {
      action = a.action.s ! PPres Pos} ;
    NegPres a = a ** {
      action = a.action.s ! PPres Neg} ;
    PosFut a = a ** {
      action = a.action.s ! PFut Pos } ;
    NegFut a = a ** {
      action = a.action.s ! PFut Neg } ;


    -- Aliases
    -- : Term -> Action -> ActionAlias ;
    AAlias alias action = {action = action ; alias = alias} ;
    -- : Term -> Party -> PartyAlias ;
    PAlias alias party = {party = party ; alias = alias} ;

    -- Parties
    Everybody = everybody_NP ;
    Nobody = nobody_NP ;
    MkParty = symb ;

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
    {-
  lincat
    WhereLimb ; Variable ;
  lin
    ParenDef, -- Cabbage (a vegetable with species Brassica oleracea)
    DefParen  -- A vegetable with species Brassica oleracea ("Cabbage")
      : Variable -> WhereLimb -> Term ;
-}

    -- Individual verbs
    Refund =
      mkDir WN.refund_V2 (mkVP WN.issue_1_V2 (mkNP aSg_Det WN.refund_1_N)) ;

}

{-
IMPORT ContractLaw


No person shall sell a cabbage (an item with species Brassica chinensis or Brassica oleracea) except on the day of a full moon, unless the seller has an exemption granted by the Director of Agriculture.

RULE 1 SaleRestricted
PARTY NOBODY AS seller
MAY sell Item AS sale
WHEN Item IS cabbage
UNLESS sale IS onLegalDate // to be defined below
OR UNLIKELY seller HAS Exemption.from ~ [DirectorOfAgriculture]
HENCE ReturnPolicy
LEST VIOLATION // next-state transitions
WHERE Item IS cabbage // "where" syntax borrowed from Haskell
        WHEN Item.species ~ ["Brassica chinensis"|"Brassica oleracea"]
      sale IS onLegalDate
        WHEN sale.date ~ LegalDates
        WHERE LegalDates = external(url="https://www.almanac.com/
                astronomy/moon/full/")
              :en: "on the day of the full moon"


-- purchase
A buyer may return their purchase within three weeks for a refund.

RULE 2 ReturnPolicy
GIVEN sale
PARTY Buyer
MAY return Item
BEFORE sale.date + 3 weeks
HENCE Net3

The refund amount is 90% of the sale price. The seller must issue the refund within 3 days.

RULE 3 Net3
GIVEN return
PARTY Seller
MUST refund Amount
BEFORE return.date + 3 days
WHERE Amount = $return.sale.price * 90%
-}
